use std::collections::HashMap;
use std::rc::Rc;

use futures::unsync::mpsc::{self, Sender, Receiver};
use futures::Sink;
use log::{debug, info, warn};
use serde::Serialize;

use crate::state::{Command, GameState as GameLogic, Player, PlayerAction, State};

pub(crate) type Waker = Receiver<()>;

pub(crate) struct GameState {
    generation: u64,
    serialized: String,
}

impl GameState {
    pub(crate) fn generation(&self) -> u64 {
        self.generation
    }
    pub(crate) fn serialized(&self) -> &str {
        &self.serialized
    }
}

enum Phase {
    Waiting {
        no_players: usize,
    },
    Playing {
        game: GameLogic,
    },
    Done {
        winner: Option<Player>,
    }
}

#[derive(Serialize)]
#[serde(rename_all = "kebab-case")]
struct PlayerInfo {
    name: String,
    #[serde(skip)]
    commands: Vec<Command>,
}

type Players = HashMap<Player, PlayerInfo>;

#[derive(Serialize)]
#[serde(rename_all = "kebab-case", untagged)]
enum SerializedGame<'a> {
    Waiting {
        players: &'a Players,
        missing: usize,
    },
    Playing {
        players: &'a Players,
        round: usize,
        state: &'a State,
    },
    Done {
        winner: Option<&'a str>,
    }
}

pub(crate) struct Game {
    wakers: Vec<Sender<()>>,
    last_player_id: Player,
    phase: Phase,
    players: Players,
    game_state: Rc<GameState>,
}

impl Game {
    pub(crate) fn new(no_players: usize) -> Self {
        Game {
            wakers: Vec::new(),
            last_player_id: Player(0),
            phase: Phase::Waiting { no_players },
            players: HashMap::new(),
            game_state: Rc::new(GameState {
                generation: 0,
                serialized: "{}".to_owned(),
            }),
        }
    }

    pub(crate) fn register(&mut self, name: String) -> Option<Player> {
        match self.phase {
            Phase::Waiting { .. } => (),
            _ => return None,
        }

        self.last_player_id.0 += 1;
        let player = self.last_player_id;
        self.players.insert(player, PlayerInfo {
            name,
            commands: Vec::new(),
        });

        self.next_gen();

        Some(player)
    }

    fn serialized(&self) -> SerializedGame<'_> {
        match &self.phase {
            Phase::Waiting { no_players } => SerializedGame::Waiting {
                players: &self.players,
                missing: no_players.checked_sub(self.players.len()).unwrap_or(0),
            },
            Phase::Playing { game } => SerializedGame::Playing {
                players: &self.players,
                round: game.current_round(),
                state: game.state(),
            },
            Phase::Done { winner } => SerializedGame::Done {
                winner: winner.as_ref().map(|p| &self.players[p].name as &str),
            }
        }
    }

    fn next_gen(&mut self) {
        let serialized = serde_json::to_string(&self.serialized()).unwrap();
        self.game_state = Rc::new(GameState {
            generation: self.game_state.generation + 1,
            serialized,
        });
        debug!("Notifying game generation {}", self.game_state.generation);
        self.notify_all();
    }

    pub(crate) fn waker(&mut self) -> Waker {
        let (mut sender, receiver) = mpsc::channel(1);
        assert!(sender.start_send(()).unwrap().is_ready());
        self.wakers.push(sender);
        receiver
    }

    fn notify_all(&mut self) {
        debug!("Notify all");
        let wakers = self
            .wakers
            .drain(..)
            // We abuse the mpsc implementation detail that it does *not* need poll_complete.
            // Unfortunately, these things don't have try_send ☹.
            .filter_map(|mut w| match w.start_send(()) {
                Err(_) => {
                    debug!("Waker dropped");
                    None
                },
                Ok(_) => Some(w),
            })
            .collect();
        self.wakers = wakers;
    }

    pub(crate) fn state(&self) -> Rc<GameState> {
        Rc::clone(&self.game_state)
    }

    pub(crate) fn submit(&mut self, round: usize, player: Player, commands: Vec<Command>) {
        match &self.phase {
            Phase::Playing { game } if game.current_round() == round => {
                self.players.get_mut(&player).unwrap().commands = commands;
            },
            Phase::Playing { .. } => {
                warn!("Player {} sent commands out of sync", self.players[&player].name);
            },
            _ => warn!("Game is not running, can't submit commands"),
        }
    }

    pub(crate) fn round(&mut self) {
        match &mut self.phase {
            Phase::Waiting { no_players } if *no_players <= self.players.len() => {
                info!("Starting game with {} players", no_players);
                self.phase = Phase::Playing {
                    game: GameLogic::start(self.players.keys().cloned()),
                }
            }
            Phase::Waiting { .. } => return,
            Phase::Playing { game } => {
                let commands = self
                    .players
                    .iter_mut()
                    .flat_map(|(player, cmds)| {
                        cmds
                            .commands
                            .drain(..)
                            .map(move |cmd| PlayerAction {
                                player: *player,
                                command: cmd,
                            })
                    });
                let new_game = game.round(commands);

                match new_game.winner() {
                    Ok(winner) => {
                        info!("Game terminated");
                        self.phase = Phase::Done { winner };
                    },
                    Err(_) => *game = new_game,
                }
            }
            Phase::Done { .. } => return,
        }

        self.next_gen();
    }
}
