use std::cell::RefCell;
use std::io::{BufRead, BufReader, BufWriter, Write};
use std::net::{Ipv6Addr, SocketAddr};
use std::rc::Rc;
use std::time::Duration;

use corona::prelude::*;
use corona::io::BlockingWrapper;
use failure::Error;
use log::{debug, error, info, warn};
use serde::{Deserialize, Serialize};
use tokio::clock;
use tokio::net::{TcpListener, TcpStream};
use tokio::prelude::*;
use tokio::timer::{Delay, Interval};

use crate::game::Game;
use crate::state::{Command, Player};

const ACCEPT_ERR_SLEEP: Duration = Duration::from_millis(100);
const GAME_ROUND: Duration = Duration::from_secs(3);

#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case", tag = "command")]
enum Hello {
    Join {
        name: String,
    },
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "kebab-case")]
struct Info {
    player: Player,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "kebab-case")]
struct UserError {
    error: &'static str,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
struct Commands {
    round: usize,
    #[serde(default)]
    commands: Vec<Command>,
}

type PGame = Rc<RefCell<Game>>;

fn send<W, M>(mut w: W, msg: &M) -> Result<(), Error>
where
    W: Write,
    M: Serialize,
{
    serde_json::to_writer(&mut w, msg)?;
    writeln!(w)?;
    w.flush()?;
    Ok(())
}

fn send_updates<W: Write>(game: PGame, mut w: W) -> Result<(), Error> {
    let waker = game.borrow_mut().waker();
    let mut last_gen = None;

    for () in waker.iter_ok() {
        let state = game.borrow().state();
        let gen = state.generation();
        if last_gen != Some(gen) {
            last_gen = Some(gen);
            writeln!(w, "{}", state.serialized())?;
            w.flush()?;
        }
    }

    Ok(())
}

fn play_game<R, W>(game: PGame, name: String, addr: SocketAddr, r: R, mut w: W)
    -> Result<(), Error>
where
    R: BufRead,
    W: Write + 'static,
{
    let player = game.borrow_mut().register(name);

    let player = match player {
        Some(player) => player,
        None => {
            send(&mut w, &UserError { error: "Game already started" })?;
            return Ok(())
        }
    };

    send(&mut w, &Info { player })?;
    corona::spawn({
        let game = Rc::clone(&game);
        move || {
            if let Err(e) = send_updates(game, w) {
                error!("Failed to send updates to {}: {}", addr, e);
            } else {
                info!("Terminate sending to {}", addr);
            }
        }
    });

    for line in r.lines() {
        let line = line?;
        let cmd = serde_json::from_str::<Commands>(&line)?;
        drop(line);
        game.borrow_mut().submit(cmd.round, player, cmd.commands);
    }

    Ok(())
}

fn handle_client_inner<R, W>(game: PGame, addr: SocketAddr, mut r: R, w: W) -> Result<(), Error>
where
    R: BufRead,
    W: Write + 'static,
{
    // TODO: Send list of games, etc.
    let mut line = String::new();
    r.read_line(&mut line)?;
    let hello = serde_json::from_str::<Hello>(&line)?;
    drop(line);
    debug!("Received hello {:?} from {}", hello, addr);

    match hello {
        Hello::Join { name } => play_game(game, name, addr, r, w)?,
    }

    Ok(())
}

fn handle_client(game: PGame, client: TcpStream) {
    let addr = client.peer_addr().unwrap();
    let (read, write) = client.split();
    let read = BufReader::new(BlockingWrapper::new(read));
    let write = BufWriter::new(BlockingWrapper::new(write));

    if let Err(e) = handle_client_inner(game, addr, read, write) {
        error!("Client {} dropped: {}", addr, e);
    }
}

fn run_game(game: PGame) {
    let interval = Interval::new_interval(GAME_ROUND);

    for _ in interval.iter_ok() {
        game.borrow_mut().round();
    }
}

pub(crate) fn run(port: u16, no_players: usize) -> Result<(), Error> {
    let listener = TcpListener::bind(&(Ipv6Addr::UNSPECIFIED, port).into())?;

    let game = Rc::new(RefCell::new(Game::new(no_players)));

    corona::spawn({
        let game = Rc::clone(&game);
        || run_game(game)
    });

    for connection in listener.incoming().iter_result() {
        match connection {
            Err(e) => {
                warn!("Failed to accept connection: {}", e);
                Delay::new(clock::now() + ACCEPT_ERR_SLEEP);
            }
            Ok(connection) => {
                info!("Accepted connection from {}", connection.peer_addr().unwrap());
                let game = Rc::clone(&game);
                corona::spawn(|| handle_client(game, connection));
            }
        }
    }
    Ok(())
}
