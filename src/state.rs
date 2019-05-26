use std::collections::{HashMap, HashSet};
use std::iter;
use std::ops::Add;
use std::rc::Rc;

use either::Either;
use itertools::{iproduct, EitherOrBoth, Itertools};
use rand::Rng;
use rand::distributions::{Distribution, Normal, Uniform};
use rand::distributions::uniform::SampleUniform;
use serde::{Deserialize, Serialize};

/// What we measure amounts in.
///
/// Usually amount of food or how fat one cell is.
pub type Mass = f64;

pub type Coord = i32;
pub const DIMENSIONS: usize = 2;

pub type Move = [i8; DIMENSIONS];

// TODO: Make this into a Simd?
#[derive(Copy, Clone, Debug, Deserialize, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize)]
#[serde(transparent)]
pub struct Coords([Coord; DIMENSIONS]);

impl Add<Move> for Coords {
    type Output = Self;
    fn add(self, other: Move) -> Self {
        // We wrap the world around so the cells don't fall off the edge. But we don't *actually*
        // expect anyone to reach it, it's far away from where the cells were born and where the
        // food appears.
        Coords([
            self.0[0].wrapping_add(Coord::from(other[0])),
            self.0[1].wrapping_add(Coord::from(other[1])),
        ])
    }
}

struct CoordDist<D>(D);

impl<D> Distribution<Coords> for CoordDist<D>
where
    D: Distribution<Coord>,
{
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> Coords {
        let mut res: [Coord; DIMENSIONS] = Default::default();
        for c in &mut res {
            *c = self.0.sample(rng);
        }
        Coords(res)
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Serialize, Hash)]
#[serde(transparent)]
pub struct Player(pub(crate) u16);

#[derive(Copy, Clone, Debug, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct Food {
    mass: Mass,
}

#[derive(Copy, Clone, Debug, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct Cell {
    mass: Mass,
    owner: Player,
}

#[derive(Copy, Clone, Debug, Serialize)]
#[serde(rename_all = "kebab-case", untagged)]
pub enum Field {
    Food(Food),
    Cell(Cell),
}

#[derive(Copy, Clone, Debug, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct StateField {
    position: Coords,
    #[serde(flatten)]
    field: Field,
}

impl StateField {
    fn starved(&self) -> bool {
        match self.field {
            Field::Cell(Cell { mass, .. }) if mass <= 0.0 => true,
            _ => false,
        }
    }
}

#[derive(Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct State {
    fields: Vec<StateField>,
}

impl State {
    fn sanity_check(&self) {
        for (l, r) in self.fields.iter().tuple_windows() {
            // The state is sorted and without any duplicates (eg. a field is not occupied twice)
            assert!(l.position < r.position);
        }
    }
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case", tag = "action")]
pub enum Action {
    Move {
        direction: Move,
    },
    Reproduce {
        direction: Move,
    },
    Rest,
}

impl Action {
    fn valid(&self) -> bool {
        let is_one = |d: Move| {
            d.iter().filter(|v| v.abs() == 1).count() == 1
                && d.iter().all(|v| v.abs() <= 1)
        };
        match self {
            Action::Move { direction } => is_one(*direction),
            Action::Reproduce { direction } => is_one(*direction),
            Action::Rest => true
        }
    }
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Command {
    pub field: Coords,
    #[serde(flatten)]
    pub action: Action,
}

pub struct PlayerAction {
    pub(crate) player: Player,
    pub(crate) command: Command,
}

trait Dist<T>: Send + Sync {
    fn gen(&self) -> T;
}

impl<T, D> Dist<T> for D
where
    D: Distribution<T> + Send + Sync,
{
    fn gen(&self) -> T {
        self.sample(&mut rand::thread_rng())
    }
}

type MassDist = Box<Dist<Mass>>;

/// Makes sure the returned values are positive.
struct Correct<D>(D);

impl<D> Distribution<Mass> for Correct<D>
where
    D: Distribution<Mass>
{
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> Mass {
        loop {
            let attempt = self.0.sample(rng);
            if attempt > 0.0 {
                return attempt;
            }
        }
    }
}

pub struct Rules {
    start_cell_cnt: usize,
    start_food_cnt: usize,
    round_food_cnt: Box<Dist<usize>>,
    area: Box<Dist<Coords>>,
    food_weight: MassDist,
    cell_weight: MassDist,
    burst_limit: MassDist,
    move_cost: MassDist,
    rest_cost: MassDist,
    reproduce_cost: MassDist,
}

impl Rules {
    fn generate() -> Self {
        fn rng<T: SampleUniform>(low: T, high: T) -> T {
            Uniform::new_inclusive(low, high).sample(&mut rand::thread_rng())
        }
        fn mass(mean_low: Mass, mean_high: Mass, dev_low: Mass, dev_high: Mass) -> MassDist {
            let mean = rng(mean_low, mean_high);
            let dev = rng(dev_low, dev_high);
            let dist = Normal::new(mean, dev);
            Box::new(Correct(dist))
        }
        // TODO: The values here definitely need tuning. And probably reading from config file too!
        let food_low = rng(30, 50);
        let food_rnd = rng(15, 30);
        let area_size: Coord = rng(300, 600);
        Self {
            start_cell_cnt: rng(50, 201),
            start_food_cnt: rng(200, 500),
            round_food_cnt: Box::new(Uniform::new_inclusive(food_low, food_low + food_rnd)),
            area: Box::new(CoordDist(Uniform::new_inclusive(-area_size, area_size))),
            food_weight: mass(20.0, 30.0, 3.0, 8.0),
            cell_weight: mass(100.0, 120.0, 16.0, 24.0),
            burst_limit: mass(500.0, 550.0, 10.0, 20.0),
            move_cost: mass(1.0, 3.0, 0.3, 1.0),
            rest_cost: mass(0.1, 1.0, 0.1, 0.5),
            reproduce_cost: mass(20.0, 50.0, 10.0, 20.0),
        }
    }

    fn new_food(&self, cnt: usize) -> impl Iterator<Item = StateField> {
        iter::repeat_with(|| StateField {
                field: Field::Food(Food {
                    mass: self.food_weight.gen(),
                }),
                position: self.area.gen(),
            })
            .take(cnt)
            .sorted_by_key(|f| f.position)
    }
}

pub(crate) struct StillPlaying;

pub struct GameState {
    rules: Rc<Rules>,
    round: usize,
    state: State,
}

impl GameState {
    fn field_action(&self, field: &StateField, action: Action) -> impl Iterator<Item = StateField> {
        let cell = match &field.field {
            Field::Cell(cell) => cell,
            Field::Food(_) => return Either::Left(iter::once(*field)),
        };

        match action {
            Action::Rest => Either::Left(iter::once(StateField {
                field: Field::Cell(Cell {
                    mass: cell.mass - self.rules.rest_cost.gen(),
                    owner: cell.owner,
                }),
                position: field.position,
            })),
            Action::Move { direction } => Either::Left(iter::once(StateField {
                field: Field::Cell(Cell {
                    mass: cell.mass - self.rules.move_cost.gen(),
                    owner: cell.owner,
                }),
                position: field.position + direction,
            })),
            Action::Reproduce { direction } => {
                let mass = cell.mass - self.rules.reproduce_cost.gen();
                let mass = mass / 2.0;
                let cell = Cell { mass, owner: cell.owner };
                Either::Right(
                    iter::once(StateField {
                        field: Field::Cell(cell),
                        position: field.position,
                    }).chain(iter::once(StateField {
                        field: Field::Cell(cell),
                        position: field.position + direction,
                    }))
                )
            }
        }
    }

    fn valid_actions<A: IntoIterator<Item = PlayerAction>>(&self, actions: A) -> impl IntoIterator<Item = PlayerAction> {
        let ownership = self
            .state
            .fields
            .iter()
            .filter_map(|f| match f.field {
                Field::Cell(Cell { owner, .. }) => Some((f.position, owner)),
                _ => None,
            })
            .collect::<HashMap<_, _>>();

        let mut actions = actions
            .into_iter()
            .filter(|pa| pa.command.action.valid())
            .filter(|PlayerAction { command, player }| ownership.get(&command.field) == Some(player))
            .sorted_by_key(|a| a.command.field)
            .collect_vec();
        actions.dedup_by_key(|a| a.command.field);
        actions
    }

    fn check_burst(&self, field: StateField) -> StateField {
        match field.field {
            Field::Cell(Cell { mass, .. }) if mass > self.rules.burst_limit.gen() => StateField {
                field: Field::Food(Food { mass }),
                ..field
            },
            _ => field,
        }
    }

    #[allow(clippy::float_cmp)] // We are actually OK with the comparison ending up randomly
    fn consolidate<F: IntoIterator<Item = StateField>>(pos: Coords, field: F) -> StateField {
        let mut total_mass = 0.0;
        let mut best_cell = 0.0;
        let mut best_owner = None;
        for content in field {
            match content.field {
                Field::Food(Food { mass }) => total_mass += mass,
                Field::Cell(Cell { mass, owner }) => {
                    if mass > best_cell || (mass == best_cell && rand::thread_rng().gen()) {
                        best_cell = mass;
                        best_owner = Some(owner);
                    }
                    total_mass += mass;
                }
            }
        }

        let field = match best_owner {
            Some(owner) => Field::Cell(Cell {
                mass: total_mass,
                owner,
            }),
            None => Field::Food(Food { mass: total_mass }),
        };

        StateField {
            field,
            position: pos,
        }
    }

    pub(crate) fn round<A: IntoIterator<Item = PlayerAction>>(&self, actions: A) -> GameState {
        self.state.sanity_check();

        let actions = self.valid_actions(actions);

        let new_positions = self
            .state
            .fields
            .iter()
            .merge_join_by(actions, |s, a| s.position.cmp(&a.command.field))
            .flat_map(|position| match position {
                EitherOrBoth::Left(f) => Either::Left(self.field_action(f, Action::Rest)),
                // Command to non-existing field → ignore
                EitherOrBoth::Right(_) => Either::Right(iter::empty()),
                EitherOrBoth::Both(f, a) => Either::Left(self.field_action(f, a.command.action)),
            })
            .filter(|a| !a.starved())
            .merge_by(
                self.rules.new_food(self.rules.round_food_cnt.gen()),
                |a, b| a.position <= b.position,
            )
            .group_by(|f| f.position)
            .into_iter()
            .map(|(pos, f)| Self::consolidate(pos, f))
            .map(|f| self.check_burst(f))
            .collect();

        let state = State { fields: new_positions };
        state.sanity_check();

        GameState {
            rules: Rc::clone(&self.rules),
            round: self.round + 1,
            state,
        }
    }

    pub(crate) fn state(&self) -> &State {
        &self.state
    }

    pub(crate) fn start<P: IntoIterator<Item = Player>>(players: P) -> GameState {
        let rules = Rules::generate();
        let mut fields = rules
            .new_food(rules.start_food_cnt)
            .group_by(|f| f.position)
            .into_iter()
            .map(|(pos, f)| Self::consolidate(pos, f))
            .collect_vec();
        let mut taken = fields
            .iter()
            .map(|f| f.position)
            .collect::<HashSet<_>>();

        let players = players.into_iter().collect_vec();
        for (_, &owner) in iproduct!(0..rules.start_cell_cnt, &players) {
            // Take the first yet unused field
            // FIXME: Make sure we don't cycle here, that there are some free spots left.
            let position = iter::repeat_with(|| rules.area.gen())
                .filter(|coords| !taken.contains(coords))
                .nth(0)
                .unwrap();
            taken.insert(position);
            let mass = rules.cell_weight.gen();
            fields.push(StateField {
                position,
                field: Field::Cell(Cell{
                    mass,
                    owner,
                }),
            });
        }
        fields.sort_by_key(|f| f.position);
        let me = Self {
            rules: Rc::new(rules),
            round: 1,
            state: State {
                fields,
            },
        };
        me.state.sanity_check();
        me
    }

    pub(crate) fn current_round(&self) -> usize {
        self.round
    }

    pub(crate) fn winner(&self) -> Result<Option<Player>, StillPlaying> {
        let alive = self
            .state
            .fields
            .iter()
            .filter_map(|f| match f.field {
                Field::Cell(Cell { owner, .. }) => Some(owner),
                _ => None,
            })
            .dedup()
            .collect_vec();
        if alive.len() <= 1 {
            Ok(alive.get(0).cloned())
        } else {
            Err(StillPlaying)
        }
    }
}
