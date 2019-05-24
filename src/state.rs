use std::collections::HashMap;
use std::iter;
use std::ops::Add;
use std::sync::Arc;

use either::Either;
use itertools::{EitherOrBoth, Itertools};
use rand::Rng;
use rand::distributions::Distribution;

/// What we measure amounts in.
///
/// Usually amount of food or how fat one cell is.
pub type Mass = f64;

pub type Coord = i32;
pub const DIMENSIONS: usize = 2;

pub type Move = [i8; DIMENSIONS];

// TODO: Make this into a Simd?
#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
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

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Player(u16);

#[derive(Debug)]
pub struct Food(Mass);

#[derive(Copy, Clone, Debug)]
pub struct Cell {
    mass: Mass,
    owner: Player,
}

#[derive(Debug)]
pub enum Field {
    Food(Food),
    Cell(Cell),
}

#[derive(Debug)]
pub struct StateField {
    position: Coords,
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

pub struct PlayerAction {
    field: Coords,
    player: Player,
    action: Action,
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

pub struct Rules {
    start_cell_cnt: usize,
    start_food_cnt: Box<Dist<usize>>,
    round_food_cnt: Box<Dist<usize>>,
    area: Box<Dist<Coords>>,
    food_weight: MassDist,
    cell_weight: MassDist,
    burst_limit: MassDist,
    move_cost: MassDist,
    rest_cost: MassDist,
    reproduce_cost: MassDist,
}

pub struct GameState {
    rules: Arc<Rules>,
    round: usize,
    state: State,
}

impl GameState {
    fn field_action(&self, field: &StateField, action: Action) -> impl Iterator<Item = StateField> {
        let cell = match &field.field {
            Field::Cell(cell) => cell,
            _ => unreachable!(),
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

    fn new_food(&self) -> impl Iterator<Item = StateField> {
        iter::repeat_with(|| StateField {
                field: Field::Food(Food(self.rules.food_weight.gen())),
                position: self.rules.area.gen(),
            })
            .take(self.rules.round_food_cnt.gen())
            .sorted_by_key(|f| f.position)
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
            .filter(|pa| pa.action.valid())
            .filter(|PlayerAction { field, player, .. }| ownership.get(field) == Some(player))
            .sorted_by_key(|a| a.field)
            .collect_vec();
        actions.dedup_by_key(|a| a.field);
        actions
    }

    fn check_burst(&self, field: StateField) -> StateField {
        match field.field {
            Field::Cell(Cell { mass, .. }) if mass > self.rules.burst_limit.gen() => StateField {
                field: Field::Food(Food(mass)),
                ..field
            },
            _ => field,
        }
    }

    #[allow(clippy::float_cmp)] // We are actually OK with the comparison ending up randomly
    fn consolidate<F: IntoIterator<Item = StateField>>(&self, pos: Coords, field: F) -> StateField {
        let mut total_mass = 0.0;
        let mut best_cell = 0.0;
        let mut best_owner = None;
        for content in field {
            match content.field {
                Field::Food(Food(mass)) => total_mass += mass,
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
            None => Field::Food(Food(total_mass)),
        };

        StateField {
            field,
            position: pos,
        }
    }

    fn round<A: IntoIterator<Item = PlayerAction>>(&self, actions: A) -> GameState {
        self.state.sanity_check();

        let actions = self.valid_actions(actions);

        let new_positions = self
            .state
            .fields
            .iter()
            .merge_join_by(actions, |s, a| s.position.cmp(&a.field))
            .flat_map(|position| match position {
                EitherOrBoth::Left(f) => Either::Left(self.field_action(f, Action::Rest)),
                // Command to non-existing field → ignore
                EitherOrBoth::Right(_) => Either::Right(iter::empty()),
                EitherOrBoth::Both(f, a) => Either::Left(self.field_action(f, a.action)),
            })
            .filter(|a| !a.starved())
            .merge_by(self.new_food(), |a, b| a.position <= b.position)
            .group_by(|f| f.position)
            .into_iter()
            .map(|(pos, f)| self.consolidate(pos, f))
            .map(|f| self.check_burst(f))
            .collect();

        let state = State { fields: new_positions };
        state.sanity_check();

        GameState {
            rules: Arc::clone(&self.rules),
            round: self.round + 1,
            state,
        }
    }
}
