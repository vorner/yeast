use corona::coroutine::{CleanupStrategy, Coroutine};
use failure::Error;
use log::error;

mod server;
mod state;

const STACK_SIZE: usize = 12 * 4096;

fn run() -> Result<(), Error> {
    let mut coro = Coroutine::new();
    coro.stack_size(STACK_SIZE);
    coro.cleanup_strategy(CleanupStrategy::AbortOnPanic);
    coro.verify()?;
    coro.run(|| server::run(12345)).unwrap()
}

fn main() {
    env_logger::init();
    if let Err(err) = run() {
        error!("{}", err);
        std::process::exit(1);
    }
}
