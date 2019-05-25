use std::io::{BufRead, BufReader, Write};
use std::net::{Ipv6Addr, SocketAddr};
use std::time::Duration;

use corona::prelude::*;
use corona::io::BlockingWrapper;
use failure::Error;
use log::{debug, error, info, warn};
use serde::Deserialize;
use tokio::clock;
use tokio::net::{TcpListener, TcpStream};
use tokio::prelude::*;
use tokio::timer::Delay;

const ACCEPT_ERR_SLEEP: Duration = Duration::from_millis(100);

#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case", tag = "command")]
enum Hello {
    Join {
        name: String,
    },
}

fn handle_client_inner<R, W>(addr: SocketAddr, mut r: R, w: W) -> Result<(), Error>
where
    R: BufRead,
    W: Write,
{
    // TODO: Send list of games, etc.
    let mut line = String::new();
    r.read_line(&mut line)?;
    let hello = serde_json::from_str::<Hello>(&line)?;
    debug!("Received hello {:?} from {}", hello, addr);
    Ok(())
}

fn handle_client(client: TcpStream) {
    let addr = client.peer_addr().unwrap();
    let (read, write) = client.split();
    let read = BufReader::new(BlockingWrapper::new(read));
    let write = BlockingWrapper::new(write);

    if let Err(e) = handle_client_inner(addr, read, write) {
        error!("Client {} dropped: {}", addr, e);
    }
}

pub(crate) fn run(port: u16) -> Result<(), Error> {
    let listener = TcpListener::bind(&(Ipv6Addr::UNSPECIFIED, port).into())?;

    for connection in listener.incoming().iter_result() {
        match connection {
            Err(e) => {
                warn!("Failed to accept connection: {}", e);
                Delay::new(clock::now() + ACCEPT_ERR_SLEEP);
            }
            Ok(connection) => {
                info!("Accepted connection from {}", connection.peer_addr().unwrap());
                corona::spawn(|| handle_client(connection));
            }
        }
    }
    Ok(())
}
