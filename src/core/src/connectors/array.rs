// Copyright (c) 2019 Libgirl
//
// Released under Apache 2.0 license as described in the file LICENSE.txt.

use connection_supervisor::ConnectionSupervisor;
use connections::Connection;
use connectors::Connector as CommonConnector;
use populations::Population;
use Num;

pub struct Connector {
    connections_: Vec<u8>,
}

impl Connector {
    pub fn new(connections: &[u8]) -> Connector {
        Connector {
            connections_: connections.to_vec(),
        }
    }
}

impl CommonConnector for Connector {
    fn connect(
        &self,
        pre: &Population,
        post: &Population,
        syn: &Connection,
        connection_supervisor: &mut ConnectionSupervisor,
    ) -> Vec<Num> {
        let post_size = post.size();

        let mut v: Vec<Num> = Vec::new();
        let mut i = 0;
        for pre_id in pre.iter() {
            let mut j = 0;
            for post_id in post.iter() {
                if self.connections_[i * post_size + j] != b'0' {
                    let id = connection_supervisor.add_connection(pre_id, post_id, syn);
                    v.push(id);
                }
                j += 1;
            }
            i += 1;
        }
        v
    }
}
