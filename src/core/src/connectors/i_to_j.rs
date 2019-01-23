// Copyright (c) 2019 Libgirl
//
// Released under Apache 2.0 license as described in the file LICENSE.txt.

use connection_supervisor::ConnectionSupervisor;
use connections::Connection;
use connectors::Connector as CommonConnector;
use populations::Population;
use Num;

pub struct Connector {
    source_index: usize,
    target_index: usize,
}

impl Connector {
    fn new (source_index: usize, target_index: usize) -> Connector {
        Connector {
            source_index: source_index,
            target_index: target_index,
        }
    }
}

impl Default for Connector {
    fn default() -> Connector {
        Connector::new(0, 0)
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
        let mut v: Vec<Num> = Vec::new();
        let id = connection_supervisor.add_connection(
            pre.get(self.source_index).unwrap(),
            post.get(self.target_index).unwrap(),
            syn,
        );
        v.push(id);
        v
    }
}
