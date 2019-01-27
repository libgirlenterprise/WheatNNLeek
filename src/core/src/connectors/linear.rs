// Copyright (c) 2019 Libgirl
//
// Released under Apache 2.0 license as described in the file LICENSE.txt.

use crate::connection_supervisor::ConnectionSupervisor;
use crate::connections::Connection;
use crate::connectors::Connector as CommonConnector;
use crate::populations::Population;
use crate::Num;

pub struct Connector {}

impl Default for Connector {
    fn default() -> Connector {
        Connector {}
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
        let mutual_size = std::cmp::min(pre.size(), post.size());
        for i in 0..mutual_size {
            let id = connection_supervisor.add_connection(
                pre.get(i).unwrap(),
                post.get(i).unwrap(),
                syn,
            );
            v.push(id);
        }
        v
    }
}
