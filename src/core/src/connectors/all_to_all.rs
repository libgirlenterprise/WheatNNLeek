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
        for i in pre.iter() {
            for j in post.iter() {
                let id = connection_supervisor.add_connection(i, j, syn);
                v.push(id);
            }
        }
        v
    }
}
