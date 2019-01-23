// Copyright (c) 2019 Libgirl
//
// Released under Apache 2.0 license as described in the file LICENSE.txt.

use connection_supervisor::ConnectionSupervisor;
use connections::Connection;
use populations::Population;
use Num;

pub trait Connector {
    fn connect(
        &self,
        pre: &Population,
        post: &Population,
        syn: &Connection,
        connection_supervisor: &mut ConnectionSupervisor,
    ) -> Vec<Num>;
}

pub mod all_to_all;
pub mod i_to_j;
pub mod all_to_all_except_diagonal;
pub mod linear;
