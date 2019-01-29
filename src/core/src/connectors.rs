// Copyright (c) 2019 Libgirl
//
// Released under Apache 2.0 license as described in the file LICENSE.txt.

use crate::connection_supervisor::ConnectionSupervisor;
use crate::connections::Connection;
use crate::populations::Population;
use crate::Num;

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
pub mod all_to_all_except_diagonal;
pub mod array;
pub mod linear;
