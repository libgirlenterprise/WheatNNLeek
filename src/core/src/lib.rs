// Copyright (c) 2019 Libgirl
//
// Released under Apache 2.0 license as described in the file LICENSE.txt.

#[macro_use]
extern crate custom_derive;
#[macro_use]
extern crate enum_derive;
#[macro_use]
extern crate serde_derive;
extern crate lazy_static;

extern crate rand;

use std::collections::HashMap;

pub mod connection_supervisor;
pub mod connections;
pub mod connectors;
pub mod events;
pub mod models;
pub mod network;
pub mod node;
mod ode;
pub mod populations;
mod utils;
pub mod ffi;

pub type Num = usize;
pub type Double = f64;
pub type Time = f64;
pub type Index = i64;
pub type Parameters = HashMap<String, f64>;
