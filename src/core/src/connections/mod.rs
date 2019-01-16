// Copyright (c) 2019 Libgirl
//
// Released under Apache 2.0 license as described in the file LICENSE.txt.

use std::fmt::Debug;
use {Double, Index};

#[derive(Clone, Copy, Debug)]
pub enum PostSynapticEffect {
    Excitatory,
    Inhibitory,
}

pub trait Connection: ConnectionClone + Debug + std::marker::Send {
    fn source(&self) -> Index;
    fn set_source(&mut self, s: Index);

    fn target(&self) -> Index;
    fn set_target(&mut self, t: Index);

    fn set_weight(&mut self, w: Double);
    fn weight(&self) -> Double;

    fn set_delay(&mut self, d: Double);
    fn delay(&self) -> Double;

    fn on_pre_spike(&mut self, t: Double);
    fn on_post_spike(&mut self, t: Double);
}

pub trait ConnectionClone {
    fn clone_box(&self) -> Box<Connection>;
}

impl<T: 'static + Connection + Clone> ConnectionClone for T {
    fn clone_box(&self) -> Box<Connection> {
        Box::new(self.clone())
    }
}

impl Clone for Box<Connection> {
    fn clone(&self) -> Box<Connection> {
        self.clone_box()
    }
}

pub mod static_connection;
pub mod stdp_connection;
