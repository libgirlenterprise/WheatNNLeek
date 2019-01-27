// Copyright (c) 2019 Libgirl
//
// Released under Apache 2.0 license as described in the file LICENSE.txt.

use crate::events::SpikeEvent;

pub trait Node: std::marker::Send {
    fn handle_spike(&self, event: SpikeEvent);
}
