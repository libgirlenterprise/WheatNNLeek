// Copyright (c) 2019 Libgirl
//
// Released under Apache 2.0 license as described in the file LICENSE.txt.

use crate::Double;

pub trait Event {
    fn set_weight(&mut self, weight: Double);
    fn weight(&self) -> Double;

    fn set_delay(&mut self, delay: Double);
    fn delay(&self) -> Double;
}

pub struct SpikeEvent {
    w_: Double,
    d_: Double,
    m_: Double,
}

impl SpikeEvent {
    pub fn new() -> SpikeEvent {
        SpikeEvent {
            w_: 0.0,
            d_: 1.0,
            m_: 1.0,
        }
    }

    pub fn multiplicity(&self) -> Double {
        self.m_
    }

    pub fn set_multiplicity(&mut self, multiplicity: Double) {
        self.m_ = multiplicity;
    }
}

impl Default for SpikeEvent {
    fn default() -> SpikeEvent {
        SpikeEvent::new()
    }
}

impl Event for SpikeEvent {
    fn set_weight(&mut self, weight: Double) {
        self.w_ = weight;
    }

    fn weight(&self) -> Double {
        self.w_
    }

    fn set_delay(&mut self, delay: Double) {
        self.d_ = delay;
    }

    fn delay(&self) -> Double {
        self.d_
    }
}
