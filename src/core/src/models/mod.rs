// Copyright (c) 2019 Libgirl
//
// Released under Apache 2.0 license as described in the file LICENSE.txt.

use events::SpikeEvent;
use {Double, Parameters};

pub enum NeuronActivity {
    Fires(SpikeEvent),
    Silent,
}

pub trait Neuron: std::marker::Send {
    fn name(&self) -> String;
    fn update(&mut self, t: Double) -> NeuronActivity;
    fn handle_spike(&mut self, event: SpikeEvent);

    fn set_neuron_id(&mut self, nid: i64);
    fn set_params(&mut self, params: &Parameters);
    fn neuron_id(&self) -> i64;
}

custom_derive! {
    #[derive(Copy, Clone, EnumFromStr)]
    pub enum NeuronType {
        HodgkinHuxley,
        IAF,
        Izhikevich,
        StaticPoisson,
        ConductionBasedAdaptiveThresholdLIF,
    }
}

pub mod cb_ath_lif;
pub mod hodgkin_huxley;
pub mod iaf;
pub mod izhikevich;
pub mod static_poisson;
