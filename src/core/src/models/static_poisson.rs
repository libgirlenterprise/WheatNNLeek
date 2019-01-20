// Copyright (c) 2019 Libgirl
//
// Released under Apache 2.0 license as described in the file LICENSE.txt.

// Static Poisson Spike Neuron
use events::SpikeEvent;
use models::{Neuron, NeuronActivity};
use network::Network;
use {Double, Index, Parameters, Time};

pub struct Model {
    freq: f64, //in Hertz
    nid: Index,
}

impl Model {
    pub fn parameters() -> Parameters {
        let mut params = Parameters::new();
        params.insert("freq".to_string(), 0.);
        params
    }

    fn get_or_default(params: &Parameters, key: &str) -> f64 {
        let default_params = Model::parameters();
        params
            .get(key)
            .cloned()
            .unwrap_or(*default_params.get(key).unwrap())
    }

    pub fn new(params: &Parameters) -> Model {
        let freq = Model::get_or_default(params, "freq");

        Model {
            freq: freq,
            nid: -1,
        }
    }
}

impl Default for Model {
    fn default() -> Model {
        let params = Parameters::new();
        Model::new(&params)
    }
}

impl Neuron for Model {
    fn name(&self) -> String {
        String::from("StaticPoisson")
    }

    fn set_params(&mut self, params: &Parameters) {
        let freq = Model::get_or_default(params, "freq");
        self.freq = freq;
    }

    fn update(&mut self, _t: Double) -> NeuronActivity {
        let mut activity = NeuronActivity::Silent;
        let resolution = Network::resolution(); // in mini second
        let step_freq = self.freq * resolution / 1000.;
        let rng_value = rand::random::<f64>();

        if step_freq >= 1. || step_freq > rng_value {
            let se = SpikeEvent::new();
            activity = NeuronActivity::Fires(se);
        }

        activity
    }

    fn handle_spike(&mut self, _event: SpikeEvent) {}

    fn set_neuron_id(&mut self, nid: i64) {
        self.nid = nid;
    }

    fn neuron_id(&self) -> i64 {
        self.nid
    }

    fn new_spike_record(&mut self) {
        ;
    }
    
    fn set_spike_recording(&mut self, _is_on: bool) {
        ;
    }

    fn clear_spike_records(&mut self) {
        ;
    }

    fn get_spike_records(self) -> Vec<Vec<Time>> {
        Vec::new()
    }

}
