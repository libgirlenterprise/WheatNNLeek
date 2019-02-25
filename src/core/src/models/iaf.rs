// Copyright (c) 2019 Libgirl
//
// Released under Apache 2.0 license as described in the file LICENSE.txt.

// Integrate-and-fire model
use crate::events::{Event, SpikeEvent};
use crate::models::{Neuron, NeuronActivity};
use crate::network::Network;
use crate::ode::rk4;
use crate::{Double, Index, Parameters, Time};

pub struct Model {
    pub e_l: Double,   // Membrane resting potential
    pub r_m: Double,   // Membrane resistance
    pub tau_m: Double, // Membrane time constant
    pub v: Double,     // Voltage
    pub v_th: Double,  // Spike thresold
    i_e: Double,
    spikes: Double,
    nid: Index,
}

impl Model {
    pub fn parameters() -> Parameters {
        let mut params = Parameters::new();
        params.insert("v_m".to_string(), -65.);
        params.insert("i_e".to_string(), 0.);
        params.insert("v_th".to_string(), -55.);
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
        let v_m = Model::get_or_default(params, "v_m");
        let i_e = Model::get_or_default(params, "i_e");
        let v_th = Model::get_or_default(params, "v_th");

        Model {
            e_l: v_m,
            r_m: 10.,
            tau_m: 10.,
            v: v_m,
            v_th: v_th,
            i_e: i_e,
            spikes: 0.,
            nid: -1,
        }
    }

    fn get_spike(&mut self, _lag: Double) -> Double {
        let spikes = self.spikes;
        self.spikes = 0.;
        spikes
    }

    fn add_spike(&mut self, e: &SpikeEvent) {
        self.spikes += e.weight() * e.multiplicity();
    }
}

impl Default for Model {
    fn default() -> Model {
        let mut params = Parameters::new();
        params.insert("v_m".to_string(), -65.);
        Model::new(&params)
    }
}

impl Neuron for Model {
    fn name(&self) -> String {
        String::from("Integrate-and-Fire")
    }

    fn set_params(&mut self, _params: &Parameters) {}

    fn update(&mut self, t: Double) -> NeuronActivity {
        let mut v = self.v;
        let e_l = self.e_l;
        let mut activity = NeuronActivity::Silent;
        if v >= self.v_th {
            let se = SpikeEvent::new();
            activity = NeuronActivity::Fires(se);
            v = e_l;
        }

        let tau_m = self.tau_m;
        let r_m = self.r_m;
        let i_e = self.i_e;
        let i_syn = self.get_spike(t);
        let dt = Network::resolution();
        let d_v = |y: f64| (e_l - y + r_m * i_e) / tau_m;
        v += rk4(d_v, v, dt) + r_m * i_syn;

        self.v = v;

        activity
    }

    fn handle_spike(&mut self, event: SpikeEvent) {
        self.add_spike(&event);
    }

    fn set_neuron_id(&mut self, nid: i64) {
        self.nid = nid;
    }

    fn neuron_id(&self) -> i64 {
        self.nid
    }

    fn new_spike_record(&mut self) {}

    fn set_spike_recording(&mut self, _is_on: bool) {}

    fn clear_spike_records(&mut self) {}

    fn get_spike_records(&self) -> Vec<Vec<Time>> {
        Vec::new()
    }

    fn get_property(&self, name: String) -> Double {
        match name.as_ref() {
            "v" => self.v,
            "v_th" => self.v_th,
            _ => 0.,
        }
    }

    fn set_property(&mut self, _name: String, _value: Double) {}
}
