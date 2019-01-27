// Copyright (c) 2019 Libgirl
//
// Released under Apache 2.0 license as described in the file LICENSE.txt.

use crate::events::{Event, SpikeEvent};
use crate::models::{Neuron, NeuronActivity};
use crate::network::Network;
use crate::ode::rk4;
use crate::{Double, Parameters, Time};

pub struct Model {
    pub g_na: Double,
    pub g_k: f64,
    pub g_l: Double,
    pub cm: Double,
    pub e_na: Double,
    pub e_k: Double,
    pub e_l: Double,
    pub v: Double,
    pub m: Double,
    pub h: Double,
    pub n: Double,
    i_e: Double,
    spikes: Double,
    nid: i64,
}

impl Model {
    pub fn new(params: &Parameters) -> Model {
        let v_m = Model::get_or_default(params, "v_m");
        let minit = Model::get_or_default(params, "m");
        let hinit = Model::get_or_default(params, "h");
        let ninit = Model::get_or_default(params, "n");
        let i_e = Model::get_or_default(params, "i_e");

        Model {
            g_na: 120.0,  // Sodium (Na) maximum conductances, in mS/cm^2
            g_k: 36.0,    // Postassium (K) maximum conductances, in mS/cm^2
            g_l: 0.3,     // Leak maximum conductances, in mS/cm^2
            cm: 1.0,      // Membrane capacitance, in uF/cm^2
            e_na: 50.0,   // Sodium (Na) Nernst reversal potentials, in mV
            e_k: -77.0,   // Postassium (K) Nernst reversal potentials, in mV
            e_l: -54.402, // Leak Nernst reversal potentials, in mV
            v: v_m,       // Voltage
            m: minit,     // Sodium channel Activation
            h: hinit,     // Sodium channel Inactivation
            n: ninit,     // Potassium channel activation
            i_e: i_e,     // Constant input current
            spikes: 0.,
            nid: -1,
        }
    }

    fn alpha_n(&self, vm: Double) -> Double {
        (0.01 * (vm + 55.)) / (1. - (-(vm + 55.) / 10.).exp())
    }

    fn beta_n(&self, vm: Double) -> Double {
        0.125 * (-(vm + 65.) / 80.).exp()
    }

    fn alpha_m(&self, vm: Double) -> Double {
        (0.1 * (vm + 40.)) / (1. - (-(vm + 40.) / 10.).exp())
    }

    fn beta_m(&self, vm: Double) -> Double {
        4.0 * (-(vm + 65.) / 18.).exp()
    }

    fn alpha_h(&self, vm: Double) -> Double {
        0.07 * (-(vm + 65.) / 20.).exp()
    }

    fn beta_h(&self, vm: Double) -> Double {
        1. / (1. + (-(vm + 35.) / 10.).exp())
    }

    fn get_spike(&mut self, _lag: Double) -> Double {
        let spikes = self.spikes;
        self.spikes = 0.;
        spikes
    }

    fn add_spike(&mut self, e: &SpikeEvent) {
        self.spikes += e.weight() * e.multiplicity();
    }

    fn parameters() -> Parameters {
        let mut params = Parameters::new();
        params.insert("v_m".to_string(), -65.);
        params.insert("m".to_string(), 0.);
        params.insert("h".to_string(), 0.);
        params.insert("n".to_string(), 0.);
        params.insert("i_e".to_string(), 0.);
        params
    }

    fn get_or_default(params: &Parameters, key: &str) -> f64 {
        let default_params = Model::parameters();
        params
            .get(key)
            .cloned()
            .unwrap_or(*default_params.get(key).unwrap())
    }
}

impl Default for Model {
    fn default() -> Model {
        Model::new(&Model::parameters())
    }
}

impl Neuron for Model {
    fn name(&self) -> String {
        String::from("HodgkinHuxley")
    }

    fn set_params(&mut self, _params: &Parameters) {}

    fn update(&mut self, t: Double) -> NeuronActivity {
        let mut v = self.v;
        let mut m = self.m;
        let mut h = self.h;
        let mut n = self.n;
        let dt = Network::resolution();

        let a_m = self.alpha_m(v);
        let a_h = self.alpha_h(v);
        let a_n = self.alpha_n(v);

        let b_m = self.beta_m(v);
        let b_h = self.beta_h(v);
        let b_n = self.beta_n(v);

        let d_m = |y: Double| a_m * (1.0 - y) - b_m * y;
        let d_h = |y: Double| a_h * (1.0 - y) - b_h * y;
        let d_n = |y: Double| a_n * (1.0 - y) - b_n * y;

        m += rk4(d_m, m, dt);
        h += rk4(d_h, h, dt);
        n += rk4(d_n, n, dt);

        let i_na = self.g_na * m * m * m * h * (v - self.e_na);
        let i_k = self.g_k * n * n * n * n * (v - self.e_k);
        let i_l = self.g_l * (v - self.e_l);

        let cm = self.cm;
        let i_e = self.i_e;
        let i_syn = self.get_spike(t);
        let d_v = |_y: Double| (i_syn + i_e - (i_na + i_k + i_l)) / cm;
        v += rk4(d_v, v, dt);

        let mut activity = NeuronActivity::Silent;
        if v >= 0. && self.v > v {
            // send spike
            let se = SpikeEvent::new();
            activity = NeuronActivity::Fires(se);
        }

        self.v = v;
        self.m = m;
        self.h = h;
        self.n = n;

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
            "m" => self.m,
            "h" => self.h,
            "n" => self.n,
            _ => 0.,
        }
    }

    fn set_property(&mut self, _name: String, _value: Double) {}
}
