// Copyright (c) 2019 Libgirl
//
// Released under Apache 2.0 license as described in the file LICENSE.txt.

// Conduction-based adaptive membrane threshold leaky integrate-and-fire  model
use crate::events::{Event, SpikeEvent};
use crate::models::{Neuron, NeuronActivity};
use crate::network::Network;
use crate::{Double, Index, Parameters, Time};

pub struct Model {
    pub e_l: Double,         // Membrane resting potential
    pub e_e: Double,         // Excitatory equilibrium potential
    pub e_i: Double,         // Inhibitory equilibrium potential
    pub reset_v: Double,     // Reset potential
    pub tau_m: Double,       // Membrane time constant
    pub tau_ge: Double,      // excitatory conductance time constant
    pub tau_gi: Double,      // inhibitory conductance time constant
    pub tau_theta: Double,   // Adaptive threshold variable time constant
    pub v: Double,           // Voltage
    pub ge: Double,          // excitatory conductance = got excitatory spikes
    pub gi: Double,          // inhibitory conductance = got inhibitory spikes
    pub v_th: Double,        // Spike thresold constant
    pub theta: Double,       // Adaptive threshold variable
    pub theta_plus: Double,  // Theta addition constant
    pub refact: Double,      // Refactory time constant
    pub last_fire_t: Double, // Last firing time
    pub during_refact: bool, // during refactory period
    is_record_spikes: bool,
    fix_theta: Double, // actually a boolean. indicating theta is changing or not
    i_e: Double,
    nid: Index,
    spike_records: Vec<Vec<Time>>,
}

impl Model {
    pub fn parameters() -> Parameters {
        let mut params = Parameters::new();
        params.insert("v_m".to_string(), -65.);
        params.insert("reset_v".to_string(), -65.);
        params.insert("tau_m".to_string(), 100.);
        params.insert("i_e".to_string(), 0.);
        params.insert("v_th".to_string(), -72.);
        params.insert("e_e".to_string(), 0.);
        params.insert("e_i".to_string(), -100.);
        params.insert("tau_ge".to_string(), 1.);
        params.insert("tau_gi".to_string(), 2.);
        params.insert("tau_theta".to_string(), 1e7);
        params.insert("theta".to_string(), 20.);
        params.insert("theta_plus".to_string(), 0.05);
        params.insert("refact".to_string(), 5.);
        params.insert("ge".to_string(), 0.);
        params.insert("gi".to_string(), 0.);
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
        let reset_v = Model::get_or_default(params, "reset_v");
        let i_e = Model::get_or_default(params, "i_e");
        let v_th = Model::get_or_default(params, "v_th");
        let e_e = Model::get_or_default(params, "e_e");
        let e_i = Model::get_or_default(params, "e_i");
        let tau_ge = Model::get_or_default(params, "tau_ge");
        let tau_gi = Model::get_or_default(params, "tau_gi");
        let tau_theta = Model::get_or_default(params, "tau_theta");
        let theta = Model::get_or_default(params, "theta");
        let theta_plus = Model::get_or_default(params, "theta_plus");
        let refact = Model::get_or_default(params, "refact");
        let ge = Model::get_or_default(params, "ge");
        let gi = Model::get_or_default(params, "gi");
        let tau_m = Model::get_or_default(params, "tau_m");

        Model {
            e_l: v_m,
            reset_v: reset_v,
            tau_m: tau_m,
            v: v_m,
            v_th: v_th,
            i_e: i_e,
            nid: -1,
            theta_plus: theta_plus,
            theta: theta,
            tau_theta: tau_theta,
            tau_gi: tau_gi,
            tau_ge: tau_ge,
            e_i: e_i,
            e_e: e_e,
            refact: refact,
            during_refact: false,
            last_fire_t: 0.0,
            ge: ge,
            gi: gi,
            spike_records: Vec::new(),
            is_record_spikes: false,
            fix_theta: 0.0,
        }
    }

    fn add_spike(&mut self, e: &SpikeEvent) {
        let multiplicity = e.multiplicity();
        let weight = e.weight();

        if multiplicity > 0.0 {
            self.ge += weight;
        } else {
            self.gi += weight;
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
        String::from("ConductionBasedAdaptiveThresholdLIF")
    }

    fn set_params(&mut self, _params: &Parameters) {}

    fn update(&mut self, t: Double) -> NeuronActivity {
        let mut v = self.v;
        let mut ge = self.ge;
        let mut gi = self.gi;
        let mut theta = self.theta;
        let e_l = self.e_l;
        let e_e = self.e_e;
        let e_i = self.e_i;
        let theta_plus = self.theta_plus;
        let refact = self.refact;
        let mut during_refact = self.during_refact;
        let mut last_fire_t = self.last_fire_t;

        if during_refact && t - last_fire_t > refact {
            during_refact = false;
        }

        let mut activity = NeuronActivity::Silent;
        if !during_refact && v >= self.v_th + theta {
            let se = SpikeEvent::new();
            activity = NeuronActivity::Fires(se);
            v = self.reset_v;
            theta += theta_plus;
            last_fire_t = t;
            during_refact = true;
            if self.is_record_spikes {
                let spike_record_index = self.spike_records.len() - 1;
                self.spike_records[spike_record_index].push(t);
            }
        }

        let tau_m = self.tau_m;
        let tau_ge = self.tau_ge;
        let tau_gi = self.tau_gi;
        let tau_theta = self.tau_theta;
        let i_e = self.i_e;
        let dt = Network::resolution();

        if self.fix_theta < 0.5 {
            let d_theta = |y: f64| -y / tau_theta;
            theta += ::ode::rk4(d_theta, theta, dt);

            self.theta = theta;
        }

        let d_ge = |y: f64| -y / tau_ge;
        ge += ::ode::rk4(d_ge, ge, dt);

        let d_gi = |y: f64| -y / tau_gi;
        gi += ::ode::rk4(d_gi, gi, dt);

        let d_v = |y: f64| (e_l - y + i_e + ge * (e_e - y) + gi * (e_i - y)) / tau_m;
        v += ::ode::rk4(d_v, v, dt);

        self.ge = ge;
        self.gi = gi;
        self.v = v;
        self.last_fire_t = last_fire_t;
        self.during_refact = during_refact;

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

    fn new_spike_record(&mut self) {
        self.spike_records.push(Vec::new());
    }

    fn set_spike_recording(&mut self, is_on: bool) {
        self.is_record_spikes = is_on;
    }

    fn clear_spike_records(&mut self) {
        self.spike_records.clear();
    }

    fn get_spike_records(&self) -> Vec<Vec<Time>> {
        let mut records = Vec::new();
        for i in 0..self.spike_records.len() {
            let mut spike_history = Vec::new();
            for j in 0..self.spike_records[i].len() {
                spike_history.push(self.spike_records[i][j]);
            }
            records.push(spike_history);
        }
        records
    }

    fn get_property(&self, name: String) -> Double {
        match name.as_ref() {
            "v" => self.v,
            "v_th" => self.v_th,
            _ => 0.,
        }
    }

    fn set_property(&mut self, name: String, value: Double) {
        match name.as_ref() {
            "v" => self.v = value,
            "v_th" => self.v_th = value,
            "theta" => self.theta = value,
            "fix_theta" => self.fix_theta = value,
            _ => (),
        }
    }
}
