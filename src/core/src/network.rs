// Copyright (c) 2019 Libgirl
//
// Released under Apache 2.0 license as described in the file LICENSE.txt.

use crate::connection_supervisor::ConnectionSupervisor;
use crate::connections::{Connection, ConnectionInfo};
use crate::connectors::Connector;
use crate::events::{Event, SpikeEvent};
use crate::models::cb_ath_lif;
use crate::models::hodgkin_huxley;
use crate::models::iaf;
use crate::models::izhikevich;
use crate::models::static_poisson;
use crate::models::iz_markram1998;
use crate::models::Neuron;
use crate::models::NeuronActivity;
use crate::models::NeuronType;
use crate::populations::Population;
use crate::{Double, Index, Num, Parameters, Time};
use lazy_static::lazy_static;
use std::sync::Mutex;

pub struct NetworkConfig {
    resolution: Double,
}

lazy_static! {
    static ref NETWORK_CONFIG: Mutex<NetworkConfig> = Mutex::new(NetworkConfig { resolution: 0.5 });
}

pub struct Network {
    neurons: Vec<Box<Neuron>>,
    populations: Vec<Box<Population>>,
    connection_supervisor: ConnectionSupervisor,
    next_neuron_id: Num,
    next_population_id: usize,
    recording_neuron_ids: Vec<Num>,
    start_step: Double,
}

impl Network {
    pub fn new() -> Network {
        Network {
            neurons: Vec::new(),
            populations: Vec::new(),
            connection_supervisor: ConnectionSupervisor::new(),
            next_neuron_id: 0,
            next_population_id: 0,
            recording_neuron_ids: Vec::new(),
            start_step: 0.,
        }
    }

    pub fn clear(&mut self) {
        self.neurons.clear();
        self.populations.clear();
        self.connection_supervisor.clear();
        self.recording_neuron_ids.clear();
        self.next_neuron_id = 0;
        self.next_population_id = 0;
        self.start_step = 0.
    }

    pub fn build_neuron(ntype: NeuronType, params: &Parameters) -> Box<Neuron> {
        match ntype {
            NeuronType::HodgkinHuxley => Box::new(hodgkin_huxley::Model::default()),
            NeuronType::IAF => Box::new(iaf::Model::default()),
            NeuronType::Izhikevich => Box::new(izhikevich::Model::new(params)),
            NeuronType::StaticPoisson => Box::new(static_poisson::Model::new(params)),
            NeuronType::ConductionBasedAdaptiveThresholdLIF => {
                Box::new(cb_ath_lif::Model::new(params))
            },
            NeuronType::IzhikevichMarkram1998 => Box::new(iz_markram1998::Model::new(params)),
        }
    }

    pub fn create(
        &mut self,
        size: usize,
        ntype: NeuronType,
        params: &Parameters,
    ) -> Result<Population, &'static str> {
        if size == 0 {
            Err("invalid size")
        } else {
            let mut ids: Vec<Index> = Vec::new();
            for _ in 0..size {
                let neuron = Network::build_neuron(ntype, &params);
                let id = self.add_neuron(neuron);

                self.neurons[id].set_neuron_id(id as i64);
                ids.push(id as i64);
            }
            let population_id = self.next_population_id;
            self.next_population_id = self.next_population_id + 1;

            let population = Population::new(population_id, &ids);
            let population_box = Box::new(population.clone());
            self.populations.push(population_box);
            Ok(population)
        }
    }

    pub fn set_neuron_params(&mut self, id: Num, params: &Parameters) {
        self.neurons[id].set_params(params);
    }

    pub fn get_population_by_id(&self, id: usize) -> Box<Population> {
        self.populations[id].clone()
    }

    pub fn add_neuron(&mut self, neuron: Box<Neuron>) -> Num {
        let neuron_id = self.next_neuron_id;
        self.next_neuron_id = neuron_id + 1;

        self.neurons.push(neuron);
        neuron_id
    }

    pub fn connect<U: Connector, T: Connection>(
        &mut self,
        pre: &Population,
        post: &Population,
        conn: &U,
        syn: &T,
    ) -> Vec<Num> {
        conn.connect(pre, post, syn, &mut self.connection_supervisor)
    }

    pub fn connect_with_initial_weights<U: Connector, T: Connection>(
        &mut self,
        pre: &Population,
        post: &Population,
        weights: Vec<Double>,
        conn: &U,
        syn: &T,
    ) -> Vec<Num> {
        let conn_ids = conn.connect(pre, post, syn, &mut self.connection_supervisor);
        let mut i = 0;
        for conn_id in &conn_ids {
            self.connection_supervisor
                .set_weight_by_conn_id(*conn_id, weights[i]);
            i = i + 1;
        }
        conn_ids
    }

    fn evolve(&mut self, step: Double) {
        for i in 0..self.neurons.len() {
            if let NeuronActivity::Fires(_) = self.neurons[i].update(step) {
                let sender_id = self.neurons[i].neuron_id();
                self.connection_supervisor.propagate(sender_id, step);
                self.deliver_spike_event(sender_id);
            }
        }
    }

    pub fn run(&mut self, t: Time) {
        let mut step = self.start_step;
        let steps: Double = t / Network::resolution() + self.start_step;
        let resolution = Network::resolution();
        for i in 0..self.recording_neuron_ids.len() {
            self.neurons[self.recording_neuron_ids[i]].new_spike_record();
        }

        while step < steps {
            self.evolve(step);
            step += resolution;
        }
        self.start_step = step;
    }

    pub fn resolution() -> Double {
        NETWORK_CONFIG.lock().unwrap().resolution
    }

    pub fn set_resolution(r: Double) {
        NETWORK_CONFIG.lock().unwrap().resolution = r;
    }

    pub fn get_conn_info_by_id(&self, conn_id: Num) -> ConnectionInfo {
        self.connection_supervisor.get_conn_info_by_id(conn_id)
    }

    fn find_target_conn_infos(&self, source_id: Index) -> Vec<ConnectionInfo> {
        let conn_ids = self.connection_supervisor.get_connections(source_id);
        let mut v: Vec<ConnectionInfo> = Vec::new();
        for i in conn_ids {
            let conn = self.get_conn_info_by_id(i);
            v.push(conn);
        }
        v
    }

    fn deliver_spike_event(&mut self, sender_id: i64) {
        let t_conns = self.find_target_conn_infos(sender_id);
        for t in t_conns {
            let target_id = t.target as Num;
            let receiver = &mut self.neurons[target_id];
            let mut event = SpikeEvent::new();
            event.set_weight(t.weight);
            receiver.handle_spike(event);
        }
    }

    pub fn record_spikes(&mut self, population_id: usize) -> Result<(), String> {
        let population = self.get_population_by_id(population_id);
        for i in population.iter() {
            self.neurons[i as usize].set_spike_recording(true);
            self.recording_neuron_ids.push(i as usize); // notice in the future there might be cases that population shares some neurons
        }
        Ok(())
    }

    pub fn clear_spike_records(&mut self, population_id: usize) -> Result<(), String> {
        let population = self.get_population_by_id(population_id);
        for i in population.iter() {
            self.neurons[i as usize].clear_spike_records();
        }
        Ok(())
    }

    pub fn get_spike_records(&self) -> Vec<(Num, Vec<Vec<Time>>)> {
        let mut spike_records = Vec::new();
        for i in 0..self.recording_neuron_ids.len() {
            let neuron_id = self.recording_neuron_ids[i];
            let neuron = &self.neurons[neuron_id];
            let spike_histories = neuron.get_spike_records();
            spike_records.push((neuron_id, spike_histories));
        }
        spike_records
    }

    pub fn set_weight_by_conn_id(&mut self, conn_id: Num, weight: Double) {
        self.connection_supervisor
            .set_weight_by_conn_id(conn_id, weight);
    }

    pub fn set_property(&mut self, pop_id: usize, name: String, value: Double) {
        // FIXME: Using name
        let pop = self.get_population_by_id(pop_id);
        for i in pop.iter() {
            self.neurons[i as usize].set_property(name.clone(), value);
        }
    }

    pub fn set_properties(&mut self, pop_id: usize, name: String, values: Vec<Double>) {
        let pop = self.get_population_by_id(pop_id);
        let mut i = 0;
        for n in pop.iter() {
            self.neurons[n as usize].set_property(name.clone(), values[i]);
            i += 1;
        }
    }

    pub fn get_property(&self, pop_id: usize, name: String) -> Vec<Double> {
        let mut v: Vec<Double> = Vec::new();
        let pop = self.get_population_by_id(pop_id);
        for i in pop.iter() {
            let value = self.neurons[i as usize].get_property(name.clone());
            v.push(value);
        }
        v
    }
}

impl Default for Network {
    fn default() -> Network {
        Network::new()
    }
}
