// Copyright (c) 2019 Libgirl
//
// Released under Apache 2.0 license as described in the file LICENSE.txt.

use std::collections::HashMap;

use connections::Connection;
use {Double, Index, Num};

pub struct ConnectionSupervisor {
    post_connections_: HashMap<usize, Vec<Box<Connection>>>,
    pre_connections_: HashMap<usize, Vec<Box<Connection>>>,
}

impl ConnectionSupervisor {
    pub fn new() -> ConnectionSupervisor {
        ConnectionSupervisor {
            post_connections_: HashMap::new(),
            pre_connections_: HashMap::new(),
        }
    }

    pub fn clear(&mut self,
    ) {
        self.post_connections_.clear();
        self.pre_connections_.clear();
    }

    pub fn add_connection(&mut self, source_id: Index, target_id: Index, syn: &Connection) {
        let mut post_conn = syn.clone_box();
        post_conn.set_source(source_id);
        post_conn.set_target(target_id);
        let source = source_id as Num;
        self.post_connections_.entry(source).or_insert(Vec::new());

        if let Some(vec) = self.post_connections_.get_mut(&source) {
            vec.push(post_conn);
        }

        let mut pre_conn = syn.clone_box();
        pre_conn.set_source(source_id);
        pre_conn.set_target(target_id);
        let target = target_id as Num;
        self.pre_connections_.entry(target).or_insert(Vec::new());
        if let Some(vec) = self.pre_connections_.get_mut(&target) {
            vec.push(pre_conn);
        }
    }

    pub fn get_connections(&self, source_id: Index) -> Vec<Box<Connection>> {
        self.get_target_connections(source_id)
    }

    fn get_target_connections(&self, source_id: Index) -> Vec<Box<Connection>> {
        let id = source_id as usize;

        match self.post_connections_.get(&id) {
            Some(targets) => targets.to_vec(),
            _ => Vec::new(),
        }
    }

    pub fn get_all_connections(&self) -> &HashMap<usize, Vec<Box<Connection>>> {
        &self.post_connections_
    }

    fn propagate_pre(&mut self, spike_id: Index, step: Double) {
        let id = spike_id as usize;
        if let Some(conns) = self.post_connections_.get_mut(&id) {
            for c in conns {
                c.on_pre_spike(step);
            }
        }
    }

    fn propagate_post(&mut self, spike_id: Index, step: Double) {
        let id = spike_id as usize;
        if let Some(conns) = self.pre_connections_.get_mut(&id) {
            for c in conns {
                c.on_post_spike(step);
            }
        }
    }

    pub fn propagate(&mut self, spike_id: Index, step: Double) {
        self.propagate_pre(spike_id, step);
        self.propagate_post(spike_id, step);
    }
}
