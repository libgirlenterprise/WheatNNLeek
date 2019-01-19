// Copyright (c) 2019 Libgirl
//
// Released under Apache 2.0 license as described in the file LICENSE.txt.

use std::collections::HashMap;

use connections::{Connection, ConnectionInfo};
use {Double, Index, Num};

pub struct ConnectionSupervisor {
    next_conn_id: Num,
    connections_: Vec<Box<Connection>>,
    post_connections_: HashMap<usize, Vec<Num>>,
    pre_connections_: HashMap<usize, Vec<Num>>,
}

impl ConnectionSupervisor {
    pub fn new() -> ConnectionSupervisor {
        ConnectionSupervisor {
            next_conn_id: 0,
            connections_: Vec::new(),
            post_connections_: HashMap::new(),
            pre_connections_: HashMap::new(),
        }
    }

    pub fn clear(&mut self) {
        self.next_conn_id = 0;
        self.connections_.clear();
        self.post_connections_.clear();
        self.pre_connections_.clear();
    }

    pub fn add_connection(&mut self, source_id: Index, target_id: Index, syn: &Connection) -> Num {
        let conn_id = self.next_conn_id;
        let mut conn = syn.clone_box();
        conn.set_source(source_id);
        conn.set_target(target_id);
        self.next_conn_id = conn_id + 1;

        conn.set_id(conn_id);
        self.connections_.push(conn);

        let source = source_id as Num;
        self.post_connections_.entry(source).or_insert(Vec::new());
        if let Some(vec) = self.post_connections_.get_mut(&source) {
            vec.push(conn_id);
        }

        let target = target_id as Num;
        self.pre_connections_.entry(target).or_insert(Vec::new());
        if let Some(vec) = self.pre_connections_.get_mut(&target) {
            vec.push(conn_id);
        }

        conn_id
    }

    pub fn get_connections(&self, source_id: Index) -> Vec<Num> {
        self.get_target_connections(source_id)
    }

    fn get_target_connections(&self, source_id: Index) -> Vec<Num> {
        let id = source_id as usize;

        match self.post_connections_.get(&id) {
            Some(targets) => targets.to_vec(),
            _ => Vec::new(),
        }
    }

    fn propagate_pre(&mut self, spike_id: Index, step: Double) {
        let id = spike_id as usize;
        if let Some(conn_ids) = self.post_connections_.get_mut(&id) {
            for i in conn_ids {
                let conn_id = *i;
                self.connections_[conn_id].on_pre_spike(step);
            }
        }
    }

    fn propagate_post(&mut self, spike_id: Index, step: Double) {
        let id = spike_id as usize;
        if let Some(conn_ids) = self.pre_connections_.get_mut(&id) {
            for i in conn_ids {
                let conn_id = *i;
                self.connections_[conn_id].on_post_spike(step);
            }
        }
    }

    pub fn propagate(&mut self, spike_id: Index, step: Double) {
        self.propagate_pre(spike_id, step);
        self.propagate_post(spike_id, step);
    }

    pub fn get_conn_info_by_id(&self, conn_id: Num) -> ConnectionInfo {
        ConnectionInfo {
            source: self.connections_[conn_id].source(),
            target: self.connections_[conn_id].target(),
            weight: self.connections_[conn_id].weight(),
            delay: self.connections_[conn_id].delay(),
        }
    }
}
