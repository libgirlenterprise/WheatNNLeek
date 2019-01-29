// Copyright (c) 2019 Libgirl
//
// Released under Apache 2.0 license as described in the file LICENSE.txt.

use crate::connections::Connection as CommonConnection;
use crate::connections::PostSynapticEffect;
use crate::Parameters;
use crate::{Double, Index, Num};

#[derive(Debug, Clone)]
pub struct Connection {
    id_: Option<Num>,
    weight_: f64,
    delay_: f64,
    target_: Index,
    source_: Index,
    post_syn_effect_: PostSynapticEffect,
}

impl Connection {
    pub fn parameters() -> Parameters {
        let mut params = Parameters::new();
        params.insert("weight".to_string(), 1.);
        params.insert("delay".to_string(), 1.);
        params
    }

    fn get_or_default(params: &Parameters, key: &str) -> f64 {
        let default_params = Connection::parameters();
        params
            .get(key)
            .cloned()
            .unwrap_or(*default_params.get(key).unwrap())
    }

    pub fn new(params: &Parameters, spec: PostSynapticEffect) -> Connection {
        let w = Connection::get_or_default(params, "weight");
        let d = Connection::get_or_default(params, "delay");
        let post_syn_effect = spec;

        Connection {
            id_: None,
            weight_: w,
            delay_: d,
            post_syn_effect_: post_syn_effect,
            source_: -1,
            target_: -1,
        }
    }
}

impl Default for Connection {
    fn default() -> Connection {
        Connection::new(&Connection::parameters(), PostSynapticEffect::Inhibitory)
    }
}

impl CommonConnection for Connection {
    fn on_pre_spike(&mut self, _t: Double) {}

    fn on_post_spike(&mut self, _t: Double) {}

    fn id(&self) -> Num {
        self.id_.unwrap()
    }

    fn set_id(&mut self, i: Num) {
        self.id_ = Some(i);
    }

    fn set_weight(&mut self, w: Double) {
        self.weight_ = w;
    }

    fn weight(&self) -> Double {
        self.weight_
    }

    fn set_delay(&mut self, d: Double) {
        self.delay_ = d;
    }

    fn delay(&self) -> Double {
        self.delay_
    }

    fn set_source(&mut self, s: Index) {
        self.source_ = s;
    }

    fn source(&self) -> Index {
        self.source_
    }

    fn set_target(&mut self, t: Index) {
        self.target_ = t;
    }

    fn target(&self) -> Index {
        self.target_
    }
}
