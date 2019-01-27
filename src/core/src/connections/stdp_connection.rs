// Copyright (c) 2019 Libgirl
//
// Released under Apache 2.0 license as described in the file LICENSE.txt.

use crate::connections::Connection as CommonConnection;
use crate::connections::PostSynapticEffect;
use crate::network::Network;
use crate::ode::rk4;
use crate::utils::clamp;
use crate::Parameters;
use crate::{Double, Index, Num, Time};

#[derive(Debug, Clone)]
pub struct Connection {
    id_: Option<Num>,
    weight_: Double,
    delay_: Double,
    source_: Index,
    target_: Index,
    tau_plus_: Double,
    tau_minus_: Double,
    a_pre_: Double,
    a_post_: Double,
    pre_rate_: Double,
    post_rate_: Double,
    last_decay_t: Double,
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
        match default_params.get(key) {
            Some(v) => params.get(key).cloned().unwrap_or(*v),
            None => panic!("There is no default value for {}", key),
        }
    }

    pub fn new(params: &Parameters, spec: PostSynapticEffect) -> Connection {
        let w = Connection::get_or_default(params, "weight");
        let d = Connection::get_or_default(params, "delay");
        let post_syn_effect = spec;

        Connection {
            id_: None,
            weight_: w,
            delay_: d,
            source_: -1,
            target_: -1,
            tau_plus_: 40.,
            tau_minus_: 20.,
            a_pre_: 0.,
            a_post_: 0.,
            pre_rate_: 0.0002, //0.0001 * 2
            post_rate_: 0.01,
            last_decay_t: -1.,
            post_syn_effect_: post_syn_effect,
        }
    }

    fn decay(&mut self, t: Time) {
        let dt = Network::resolution();
        let last_decay_t = self.last_decay_t;

        let a_pre = self.a_pre_;
        let a_post = self.a_post_;

        let tau_minus = self.tau_minus_;
        let d_apost = move |y: Double| -y / tau_minus;
        let tau_plus = self.tau_plus_;
        let d_apre = move |y: Double| -y / tau_plus;

        if last_decay_t > 0. {
            let steps = (t - last_decay_t) as i64;
            let mut difference_pre = 0.;
            let mut difference_post = 0.;
            for _ in 0..steps {
                difference_pre += rk4(d_apre, a_pre, dt);
                difference_post += rk4(d_apost, a_post, dt);
            }
            self.a_pre_ += difference_pre;
            self.a_post_ += difference_post;
        }
        self.last_decay_t = t;
    }
}

impl Default for Connection {
    fn default() -> Connection {
        let params = Connection::parameters();
        Connection::new(&params, PostSynapticEffect::Excitatory)
    }
}

impl CommonConnection for Connection {
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

    fn source(&self) -> Index {
        self.source_
    }

    fn set_source(&mut self, s: Index) {
        self.source_ = s;
    }

    fn target(&self) -> Index {
        self.target_
    }

    fn set_target(&mut self, t: Index) {
        self.target_ = t;
    }

    fn on_pre_spike(&mut self, t: Double) {
        self.decay(t);

        self.a_pre_ += 1.;
        self.weight_ = clamp(self.weight_ - self.pre_rate_ * self.a_post_, 0., 1.);
    }

    fn on_post_spike(&mut self, t: Double) {
        self.decay(t);

        self.weight_ = clamp(
            self.weight_ + self.post_rate_ * (self.a_pre_ - self.a_post_),
            0.,
            1.,
        );
        self.a_post_ += 1.;
    }
}
