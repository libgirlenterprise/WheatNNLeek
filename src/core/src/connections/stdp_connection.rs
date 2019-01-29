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
    tc_pre_: Double,
    tc_post_1_: Double,
    tc_post_2_: Double,
    a_pre_: Double,
    a_post1_: Double,
    a_post2_: Double,
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
            tc_pre_: 20.,
            tc_post_1_: 20.,
            tc_post_2_: 40.,
            a_pre_: 0.,
            a_post1_: 0.,
            a_post2_: 0.,
            pre_rate_: 0.0001,
            post_rate_: 0.01,
            last_decay_t: -1.,
            post_syn_effect_: post_syn_effect,
        }
    }

    fn decay(&mut self, t: Time) {
        let dt = Network::resolution();
        let last_decay_t = self.last_decay_t;

        let a_pre = self.a_pre_;
        let a_post1 = self.a_post1_;
        let a_post2 = self.a_post2_;

        let tc_post_1 = self.tc_post_1_;
        let d_apost1 = move |y: Double| -y / tc_post_1;

        let tc_post_2 = self.tc_post_2_;
        let d_apost2 = move |y: Double| -y / tc_post_2;

        let tc_pre = self.tc_pre_;
        let d_apre = move |y: Double| -y / tc_pre;

        if last_decay_t > 0. && (t - last_decay_t).abs() > Network::resolution() {
            let steps = (t - last_decay_t) as i64;
            let mut difference_pre = 0.;
            let mut difference_post1 = 0.;
            let mut difference_post2 = 0.;
            for _ in 0..steps {
                difference_pre += rk4(d_apre, a_pre, dt);
                difference_post1 += rk4(d_apost1, a_post1, dt);
                difference_post2 += rk4(d_apost2, a_post2, dt);
            }
            self.a_pre_ += difference_pre;
            self.a_post1_ += difference_post1;
            self.a_post2_ += difference_post2;
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

        self.a_pre_ = 1.;
        self.weight_ = clamp(self.weight_ - self.pre_rate_ * self.a_post1_, 0., 1.);
    }

    fn on_post_spike(&mut self, t: Double) {
        self.decay(t);
        let a_post2 = self.a_post2_;

        self.weight_ = clamp(
            self.weight_ + self.post_rate_ * self.a_pre_ * a_post2,
            0.,
            1.,
        );
        self.a_post1_ = 1.;
        self.a_post2_ = 1.;
    }
}
