// Copyright (c) 2019 Libgirl
//
// Released under Apache 2.0 license as described in the file LICENSE.txt.
// Integrate-and-fire model
use num_traits::identities::Zero;
use crossbeam_channel::Receiver as CCReceiver;
use crossbeam_channel::Sender as CCSender;

use crate::{AcMx, Time, Resistance, Current, Voltage};
use crate::agents::neurons::Neuron;
use crate::signals::dirac_delta_voltage::{
    NeuronAcceptorDiracV,
    // PostSynDiracV, FiringTime,
    MulInCmpPostSynDiracV, SmplChsCarPostSynDiracV, SmplLnkrPostSynDiracV,
    PostSynChsCarDiracV, PostSynLnkrDiracV, NeuronPostSynCmpDiracV,
    GeneratorDiracV, SmplChsCarDiracV, SmplnkrDiracV,
    MulOutCmpDiracV,
};
use crate::connectivity::{
    Acceptor,
    AppendableOneWayBackEnd, AppendableTwoWayBackEnd,
    Generator, ActiveGenerator, PassiveGenerator,
    AppendableForeEnd,
    ActiveAcceptor, PassiveAcceptor,
};

use crate::operation::{Configurable, RunMode, ActiveAgent, Fired, PassiveBackOpeChs, OpeChs, Active, Broadcast};
use crate::agents::Agent;
use crate::operation::op_agent::FiringActiveAgent;
use crate::utils::{rk4};

pub struct NeuronModel {
    v_rest: Voltage,   // Membrane resting potential
    r_m: Resistance,   // Membrane resistance
    tau_m: Time,       // Membrane time constant
    tau_refrac: Time,  // Refractory time
    v: Voltage,        // Membrane Voltage
    v_th: Voltage,     // Thresold Voltage of firing
    i_e: Current,      // constant current injection
    refrac_countdown: Time,                    // counddown the time after fire
    firing_history: Vec<RefracDuration>,
    buffer_dirac_v: Vec<PostSynDiracV>,
    acted_dirac_v: Vec<PostSynDiracV>,
    void_dirac_v: Vec<PostSynDiracV>,
    error_dirac_v: Vec<PostSynDiracV>,
    ope_chs_gen: OpeChs<Fired>,
    device_in_dirac_v: MulInCmpPostSynDiracV,
    post_syn_dirac_v: NeuronPostSynCmpDiracV,
    out_dirac_v: MulOutCmpDiracV,
}

struct RefracDuration {
    begin: Time,
    end: Time,
}

impl NeuronAcceptorDiracV for NeuronModel {}
impl Neuron for NeuronModel {}

impl Acceptor<PostSynChsCarDiracV> for NeuronModel {}

impl Acceptor<SmplChsCarPostSynDiracV> for NeuronModel {}

impl AppendableOneWayBackEnd<SmplChsCarPostSynDiracV> for NeuronModel {
    fn add(&mut self, pre: AcMx<dyn Generator<SmplChsCarPostSynDiracV> + Send>, linker: AcMx<SmplLnkrPostSynDiracV>) {
        self.device_in_dirac_v.add(pre, linker);
    }
}

impl AppendableTwoWayBackEnd<PostSynChsCarDiracV> for NeuronModel {
    fn add_active(&mut self, pre: AcMx<dyn ActiveGenerator<PostSynChsCarDiracV> + Send>, linker: AcMx<PostSynLnkrDiracV>) {
        self.post_syn_dirac_v.add_active(pre, linker);
    }

    fn add_passive(&mut self, pre: AcMx<dyn PassiveGenerator<PostSynChsCarDiracV> + Send>, linker: AcMx<PostSynLnkrDiracV>) {
        self.post_syn_dirac_v.add_passive(pre, linker);
    }
}

impl GeneratorDiracV for NeuronModel {}

impl Generator<SmplChsCarDiracV> for NeuronModel {
    
}

impl AppendableForeEnd<SmplChsCarDiracV> for NeuronModel {
    fn add_active(&mut self, post: AcMx<dyn ActiveAcceptor<SmplChsCarDiracV> + Send>, linker: AcMx<SmplnkrDiracV>) {
        self.out_dirac_v.add_active_target(post, linker);
    }

    fn add_passive(&mut self, post: AcMx<dyn PassiveAcceptor<SmplChsCarDiracV> + Send>, linker: AcMx<SmplnkrDiracV>) {
        self.out_dirac_v.add_passive_target(post, linker);
    }
}

impl Configurable for NeuronModel {
    fn config_mode(&mut self, mode: RunMode) {
        self.post_syn_dirac_v.config_mode(mode);
        self.device_in_dirac_v.config_mode(mode);
        self.out_dirac_v.config_mode(mode);
    }
    
    fn config_channels(&mut self) {
        self.post_syn_dirac_v.config_channels();
        self.device_in_dirac_v.config_channels();
        self.out_dirac_v.config_channels();   
    }

    fn mode(&self) -> RunMode {
        match (self.out_dirac_v.mode(), self.post_syn_dirac_v.mode(), self.device_in_dirac_v.mode()) {
            (out_s0, post_syn_s1, device_in_s1) if out_s0 == post_syn_s1 && out_s0 == device_in_s1 => out_s0,
            (out_s0, post_syn_s1, device_in_s1) => panic!(
                "components of NeuronT have different modes, out_s0: {:?}, post_syn_s1: {:?}, device_in_s1: {:?}.",
                out_s0, post_syn_s1, device_in_s1
            ),
        }
    }
}

impl Agent for NeuronModel {}

impl ActiveAgent for NeuronModel {}

impl Active for NeuronModel {
    type Report = Fired;
    fn run(&mut self, dt: Time, time: Time) {
        <Self as FiringActiveAgent>::run(self, dt, time);
    }
    fn confirm_sender(&self) -> CCSender<Broadcast> {
        self.ope_chs_gen.confirm_sender()
    }
    
    fn confirm_receiver(&self) -> CCReceiver<Broadcast> {
        self.ope_chs_gen.confirm_receiver()
    }
    
    fn report_receiver(&self) -> CCReceiver<<Self as Active>::Report> {
        self.ope_chs_gen.report_receiver()
    }
    
    fn report_sender(&self) -> CCSender<<Self as Active>::Report> {
        self.ope_chs_gen.report_sender()
    }
}

impl FiringActiveAgent for NeuronModel {
    fn end(&mut self) {}

    fn evolve(&mut self, dt: Time, begin_t: Time) -> Fired {
        self.store();
        if begin_t > last_refrac_end_t {
            self.state_evolve(begin_t, begin_t + dt)
        } else if last_refrac_end_t > begin_t + dt {
            Fired::N
        } else {
            self.state_evolve(last_refrac_end_t, begin_t + dt)
        }
    }
    
    fn passive_sync_chs_sets(&mut self) -> Vec<PassiveBackOpeChs> {
        let mut v1 = self.out_dirac_v.passive_sync_chs_sets();
        let mut v2 = self.post_syn_dirac_v.passive_sync_chs_sets();
        v1.append(&mut v2);
        v1
    }
}

impl NeuronModel {
    fn fire(&self) {
        self.v = self.v_rest;
        self.firing_times.push(time + dt);
        self.filter_stored();
    }


    fn state_evolve(&mut self, begin: Time, end: Time) -> Fired {
        

        
        self.v += rk4(
            |y| (-(y - self.v_rest) + self.i_e * self.r_m) / self.tau_m,
            self.v,
            dt
        ) + self.sum_acting_dirac_v();
        if self.v >= self.v_th {
            self.fire();
            Fired::Y
        } else {
            Fired::N
        }       
    }


    fn store(&mut self) {
        self.device_in_dirac_v.ffw_accepted()
            .chain(
                self.post_syn_dirac_v.ffw_accepted()
            ).for_each(|s| {
                if s.t >= t_ref {
                    self.store_to_buffer(s);
                } else if s.t > last_refrac_end_t {
                    self.store_to_buffer(self.shift_to_ref_t(s));
                } else if s.t > last_refrac_begin_t {
                    self.store_to_void(s);
                } else {
                    self.store_to_error(s);
                }
            })
    }
    
    // fn accepted_dirac_v(&self) -> Voltage {
    //     self.device_in_dirac_v.ffw_accepted()
    //         .chain(
    //         self.post_syn_dirac_v.ffw_accepted()
    //     ).fold(Voltage::zero(),|sum, s| sum + s.v * s.w)
    // }


}
