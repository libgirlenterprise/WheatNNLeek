// Copyright (c) 2019 Libgirl
//
// Released under Apache 2.0 license as described in the file LICENSE.txt.
// Integrate-and-fire model
use uom::si::f64::Time;
// use uom::si::time::millisecond;
use uom::si::f64::ElectricalResistance as Resistance;
// use uom::si::electrical_resistance::megaohm;
use uom::si::f64::ElectricCurrent as Current;
// use uom::si::electric_current::nanoampere;
use uom::si::f64::ElectricPotential as Voltage;
// use uom::si::electric_potential::millivolt;

use crate::{AcMx};
use crate::agents::neurons::Neuron;
use crate::signals::dirac_delta_voltage::{
    NeuronAcceptorDiracV, PostSynDiracV, FiringTime,
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
use crate::connectivity::post_syn_joint::PostSynChsCarrier;
use crate::connectivity::simple_joint::SimpleChsCarrier;

use crate::operation::{Configurable, RunMode, ActiveAgent, Fired, PassiveBackOpeChs, OpeChs, Active, Broadcast};
use crate::agents::Agent;
use crate::operation::op_agent::FiringActiveAgent;
use crossbeam_channel::Receiver as CCReceiver;
use crossbeam_channel::Sender as CCSender;

pub struct NeuronModel {
    v_rest: Voltage,   // Membrane resting potential
    r_m: Resistance,   // Membrane resistance
    tau_m: Time,       // Membrane time constant
    v: Voltage,        // Membrane Voltage
    v_th: Voltage,     // Thresold Voltage of firing
    i_e: Current,      // constant current injection
    ope_chs_gen: OpeChs<Fired>,
    device_in_dirac_v: MulInCmpPostSynDiracV,
    post_syn_dirac_v: NeuronPostSynCmpDiracV,
    out_dirac_v: MulOutCmpDiracV,
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
    fn run(&mut self) {
        <Self as FiringActiveAgent>::run(self);
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

    fn evolve(&mut self) -> Fired {
        
    }
    
    fn passive_sync_chs_sets(&mut self) -> Vec<PassiveBackOpeChs> {
        
    }
}
