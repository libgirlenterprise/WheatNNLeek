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

pub struct NeuronModel {
    v_rest: Voltage,   // Membrane resting potential
    r_m: Resistance,   // Membrane resistance
    tau_m: Time,       // Membrane time constant
    v: Voltage,        // Membrane Voltage
    v_th: Voltage,     // Thresold Voltage of firing
    i_e: Current,      // constant current injection
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
