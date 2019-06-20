use crate::connectivity::{
    Acceptor, ActiveAcceptor, PassiveAcceptor,
    Generator, Linker, ActiveGenerator, PassiveGenerator
};
use crate::connectivity::simple_joint::SimpleChsCarrier;
use crate::connectivity::post_syn_joint::PostSynChsCarrier;
pub use crate::signals::firing_time::FiringTime;
use crate::components::{NeuronPostSynComponent, MultiInComponent, MultiOutComponent};
use crate::components::synapse_component::SynapseComponent;
use uom::si::f64::ElectricPotential as Voltage;
use uom::si::f64::Time;
use uom::si::f64::Ratio;

#[derive(Copy, Clone)]
pub struct PreSynDiracV {
    pub v: Voltage, // post synaptic potential increase.
    pub t: Time, // firing time.
}

pub trait GeneratorDiracV: Generator<SmplChsCarPreSynDiracV> {}
pub trait AcceptorDiracV: Acceptor<SmplChsCarPreSynDiracV> {}

pub type SmplChsCarPreSynDiracV = SimpleChsCarrier<PreSynDiracV>;
pub type SmplLnkrPreSynDiracV = Linker<SmplChsCarPreSynDiracV>;

pub type MulOutCmpDiracV = MultiOutComponent<dyn ActiveAcceptor<SmplChsCarPreSynDiracV> + Send,
                                             dyn PassiveAcceptor<SmplChsCarPreSynDiracV> + Send,
                                             PreSynDiracV>;

#[derive(Copy, Clone)]
pub struct PostSynDiracV {
    pub v: Voltage, // post synaptic potential increase.
    pub t: Time, // firing time + propagation time delay.
    pub w: Ratio, //weight.
}

pub trait DeviceGeneratorDiracV: Generator<SmplChsCarPostSynDiracV> {}
pub trait SynapseGeneratorDiracV: Generator<PostSynChsCarDiracV>{}
pub trait NeuronAcceptorDiracV:
Acceptor<SimpleChsCarrier<PostSynDiracV>>
    + Acceptor<PostSynChsCarrier<PostSynDiracV, FiringTime>> {}

pub type MulInCmpPostSynDiracV = MultiInComponent<dyn Generator<SimpleChsCarrier<PostSynDiracV>> + Send, PostSynDiracV>;

pub type SmplChsCarPostSynDiracV = SimpleChsCarrier<PostSynDiracV>;
pub type SmplLnkrPostSynDiracV = Linker<SmplChsCarPostSynDiracV>;

pub type NeuronPostSynCmpDiracV = NeuronPostSynComponent
    <dyn ActiveGenerator<PostSynChsCarDiracV> + Send,
     dyn PassiveGenerator<PostSynChsCarDiracV> + Send,
     PostSynDiracV,
     FiringTime>;

pub type PostSynChsCarDiracV = PostSynChsCarrier<PostSynDiracV, FiringTime>;
pub type PostSynLnkrDiracV = Linker<PostSynChsCarDiracV>;

pub type SynapseComponentDiracV = SynapseComponent<dyn Generator<SmplChsCarPreSynDiracV> + Send,
                                                   PreSynDiracV,
                                                   dyn ActiveAcceptor<PostSynChsCarDiracV> + Send,
                                                   dyn PassiveAcceptor<PostSynChsCarDiracV> + Send,
                                                   PostSynDiracV,
                                                   FiringTime>;

