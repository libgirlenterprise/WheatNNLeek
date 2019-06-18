use crate::connectivity::{Acceptor, Generator, Linker, ActiveGenerator, PassiveGenerator};
use crate::connectivity::simple_joint::SimpleChsCarrier;
use crate::connectivity::post_syn_joint::PostSynChsCarrier;
pub use crate::signals::firing_time::FiringTime;
use crate::components::{NeuronPostSynComponent, MultiInComponent};
use uom::si::f64::ElectricPotential as Voltage;
use uom::si::f64::Time;
use uom::si::f64::Ratio;

#[derive(Copy, Clone)]
pub struct DiracV(Voltage);

#[derive(Copy, Clone)]
pub struct PostSynDiracV {
    v: Voltage,
    delay: Time,
    weight: Ratio,
}

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
