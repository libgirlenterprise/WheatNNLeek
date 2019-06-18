use crate::connectivity::{Acceptor};
use crate::connectivity::simple_joint::SimpleChsCarrier;
use crate::connectivity::post_syn_joint::PostSynChsCarrier;
pub use crate::signals::firing_time::FiringTime;
use uom::si::f64::ElectricPotential as Voltage;
use uom::si::f64::Time;
use uom::si::f64::Ratio;

pub struct DiracV(Voltage);

pub struct PostSynDiracV {
    v: Voltage,
    delay: Time,
    weight: Ratio,
}

pub trait NeuronAcceptorDiracV:
Acceptor<SimpleChsCarrier<PostSynDiracV>>
    + Acceptor<PostSynChsCarrier<PostSynDiracV, FiringTime>> {}
