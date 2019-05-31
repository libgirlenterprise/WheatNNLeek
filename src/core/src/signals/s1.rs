use crate::components::{NeuronPostSynComponent, MultiInComponent};
use crate::connectivity::{Generator, Acceptor, PassiveAcceptor, ActiveAcceptor};
use crate::connectivity::simple_joint::SimpleChsCarrier;
use crate::connectivity::post_syn_joint::PostSynChsCarrier;
use crate::connectivity::linker::Linker;
use crate::signals::stdp_bkwd_0::StdpBkwd0;
use crate::agents::{Synapse, Device, Neuron};

#[derive(Copy, Clone)]
pub struct S1 {
    pub msg_gen: i32
}

pub trait DeviceGeneratorS1: Generator<SimpleChsCarrier<S1>> + Device {}

impl<T> DeviceGeneratorS1 for T
where T: Generator<SimpleChsCarrier<S1>> + Device,
{}

pub trait SynapseGeneratorS1: Generator<PostSynChsCarrier<S1, StdpBkwd0>> + Synapse {}

impl<T> SynapseGeneratorS1 for T
where T: Generator<PostSynChsCarrier<S1, StdpBkwd0>> + Synapse
{}

pub trait NeuronAcceptorS1: Acceptor<SimpleChsCarrier<S1>> + Acceptor<PostSynChsCarrier<S1, StdpBkwd0>> + Neuron {}

impl<T> NeuronAcceptorS1 for T
where T: Acceptor<SimpleChsCarrier<S1>> + Acceptor<PostSynChsCarrier<S1, StdpBkwd0>> + Neuron
{}

pub type NeuronPostSynComponentS1 = NeuronPostSynComponent<dyn SynapseGeneratorS1 + Send, S1, StdpBkwd0>;
pub type MultiInComponentS1 = MultiInComponent<dyn DeviceGeneratorS1 + Send, S1>;
