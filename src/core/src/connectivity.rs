use crate::operation::{PassiveAgent, ActiveAgent, RunMode};
use crate::{AcMx};
use crate::agents::{Device, Neuron, Synapse};
pub mod linker;
use self::linker::Linker;
mod tmp_contents;
pub mod simple_joint;
pub mod post_syn_joint;
pub mod channels_sets;

pub trait ChannelsCarrier {
    // type ContentFWD;
    type ForeEndChs;
    type BackEndChs;
    
    fn new() -> Self;
    fn reset_idle(&mut self);
    fn mode(&self) -> RunMode;
    fn fore_chs(&mut self, mode: RunMode) -> <Self as ChannelsCarrier>::ForeEndChs;
    fn back_chs(&mut self, mode: RunMode) -> <Self as ChannelsCarrier>::BackEndChs;
    // fn take_post(&mut self) -> AgentMode<<Self as ChannelsCarrier>::ChsInFwd>;
}

pub trait OneWayChannelsCarrier: ChannelsCarrier {}

pub trait TwoWayChannelsCarrier: ChannelsCarrier {}

pub trait AppendableForeEnd<C: ChannelsCarrier + Send>: Generator<C> {
    fn add_active(&mut self, post: AcMx<dyn ActiveAcceptor<C> + Send>, linker: AcMx<Linker<C>>);
    fn add_passive(&mut self, post: AcMx<dyn PassiveAcceptor<C> + Send>, linker: AcMx<Linker<C>>);    
}

pub trait Generator<C: ChannelsCarrier + Send> {} // remove Send?

// used by NeuronPostSynComponent
pub trait ActiveGenerator<C: ChannelsCarrier + Send>: ActiveAgent + Generator<C> {}

impl<C, G> ActiveGenerator<C> for G
where C: ChannelsCarrier + Send,
      G: Generator<C> + ActiveAgent,
{}

// used by NeuronPostSynComponent
pub trait PassiveGenerator<C: ChannelsCarrier + Send>: PassiveAgent + Generator<C> {}

impl<C, G> PassiveGenerator<C> for G
where C: ChannelsCarrier + Send,
      G: Generator<C> + PassiveAgent,
{}

pub trait DeviceGenerator<C: ChannelsCarrier + Send>: Device + Generator<C> {}
pub trait SynapseGenerator<C: ChannelsCarrier + Send>: Synapse + Generator<C> {}
pub trait NeuronGenerator<C: ChannelsCarrier + Send>: Neuron + Generator<C> {}

// Weak<Mutex<(dyn Generator<SimpleChsCarrier<S1>> + Send + 'static)>>

///required by Components
pub trait Acceptor<C: ChannelsCarrier + Send> {}

pub trait AppendableTwoWayBackEnd<C>: Acceptor<C>
where C: TwoWayChannelsCarrier + Send,
{
    fn add_active(&mut self, pre: AcMx<dyn ActiveGenerator<C> + Send>, linker: AcMx<Linker<C>>);
    fn add_passive(&mut self, pre: AcMx<dyn PassiveGenerator<C> + Send>, linker: AcMx<Linker<C>>);
}

pub trait AppendableOneWayBackEnd<C>: Acceptor<C>
where C: OneWayChannelsCarrier + Send,
{
    fn add(&mut self, pre: AcMx<dyn Generator<C> + Send>, linker: AcMx<Linker<C>>);
}

///required by Components
pub trait ActiveAcceptor<C: ChannelsCarrier + Send>: ActiveAgent + Acceptor<C> {}

impl<C, A> ActiveAcceptor<C> for A
where C: ChannelsCarrier + Send,
      A: Acceptor<C> + ActiveAgent,
{}

///required by Components, need for generate running_sets.
// Passive and has only 1 input channel, 1 type of input signal.
pub trait PassiveAcceptor<C: ChannelsCarrier + Send>: PassiveAgent + Acceptor<C> {}

impl<C, A> PassiveAcceptor<C> for A
where C: ChannelsCarrier + Send,
      A: Acceptor<C> + PassiveAgent,
{}
