use crate::components::{NeuronPreSynComponent, NeuronPostSynComponent};
// use crate::components::{MultiInComponent, MultiOutComponent, SingleInComponent, SingleOutComponent};
use crate::connectivity::{Generator, PassiveAcceptor, ActiveAcceptor};
use crate::connectivity::simple_joint::SimpleChsCarrier;
use crate::connectivity::post_syn_joint::PostSynChsCarrier;
use crate::connectivity::linker::Linker;

#[derive(Copy, Clone)]
pub struct S0 {
    pub msg_gen: i32
}

pub trait PreSynGeneratorS1: Generator<SimpleChsCarrier<FwdPreS1>> {}

impl<T> PreSynGeneratorS1 for T
where T: Generator<SimpleChsCarrier<FwdPreS1>>,
{}

pub trait PreSynActiveAcceptorS1: ActiveAcceptor<SimpleChsCarrier<FwdPreS1>> {}
pub trait PreSynPassiveAcceptorS1: PassiveAcceptor<SimpleChsCarrier<FwdPreS1>> {}
// pub trait PostSynGeneratorS1 = dyn Generator<PostSynChsCarrier<FwdPreS1, BkwdPreS1>> + Send {}

pub type PreSynLinkerS1 =  Linker<SimpleChsCarrier<FwdPreS1>>;


// type NeuronOutComponentS0 = NeuronPreSynComponent<dyn >

type PreSynS1DynAA = dyn ActiveAcceptor<SimpleChsCarrier<FwdPreS1>> + Send;
type PreSynS1DynPAS1 = dyn PassiveAcceptor<SimpleChsCarrier<FwdPreS1>> + Send;
pub type NeuronPreSynComponentS1 = NeuronPreSynComponent<DynAA, DynPA, FwdPreS1>;

type PostSynDynG = dyn Generator<PostSynChsCarrier<FwdPreS1, BkwdPreS1>> + Send;
pub type NeuronPostSynComponentS1 = NeuronPostSynComponent<PostSynDynG, FwdPreS1, BkwdPreS1>;

// pub type MultiOutComponentS1Pre = MultiOutComponent<dyn ActiveAcceptor<FwdPreS1>, dyn PassiveAcceptor<FwdPreS1>, FwdPreS1>;

// pub type MultiInComponentS1Pre = MultiInComponent<dyn Generator<FwdPreS1>, FwdPreS1>;

// SingleOut to PassiveSingleOut not implemented yet.
// pub type SingleOutComponentS1Pre = SingleOutComponent<dyn ActiveAcceptor<FwdPreS1>, dyn PassiveAcceptor<FwdPreS1>, FwdPreS1>;

// pub type SingleInComponentS1Pre = SingleInComponent<dyn Generator<FwdPreS1>, FwdPreS1>;
