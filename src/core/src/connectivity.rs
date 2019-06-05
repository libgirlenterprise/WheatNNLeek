use std::sync::{Arc};
use crate::operation::{PassiveAgent, ActiveAgent, RunMode};
use crate::populations::HoldAgents;
use crate::{AcMx};
// use crate::components::synapse_component::SynapseRunFlag;
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


pub trait Generator<C: ChannelsCarrier + Send> {
    fn add_active(&mut self, post: AcMx<dyn ActiveAcceptor<C> + Send>, linker: AcMx<Linker<C>>);
    fn add_passive(&mut self, post: AcMx<dyn PassiveAcceptor<C> + Send>, linker: AcMx<Linker<C>>);
}

pub trait DeviceGenerator<C: ChannelsCarrier + Send>: Device + Generator<C> {}
pub trait SynapseGenerator<C: ChannelsCarrier + Send>: Synapse + Generator<C> {}
pub trait NeuronGenerator<C: ChannelsCarrier + Send>: Neuron + Generator<C> {}

// Weak<Mutex<(dyn Generator<SimpleChsCarrier<S1>> + Send + 'static)>>

///required by Components
pub trait Acceptor<C: ChannelsCarrier + Send> {
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

pub fn connect_passive<G, A, C>(pre: AcMx<G>, post: AcMx<A>)
where G: 'static + Generator<C> + Send,
      A: 'static + PassiveAcceptor<C> + Send,
      C: ChannelsCarrier + Send,
{
    let linker = Linker::new();
    pre.lock().unwrap().add_passive(post, Arc::clone(&linker));
    post.lock().unwrap().add(pre, linker);
}

pub fn connect_on_population_passive<G, A, C, PG, PA>(p1: &AcMx<PG>, n1: usize, p2: &AcMx<PA>, n2: usize)
where G: 'static + Generator<C> + Send,
      A: 'static + PassiveAcceptor<C> + Send,
      C: ChannelsCarrier + Send,
      PG: HoldAgents<G>,
      PA: HoldAgents<A>,
{
    let agent1 = p1.lock().unwrap().agent_by_id(n1).clone();
    let agent2 = p2.lock().unwrap().agent_by_id(n2).clone();
    connect_passive(agent1, agent2);
}

pub fn connect_active<G, A, C>(pre: AcMx<G>, post: AcMx<A>)
where G: 'static + Generator<C> + Send,
      A: 'static + ActiveAcceptor<C> + Send,
      C: ChannelsCarrier + Send,
{
    let linker = Linker::new();
    pre.lock().unwrap().add_active(post, Arc::clone(&linker));
    post.lock().unwrap().add(pre, linker);
}

pub fn connect_on_population_active<G, A, C, PG, PA>(p1: &AcMx<PG>, n1: usize, p2: &AcMx<PA>, n2: usize)
where G: 'static + Generator<C> + Send,
      A: 'static + ActiveAcceptor<C> + Send,
      C: ChannelsCarrier + Send,
      PG: HoldAgents<G>,
      PA: HoldAgents<A>,
{
    let agent1 = p1.lock().unwrap().agent_by_id(n1).clone(); // why clone?
    let agent2 = p2.lock().unwrap().agent_by_id(n2).clone();
    connect_active(agent1, agent2);
}

// // neuron acceptor has multi input, impossible to be passive! only active/passive of Synapse has to be considered.
// // should be move to SynapseModel::new().
// pub fn build_active_synapse<NPre, CPre, Syn, CPost, NPost>(pre: AcMx<NPre>, syn: AcMx<Syn>, post: AcMx<NPost>)
// where NPre: 'static + Neuron + Generator<CPre>,
//       CPre: ChannelsCarrier + Send,
//       Syn: 'static + Synapse + ActiveAcceptor<CPre> + Generator<CPost>,
//       CPost: ChannelsCarrier + Send,
//       NPost: 'static + Neuron + ActiveAcceptor<CPost>,
// {
//     let linker = Linker::new();
//     pre.lock().unwrap().add_active(Arc::<Mutex<Syn>>::downgrade(&syn), Arc::clone(&linker));
//     syn.lock().unwrap().add(Arc::<Mutex<NPre>>::downgrade(&pre), linker);
//     let linker = Linker::new();
//     syn.lock().unwrap().add_active(Arc::<Mutex<NPost>>::downgrade(&post), Arc::clone(&linker));
//     post.lock().unwrap().add(Arc::<Mutex<Syn>>::downgrade(&syn), linker);
// }

// pub fn build_passive_synapse<NPre, CPre, Syn, CPost, NPost>(pre: AcMx<NPre>, syn: AcMx<Syn>, post: AcMx<NPost>)
// where NPre: 'static + Neuron + Generator<CPre>,
//       CPre: ChannelsCarrier + Send,
//       Syn: 'static + Synapse + PassiveAcceptor<CPre> + Generator<CPost>,
//       CPost: ChannelsCarrier + Send,
//       NPost: 'static + Neuron + ActiveAcceptor<CPost>,
// {
//     let linker = Linker::new();
//     pre.lock().unwrap().add_passive(Arc::<Mutex<Syn>>::downgrade(&syn), Arc::clone(&linker));
//     syn.lock().unwrap().add(Arc::<Mutex<NPre>>::downgrade(&pre), linker);
//     let linker = Linker::new();
//     syn.lock().unwrap().add_active(Arc::<Mutex<NPost>>::downgrade(&post), Arc::clone(&linker));
//     post.lock().unwrap().add(Arc::<Mutex<Syn>>::downgrade(&syn), linker);
// }
