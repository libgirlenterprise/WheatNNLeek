use std::sync::{Arc, Mutex};
use crate::operation::{PassiveDevice, ActiveDevice, RunMode};
use crate::populations::HoldDevices;
use crate::{AcMx, WkMx};
// use crate::components::synapse_component::SynapseRunFlag;

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
    // fn take_post(&mut self) -> DeviceMode<<Self as ChannelsCarrier>::ChsInFwd>;
}


pub trait Generator<C: ChannelsCarrier + Send> {
//    fn add_active(&mut self, post: WkMx<dyn ActiveAcceptor<C::<ChsInFFW = <>>>>, linker: AcMx<Linker<C>>);
    fn add_active(&mut self, post: WkMx<dyn ActiveAcceptor<C>>, linker: AcMx<Linker<C>>);
    fn add_passive(&mut self, post: WkMx<dyn PassiveAcceptor<C>>, linker: AcMx<Linker<C>>);
}

///required by Components
pub trait Acceptor<C: ChannelsCarrier + Send> {
    fn add(&mut self, pre: WkMx<dyn Generator<C>>, linker: AcMx<Linker<C>>);
}

///required by Components
pub trait ActiveAcceptor<C: ChannelsCarrier + Send>: ActiveDevice + Acceptor<C> {}

impl<C, A> ActiveAcceptor<C> for A
where C: ChannelsCarrier + Send,
      A: Acceptor<C> + ActiveDevice,
{}

///required by Components, need for generate running_sets.
// Passive and has only 1 input channel, 1 type of input signal.
pub trait PassiveAcceptor<C: ChannelsCarrier + Send>: PassiveDevice + Acceptor<C> {}

impl<C, A> PassiveAcceptor<C> for A
where C: ChannelsCarrier + Send,
      A: Acceptor<C> + PassiveDevice,
{}

pub fn connect_passive<G, A, C>(pre: AcMx<G>, post: AcMx<A>)
where G: 'static + Generator<C>,
      A: 'static + PassiveAcceptor<C> ,
      C: ChannelsCarrier + Send,
{
    let linker = Linker::new();
    pre.lock().unwrap().add_passive(Arc::<Mutex<A>>::downgrade(&post), Arc::clone(&linker));
    post.lock().unwrap().add(Arc::<Mutex<G>>::downgrade(&pre), linker);
}

pub fn connect_on_population_passive<G, A, C, PG, PA>(p1: &AcMx<PG>, n1: usize, p2: &AcMx<PA>, n2: usize)
where G: 'static + Generator<C>,
      A: 'static + PassiveAcceptor<C> ,
      C: ChannelsCarrier + Send,
      PG: HoldDevices<G>,
      PA: HoldDevices<A>,
{
    let device1 = p1.lock().unwrap().device_by_id(n1).clone();
    let device2 = p2.lock().unwrap().device_by_id(n2).clone();
    connect_passive(device1, device2);
}

pub fn connect_active<G, A, C>(pre: AcMx<G>, post: AcMx<A>)
where G: 'static + Generator<C>,
      A: 'static + ActiveAcceptor<C> ,
      C: ChannelsCarrier + Send,
{
    let linker = Linker::new();
    pre.lock().unwrap().add_active(Arc::<Mutex<A>>::downgrade(&post), Arc::clone(&linker));
    post.lock().unwrap().add(Arc::<Mutex<G>>::downgrade(&pre), linker);
}

pub fn connect_on_population_active<G, A, C, PG, PA>(p1: &AcMx<PG>, n1: usize, p2: &AcMx<PA>, n2: usize)
where G: 'static + Generator<C>,
      A: 'static + ActiveAcceptor<C> ,
      C: ChannelsCarrier + Send,
      PG: HoldDevices<G>,
      PA: HoldDevices<A>,
{
    let device1 = p1.lock().unwrap().device_by_id(n1).clone(); // why clone?
    let device2 = p2.lock().unwrap().device_by_id(n2).clone();
    connect_active(device1, device2);
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
