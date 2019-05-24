use std::sync::{Arc, Mutex};
use crate::operation::{PassiveDevice, ActiveDevice};
use crate::components::Linker;
use crate::populations::HoldDevices;
use crate::{AcMx, WcMx};
// use crate::components::synapse_component::SynapseRunFlag;

pub mod s1_pre;
pub mod s1_post;
// pub mod signal_2;

pub trait Generator<S: Send>: Send {
    fn add_active(&mut self, post: WcMx<dyn ActiveAcceptor<S>>, linker: AcMx<Linker<S>>);
    fn add_passive(&mut self, post: WcMx<dyn PassiveAcceptor<S>>, linker: AcMx<Linker<S>>);
}

///required by Components
pub trait Acceptor<S: Send>: Send {
    fn add(&mut self, pre: WcMx<dyn Generator<S>>, linker: AcMx<Linker<S>>);
}

// pub trait Synapse<SPre, SPost>: SynapseAcceptor<SPre> + SynapseGenerator<SPost>
// where SPre: Send,
//       SPost: Send,
// {}

// impl<T, SPre, SPost> Synapse<SPre, SPost> for T
// where T: SynapseAcceptor<SPre> + SynapseGenerator<SPost>,
//       SPre: Send,
//       SPost: Send,
// {}

// pub trait DeviceGenerator<S: Send>: Send {
//     // fn add_active(&mut self, post: WcMx<dyn ActiveAcceptor<S>>, linker: AcMx<Linker<S>>);
//     // fn add_passive(&mut self, post: WcMx<dyn PassiveAcceptor<S>>, linker: AcMx<Linker<S>>);
// }

// pub trait NeuronGenerator<S: Send>: Send {
//     // fn add_active(&mut self, post: WcMx<dyn ActiveAcceptor<S>>, linker: AcMx<Linker<S>>);
//     // fn add_passive(&mut self, post: WcMx<dyn PassiveAcceptor<S>>, linker: AcMx<Linker<S>>);
// }

// pub trait SynapseGenerator<S: Send>: Send {
//     fn synapse_run_flag(&self) -> SynapseRunFlag;
//     // fn add_active(&mut self, post: WcMx<dyn ActiveAcceptor<S>>, linker: AcMx<Linker<S>>);
//     // fn add_passive(&mut self, post: WcMx<dyn PassiveAcceptor<S>>, linker: AcMx<Linker<S>>);
// }

// pub trait DeviceAcceptor<S: Send>: Send {
//     fn add(&mut self, pre: WcMx<dyn Generator<S>>, linker: AcMx<Linker<S>>);
// }

// pub trait NeuronAcceptor<S: Send>: Send {
//     fn add_synapse(&mut self, pre: WcMx<dyn SynapseGenerator<S>>, linker: AcMx<Linker<S>>);
//     fn add_device(&mut self, pre: WcMx<dyn Generator<S>>, linker: AcMx<Linker<S>>);
// }

// pub trait SynapseAcceptor<S: Send>: Send {
//     fn add(&mut self, pre: WcMx<dyn Generator<S>>, linker: AcMx<Linker<S>>);
// }

///required by Components
pub trait ActiveAcceptor<S: Send>: ActiveDevice + Acceptor<S> {}

impl<S, A> ActiveAcceptor<S> for A
where S: Send,
      A: Acceptor<S> + ActiveDevice,
{}

///required by Components, need for generate running_sets.
// Passive and has only 1 input channel, 1 type of input signal.
pub trait PassiveAcceptor<S: Send>: PassiveDevice + Acceptor<S> {}

impl<S, A> PassiveAcceptor<S> for A
where S: Send,
      A: Acceptor<S> + PassiveDevice,
{}

pub trait Neuron {}

pub trait Synapse {}

pub trait Device {}

pub fn connect_passive<G, A, S>(pre: AcMx<G>, post: AcMx<A>)
where G: 'static + Generator<S>,
      A: 'static + PassiveAcceptor<S> ,
      S: Send,
{
    let linker = Linker::new();
    pre.lock().unwrap().add_passive(Arc::<Mutex<A>>::downgrade(&post), Arc::clone(&linker));
    post.lock().unwrap().add(Arc::<Mutex<G>>::downgrade(&pre), linker);
}

pub fn connect_on_population_passive<G, A, S, PG, PA>(p1: &AcMx<PG>, n1: usize, p2: &AcMx<PA>, n2: usize)
where G: 'static + Generator<S>,
      A: 'static + PassiveAcceptor<S> ,
      S: Send,
      PG: HoldDevices<G>,
      PA: HoldDevices<A>,
{
    let device1 = p1.lock().unwrap().device_by_id(n1).clone();
    let device2 = p2.lock().unwrap().device_by_id(n2).clone();
    connect_passive(device1, device2);
}

pub fn connect_active<G, A, S>(pre: AcMx<G>, post: AcMx<A>)
where G: 'static + Generator<S>,
      A: 'static + ActiveAcceptor<S> ,
      S: Send,
{
    let linker = Linker::new();
    pre.lock().unwrap().add_active(Arc::<Mutex<A>>::downgrade(&post), Arc::clone(&linker));
    post.lock().unwrap().add(Arc::<Mutex<G>>::downgrade(&pre), linker);
}

pub fn connect_on_population_active<G, A, S, PG, PA>(p1: &AcMx<PG>, n1: usize, p2: &AcMx<PA>, n2: usize)
where G: 'static + Generator<S>,
      A: 'static + ActiveAcceptor<S> ,
      S: Send,
      PG: HoldDevices<G>,
      PA: HoldDevices<A>,
{
    let device1 = p1.lock().unwrap().device_by_id(n1).clone();
    let device2 = p2.lock().unwrap().device_by_id(n2).clone();
    connect_active(device1, device2);
}

// neuron acceptor has multi input, impossible to be passive! only active/passive of Synapse has to be considered.
// should be move to SynapseModel::new().
pub fn build_active_synapse<NPre, SPre, Syn, SPost, NPost>(pre: AcMx<NPre>, syn: AcMx<Syn>, post: AcMx<NPost>)
where NPre: 'static + Neuron + Generator<SPre>,
      SPre: Send,
      Syn: 'static + Synapse + ActiveAcceptor<SPre> + Generator<SPost>,
      SPost: Send,
      NPost: 'static + Neuron + ActiveAcceptor<SPost>,
{
    let linker = Linker::new();
    pre.lock().unwrap().add_active(Arc::<Mutex<Syn>>::downgrade(&syn), Arc::clone(&linker));
    syn.lock().unwrap().add(Arc::<Mutex<NPre>>::downgrade(&pre), linker);
    let linker = Linker::new();
    syn.lock().unwrap().add_active(Arc::<Mutex<NPost>>::downgrade(&post), Arc::clone(&linker));
    post.lock().unwrap().add(Arc::<Mutex<Syn>>::downgrade(&syn), linker);
}

pub fn build_passive_synapse<NPre, SPre, Syn, SPost, NPost>(pre: AcMx<NPre>, syn: AcMx<Syn>, post: AcMx<NPost>)
where NPre: 'static + Neuron + Generator<SPre>,
      SPre: Send,
      Syn: 'static + Synapse + PassiveAcceptor<SPre> + Generator<SPost>,
      SPost: Send,
      NPost: 'static + Neuron + ActiveAcceptor<SPost>,
{
    let linker = Linker::new();
    pre.lock().unwrap().add_passive(Arc::<Mutex<Syn>>::downgrade(&syn), Arc::clone(&linker));
    syn.lock().unwrap().add(Arc::<Mutex<NPre>>::downgrade(&pre), linker);
    let linker = Linker::new();
    syn.lock().unwrap().add_active(Arc::<Mutex<NPost>>::downgrade(&post), Arc::clone(&linker));
    post.lock().unwrap().add(Arc::<Mutex<Syn>>::downgrade(&syn), linker);
}
