use std::sync::{Arc, Mutex};
use crate::operation::{PassiveDevice, ActiveDevice};
use crate::components::Linker;
use crate::populations::HoldDevices;
use crate::{AcMx, WcMx};

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

// /// impl on Devices
// pub trait CanAccept<S: Send>: Send {
//     fn add<G: CanGenerate<S>>(&mut self, pre: WcMx<G>>, linker: AcMx<Linker<S>>>);
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


// pub fn connect_passive<S> (pre: AcMx<dyn Generator<S>>, post: AcMx<dyn PassiveAcceptor<S>>)
// where S: Send,
// {
//     let linker = Linker::new();
//     pre.lock().unwrap().add_passive(Arc::downgrade(&post), Arc::clone(&linker));
//     post.lock().unwrap().add(Arc::downgrade(&pre), linker);
// }

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

// pub fn connect_active<S> (pre: AcMx<dyn Generator<S>>, post: AcMx<dyn ActiveAcceptor<S>>)
// where S: Send,
// {
//     let linker = Linker::new();
//     pre.lock().unwrap().add_active(Arc::downgrade(&post), Arc::clone(&linker));
//     post.lock().unwrap().add(Arc::downgrade(&pre), linker);
// }

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
