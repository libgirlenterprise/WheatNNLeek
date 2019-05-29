use std::sync::{Arc, Mutex, Weak};
use crate::{AcMx, WkMx};
use crossbeam_channel::Receiver as CCReceiver;
use crossbeam_channel::Sender as CCSender;
use crossbeam_channel::TryIter as CCTryIter;
use crate::operation::{RunMode, DeviceMode, Broadcast, RunningSet};
use crate::connectivity::{Acceptor, PassiveAcceptor, Generator};
use crate::components::joints::{Linker, ChannelsCarrier};
use crate::components::joints::channels_sets::{SimpleForeChs, SimpleBackChs};
use crate::components::joints::tmp_contents::TmpContentSimpleFwd;

type SimpleLinker<S> = AcMx<Linker<SimpleChsCarrier<S>>>;

pub struct SimpleForeJoint<A, S>
where A: Acceptor<SimpleChsCarrier<S>> + Send + ?Sized,
      S: Send,
{
    pub target: WkMx<A>,
    channels: SimpleForeChs<S>,
    linker: SimpleLinker<S>,
}

impl<A, S> SimpleForeJoint<A, S>
where A: Acceptor<SimpleChsCarrier<S>> + Send + ?Sized,
      S: Send,
{
    pub fn new(target: WkMx<A>, linker: SimpleLinker<S>) -> SimpleForeJoint<A, S> {
        SimpleForeJoint {
            target,
            channels: DeviceMode::Idle,
            linker,
        }
    }

    pub fn config_mode(&mut self, mode: RunMode) {
        match mode {
            RunMode::Idle => {
                self.channels = DeviceMode::Idle;
                self.linker.lock().unwrap().config_idle();
            },
            _  => self.linker.lock().unwrap().config_pre(mode),
        }
    }

    pub fn config_channels(&mut self) {
        let mut lnkr = self.linker.lock().unwrap();
        self.channels = lnkr.make_pre();
    }
    
    pub fn feedforward(&self, s: S) {
        match &self.channels {
            DeviceMode::Idle => (),
            DeviceMode::Feedforward(chs) => chs.ch_ffw.send(s).unwrap(),
        }
    }
}

impl<A, S> SimpleForeJoint<A, S>
where A: 'static + PassiveAcceptor<SimpleChsCarrier<S>> + Send + ?Sized,
      S: Send,
{
    pub fn running_target(&self) -> Option<RunningSet::<Broadcast, ()>> {
        match self.channels {
            DeviceMode::Idle => None,
            DeviceMode::Feedforward(_) => Some(RunningSet::<Broadcast, ()>::new(self.target.upgrade().unwrap()))
        }
    }
}

pub struct SimpleBackJoint<G, S>
where G: Generator<SimpleChsCarrier<S>> + Send + ?Sized,
      S: Send,
{
    pub target: Weak<Mutex<G>>,
    channels: SimpleBackChs<S>,
    linker: SimpleLinker<S>,
}

impl<G, S> SimpleBackJoint<G, S>
where G: Generator<SimpleChsCarrier<S>> + Send + ?Sized,
      S: Send,
{
    pub fn new(target: Weak<Mutex<G>>, linker: SimpleLinker<S>) -> SimpleBackJoint<G, S> {
        SimpleBackJoint {
            target,
            channels: DeviceMode::Idle,
            linker,
        }
    }

    pub fn config_mode(&mut self, mode: RunMode) {
        match mode {
            RunMode::Idle => {
                self.channels = DeviceMode::Idle;
                self.linker.lock().unwrap().config_idle();
            },
            _  => self.linker.lock().unwrap().config_post(mode),
        }
    }

    pub fn config_channels(&mut self) {
        let mut lnkr = self.linker.lock().unwrap();
        self.channels = lnkr.make_post();
    }

    pub fn ffw_accepted_iter(&self) -> Option<CCTryIter<S>> {
        match &self.channels {
            DeviceMode::Idle => None,
            DeviceMode::Feedforward(chs_in_ffw) => Some(chs_in_ffw.ch_ffw.try_iter()),
        }
    }
}

struct SimpleChsCarrier<S: Send> {
    content: DeviceMode<TmpContentSimpleFwd<S>>,
}

impl<S:Send> ChannelsCarrier for SimpleChsCarrier<S> {
    type BackEndChs = SimpleBackChs<S>;
    type ForeEndChs = SimpleForeChs<S>;

    fn new() -> SimpleChsCarrier<S> {
        SimpleChsCarrier {content: DeviceMode::Idle}
    }

    // fn reset_idle(&mut self) {
        
    // }

    
    // fn mode(&self) -> RunMode;
    // fn make_pre(&mut self, mode: RunMode) -> DeviceMode<<Self as ChannelsCarrier>::ChsOutFFW>;
    // fn take_pre(&mut self) -> DeviceMode<<Self as ChannelsCarrier>::ChsOutFFW>;
    // fn make_post(&mut self, mode: RunMode) -> DeviceMode<<Self as ChannelsCarrier>::ChsInFFW>;
    // fn take_post(&mut self) -> DeviceMode<<Self as ChannelsCarrier>::ChsInFFW>;
}
