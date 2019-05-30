use std::sync::{Arc, Mutex, Weak};
use crate::{AcMx, WkMx};
use crossbeam_channel::Receiver as CCReceiver;
use crossbeam_channel::Sender as CCSender;
use crossbeam_channel::TryIter as CCTryIter;
use crate::operation::{RunMode, DeviceMode, Broadcast, RunningSet};
use crate::connectivity::{Acceptor, PassiveAcceptor, Generator, ChannelsCarrier};
use crate::connectivity::linker::Linker;
use crate::connectivity::channels_sets::{SimpleForeChs, SimpleBackChs, SimpleForeChsFwd, SimpleBackChsFwd};
use crate::connectivity::tmp_contents::TmpContentSimpleFwd;

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
        self.channels = lnkr.fore_end_chs();
    }
    
    pub fn feedforward(&self, s: S) {
        match &self.channels {
            DeviceMode::Idle => (),
            DeviceMode::ForwardStepping(chs) => chs.ch_ffw.send(s).unwrap(),
            DeviceMode::ForwardRealTime => panic!("ForwardRealTime not yet implemented!"),
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
            DeviceMode::ForwardStepping(_) => Some(RunningSet::<Broadcast, ()>::new(self.target.upgrade().unwrap())),
            DeviceMode::ForwardRealTime => panic!("ForwardRealTime not yet implemented!"),
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
        self.channels = lnkr.back_end_chs();
    }

    pub fn ffw_accepted_iter(&self) -> Option<CCTryIter<S>> {
        match &self.channels {
            DeviceMode::Idle => None,
            DeviceMode::ForwardStepping(chs_in_ffw) => Some(chs_in_ffw.ch_ffw.try_iter()),
            DeviceMode::ForwardRealTime => panic!("ForwardRealTime not yet implemented!"),
        }
    }
}

pub struct SimpleChsCarrier<S: Send> {
    content: DeviceMode<TmpContentSimpleFwd<S>>,
}

impl<S:Send> ChannelsCarrier for SimpleChsCarrier<S> {
    type BackEndChs = SimpleBackChs<S>;
    type ForeEndChs = SimpleForeChs<S>;

    fn new() -> SimpleChsCarrier<S> {
        SimpleChsCarrier {content: DeviceMode::Idle}
    }

    fn reset_idle(&mut self) {
        self.content = DeviceMode::Idle;
    }
    
    fn mode(&self) -> RunMode {
        self.content.variant()
    }

    fn fore_chs(&mut self, mode: RunMode) -> <Self as ChannelsCarrier>::ForeEndChs {
        match (mode, &mut self.content) {
            (RunMode::Idle, DeviceMode::Idle) => DeviceMode::Idle,
            (RunMode::Idle, c_mode) => panic!(
                "SimpleChsCarrier fore_chs(Idle) should be run after reset_idle, should be unreachable! content: {:?}",
                c_mode.variant()
            ),

            (RunMode::ForwardStepping, DeviceMode::Idle) => {
                let (tx, rx) = crossbeam_channel::unbounded();
                self.content = DeviceMode::ForwardStepping(TmpContentSimpleFwd {
                    ffw_pre: None,
                    ffw_post: Some(rx),
                });
                DeviceMode::ForwardStepping(
                    SimpleForeChsFwd {
                        ch_ffw: tx,
                    }
                )
            },
            
            (RunMode::ForwardStepping, DeviceMode::ForwardStepping(content)) => DeviceMode::ForwardStepping(
                SimpleForeChsFwd {
                    ch_ffw: content.ffw_pre.take().expect("No ffw_pre in TmpContentSimpleFwd!")
                }
            ),

            (RunMode::ForwardRealTime, _) => {
                panic!("RunMode Forwardrealtime not yet implemented!")
            },
            (cmd_mode, car_mode) => {
                panic!("SimpleChsCarrier pre_chs() w/ unmatched modes, cmd: {:?}, carrier: {:?}.", cmd_mode, car_mode.variant());
            },
        }
    }

    fn back_chs(&mut self, mode: RunMode) -> <Self as ChannelsCarrier>::BackEndChs {
        match (mode, &mut self.content) {
            (RunMode::Idle, DeviceMode::Idle) => DeviceMode::Idle,
            (RunMode::Idle, c_mode) => panic!(
                "SimpleChsCarrier back_chs(Idle) should be run after reset_idle, should be unreachable! content: {:?}",
                c_mode.variant()
            ),

            (RunMode::ForwardStepping, DeviceMode::Idle) => {
                let (tx, rx) = crossbeam_channel::unbounded();
                self.content = DeviceMode::ForwardStepping(TmpContentSimpleFwd {
                    ffw_pre: Some(tx),
                    ffw_post: None,
                });
                DeviceMode::ForwardStepping(
                    SimpleBackChsFwd {
                        ch_ffw: rx,
                    }
                )
            },
            
            (RunMode::ForwardStepping, DeviceMode::ForwardStepping(content)) => DeviceMode::ForwardStepping(
                SimpleBackChsFwd {
                    ch_ffw: content.ffw_post.take().expect("No ffw_post in TmpContentSimpleFwd!")
                }
            ),

            (RunMode::ForwardRealTime, _) => {
                panic!("RunMode Forwardrealtime not yet implemented!")
            },
            (cmd_mode, car_mode) => {
                panic!("SimpleChsCarrier post_chs() w/ unmatched modes, cmd: {:?}, carrier: {:?}.", cmd_mode, car_mode.variant());
            },
        }        
    }
}
