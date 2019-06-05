use std::sync::{Mutex, Weak, Arc};
use crate::{AcMx, WkMx};
use crossbeam_channel::TryIter as CCTryIter;
use crate::operation::{RunMode, AgentRunMode, Broadcast, RunningSet};
use crate::connectivity::{Acceptor, PassiveAcceptor, Generator, ChannelsCarrier};
use crate::connectivity::linker::Linker;
use crate::connectivity::channels_sets::{SimpleForeChs, SimpleBackChs, SimpleForeChsFwd, SimpleBackChsFwd};
use crate::connectivity::tmp_contents::TmpContentSimpleFwd;

pub type AcMxSimpleLnkr<S> = AcMx<Linker<SimpleChsCarrier<S>>>;

pub struct SimpleForeJoint<A, S>
where A: Acceptor<SimpleChsCarrier<S>> + Send + ?Sized,
      S: Send,
{
    pub target: WkMx<A>,
    channels: SimpleForeChs<S>,
    linker: AcMxSimpleLnkr<S>,
}

impl<A, S> SimpleForeJoint<A, S>
where A: Acceptor<SimpleChsCarrier<S>> + Send + ?Sized,
      S: Send,
{
    pub fn new(target: AcMx<A>, linker: AcMxSimpleLnkr<S>) -> SimpleForeJoint<A, S> {
        SimpleForeJoint {
            target: Arc::downgrade(&target),
            channels: AgentRunMode::Idle,
            linker,
        }
    }

    pub fn config_mode(&mut self, mode: RunMode) {
        match mode {
            RunMode::Idle => {
                self.channels = AgentRunMode::Idle;
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
            AgentRunMode::Idle => (),
            AgentRunMode::ForwardStepping(chs) => chs.ch_ffw.send(s).unwrap(),
            AgentRunMode::ForwardRealTime => panic!("ForwardRealTime not yet implemented!"),
        }
    }
}

impl<A, S> SimpleForeJoint<A, S>
where A: 'static + PassiveAcceptor<SimpleChsCarrier<S>> + Send + ?Sized,
      S: Send,
{
    pub fn running_target(&self) -> Option<RunningSet::<Broadcast, ()>> {
        match self.channels {
            AgentRunMode::Idle => None,
            AgentRunMode::ForwardStepping(_) => Some(RunningSet::<Broadcast, ()>::new(self.target.upgrade().unwrap())),
            AgentRunMode::ForwardRealTime => panic!("ForwardRealTime not yet implemented!"),
        }
    }
}

pub struct SimpleBackJoint<G, S>
where G: Generator<SimpleChsCarrier<S>> + Send + ?Sized,
      S: Send,
{
    pub target: Weak<Mutex<G>>,
    channels: SimpleBackChs<S>,
    linker: AcMxSimpleLnkr<S>,
}

impl<G, S> SimpleBackJoint<G, S>
where G: Generator<SimpleChsCarrier<S>> + Send + ?Sized,
      S: Send,
{
    pub fn new(target: AcMx<G>, linker: AcMxSimpleLnkr<S>) -> SimpleBackJoint<G, S> {
        SimpleBackJoint {
            target: Arc::downgrade(&target),
            channels: AgentRunMode::Idle,
            linker,
        }
    }

    pub fn config_mode(&mut self, mode: RunMode) {
        match mode {
            RunMode::Idle => {
                self.channels = AgentRunMode::Idle;
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
            AgentRunMode::Idle => None,
            AgentRunMode::ForwardStepping(chs_in_ffw) => Some(chs_in_ffw.ch_ffw.try_iter()),
            AgentRunMode::ForwardRealTime => panic!("ForwardRealTime not yet implemented!"),
        }
    }
}

pub struct SimpleChsCarrier<S: Send> {
    content: AgentRunMode<TmpContentSimpleFwd<S>>,
}

impl<S:Send> ChannelsCarrier for SimpleChsCarrier<S> {
    type BackEndChs = SimpleBackChs<S>;
    type ForeEndChs = SimpleForeChs<S>;

    fn new() -> SimpleChsCarrier<S> {
        SimpleChsCarrier {content: AgentRunMode::Idle}
    }

    fn reset_idle(&mut self) {
        self.content = AgentRunMode::Idle;
    }
    
    fn mode(&self) -> RunMode {
        self.content.variant()
    }

    fn fore_chs(&mut self, mode: RunMode) -> <Self as ChannelsCarrier>::ForeEndChs {
        match (mode, &mut self.content) {
            (RunMode::Idle, AgentRunMode::Idle) => AgentRunMode::Idle,
            (RunMode::Idle, c_mode) => panic!(
                "SimpleChsCarrier fore_chs(Idle) should be run after reset_idle, should be unreachable! content: {:?}",
                c_mode.variant()
            ),

            (RunMode::ForwardStepping, AgentRunMode::Idle) => {
                let (tx, rx) = crossbeam_channel::unbounded();
                self.content = AgentRunMode::ForwardStepping(TmpContentSimpleFwd {
                    ffw_pre: None,
                    ffw_post: Some(rx),
                });
                AgentRunMode::ForwardStepping(
                    SimpleForeChsFwd {
                        ch_ffw: tx,
                    }
                )
            },
            
            (RunMode::ForwardStepping, AgentRunMode::ForwardStepping(content)) => AgentRunMode::ForwardStepping(
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
            (RunMode::Idle, AgentRunMode::Idle) => AgentRunMode::Idle,
            (RunMode::Idle, c_mode) => panic!(
                "SimpleChsCarrier back_chs(Idle) should be run after reset_idle, should be unreachable! content: {:?}",
                c_mode.variant()
            ),

            (RunMode::ForwardStepping, AgentRunMode::Idle) => {
                let (tx, rx) = crossbeam_channel::unbounded();
                self.content = AgentRunMode::ForwardStepping(TmpContentSimpleFwd {
                    ffw_pre: Some(tx),
                    ffw_post: None,
                });
                AgentRunMode::ForwardStepping(
                    SimpleBackChsFwd {
                        ch_ffw: rx,
                    }
                )
            },
            
            (RunMode::ForwardStepping, AgentRunMode::ForwardStepping(content)) => AgentRunMode::ForwardStepping(
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
