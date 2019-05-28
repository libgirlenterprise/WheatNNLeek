use std::sync::{Arc, Mutex, Weak};
use crate::{AcMx, WkMx};
use crossbeam_channel::Receiver as CCReceiver;
use crossbeam_channel::Sender as CCSender;
use crossbeam_channel::TryIter as CCTryIter;
use crate::operation::{RunMode, DeviceMode, Broadcast, RunningSet};
use crate::connectivity::{Acceptor, PassiveAcceptor, Generator};
use crate::components::{Linker, ChannelsCarrier};

pub struct PostSynOutJoint<A, SF, SB>
where A: Acceptor<PostSynChsCarrier<SF, SB>> + Send + ?Sized,
      SF: Send,
      SB: Send,
{
    pub target: Weak<Mutex<A>>,
    channels: DeviceMode<PostSynChsOutFwd<SF, SB>>,
    linker: AcMx<Linker<PostSynChsCarrier<SF, SB>>>,
}

impl<C, S> PostSynOutJoint<C, S>
where A: Acceptor<PostSynChsCarrier<SF, SB>> + Send + ?Sized,
      SF: Send,
      SB: Send,
{
    pub fn new(target: Weak<Mutex<A>>, linker: AcMx<Linker<PostSynChsCarrier<SF, SB>>>) -> PostSynOutJoint<C, S> {
        PostSynJoint {
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
    
    pub fn feedforward(&self, s: SF) {
        match &self.channels {
            DeviceMode::Idle => (),
            DeviceMode::Feedforward(chs) => chs.feedforward(),
        }
    }
}

impl<C, S> PostSynOutJoint<C, S>
where A: 'static + PassiveAcceptor<PostSynChsCarrier<SF, SB>> + Send + ?Sized,
      SF: Send,
      SB: Send,
{
    pub fn running_target(&self) -> Option<RunningSet::<Broadcast, ()>> {
        match self.channels {
            DeviceMode::Idle => None,
            DeviceMode::Feedforward(_) => Some(RunningSet::<Broadcast, ()>::new(self.target.upgrade().unwrap()))
        }
    }
}

pub struct PostSynInJoint<G, C, SF, SB>
where A: Acceptor<C> + Send + ?Sized,
      SF: Send,
      SB: Send,
pub struct InSet<C, S>
where C: Generator<S> + Send + ?Sized,
      S: Send,
{
    target: Weak<Mutex<C>>,
    channels: DeviceMode<ChannelsInFFW<S>>,
    linker: Arc<Mutex<Linker<S>>>,
}

impl<C, S> InSet<C, S>
where C: Generator<S> + Send + ?Sized,
      S: Send,
{
    pub fn new(target: Weak<Mutex<C>>, linker: Arc<Mutex<Linker<S>>>) -> InSet<C, S> {
        InSet {
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

    pub fn feedbackward(&self, S) {
        match &self.channels {
            DeviceMode::Idle => (),
            DeviceMode::Feedforward(chs) => chs.ch_ffw.send(s).unwrap(),
        }
    }
    
}

pub struct PostSynChsOutFwd<SF: Send, SB: Send> {
    pub ch_ffw: CCSender<SF>,
    pub ch_fbw: PostSynFlag<(), CCReceiver<SB>>,
}

pub struct PostSynChsInFwd<SF: Send, SB: Send> {
    pub ch_ffw: CCReceiver<SF>,
    pub ch_fbw: PostSynFlag<(), CCSender<SB>>,
}

pub struct TmpContentSimpleFwd<SF: Send, SB: Send> {
    pub ffw_pre: Option<CCSender<SF>>,
    pub ffw_post: Option<CCReceiver<SF>>,
}

pub struct TmpContentStdpFwd<SF: Send, SB: Send> {
    pub ffw_pre: Option<CCSender<SF>>,
    pub ffw_post: Option<CCReceiver<SF>>,
    pub ffw_pre: Option<CCReceiver<SB>>,
    pub fbw_post: Option<CCSender<SB>>,
}

pub struct PostSynChsCarrier<SF: Send, SB: Send> {
    content: PostSynFlag<DeviceMode<TmpContentSimpleFwd>,
                         DeviceMode<TmpContentStdpFwd>>,
}

impl<SF: Send, SB: Send> ChannelsCarrier for  PostSynChsCarrier<SF, SB> {
    type ChsInFwd =  PostSynChsInFwd<SF, SB>;
    type ChsOutFwd = PostSynChsOutFwd<SF, SB>;
    
    fn new() -> Self {
        
    }
    fn reset_idle(&mut self);
    fn mode(&self) -> RunMode;
    fn make_pre(&mut self, mode: RunMode) -> DeviceMode<<Self as ChannelsCarrier>::ChsOutFFW>;
    fn take_pre(&mut self) -> DeviceMode<<Self as ChannelsCarrier>::ChsOutFFW>;
    fn make_post(&mut self, mode: RunMode) -> DeviceMode<<Self as ChannelsCarrier>::ChsInFFW>;
    fn take_post(&mut self) -> DeviceMode<<Self as ChannelsCarrier>::ChsInFFW>;

        pub fn make_pre(&mut self) -> DeviceMode<ChannelsOutFFW<S>> {
        match (self.pre_mode, self.post_mode, &mut self.tmp) {

            (RunMode::Feedforward, RunMode::Feedforward, DeviceMode::Idle) => {
                let (tx, rx) = crossbeam_channel::unbounded();
                self.tmp = DeviceMode::Feedforward(TmpFFW {
                    pre: None,
                    post: Some(rx),
                });
                DeviceMode::Feedforward(
                    ChannelsOutFFW {
                        ch_ffw: tx
                    }
                )                            
            },
        }
    }

    pub fn make_post(&mut self) -> DeviceMode<ChannelsInFFW<S>> {
        match (self.pre_mode, self.post_mode, &mut self.tmp) {
            (RunMode::Feedforward, RunMode::Feedforward, DeviceMode::Idle) => {
                let (tx, rx) = crossbeam_channel::unbounded();
                self.tmp = DeviceMode::Feedforward(TmpFFW {
                    pre: Some(tx),
                    post: None,
                });
                DeviceMode::Feedforward(
                    ChannelsInFFW {
                        ch_ffw: rx
                    }
                )                            
            },
        }
    }
}

enum SynapseFlag {
    Simple,
    STDP,
}

enum PostSynFlag<SI, ST> {
    Simple(SI),
    STDP(ST),
}
