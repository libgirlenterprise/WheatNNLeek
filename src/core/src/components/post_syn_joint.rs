use std::sync::{Arc, Mutex, Weak};
use crate::{AcMx, WkMx};
use crossbeam_channel::Receiver as CCReceiver;
use crossbeam_channel::Sender as CCSender;
use crossbeam_channel::TryIter as CCTryIter;
use crate::operation::{RunMode, DeviceMode, Broadcast, RunningSet};
use crate::connectivity::{Acceptor, PassiveAcceptor, Generator};
use crate::components::{Linker, ChannelsCarrier};

pub struct PostSynJoint<A, C>
where A: Acceptor<C> + Send + ?Sized,
      // C: ChannelsCarrier + Send,
{
    pub target: Weak<Mutex<C>>,
    channels: DeviceMode<ChannelsOutFFW<S>>,
    linker: Arc<Mutex<Linker<S>>>,
}

impl<C, S> PostSynJoint<C, S>
where C: Acceptor<S> + Send + ?Sized,
      S: Send,
{
    pub fn new(target: Weak<Mutex<C>>, linker: Arc<Mutex<Linker<S>>>) -> PostSynJoint<C, S> {
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
    
    pub fn feedforward(&self, s: S) {
        match &self.channels {
            DeviceMode::Idle => (),
            DeviceMode::Feedforward(chs) => chs.ch_ffw.send(s).unwrap(),
        }
    }
}

impl<C, S> PostSynJoint<C, S>
where C: 'static + PassiveAcceptor<S> + Send + ?Sized,
      S: Send,
{
    pub fn running_target(&self) -> Option<RunningSet::<Broadcast, ()>> {
        match self.channels {
            DeviceMode::Idle => None,
            DeviceMode::Feedforward(_) => Some(RunningSet::<Broadcast, ()>::new(self.target.upgrade().unwrap()))
        }
    }
}

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

pub struct Linker<S: Send> {
    pre_mode: RunMode,
    post_mode: RunMode,
    tmp: DeviceMode<TmpFFW<S>>,
}

impl<S: Send> Linker<S> {
    pub fn new() -> Arc<Mutex<Linker<S>>> {
        Arc::new(Mutex::new(Linker {
            pre_mode: RunMode::Idle,
            post_mode: RunMode::Idle,
            tmp: DeviceMode::Idle,
        }))
    }

    pub fn config_post(&mut self, mode: RunMode) {
        match (mode, &mut self.post_mode) {
            (RunMode::Idle, _) => self.config_idle(),
            (_, RunMode::Idle) => self.post_mode = mode,
            (_, _) => panic!("config_post on Linker: from {:?} to {:?}.", self.post_mode, mode),
        }
    }

    pub fn config_pre(&mut self, mode: RunMode) {
        match (mode, &mut self.pre_mode) {
            (RunMode::Idle, _) => self.config_idle(),
            (_, RunMode::Idle) => self.pre_mode = mode,
            (_, _) => panic!("config_pre on Linker: from {:?} to {:?}.", self.pre_mode, mode),
        }
    }
    
    pub fn make_pre(&mut self) -> DeviceMode<ChannelsOutFFW<S>> {
        match (self.pre_mode, self.post_mode, &mut self.tmp) {
            (RunMode::Idle, _, DeviceMode::Idle) | (_, RunMode::Idle, DeviceMode::Idle) => {
                self.config_idle();
                DeviceMode::Idle
            },
            (RunMode::Idle, _, ch_m) | (_, RunMode::Idle, ch_m) => panic!(
                "pre or post of Linker is Idle, but Channels is {:?}!", RunMode::mode_from_device(&ch_m)
            ),

            (pre_m, post_m, ref ch_m) if pre_m != post_m => panic!(
                "pre & post of Linker configed into different modes! pre: {:?}, post: {:?}, ch: {:?}.",
                pre_m, post_m, RunMode::mode_from_device(&ch_m)
            ),

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

            (RunMode::Feedforward, RunMode::Feedforward, DeviceMode::Feedforward(chs)) => DeviceMode::Feedforward(
                ChannelsOutFFW {
                    ch_ffw: chs.pre.take().expect("None Out Channels in Linker!"),
                }
            ),
            // for use in future when having more modes.
            // (pre_m, post_m, ch_m) => panic!(
            //     "Channels of Linker != pre/post or idle. pre: {:?}, post: {:?}, ch: {:?}",
            //     pre_m, post_m, RunMode::mode_from_device(&ch_m)
            // ),
        }
    }

    pub fn make_post(&mut self) -> DeviceMode<ChannelsInFFW<S>> {
        match (self.pre_mode, self.post_mode, &mut self.tmp) {
            (RunMode::Idle, _, DeviceMode::Idle) | (_, RunMode::Idle, DeviceMode::Idle) => {
                self.config_idle();
                DeviceMode::Idle
            },
            (RunMode::Idle, _, ch_m) | (_, RunMode::Idle, ch_m) => panic!(
                "pre or post of Linker is Idle, but Channels is {:?}!", RunMode::mode_from_device(&ch_m)
            ),

            (pre_m, post_m, ref ch_m) if pre_m != post_m => panic!(
                "pre & post of Linker configed into different modes! pre: {:?}, post: {:?}, ch: {:?}.",
                pre_m, post_m, RunMode::mode_from_device(&ch_m)
            ),

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

            (RunMode::Feedforward, RunMode::Feedforward, DeviceMode::Feedforward(chs)) => DeviceMode::Feedforward(
                ChannelsInFFW {
                    ch_ffw: chs.post.take().expect("None Out Channels in Linker!"),
                }
            ),
            // for use in future when having more modes
            // (pre_m, post_m, ch_m) => panic!(
            //     "Channels of Linker != pre/post or idle. pre: {:?}, post: {:?}, ch: {:?}",
            //     pre_m, post_m, RunMode::mode_from_device(&ch_m)
            // ),
        }
    }

    pub fn config_idle(&mut self) {
        self.pre_mode = RunMode::Idle;
        self.post_mode = RunMode::Idle;
        self.tmp = DeviceMode::Idle;
    }
}

pub struct TmpContent<SF: Send, SB: Send> {
    pub ch: PostSynFlag<{
        pub ffw_pre: Option<CCSender<S>>,
        pub ffw_post: Option<CCReceiver<S>>,
    }, {
        pub ffw_pre: Option<CCSender<S>>,
        pub ffw_post: Option<CCReceiver<S>>,
        pub fbw_pre: Option<CCSender<S>>,
        pub fbw_post: Option<CCReceiver<S>>,
    }>,
    // pub ffw_pre: Option<CCSender<SF>>,
    // pub ffw_post: Option<CCReceiver<SF>>,
    // pub ffw_pre: Option<CCReceiver<SB>>,
    // pub fbw_post: Option<CCSender<SB>>,
}

pub struct ChsOut<SF: Send, SB: Send> {
    pub ch: PostSynFlag<{
        pub ch_ffw: CCSender<SF>,
    }, {
        pub ch_ffw: CCSender<SF>,
        pub ch_fbw: CCReceiver<SB>,
    }>,
    // pub ch_ffw: CCSender<SF>,
    // pub ch_fbw: CCReceiver<SB>,
}

pub struct ChsIn<SF: Send, SB: Send> {
    pub ch: PostSynFlag<{pub ch_ffw: CCReceiver<SF>},
                        {pub ch_ffw: CCReceiver<SF>,
                         pub ch_fbw: CCSender<SB>}>,
    // pub ch_ffw: CCReceiver<SF>,
    // pub ch_ffw: CCSender<SB>
}

pub struct PostSynChsCarrier<SF: Send, SB: Send> {
    content: PostSynFlag<DeviceMode<
            ContentSimpleFFW<SF>,
        >>,
    
}

enum SynapseFlag {
    Simple,
    STDP,
}

enum PostSynFlag<SI, ST> {
    Simple(SI),
    STDP(ST),
}

pub struct ChannelsOutFFW<S: Send> {
    pub ch_ffw: CCSender<S>,
}

pub struct ChannelsInFFW<S: Send> {
    pub ch_ffw: CCReceiver<S>,
}
