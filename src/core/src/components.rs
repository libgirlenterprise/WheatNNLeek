use std::sync::{Arc, Mutex, Weak};
extern crate crossbeam_channel;
use crossbeam_channel::Receiver as CCReceiver;
use crossbeam_channel::Sender as CCSender;
use crossbeam_channel::TryIter as CCTryIter;
use crate::operation::{RunMode, DeviceMode};
use crate::connectivity::{Acceptor, Generator};

pub use multi_in_component::MultiInComponent;
pub use multi_out_component::MultiOutComponent;
pub use single_in_component::SingleInComponent;
pub use single_out_component::SingleOutComponent;

mod multi_in_component;
mod multi_out_component;
mod single_in_component;
mod single_out_component;

pub struct OutSet<C, S>
where C: Acceptor<S> + Send + ?Sized,
      S: Send,
{
    pub target: Weak<Mutex<C>>,
    pub channels: DeviceMode<ChannelsOutFFW<S>>,
    pub linker: Arc<Mutex<Linker<S>>>,
}

impl<C, S> OutSet<C, S>
where C: Acceptor<S> + Send + ?Sized,
      S: Send,
{
    pub fn new(target: Weak<Mutex<C>>, linker: Arc<Mutex<Linker<S>>>) -> OutSet<C, S> {
        OutSet {
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

pub struct InSet<C, S>
where C: Generator<S> + Send + ?Sized,
      S: Send,
{
    pub target: Weak<Mutex<C>>,
    pub channels: DeviceMode<ChannelsInFFW<S>>,
    pub linker: Arc<Mutex<Linker<S>>>,
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

pub struct TmpFFW<S: Send> {
    pub pre: Option<CCSender<S>>,
    pub post: Option<CCReceiver<S>>,
}

pub struct ChannelsOutFFW<S: Send> {
    pub ch_ffw: CCSender<S>,
}

pub struct ChannelsInFFW<S: Send> {
    pub ch_ffw: CCReceiver<S>,
}
