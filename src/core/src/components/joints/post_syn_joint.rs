use std::sync::{Mutex, Weak};
use crate::{AcMx, WkMx};
use crossbeam_channel;
use crossbeam_channel::TryIter as CCTryIter;
use crate::operation::{RunMode, DeviceMode, Broadcast, RunningSet};
use crate::connectivity::{Acceptor, PassiveAcceptor, Generator};
use crate::components::joints::{Linker, ChannelsCarrier};
use crate::components::joints::channels_sets::{PostSynBackChs, PostSynForeChs, PostSynBackChsFwd, PostSynForeChsFwd};
use crate::components::joints::tmp_contents::{TmpContentSimpleFwd, TmpContentStdpFwd};
use crate::devices::synapses::{SynapseFlag, PostSynFlag};

type PostSynLinker<SF, SB> = AcMx<Linker<PostSynChsCarrier<SF, SB>>>;

pub struct PostSynForeJoint<A, SF, SB>
where A: Acceptor<PostSynChsCarrier<SF, SB>> + Send + ?Sized,
      SF: Send,
      SB: Send,
{
    pub target: WkMx<A>,
    channels: PostSynForeChs<SF, SB>,
    linker: PostSynLinker<SF, SB>,
}

impl<A, SF, SB> PostSynForeJoint<A, SF, SB>
where A: Acceptor<PostSynChsCarrier<SF, SB>> + Send + ?Sized,
      SF: Send,
      SB: Send,
{
    pub fn new(target: Weak<Mutex<A>>, linker: PostSynLinker<SF, SB>) -> PostSynForeJoint<A, SF, SB> {
        PostSynForeJoint {
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
    
    pub fn feedforward(&self, s: SF) {
        match &self.channels {
            DeviceMode::Idle => (),
            DeviceMode::ForwardStepping(chs) => chs.feedforward(s),
            _ => panic!("not yet implemented!"),
        }
    }
}

impl<A, SF, SB> PostSynForeJoint<A, SF, SB>
where A: 'static + PassiveAcceptor<PostSynChsCarrier<SF, SB>> + Send + ?Sized,
      SF: Send,
      SB: Send,
{
    pub fn running_target(&self) -> Option<RunningSet::<Broadcast, ()>> {
        match self.channels {
            DeviceMode::Idle => None,
            DeviceMode::ForwardStepping(_) => Some(RunningSet::<Broadcast, ()>::new(self.target.upgrade().unwrap())),
            DeviceMode::ForwardRealTime => panic!("ForwardRealTime not yet implemented!"),
        }
    }
}

pub struct PostSynBackJoint<G, SF, SB>
where G: Generator<PostSynChsCarrier<SF, SB>> + Send + ?Sized,
      SF: Send,
      SB: Send,
{
    pub target: Weak<Mutex<G>>,
    channels: PostSynBackChs<SF, SB>,
    linker: PostSynLinker<SF, SB>,
}

impl<G, SF, SB> PostSynBackJoint<G, SF, SB>
where G: Generator<PostSynChsCarrier<SF, SB>> + Send + ?Sized,
      SF: Send,
      SB: Send,
{
    pub fn new(target: WkMx<G>, linker: PostSynLinker<SF, SB>) -> PostSynBackJoint<G, SF, SB> {
        PostSynBackJoint {
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

    pub fn ffw_accepted_iter(&self) -> Option<CCTryIter<SF>> {
        match &self.channels {
            DeviceMode::Idle => None,
            DeviceMode::ForwardStepping(chs_in_ffw) => Some(chs_in_ffw.ch_ffw.try_iter()),
            DeviceMode::ForwardRealTime => panic!("ForwardRealTime not yet implemented!"),
        }
    }

    pub fn feedbackward(&self, s: SB) {
        match &self.channels {
            DeviceMode::Idle => (),
            DeviceMode::ForwardStepping(chs) => chs.feedbackward(s),
            DeviceMode::ForwardRealTime => panic!("ForwardRealTime not yet implemented!"),
        }
    }
    
}

pub struct PostSynChsCarrier<SF: Send, SB: Send> {
    content: PostSynFlag<DeviceMode<TmpContentSimpleFwd<SF>>,
                         DeviceMode<TmpContentStdpFwd<SF, SB>>>,
}

impl<SF: Send, SB: Send> ChannelsCarrier for  PostSynChsCarrier<SF, SB> {
    type BackEndChs = PostSynBackChs<SF, SB>;
    type ForeEndChs = PostSynForeChs<SF, SB>;
    
    fn new() -> Self {
        PostSynChsCarrier {content: PostSynFlag::Simple(DeviceMode::Idle)}
    }
        
    fn reset_idle(&mut self) {
        self.content = match &self.content {
            PostSynFlag::Simple(_) => PostSynFlag::Simple(DeviceMode::Idle),
            PostSynFlag::STDP(_) => PostSynFlag::STDP(DeviceMode::Idle),
        };
    }
    
    fn mode(&self) -> RunMode {
        match &self.content {
            PostSynFlag::Simple(d_mode) => d_mode.variant(),
            PostSynFlag::STDP(d_mode) => d_mode.variant(),
        }
    }

    fn fore_chs(&mut self, mode: RunMode) -> <Self as ChannelsCarrier>::ForeEndChs {
        match (mode, self.mode(), &mut self.content) {
            (RunMode::Idle, RunMode::Idle, _) => DeviceMode::Idle,
            (RunMode::Idle, c_mode, content) => panic!(
                "PostSynChsCarrier fore_chs(Idle) should be run after reset_idle, should be unreachable! DeviceMode: {:?}, Flag: {:?}",
                c_mode,
                content.variant()
            ),

            (RunMode::ForwardStepping, RunMode::Idle, PostSynFlag::Simple(_)) => {
                let (tx, rx) = crossbeam_channel::unbounded();
                self.content = PostSynFlag::Simple(DeviceMode::ForwardStepping(TmpContentSimpleFwd {
                    ffw_pre: None,
                    ffw_post: Some(rx),
                }));
                DeviceMode::ForwardStepping(
                    PostSynForeChsFwd {
                        ch_ffw: tx,
                        ch_fbw: PostSynFlag::Simple(()),
                    }
                )
            },

            (RunMode::ForwardStepping, RunMode::Idle, PostSynFlag::STDP(_)) => {
                let (ffw_tx, ffw_rx) = crossbeam_channel::unbounded();
                let (fbw_tx, fbw_rx) = crossbeam_channel::unbounded();
                self.content = PostSynFlag::STDP(DeviceMode::ForwardStepping(TmpContentStdpFwd {
                    ffw_pre: None,
                    ffw_post: Some(ffw_rx),
                    fbw_pre: None,
                    fbw_post: Some(fbw_tx),
                }));
                DeviceMode::ForwardStepping(
                    PostSynForeChsFwd {
                        ch_ffw: ffw_tx,
                        ch_fbw: PostSynFlag::STDP(fbw_rx),
                    }
                )
            },
    
            (RunMode::ForwardStepping, RunMode::ForwardStepping, PostSynFlag::Simple(x)) => DeviceMode::ForwardStepping(
                PostSynForeChsFwd {
                    ch_ffw: match x {
                        DeviceMode::ForwardStepping(content) => {
                            content.ffw_pre.take().expect("No ffw_pre in TmpContentSimpleFwd!")
                        },
                        _ => panic!("unreachable!"),
                    },
                    ch_fbw: PostSynFlag::Simple(()),
                }
            ),
            (RunMode::ForwardStepping, RunMode::ForwardStepping, PostSynFlag::STDP(x)) => DeviceMode::ForwardStepping(
                PostSynForeChsFwd {
                    ch_ffw: match x {
                        DeviceMode::ForwardStepping(content) => {
                            content.ffw_pre.take().expect("No ffw_pre in TmpContentSimpleFwd!")
                        },
                        _ => panic!("unreachable!"),
                    },
                    ch_fbw: PostSynFlag::STDP(match x {
                        DeviceMode::ForwardStepping(content) => {
                            content.fbw_pre.take().expect("No fbw_pre in TmpContentSimpleFwd!")
                        },
                        _ => panic!("unreachable!"),
                    }),
                }
            ),
            (RunMode::ForwardRealTime, _, _) => {
                panic!("RunMode Forwardrealtime not yet implemented!")
            },
            (cmd_mode, car_mode, _) => {
                panic!("PostSynChsCarrier make_pre() w/ unmatched modes, cmd: {:?}, carrier: {:?}.", cmd_mode, car_mode);
            }
        }
    }

    fn back_chs(&mut self, mode: RunMode) -> <Self as ChannelsCarrier>::BackEndChs {
        
        match (mode, self.mode(), &mut self.content) {
            (RunMode::Idle, RunMode::Idle, _) => DeviceMode::Idle,
            (RunMode::Idle, c_mode, flag) => panic!(
                "PostSynChsCarrier back_chs(Idle) should be run after reset_idle, should be unreachable! DeviceMode: {:?}, Flag: {:?}",
                c_mode,
                flag.variant()
            ),

            (RunMode::ForwardStepping, RunMode::Idle, PostSynFlag::Simple(_)) => {
                let (tx, rx) = crossbeam_channel::unbounded();
                self.content = PostSynFlag::Simple(DeviceMode::ForwardStepping(TmpContentSimpleFwd {
                    ffw_pre: Some(tx),
                    ffw_post: None,
                }));
                DeviceMode::ForwardStepping(
                    PostSynBackChsFwd {
                        ch_ffw: rx,
                        ch_fbw: PostSynFlag::Simple(()),
                    }
                )
            },

            (RunMode::ForwardStepping, RunMode::Idle, PostSynFlag::STDP(_)) => {
                let (ffw_tx, ffw_rx) = crossbeam_channel::unbounded();
                let (fbw_tx, fbw_rx) = crossbeam_channel::unbounded();
                self.content = PostSynFlag::STDP(DeviceMode::ForwardStepping(TmpContentStdpFwd {
                    ffw_pre: Some(ffw_tx),
                    ffw_post: None,
                    fbw_pre: Some(fbw_rx),
                    fbw_post: None,
                }));
                DeviceMode::ForwardStepping(
                    PostSynBackChsFwd {
                        ch_ffw: ffw_rx,
                        ch_fbw: PostSynFlag::STDP(fbw_tx),
                    }
                )
            },
            
            (RunMode::ForwardStepping, RunMode::ForwardStepping, PostSynFlag::Simple(c_mode)) => DeviceMode::ForwardStepping(
                PostSynBackChsFwd {
                    ch_ffw: match c_mode {
                        DeviceMode::ForwardStepping(content) => {
                            content.ffw_post.take().expect("No ffw_post in TmpContentSimpleFwd!")
                        },
                        _ => panic!("unreachable!"),
                    },
                    ch_fbw: PostSynFlag::Simple(()),
                }
            ),
            (RunMode::ForwardStepping, RunMode::ForwardStepping, PostSynFlag::STDP(c_mode)) => DeviceMode::ForwardStepping(
                PostSynBackChsFwd {
                    ch_ffw: match c_mode {
                        DeviceMode::ForwardStepping(content) => {
                            content.ffw_post.take().expect("No ffw_post in TmpContentSimpleFwd!")
                        },
                        _ => panic!("unreachable!"),
                    },
                    ch_fbw: PostSynFlag::STDP(match c_mode {
                        DeviceMode::ForwardStepping(content) => {
                            content.fbw_post.take().expect("No fbw_post in TmpContentSimpleFwd!")
                        },
                        _ => panic!("unreachable!"),
                    }),
                }                
            ),
            (RunMode::ForwardRealTime, _, _) => {
                panic!("RunMode Forwardrealtime not yet implemented!")
            },
            (cmd_mode, car_mode, _) => {
                panic!("PostSynChsCarrier make_pre() w/ unmatched modes, cmd: {:?}, carrier: {:?}.", cmd_mode, car_mode);
            }
        }
    }
}

impl<SF: Send, SB: Send> PostSynChsCarrier<SF, SB> {
    pub fn flag(&self) -> SynapseFlag {
        self.content.variant()
    }
}
