use std::sync::{Mutex, Weak};
use crate::{AcMx, WkMx};
use crossbeam_channel;
use crossbeam_channel::TryIter as CCTryIter;
use crate::operation::{RunMode, DeviceMode, Broadcast, RunningSet};
use crate::connectivity::{Acceptor, PassiveAcceptor, Generator};
use crate::components::{Linker, ChannelsCarrier};
use self::channels_set::{PostSynChsOutFwd, PostSynChsInFwd};
use self::tmp_content::{TmpContentSimpleFwd, TmpContentStdpFwd};

enum SynapseFlag {
    Simple,
    STDP,
}

enum PostSynFlag<SI, ST> {
    Simple(SI),
    STDP(ST),
}

type PostSynLinker<SF, SB> = AcMx<Linker<PostSynChsCarrier<SF, SB>>>;

pub struct PostSynOutJoint<A, SF, SB>
where A: Acceptor<PostSynChsCarrier<SF, SB>> + Send + ?Sized,
      SF: Send,
      SB: Send,
{
    pub target: WkMx<A>,
    channels: DeviceMode<PostSynChsOutFwd<SF, SB>>,
    linker: PostSynLinker<SF, SB>,
}

impl<A, SF, SB> PostSynOutJoint<A, SF, SB>
where A: Acceptor<PostSynChsCarrier<SF, SB>> + Send + ?Sized,
      SF: Send,
      SB: Send,
{
    pub fn new(target: Weak<Mutex<A>>, linker: PostSynLinker<SF, SB>) -> PostSynOutJoint<A, SF, SB> {
        PostSynOutJoint {
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

impl<A, SF, SB> PostSynOutJoint<A, SF, SB>
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

pub struct PostSynInJoint<G, SF, SB>
where G: Generator<PostSynChsCarrier<SF, SB>> + Send + ?Sized,
      SF: Send,
      SB: Send,
{
    pub target: Weak<Mutex<G>>,
    channels: DeviceMode<PostSynChsInFwd<SF, SB>>,
    linker: PostSynLinker<SF, SB>,
}

impl<G, SF, SB> PostSynInJoint<G, SF, SB>
where G: Generator<PostSynChsCarrier<SF, SB>> + Send + ?Sized,
      SF: Send,
      SB: Send,
{
    pub fn new(target: WkMx<G>, linker: PostSynLinker<SF, SB>) -> PostSynInJoint<G, SF, SB> {
        PostSynInJoint {
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

    pub fn ffw_accepted_iter(&self) -> Option<CCTryIter<SF>> {
        match &self.channels {
            DeviceMode::Idle => None,
            DeviceMode::Feedforward(chs_in_ffw) => Some(chs_in_ffw.ch_ffw.try_iter()),
        }
    }

    pub fn feedbackward(&self, s: SB) {
        match &self.channels {
            DeviceMode::Idle => (),
            DeviceMode::Feedforward(chs) => chs.feedbackward(s),
        }
    }
    
}

pub struct PostSynChsCarrier<SF: Send, SB: Send> {
    content: PostSynFlag<DeviceMode<TmpContentSimpleFwd<SF>>,
                         DeviceMode<TmpContentStdpFwd<SF, SB>>>,
    // content: DeviceMode<PostSynFlag<TmpContentSimpleFwd<SF>,
    //                                 TmpContentStdpFwd<SF, SB>>>,
}

impl<SF: Send, SB: Send> ChannelsCarrier for  PostSynChsCarrier<SF, SB> {
    type ChsInFwd =  PostSynChsInFwd<SF, SB>;
    type ChsOutFwd = PostSynChsOutFwd<SF, SB>;
    
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
        RunMode::mode_from_device(match &self.content {
            PostSynFlag::Simple(d_mode) => d_mode,
            PostSynFlag::STDP(d_mode) => d_mode,
        })
    }

    fn pre_chs(&mut self, mode: RunMode) -> DeviceMode<<Self as ChannelsCarrier>::ChsOutFFW> {
        
        match (mode, self.mode(), self.flag()) {
            (RunMode::Idle, _, _) => {
                println!("PostSynChsCarrier make_pre() on Idle, no effect & nonsense.");
            },

            (RunMode::ForwardStepping, RunMode::Idle, PostSynFlag::Simple(_)) => {
                let (tx, rx) = crossbeam_channel::unbounded();
                self.content = PostSynFlag::Simple(DeviceMode::ForwardStepping(TmpContentSimpleFwd {
                    ffw_pre: None,
                    ffw_post: Some(rx),
                }));
                DeviceMode::ForwardStepping(
                    PostSynChsOutFwd {
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
                    PostSynChsOutFwd {
                        ch_ffw: ffw_tx,
                        ch_fbw: PostSynFlag::STDP(fbw_rx),
                    }
                )
            },
    
            (RunMode::ForwardStepping, RunMode::ForwardStepping, PostSynFlag::Simple(x)) => DeviceMode::ForwardStepping(
                PostSynChsOutFwd {
                    ch_ffw: match x {
                        DeviceMode::ForwardStepping(content) => {
                            content.ffw_pre.take().expext("No ffw_pre in TmpContentSimpleFwd!")
                        }
                    },
                    ch_fbw: PostSynFlag::Simple(()),
                }
            ),
            (RunMode::ForwardStepping, RunMode::ForwardStepping, PostSynFlag::STDP(x)) => DeviceMode::ForwardStepping(
                PostSynChsOutFwd {
                    ch_ffw: match x {
                        DeviceMode::ForwardStepping(content) => {
                            content.ffw_pre.take().expext("No ffw_pre in TmpContentSimpleFwd!")
                        },
                        _ => panic!("unreachable!"),
                    },
                    ch_fbw: PostSynFlag::Simple(match x {
                        DeviceMode::ForwardStepping(content) => {
                            content.fbw_pre.take().expext("No fbw_pre in TmpContentSimpleFwd!")
                        },
                        _ => panic!("unreachable!"),
                    }),
                }
            ),
            (RunMode::ForwardRealTime, _, _) => {
                panic!("RunMode Forwardrealtime not yet implemented!")
            },
            (cmd_mode, car_mode, _) => {
                panic!("PostSynChsCarrier make_pre() w/ unmatched modes, cmd: {}, carrier: {}.", cmd_mode, car_mode);
            }
        }
    }

        fn post_chs(&mut self, mode: RunMode) -> DeviceMode<<Self as ChannelsCarrier>::ChsInFwd> {
        
        match (mode, self.mode(), self.flag()) {
            (RunMode::Idle, _, _) => {
                println!("PostSynChsCarrier post_chs() on Idle, no effect & nonsense.");
            },

            (RunMode::ForwardStepping, RunMode::Idle, PostSynFlag::Simple(_)) => {
                let (tx, rx) = crossbeam_channel::unbounded();
                self.content = PostSynFlag::Simple(DeviceMode::ForwardStepping(TmpContentSimpleFwd {
                    ffw_pre: Some(tx),
                    ffw_post: None,
                }));
                DeviceMode::ForwardStepping(
                    PostSynChsInFwd {
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
                    fbw_pre: Some(fbw_tx),
                    fbw_post: None,
                }));
                DeviceMode::ForwardStepping(
                    PostSynChsInFwd {
                        ch_ffw: ffw_rx,
                        ch_fbw: PostSynFlag::STDP(fbw_rx),
                    }
                )
            },
    
            (RunMode::ForwardStepping, RunMode::ForwardStepping, PostSynFlag::Simple(x)) => DeviceMode::ForwardStepping(
                PostSynChsInFwd {
                    ch_ffw: match x {
                        DeviceMode::ForwardStepping(content) => {
                            content.ffw_post.take().expext("No ffw_post in TmpContentSimpleFwd!")
                        }
                    },
                    ch_fbw: PostSynFlag::Simple(()),
                }
            ),
            (RunMode::ForwardStepping, RunMode::ForwardStepping, PostSynFlag::STDP(x)) => DeviceMode::ForwardStepping(
                PostSynChsInFwd {
                    ch_ffw: match x {
                        DeviceMode::ForwardStepping(content) => {
                            content.ffw_post.take().expext("No ffw_post in TmpContentSimpleFwd!")
                        }
                    },
                    ch_fbw: PostSynFlag::STDP(match x {
                        DeviceMode::ForwardStepping(content) => {
                            content.fbw_post.take().expext("No fbw_post in TmpContentSimpleFwd!")
                        }
                    }),
                }                
            ),
            (RunMode::ForwardRealTime, _, _) => {
                panic!("RunMode Forwardrealtime not yet implemented!")
            },
            (cmd_mode, car_mode, _) => {
                panic!("PostSynChsCarrier make_pre() w/ unmatched modes, cmd: {}, carrier: {}.", cmd_mode, car_mode);
            }
        }
    }
}

impl<SF: Send, SB: Send> PostSynChsCarrier<SF, SB> {
    fn flag(&self) -> SynapseFlag {
        match &self.content {
            PostSynFlag::Simple(_) => SynapseFlag::Simple,
            PostSynFlag::STDP(_) => SynapseFlag::STDP,
        }
    }
}

mod channels_set {
    use crossbeam_channel::Receiver as CCReceiver;
    use crossbeam_channel::Sender as CCSender;
    use super::PostSynFlag;
    
    pub struct PostSynChsOutFwd<SF: Send, SB: Send> {
        pub ch_ffw: CCSender<SF>,
        pub ch_fbw: PostSynFlag<(), CCReceiver<SB>>,
    }

    pub struct PostSynChsInFwd<SF: Send, SB: Send> {
        pub ch_ffw: CCReceiver<SF>,
        pub ch_fbw: PostSynFlag<(), CCSender<SB>>,
    }    
}

mod tmp_content {
    use crossbeam_channel::Receiver as CCReceiver;
    use crossbeam_channel::Sender as CCSender;

    pub struct TmpContentSimpleFwd<SF: Send> {
        pub ffw_pre: Option<CCSender<SF>>,
        pub ffw_post: Option<CCReceiver<SF>>,
    }

    pub struct TmpContentStdpFwd<SF: Send, SB: Send> {
        pub ffw_pre: Option<CCSender<SF>>,
        pub ffw_post: Option<CCReceiver<SF>>,
        pub fbw_pre: Option<CCReceiver<SB>>,
        pub fbw_post: Option<CCSender<SB>>,
    }    
}
