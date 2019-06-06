use std::sync::{Mutex, Weak, Arc};
use crate::{AcMx, WkMx};
use crossbeam_channel;
use crossbeam_channel::TryIter as CCTryIter;
use crate::operation::{RunMode, AgentRunMode, Broadcast, RunningSet};
use crate::connectivity::{ActiveAcceptor ,PassiveAcceptor, Generator};
use crate::connectivity::{ChannelsCarrier};
use crate::connectivity::linker::Linker;
use crate::connectivity::channels_sets::{PostSynBackChs, PostSynForeChs, PostSynBackChsFwd, PostSynForeChsFwd};
use crate::connectivity::tmp_contents::{TmpContentSimpleFwd, TmpContentStdpFwd};
use crate::agents::synapses::{SynapseFlag, PostSynFlag};

pub type AcMxPostSynLnkr<SF, SB> = AcMx<Linker<PostSynChsCarrier<SF, SB>>>;

pub enum OpePost<A, P> {
    Active(A),
    Passive(P),
}

pub struct PostSynForeJoint<AA, PA, SF, SB>
where AA: ActiveAcceptor<PostSynChsCarrier<SF, SB>> + Send + ?Sized,
      PA: PassiveAcceptor<PostSynChsCarrier<SF, SB>> + Send + ?Sized,
      SF: Send,
      SB: Send,
{
    pub target: OpePost<WkMx<AA>, WkMx<PA>>,
    channels: PostSynForeChs<SF, SB>,
    linker: AcMxPostSynLnkr<SF, SB>,
}

impl<AA, PA, SF, SB> PostSynForeJoint<AA, PA, SF, SB>
where AA: ActiveAcceptor<PostSynChsCarrier<SF, SB>> + Send + ?Sized,
      PA: PassiveAcceptor<PostSynChsCarrier<SF, SB>> + Send + ?Sized,
      SF: Send,
      SB: Send,
{
    pub fn new_on_active(target: AcMx<AA>, linker: AcMxPostSynLnkr<SF, SB>) -> PostSynForeJoint<AA, PA, SF, SB> {
        PostSynForeJoint {
            target: OpePost::Active(Arc::downgrade(&target)),
            channels: AgentRunMode::Idle,
            linker,
        }
    }

    pub fn new_on_passive(target: AcMx<PA>, linker: AcMxPostSynLnkr<SF, SB>) -> PostSynForeJoint<AA, PA, SF, SB> {
        PostSynForeJoint {
            target: OpePost::Passive(Arc::downgrade(&target)),
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
    
    pub fn feedforward(&self, s: SF) {
        match &self.channels {
            AgentRunMode::Idle => (),
            AgentRunMode::ForwardStepping(chs) => chs.feedforward(s),
            _ => panic!("not yet implemented!"),
        }
    }

    pub fn opt_fbw_accepted(&self) -> Option<CCTryIter<SB>> {
        match &self.channels {
            AgentRunMode::Idle => None,
            AgentRunMode::ForwardStepping(chs_back) => match &chs_back.ch_fbw {
                PostSynFlag::Static(_) => None,
                PostSynFlag::STDP(ch) => Some(ch.try_iter()),
            },
            AgentRunMode::ForwardRealTime => panic!("ForwardRealTime not yet implemented!"),
        }
    }

    pub fn config_flag(&mut self, flag: SynapseFlag) {
        self.linker.lock().unwrap().tmp.config_flag(flag);
    }

}

impl<AA, PA, SF, SB> PostSynForeJoint<AA, PA, SF, SB>
where AA: ActiveAcceptor<PostSynChsCarrier<SF, SB>> + Send + ?Sized,
      PA: 'static + PassiveAcceptor<PostSynChsCarrier<SF, SB>> + Send + ?Sized,
      SF: Send,
      SB: Send,
{
    pub fn running_target(&self) -> Option<RunningSet::<Broadcast, ()>> {
        match &self.target {
            OpePost::Active(_) => None,
            OpePost::Passive(target) => match self.channels {
                AgentRunMode::Idle => None,
                AgentRunMode::ForwardStepping(_) => Some(RunningSet::<Broadcast, ()>::new(target.upgrade().unwrap())),
                AgentRunMode::ForwardRealTime => panic!("ForwardRealTime not yet implemented!"),
            },
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
    linker: AcMxPostSynLnkr<SF, SB>,
}

impl<G, SF, SB> PostSynBackJoint<G, SF, SB>
where G: Generator<PostSynChsCarrier<SF, SB>> + Send + ?Sized,
      SF: Send,
      SB: Send,
{
    pub fn new(target: WkMx<G>, linker: AcMxPostSynLnkr<SF, SB>) -> PostSynBackJoint<G, SF, SB> {
        PostSynBackJoint {
            target,
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

    pub fn opt_ffw_accepted(&self) -> Option<CCTryIter<SF>> {
        match &self.channels {
            AgentRunMode::Idle => None,
            AgentRunMode::ForwardStepping(chs_in_ffw) => Some(chs_in_ffw.ch_ffw.try_iter()),
            AgentRunMode::ForwardRealTime => panic!("ForwardRealTime not yet implemented!"),
        }
    }

    pub fn feedbackward(&self, s: SB) {
        match &self.channels {
            AgentRunMode::Idle => (),
            AgentRunMode::ForwardStepping(chs) => chs.feedbackward(s),
            AgentRunMode::ForwardRealTime => panic!("ForwardRealTime not yet implemented!"),
        }
    }
    
}

pub struct PostSynChsCarrier<SF: Send, SB: Send> {
    content: PostSynFlag<AgentRunMode<TmpContentSimpleFwd<SF>>,
                         AgentRunMode<TmpContentStdpFwd<SF, SB>>>,
}

impl<SF: Send, SB: Send> ChannelsCarrier for  PostSynChsCarrier<SF, SB> {
    type BackEndChs = PostSynBackChs<SF, SB>;
    type ForeEndChs = PostSynForeChs<SF, SB>;
    
    fn new() -> Self {
        PostSynChsCarrier {content: PostSynFlag::Static(AgentRunMode::Idle)}
    }
        
    fn reset_idle(&mut self) {
        self.content = match &self.content {
            PostSynFlag::Static(_) => PostSynFlag::Static(AgentRunMode::Idle),
            PostSynFlag::STDP(_) => PostSynFlag::STDP(AgentRunMode::Idle),
        };
    }
    
    fn mode(&self) -> RunMode {
        match &self.content {
            PostSynFlag::Static(d_mode) => d_mode.variant(),
            PostSynFlag::STDP(d_mode) => d_mode.variant(),
        }
    }

    fn fore_chs(&mut self, mode: RunMode) -> <Self as ChannelsCarrier>::ForeEndChs {
        match (mode, self.mode(), &mut self.content) {
            (RunMode::Idle, RunMode::Idle, _) => AgentRunMode::Idle,
            (RunMode::Idle, c_mode, content) => panic!(
                "PostSynChsCarrier fore_chs(Idle) should be run after reset_idle, should be unreachable! AgentRunMode: {:?}, Flag: {:?}",
                c_mode,
                content.variant()
            ),

            (RunMode::ForwardStepping, RunMode::Idle, PostSynFlag::Static(_)) => {
                let (tx, rx) = crossbeam_channel::unbounded();
                self.content = PostSynFlag::Static(AgentRunMode::ForwardStepping(TmpContentSimpleFwd {
                    ffw_pre: None,
                    ffw_post: Some(rx),
                }));
                AgentRunMode::ForwardStepping(
                    PostSynForeChsFwd {
                        ch_ffw: tx,
                        ch_fbw: PostSynFlag::Static(()),
                    }
                )
            },

            (RunMode::ForwardStepping, RunMode::Idle, PostSynFlag::STDP(_)) => {
                let (ffw_tx, ffw_rx) = crossbeam_channel::unbounded();
                let (fbw_tx, fbw_rx) = crossbeam_channel::unbounded();
                self.content = PostSynFlag::STDP(AgentRunMode::ForwardStepping(TmpContentStdpFwd {
                    ffw_pre: None,
                    ffw_post: Some(ffw_rx),
                    fbw_pre: None,
                    fbw_post: Some(fbw_tx),
                }));
                AgentRunMode::ForwardStepping(
                    PostSynForeChsFwd {
                        ch_ffw: ffw_tx,
                        ch_fbw: PostSynFlag::STDP(fbw_rx),
                    }
                )
            },
    
            (RunMode::ForwardStepping, RunMode::ForwardStepping, PostSynFlag::Static(x)) => AgentRunMode::ForwardStepping(
                PostSynForeChsFwd {
                    ch_ffw: match x {
                        AgentRunMode::ForwardStepping(content) => {
                            content.ffw_pre.take().expect("No ffw_pre in TmpContentSimpleFwd!")
                        },
                        _ => panic!("unreachable!"),
                    },
                    ch_fbw: PostSynFlag::Static(()),
                }
            ),
            (RunMode::ForwardStepping, RunMode::ForwardStepping, PostSynFlag::STDP(x)) => AgentRunMode::ForwardStepping(
                PostSynForeChsFwd {
                    ch_ffw: match x {
                        AgentRunMode::ForwardStepping(content) => {
                            content.ffw_pre.take().expect("No ffw_pre in TmpContentSimpleFwd!")
                        },
                        _ => panic!("unreachable!"),
                    },
                    ch_fbw: PostSynFlag::STDP(match x {
                        AgentRunMode::ForwardStepping(content) => {
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
            (RunMode::Idle, RunMode::Idle, _) => AgentRunMode::Idle,
            (RunMode::Idle, c_mode, flag) => panic!(
                "PostSynChsCarrier back_chs(Idle) should be run after reset_idle, should be unreachable! AgentRunMode: {:?}, Flag: {:?}",
                c_mode,
                flag.variant()
            ),

            (RunMode::ForwardStepping, RunMode::Idle, PostSynFlag::Static(_)) => {
                let (tx, rx) = crossbeam_channel::unbounded();
                self.content = PostSynFlag::Static(AgentRunMode::ForwardStepping(TmpContentSimpleFwd {
                    ffw_pre: Some(tx),
                    ffw_post: None,
                }));
                AgentRunMode::ForwardStepping(
                    PostSynBackChsFwd {
                        ch_ffw: rx,
                        ch_fbw: PostSynFlag::Static(()),
                    }
                )
            },

            (RunMode::ForwardStepping, RunMode::Idle, PostSynFlag::STDP(_)) => {
                let (ffw_tx, ffw_rx) = crossbeam_channel::unbounded();
                let (fbw_tx, fbw_rx) = crossbeam_channel::unbounded();
                self.content = PostSynFlag::STDP(AgentRunMode::ForwardStepping(TmpContentStdpFwd {
                    ffw_pre: Some(ffw_tx),
                    ffw_post: None,
                    fbw_pre: Some(fbw_rx),
                    fbw_post: None,
                }));
                AgentRunMode::ForwardStepping(
                    PostSynBackChsFwd {
                        ch_ffw: ffw_rx,
                        ch_fbw: PostSynFlag::STDP(fbw_tx),
                    }
                )
            },
            
            (RunMode::ForwardStepping, RunMode::ForwardStepping, PostSynFlag::Static(c_mode)) => AgentRunMode::ForwardStepping(
                PostSynBackChsFwd {
                    ch_ffw: match c_mode {
                        AgentRunMode::ForwardStepping(content) => {
                            content.ffw_post.take().expect("No ffw_post in TmpContentSimpleFwd!")
                        },
                        _ => panic!("unreachable!"),
                    },
                    ch_fbw: PostSynFlag::Static(()),
                }
            ),
            (RunMode::ForwardStepping, RunMode::ForwardStepping, PostSynFlag::STDP(c_mode)) => AgentRunMode::ForwardStepping(
                PostSynBackChsFwd {
                    ch_ffw: match c_mode {
                        AgentRunMode::ForwardStepping(content) => {
                            content.ffw_post.take().expect("No ffw_post in TmpContentSimpleFwd!")
                        },
                        _ => panic!("unreachable!"),
                    },
                    ch_fbw: PostSynFlag::STDP(match c_mode {
                        AgentRunMode::ForwardStepping(content) => {
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

    pub fn mode(&self) -> RunMode {
        match &self.content {
            PostSynFlag::Static(mode) => mode.variant(),
            PostSynFlag::STDP(mode) => mode.variant(),
        }
    }

    pub fn config_flag(&mut self, flag: SynapseFlag) {
        match self.mode() {
            RunMode::Idle => self.content = match flag {
                SynapseFlag::Static => PostSynFlag::Static(AgentRunMode::Idle),
                SynapseFlag::STDP => PostSynFlag::STDP(AgentRunMode::Idle),
            },
            _ => panic!("PostSynjoint can only config_flag when Idle!"),
        }
    }
}
