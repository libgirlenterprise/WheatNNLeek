use crate::{AcMx};
use crate::operation::{RunMode, PassiveSyncChsSet};
use crate::agents::synapses::{SynapseFlag};
use crate::connectivity::{Generator, ActiveAcceptor, PassiveAcceptor};
use crate::connectivity::simple_joint::{SimpleBackJoint, SimpleChsCarrier, AcMxSimpleLnkr};
use crate::connectivity::post_syn_joint::{PostSynForeJoint, PostSynChsCarrier, AcMxPostSynLnkr};


pub struct SynapseComponent<G, SPre, AA, PA,  SPost, SStdp>
where G: Generator<SimpleChsCarrier<SPre>> + Send + ?Sized,
      SPre: Send,
      AA: ActiveAcceptor<PostSynChsCarrier<SPost, SStdp>> + Send + ?Sized,
      PA:  PassiveAcceptor<PostSynChsCarrier<SPost, SStdp>> + Send + ?Sized,
      SPost: Send,
      SStdp: Send,
{
    mode:  RunMode,
    mode_checked: bool,
    flag: SynapseFlag,
    pre: SimpleBackJoint<G, SPre>,
    post: PostSynForeJoint<AA, PA, SPost, SStdp>,
}

impl<G, SPre, AA, PA,  SPost, SStdp> SynapseComponent<G, SPre, AA, PA,  SPost, SStdp>
where G: Generator<SimpleChsCarrier<SPre>> + Send + ?Sized,
      SPre: Send,
      AA: ActiveAcceptor<PostSynChsCarrier<SPost, SStdp>> + Send + ?Sized,
      PA: PassiveAcceptor<PostSynChsCarrier<SPost, SStdp>> + Send + ?Sized,
      SPost: Send,
      SStdp: Send,
{
    pub fn config_syn_flag(&mut self, flag: SynapseFlag) {
        self.flag = flag;
        self.post.config_flag(flag);
    }

    pub fn new_on_active(pre: AcMx<G>, pre_linker: AcMxSimpleLnkr<SPre>, post: AcMx<AA>, post_linker: AcMxPostSynLnkr<SPost, SStdp>) -> SynapseComponent<G, SPre, AA, PA,  SPost, SStdp> {
        SynapseComponent {
            mode: RunMode::Idle,
            mode_checked: true,
            flag: SynapseFlag::Static,
            pre: SimpleBackJoint::new(pre, pre_linker),
            post: PostSynForeJoint::new_on_active(post, post_linker),
        }
    }
    
    pub fn new_on_passive(pre: AcMx<G>, pre_linker: AcMxSimpleLnkr<SPre>, post: AcMx<PA>, post_linker: AcMxPostSynLnkr<SPost, SStdp>) -> SynapseComponent<G, SPre, AA, PA,  SPost, SStdp> {
        SynapseComponent {
            mode: RunMode::Idle,
            mode_checked: true,
            flag: SynapseFlag::Static,
            pre: SimpleBackJoint::new(pre, pre_linker),
            post: PostSynForeJoint::new_on_passive(post, post_linker),
        }
    }

    pub fn mode(&self) -> RunMode {
        self.mode
    }

    pub fn config_mode(&mut self, mode: RunMode) {
        match (mode, &self.mode) {
            (RunMode::Idle, RunMode::Idle) => println!("SynapseComponent config_mode from Idle to Idle, no effect."),
            (RunMode::Idle, _) => self.config_mode_to(mode),
            (_, RunMode::Idle) => self.config_mode_to(mode),
            (_, _) => panic!("SynapseComponent unhandled config_mode: from {:?} to {:?}.", self.mode(), mode),
        }
    }

    fn config_mode_to(&mut self, mode: RunMode) {
        self.mode = mode;
        self.pre.config_mode(mode);
        self.post.config_mode(mode);
        self.mode_checked = false;
    }

    pub fn recheck_mode(&mut self) {
        if !self.mode_checked  {
            match (self.mode(), self.pre.mode(), self.post.mode()) {
                (RunMode::Idle, _, _) => (),
                (_, RunMode::Idle, _) | (_, _, RunMode::Idle) => self.config_mode_to(RunMode::Idle),
                (syn_m, pre_m, post_m) if syn_m == pre_m && syn_m == post_m => (),
                (syn_m, pre_m, post_m) => panic!(
                    "SynapseComponent.recheck_mode() at syn: {:?}, pre: {:?}, post: {:?}",
                    syn_m,
                    pre_m,
                    post_m
                ),  
            }
            self.mode_checked = true;
        }

        // match (self.mode(), self.pre.try_recv_mode(), self.post.try_recv_mode()) {
        //     (_, Err(TryRecvError::Empty), _) | (_, _, Err(TryRecvError::Empty)) => {
        //         self.mode = RunMode::Idle;
        //         self.pre.clean_mode_communicator();
        //         self.post.clean_mode_communicator();
        //     },
        //     (RunMode::ForwardStepping, Ok(RunMode::ForwardStepping), Ok(RunMode::ForwardStepping)) => {
        //         self.pre.config_channels();
        //         self.post.config_channels(); // if recv channels => set chs, else => create & send chs.
        //     },
        //     (syn_m, pre_recv_m, post_recv_m) => panic!(
        //         "SynapseComponent.config_channels() at syn_m: {:?}, pre_recv_m: {:?}, post_recv_m: {:?}",
        //         syn_m,
        //         pre_recv_m,
        //         post_recv_m
        //     ),
        // }
    }
    
    pub fn config_channels(&mut self) {        
        self.pre.config_channels();
        self.post.config_channels();
    }
    
    pub fn ffw_accepted(&self) -> impl Iterator<Item = SPre> + '_ {
        match &self.mode {
            RunMode::ForwardStepping => self.pre.opt_ffw_accepted().expect("ffw_accepted: SynapseComponent should only be run when both pre/post neurons are ForwardStepping!"),
            RunMode::ForwardRealTime => panic!("SynapseCoponent ForwardRealTime not yet implemented!"),
            RunMode::Idle => panic!("SynapseComponent is Idle when accepted() called!"),
        }
    }

    pub fn fbw_accepted(&self) -> impl Iterator<Item = SStdp> + '_ {
        match &self.mode {
            // RunMode::ForwardStepping => self.post.opt_fbw_accepted().into_iter().flatten(),
            RunMode::ForwardStepping => self.post.opt_fbw_accepted().expect("fbw_accepted: SynapseComponent should only be run when both pre/post neurons are ForwardStepping!"),
            RunMode::ForwardRealTime => panic!("SynapseCoponent ForwardRealTime not yet implemented!"),
            RunMode::Idle => panic!("SynapseComponent is Idle when accepted() called!"),
        }
    }

    pub fn feedforward(&self, s: SPost) {
        match &self.mode {
            RunMode::ForwardStepping => self.post.feedforward(s),
            _ => panic!("PreAgentmodules1 is not ForwardStepping when feedforward called!"),
        }
    }
}

impl<G, SPre, AA, PA,  SPost, SStdp> SynapseComponent<G, SPre, AA, PA,  SPost, SStdp>
where G: Generator<SimpleChsCarrier<SPre>> + Send + ?Sized,
      SPre: Send,
      AA: ActiveAcceptor<PostSynChsCarrier<SPost, SStdp>> + Send + ?Sized,
      PA: 'static + PassiveAcceptor<PostSynChsCarrier<SPost, SStdp>> + Send + ?Sized,
      SPost: Send,
      SStdp: Send,
{
    pub fn passive_sync_chs_sets(&self) -> Vec<PassiveSyncChsSet> {
        match &self.mode {
            RunMode::Idle => panic!("Synapse call passive_sync_chs_sets when agent Idle!"),
            RunMode::ForwardStepping => self.post.passive_sync_chs_set().map_or(Vec::with_capacity(0), |r_set| vec![r_set]),
            RunMode::ForwardRealTime => panic!("ForwardRealTime not yet implemented!"),
        }
    }
}
