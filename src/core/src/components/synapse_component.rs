use crate::{AcMx};
use crate::operation::{RunMode, RunningSet, Broadcast};
use crate::agents::synapses::{SynapseFlag};
use crate::connectivity::{Generator, ActiveAcceptor, PassiveAcceptor};
use crate::connectivity::simple_joint::{SimpleBackJoint, SimpleChsCarrier, AcMxSimpleLnkr};
use crate::connectivity::post_syn_joint::{PostSynForeJoint, PostSynChsCarrier, AcMxPostSynLnkr};


pub struct SynapseComponent<G, SPre, AA, PA,  SPost, SStdp>
where G: Generator<SimpleChsCarrier<SPre>> + Send,
      SPre: Send,
      AA: ActiveAcceptor<PostSynChsCarrier<SPost, SStdp>> + Send,
      PA:  PassiveAcceptor<PostSynChsCarrier<SPost, SStdp>> + Send,
      SPost: Send,
      SStdp: Send,
{
    mode: RunMode,
    flag: SynapseFlag,
    pre: SimpleBackJoint<G, SPre>,
    post: PostSynForeJoint<AA, PA, SPost, SStdp>,
}

impl<G, SPre, AA, PA,  SPost, SStdp> SynapseComponent<G, SPre, AA, PA,  SPost, SStdp>
where G: Generator<SimpleChsCarrier<SPre>> + Send,
      SPre: Send,
      AA: ActiveAcceptor<PostSynChsCarrier<SPost, SStdp>> + Send,
      PA: PassiveAcceptor<PostSynChsCarrier<SPost, SStdp>> + Send,
      SPost: Send,
      SStdp: Send,
{

    pub fn new_on_active(pre: AcMx<G>, pre_linker: AcMxSimpleLnkr<SPre>, post: AcMx<AA>, post_linker: AcMxPostSynLnkr<SPost, SStdp>) -> SynapseComponent<G, SPre, AA, PA,  SPost, SStdp> {
        SynapseComponent {
            mode: RunMode::Idle,
            flag: SynapseFlag::Static,
            pre: SimpleBackJoint::new(pre, pre_linker),
            post: PostSynForeJoint::new_on_active(post, post_linker),
        }
    }
    
    pub fn new_on_passive(pre: AcMx<G>, pre_linker: AcMxSimpleLnkr<SPre>, post: AcMx<PA>, post_linker: AcMxPostSynLnkr<SPost, SStdp>) -> SynapseComponent<G, SPre, AA, PA,  SPost, SStdp> {
        SynapseComponent {
            mode: RunMode::Idle,
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
    }

    pub fn config_channels(&mut self) {
        self.pre.config_channels();
        self.post.config_channels();
    }
    

}

impl<G, SPre, AA, PA,  SPost, SStdp> SynapseComponent<G, SPre, AA, PA,  SPost, SStdp>
where G: Generator<SimpleChsCarrier<SPre>> + Send,
      SPre: Send,
      AA: ActiveAcceptor<PostSynChsCarrier<SPost, SStdp>> + Send,
      PA: 'static + PassiveAcceptor<PostSynChsCarrier<SPost, SStdp>> + Send,
      SPost: Send,
      SStdp: Send,
{
    pub fn running_passive_agents(&self) -> Vec<RunningSet<Broadcast, ()>> {
        match &self.mode {
            RunMode::Idle => panic!("MultiOutComponent call running_passive_targets when agent Idle!"),
            RunMode::ForwardStepping => self.post.running_target().map_or(Vec::with_capacity(0), |r_set| vec![r_set]),
            RunMode::ForwardRealTime => panic!("ForwardRealTime not yet implemented!"),
        }
    }   
}
