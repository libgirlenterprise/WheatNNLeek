use crate::{AcMx};
use crate::operation::{RunMode};
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
}
