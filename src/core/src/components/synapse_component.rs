use crate::operation::{RunMode};
use crate::agents::synapses::{SynapseFlag};
use crate::connectivity::{Generator, ActiveAcceptor, PassiveAcceptor};
use crate::connectivity::simple_joint::{SimpleBackJoint, SimpleChsCarrier};
use crate::connectivity::post_syn_joint::{PostSynForeJoint, PostSynChsCarrier};

pub struct SynapseComponent<G, SPre, AA, PA,  SPost, SStdp>
where G: Generator<SimpleChsCarrier<SPre>> + Send,
      SPre: Send,
      AA: ActiveAcceptor<PostSynChsCarrier<SPost, SStdp>> + Send,
      PA: PassiveAcceptor<PostSynChsCarrier<SPost, SStdp>> + Send,
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
    
}
