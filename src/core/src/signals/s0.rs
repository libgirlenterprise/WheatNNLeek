pub use crate::signals::s1::{S1, PostSynChsCarrierS1};
pub use crate::signals::stdp_bkwd_0::StdpBkwd0;
use crate::components::{MultiOutComponent, SynapseComponent};
use crate::connectivity::{Generator, PassiveAcceptor, ActiveAcceptor, Acceptor};
use crate::connectivity::simple_joint::SimpleChsCarrier;
// use crate::connectivity::post_syn_joint::PostSynChsCarrier;
use crate::connectivity::linker::Linker;


#[derive(Copy, Clone)]
pub struct S0 {
    pub msg_gen: i32
}


pub trait SimpleGeneratorS0: Generator<SimpleChsCarrier<S0>> {}
pub trait SimpleAcceptorS0: Acceptor<SimpleChsCarrier<S0>> {}

pub type MultiOutComponentS0<Rp> = MultiOutComponent<dyn ActiveAcceptor<SimpleChsCarrier<S0>, Report = Rp> + Send,
                                                     dyn PassiveAcceptor<SimpleChsCarrier<S0>> + Send,
                                                     S0>;
// pub type SynapsePreComponentS0<G> = SynapsePreComponent<G>
// where G: Generator<SimpleChsCarrier<S0>> + Send;

pub type SimpleChsCarrierS0 = SimpleChsCarrier<S0>;
pub type SimpleLinkerS0 = Linker<SimpleChsCarrier<S0>>;

// pub type SynapseComponentS0S1<G, AA, PA>
// where G: Generator<SimpleChsCarrierS0> + Send,
//       AA: ActiveAcceptor<PostSynChsCarrierS1> + Send,
//       PA: PassiveAcceptor<PostSynChsCarrierS1> + Send,
// = SynapseComponent<G, S0, AA, PA, S1, StdpBkwd0>;

pub type SynapseComponentS0S1<Rp> = SynapseComponent<dyn Generator<SimpleChsCarrierS0> + Send,
                                                     S0,
                                                     dyn ActiveAcceptor<PostSynChsCarrierS1, Report = Rp> + Send,
                                                     dyn PassiveAcceptor<PostSynChsCarrierS1> + Send,
                                                     S1,
                                                     StdpBkwd0>;
