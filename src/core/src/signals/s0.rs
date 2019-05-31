use crate::components::{MultiOutComponent};
// use crate::components::{MultiInComponent, MultiOutComponent, SingleInComponent, SingleOutComponent};
use crate::connectivity::{Generator, PassiveAcceptor, ActiveAcceptor};
use crate::connectivity::simple_joint::SimpleChsCarrier;
use crate::connectivity::post_syn_joint::PostSynChsCarrier;
use crate::connectivity::linker::Linker;

#[derive(Copy, Clone)]
pub struct S0 {
    pub msg_gen: i32
}


pub trait SimpleGeneratorS0: Generator<SimpleChsCarrier<S0>> {}
pub trait SimpleAcceptorS0: Acceptor<SimpleChsCarrier<S0>> {}

pub type MultiOutComponentS0 = MultiOutComponent<dyn Generator<SimpleChsCarrier<S0>> + Send, S0>;

pub type SimpleChsCarrierS0 = SimpleChsCarrierS0;
pub type SimpleLinkerS0 = Linker<SimpleChsCarrier<S0>>;
