use crate::connectivity::{Acceptor};
use crate::connectivity::simple_joint::SimpleChsCarrier;
use crate::connectivity::post_syn_joint::PostSynChsCarrier;
use crate::signals::stdp_bkwd_0::StdpBkwd0;

struct SimplePostSynSignal {}

pub trait SimpleNeuronAcceptor:
Acceptor<SimpleChsCarrier<SimplePostSynSignal>>
    + Acceptor<PostSynChsCarrier<SimplePostSynSignal, StdpBkwd0>> {}
