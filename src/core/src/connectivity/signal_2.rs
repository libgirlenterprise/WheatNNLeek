// use std::sync::{Mutex, Weak};
use crate::connections::{PassiveConnection};
use crate::connection_component::{ConnectionComponent};
use crate::agent_components::pre_component::{PreComponent};
use crate::agent_components::post_component::{PostComponent};
// use crate::agents::{Agent, Generator, Acceptor};

pub mod connection_2x;

#[derive(Copy, Clone)]
pub struct FwdPreS2 {
    pub msg_gen: i32
}

#[derive(Copy, Clone)]
pub struct FwdPostS2 {
    pub msg_gen: i32,
    pub msg_prop: i32,
}

// pub struct BkwdPreS2 {
//     pub msg_gen: i32
// }

// pub struct BkwdPostS2 {
//     pub msg_gen: i32,
//     pub msg_prop: i32,
// }

pub type PreAgentComponentS2 = PreComponent<dyn PassiveConnection<FwdPreS2, FwdPostS2> + Send, FwdPreS2, FwdPostS2>;
pub type PostAgentComponentS2 = PostComponent<dyn PassiveConnection<FwdPreS2, FwdPostS2> + Send, FwdPreS2, FwdPostS2>;
pub type ConnectionComponentS2<G, A> = ConnectionComponent<G, A, FwdPreS2, FwdPostS2>;
