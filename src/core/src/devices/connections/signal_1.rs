// use std::sync::{Mutex, Weak};
use crate::connections::{PassiveConnection};
use crate::connection_component::{ConnectionComponent};
use crate::agent_components::pre_component::{PreComponent};
use crate::agent_components::post_component::{PostComponent};
// use crate::agents::{Agent, Generator, Acceptor};

pub mod connection_1x;

#[derive(Copy, Clone)]
pub struct FwdPreS1 {
    pub msg_gen: i32
}

#[derive(Copy, Clone)]
pub struct FwdPostS1 {
    pub msg_gen: i32,
    pub msg_prop: i32,
}

// pub struct BkwdPreS1 {
//     pub msg_gen: i32
// }

// pub struct BkwdPostS1 {
//     pub msg_gen: i32,
//     pub msg_prop: i32,
// }



pub type PreAgentComponentS1 = PreComponent<dyn PassiveConnection<FwdPreS1, FwdPostS1> + Send, FwdPreS1, FwdPostS1>;
pub type PostAgentComponentS1 = PostComponent<dyn PassiveConnection<FwdPreS1, FwdPostS1> + Send, FwdPreS1, FwdPostS1>;
pub type ConnectionComponentS1<G, A> = ConnectionComponent<G, A, FwdPreS1, FwdPostS1>;
