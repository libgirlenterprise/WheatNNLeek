/// single_in S0, single_out S1.
use crossbeam_channel::Receiver as CCReceiver;
use crossbeam_channel::Sender as CCSender;
use std::sync::{Mutex, Arc};
use crate::{AcMx, WkMx};
use crate::signals::s1::{SynapsePostComponentS1, S1};
use crate::signals::s0::{SynapsePreComponentS0, S0};
// use crate::connectivity::{Generator, Acceptor, PassiveAcceptor, ActiveAcceptor};
use crate::operation::{Configurable, Runnable, RunningSet, Broadcast, RunMode, PassiveAgent};
use crate::operation::op_agent::{ConsecutivePassiveAgent};
// use crate::populations::HoldAgents;

pub struct SynapseS0S1 {
    in_s0: SynapsePreComponentS0,
    out_s1: SynapsePostComponentS1,
    value: i32,
}

impl Configurable for SynapseS0S1 {
    fn config_mode(&mut self, mode: RunMode) {
        self.in_s0.config_mode(mode);
        self.out_s1.config_mode(mode);
    }
    
    fn config_channels(&mut self) {
        self.in_s0.config_channels();
        self.out_s1.config_channels();   
    }

    fn mode(&self) -> RunMode {
        match (self.in_s0.mode(), self.out_s1.mode()) {
            (in_s0, out_s1) if in_s0 == out_s1 => in_s0,
            (in_s0, out_s1) => panic!(
                "components of SynapseS0S1 have different modes, in_s0: {:?}, out_s1: {:?}.",
                in_s0, out_s1
            ),
        }
    }
}

impl PassiveAgent for SynapseS0S1 {}

impl Runnable for SynapseS0S1 {
    type Confirm = Broadcast;
    type Report = ();

    fn run(&mut self, rx_confirm: CCReceiver<<Self as Runnable>::Confirm>, tx_report: CCSender<<Self as Runnable>::Report>) {
        <Self as ConsecutivePassiveAgent>::run(self, rx_confirm, tx_report);
    }
}

impl ConsecutivePassiveAgent for SynapseS0S1 {
    fn respond(&mut self) {
        self.in_s0.ffw_accepted().into_iter().for_each(|s| self.out_s1.feedforward(self.refine(s)));
    }
    
    fn running_passive_agents(&self) -> Vec<RunningSet<Broadcast, ()>> {
        self.out_s1.running_passive_agents()
    }
}

// impl Acceptor<FwdPreS1> for SynapseS0S1 {
//     fn add(&mut self, pre: WkMx<dyn Generator<FwdPreS1>>, linker: AcMx<Linker<FwdPreS1>>) {
//         self.in_s0.add_target(pre, linker);
//     }
// }

// impl Generator<FwdPostS1> for SynapseS0S1 {
//     fn add_active(&mut self, post: WkMx<dyn ActiveAcceptor<FwdPostS1>>, linker: AcMx<Linker<FwdPostS1>>) {
//         self.out_s1.add_active_target(post, linker);
//     }
    
//     fn add_passive(&mut self, post: WkMx<dyn PassiveAcceptor<FwdPostS1>>, linker: AcMx<Linker<FwdPostS1>>) {
//         self.out_s1.add_passive_target(post, linker);
//     }
// }

impl SynapseS0S1 {
    pub fn new(value: i32) -> AcMx<SynapseS0S1> {
        Arc::new(Mutex::new(SynapseS0S1 {
            // in_s0: SingleInComponentS1Pre::new(),
            // out_s1: SingleOutComponentS1Post::new(),
            value,
        }))
    }

//     pub fn new_with_passive<G, A>(value: i32, pre: AcMx<G>, post: AcMx<A>) -> AcMx<SynapseS0S1>
//     where G: 'static + Generator<FwdPreS1>,
//           A: 'static + PassiveAcceptor<FwdPostS1>,
//     {
//         let conn = SynapseS0S1::new(value);
//         connectivity::connect_passive(pre, conn.clone());
//         connectivity::connect_passive(conn.clone(), post);
//         conn
//     }

//     pub fn new_with_active<G, A>(value: i32, pre: AcMx<G>, post: AcMx<A>) -> AcMx<SynapseS0S1>
//     where G: 'static + Generator<FwdPreS1>,
//           A: 'static + ActiveAcceptor<FwdPostS1>,
//     {
//         let conn = SynapseS0S1::new(value);
//         connectivity::connect_passive(pre, conn.clone());
//         connectivity::connect_active(conn.clone(), post);
//         conn
//     }
    
//     // pub fn new_with_passive(value: i32, pre: AcMx<dyn Generator<FwdPreS1>>>, post: AcMx<dyn PassiveAcceptor<FwdPostS1>>>) -> AcMx<SynapseS0S1>> {
//     //     let conn = SynapseS0S1::new(value);
//     //     connectivity::connect_passive(pre, conn.clone());
//     //     connectivity::connect_passive(conn.clone(), post);
//     //     conn
//     // }

//     // pub fn new_with_active(value: i32, pre: AcMx<dyn Generator<FwdPreS1>>>, post: AcMx<dyn ActiveAcceptor<FwdPostS1>>>) -> AcMx<SynapseS0S1>> {
//     //     let conn = SynapseS0S1::new(value);
//     //     connectivity::connect_passive(pre, conn.clone());
//     //     connectivity::connect_active(conn.clone(), post);
//     //     conn
//     // }

//     pub fn new_with_passive_population<G, A, PG, PA>(value: i32, p1: &AcMx<PG>, n1: usize, p2: &AcMx<PA>, n2: usize) -> AcMx<SynapseS0S1>
//     where PG: HoldAgents<G>,
//           PA: HoldAgents<A>,
//           G: 'static + Generator<FwdPreS1>,
//           A: 'static + PassiveAcceptor<FwdPostS1>,
//     {
//         let agent1 = p1.lock().unwrap().agent_by_id(n1).clone();
//         let agent2 = p2.lock().unwrap().agent_by_id(n2).clone();
//         SynapseS0S1::new_with_passive(value, agent1, agent2)
//     }

//     pub fn new_with_active_population<G, A, PG, PA>(value: i32, p1: &AcMx<PG>, n1: usize, p2: &AcMx<PA>, n2: usize) -> AcMx<SynapseS0S1>
//     where PG: HoldAgents<G>,
//           PA: HoldAgents<A>,
//           G: 'static + Generator<FwdPreS1>,
//           A: 'static + ActiveAcceptor<FwdPostS1>,
//     {
//         let agent1 = p1.lock().unwrap().agent_by_id(n1).clone();
//         let agent2 = p2.lock().unwrap().agent_by_id(n2).clone();
//         SynapseS0S1::new_with_active(value, agent1, agent2)
//     }
    
    fn refine(&self, s: FwdPreS1) -> FwdPostS1 {
        FwdPostS1 {
            msg_gen: s.msg_gen,
            msg_prop: self.value,
        }
    }
}
