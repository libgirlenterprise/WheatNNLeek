/// single_in S0, single_out S1.
use crossbeam_channel::Receiver as CCReceiver;
use crossbeam_channel::Sender as CCSender;
use std::sync::{Mutex, Arc};
use crate::{AcMx, WkMx};
use crate::signals::s0::{S0, SynapseComponentS0S1, SimpleChsCarrierS0};
use crate::signals::s1::{S1, PostSynChsCarrierS1};
use crate::connectivity::{
    Generator,
    PassiveAcceptor, ActiveAcceptor
};
use crate::operation::{Configurable, Runnable, RunningSet, Broadcast, RunMode, PassiveAgent};
use crate::operation::op_agent::{ConsecutivePassiveAgent};
// use crate::populations::HoldAgents;

pub struct SynapseS0S1<G, AA, PA>
where G: Generator<SimpleChsCarrierS0> + Send,
      AA: ActiveAcceptor<PostSynChsCarrierS1> + Send,
      PA: PassiveAcceptor<PostSynChsCarrierS1> + Send,
{
    // in_s0: SynapsePreComponentS0,
    // out_s1: SynapsePostComponentS1,
    component: SynapseComponentS0S1<G, AA, PA>,
    value: i32,
}

impl<G, AA, PA> Configurable for SynapseS0S1<G, AA, PA>
where G: Generator<SimpleChsCarrierS0> + Send,
      AA: ActiveAcceptor<PostSynChsCarrierS1> + Send,
      PA: PassiveAcceptor<PostSynChsCarrierS1> + Send,
{
    fn config_mode(&mut self, mode: RunMode) {
        self.component.config_mode(mode);
        // self.in_s0.config_mode(mode);
        // self.out_s1.config_mode(mode);
    }
    
    fn config_channels(&mut self) {
        self.component.config_channels();
        // self.in_s0.config_channels();
        // self.out_s1.config_channels();   
    }

    fn mode(&self) -> RunMode {
        self.component.mode()
        // match (self.in_s0.mode(), self.out_s1.mode()) {
        //     (in_s0, out_s1) if in_s0 == out_s1 => in_s0,
        //     (in_s0, out_s1) => panic!(
        //         "components of SynapseS0S1 have different modes, in_s0: {:?}, out_s1: {:?}.",
        //         in_s0, out_s1
        //     ),
        // }
    }
}

impl<G, AA, PA> PassiveAgent for SynapseS0S1<G, AA, PA>
where G: Generator<SimpleChsCarrierS0> + Send,
      AA: ActiveAcceptor<PostSynChsCarrierS1> + Send,
      PA: PassiveAcceptor<PostSynChsCarrierS1> + Send,
{}

impl<G, AA, PA> Runnable for SynapseS0S1<G, AA, PA>
where G: Generator<SimpleChsCarrierS0> + Send,
      AA: ActiveAcceptor<PostSynChsCarrierS1> + Send,
      PA: PassiveAcceptor<PostSynChsCarrierS1> + Send,
{
    type Confirm = Broadcast;
    type Report = ();

    fn run(&mut self, rx_confirm: CCReceiver<<Self as Runnable>::Confirm>, tx_report: CCSender<<Self as Runnable>::Report>) {
        <Self as ConsecutivePassiveAgent>::run(self, rx_confirm, tx_report);
    }
}

impl<G, AA, PA> ConsecutivePassiveAgent for SynapseS0S1<G, AA, PA>
where G: Generator<SimpleChsCarrierS0> + Send,
      AA: ActiveAcceptor<PostSynChsCarrierS1> + Send,
      PA: PassiveAcceptor<PostSynChsCarrierS1> + Send,
{
    fn respond(&mut self) {
        self.component.ffw_accepted().into_iter().for_each(|s| self.component.feedforward(self.refine(s)));
        // self.in_s0.ffw_accepted().into_iter().for_each(|s| self.out_s1.feedforward(self.refine(s)));
    }
    
    fn running_passive_agents(&self) -> Vec<RunningSet<Broadcast, ()>> {
        self.component.running_passive_agents()
        // self.out_s1.running_passive_agents()
    }
}

// impl Acceptor<S0> for SynapseS0S1 {
//     fn add(&mut self, pre: WkMx<dyn Generator<S0>>, linker: AcMx<Linker<S0>>) {
//         self.in_s0.add_target(pre, linker);
//     }
// }

// impl Generator<S1> for SynapseS0S1 {
//     fn add_active(&mut self, post: WkMx<dyn ActiveAcceptor<S1>>, linker: AcMx<Linker<S1>>) {
//         self.out_s1.add_active_target(post, linker);
//     }
    
//     fn add_passive(&mut self, post: WkMx<dyn PassiveAcceptor<S1>>, linker: AcMx<Linker<S1>>) {
//         self.out_s1.add_passive_target(post, linker);
//     }
// }

impl<G, AA, PA> SynapseS0S1<G, AA, PA>
where G: Generator<SimpleChsCarrierS0> + Send,
      AA: ActiveAcceptor<PostSynChsCarrierS1> + Send,
      PA: PassiveAcceptor<PostSynChsCarrierS1> + Send,
{
    pub fn new_on_active(pre: WkMx<G>, post: WkMx<AA>, value: i32) -> AcMx<SynapseS0S1<G, AA, PA>> {
        Arc::new(Mutex::new(SynapseS0S1 {
            component: SynapseComponentS0S1::new_on_active(pre, post),
            // in_s0: SynapsePreComponentS0::new(),
            // out_s1: SynapsePostComponentS1::new(),
            value,
        }))
    }

    pub fn new_on_passive(pre: WkMx<G>, post: WkMx<PA>, value: i32) -> AcMx<SynapseS0S1<G, AA, PA>> {
        Arc::new(Mutex::new(SynapseS0S1 {
            component: SynapseComponentS0S1::new_on_passive(pre, post),
            value,
        }))
    }

//     pub fn new_with_passive<G, A>(value: i32, pre: AcMx<G>, post: AcMx<A>) -> AcMx<SynapseS0S1>
//     where G: 'static + Generator<S0>,
//           A: 'static + PassiveAcceptor<S1>,
//     {
//         let conn = SynapseS0S1::new(value);
//         connectivity::connect_passive(pre, conn.clone());
//         connectivity::connect_passive(conn.clone(), post);
//         conn
//     }

//     pub fn new_with_active<G, A>(value: i32, pre: AcMx<G>, post: AcMx<A>) -> AcMx<SynapseS0S1>
//     where G: 'static + Generator<S0>,
//           A: 'static + ActiveAcceptor<S1>,
//     {
//         let conn = SynapseS0S1::new(value);
//         connectivity::connect_passive(pre, conn.clone());
//         connectivity::connect_active(conn.clone(), post);
//         conn
//     }
    
//     // pub fn new_with_passive(value: i32, pre: AcMx<dyn Generator<S0>>>, post: AcMx<dyn PassiveAcceptor<S1>>>) -> AcMx<SynapseS0S1>> {
//     //     let conn = SynapseS0S1::new(value);
//     //     connectivity::connect_passive(pre, conn.clone());
//     //     connectivity::connect_passive(conn.clone(), post);
//     //     conn
//     // }

//     // pub fn new_with_active(value: i32, pre: AcMx<dyn Generator<S0>>>, post: AcMx<dyn ActiveAcceptor<S1>>>) -> AcMx<SynapseS0S1>> {
//     //     let conn = SynapseS0S1::new(value);
//     //     connectivity::connect_passive(pre, conn.clone());
//     //     connectivity::connect_active(conn.clone(), post);
//     //     conn
//     // }

//     pub fn new_with_passive_population<G, A, PG, PA>(value: i32, p1: &AcMx<PG>, n1: usize, p2: &AcMx<PA>, n2: usize) -> AcMx<SynapseS0S1>
//     where PG: HoldAgents<G>,
//           PA: HoldAgents<A>,
//           G: 'static + Generator<S0>,
//           A: 'static + PassiveAcceptor<S1>,
//     {
//         let agent1 = p1.lock().unwrap().agent_by_id(n1).clone();
//         let agent2 = p2.lock().unwrap().agent_by_id(n2).clone();
//         SynapseS0S1::new_with_passive(value, agent1, agent2)
//     }

//     pub fn new_with_active_population<G, A, PG, PA>(value: i32, p1: &AcMx<PG>, n1: usize, p2: &AcMx<PA>, n2: usize) -> AcMx<SynapseS0S1>
//     where PG: HoldAgents<G>,
//           PA: HoldAgents<A>,
//           G: 'static + Generator<S0>,
//           A: 'static + ActiveAcceptor<S1>,
//     {
//         let agent1 = p1.lock().unwrap().agent_by_id(n1).clone();
//         let agent2 = p2.lock().unwrap().agent_by_id(n2).clone();
//         SynapseS0S1::new_with_active(value, agent1, agent2)
//     }
    
    fn refine(&self, s: S0) -> S1 {
        S1 {
            msg_gen: s.msg_gen,
            msg_prop: self.value,
        }
    }
}
