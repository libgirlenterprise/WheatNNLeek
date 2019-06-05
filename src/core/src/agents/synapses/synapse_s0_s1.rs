/// single_in S0, single_out S1.
use crossbeam_channel::Receiver as CCReceiver;
use crossbeam_channel::Sender as CCSender;
use std::sync::{Mutex, Arc};
use crate::{AcMx};
use crate::signals::s0::{S0, SynapseComponentS0S1, SimpleChsCarrierS0, SimpleAcceptorS0, SimpleLinkerS0};
use crate::signals::s1::{S1, PostSynChsCarrierS1, PostSynLinkerS1 , SynapseGeneratorS1};
use crate::connectivity::{
    Generator, Acceptor,
    PassiveAcceptor, ActiveAcceptor
};
use crate::operation::{Configurable, Runnable, RunningSet, Broadcast, RunMode, PassiveAgent};
use crate::operation::op_agent::{ConsecutivePassiveAgent};
use crate::connectivity::linker::Linker;
// use crate::connectivity::simple_joint::SimpleChsCarrier;
// use crate::populations::HoldAgents;

pub struct SynapseS0S1<G, AA, PA>
where G: Generator<SimpleChsCarrierS0> + Send,
      AA: ActiveAcceptor<PostSynChsCarrierS1> + Send,
      PA: PassiveAcceptor<PostSynChsCarrierS1> + Send,
{
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
    }
    
    fn config_channels(&mut self) {
        self.component.config_channels();
    }

    fn mode(&self) -> RunMode {
        self.component.mode()
    }
}

impl<G, AA, PA> PassiveAgent for SynapseS0S1<G, AA, PA>
where G: Generator<SimpleChsCarrierS0> + Send,
      AA: ActiveAcceptor<PostSynChsCarrierS1> + Send,
      PA: 'static + PassiveAcceptor<PostSynChsCarrierS1> + Send,
{}

impl<G, AA, PA> Runnable for SynapseS0S1<G, AA, PA>
where G: Generator<SimpleChsCarrierS0> + Send,
      AA: ActiveAcceptor<PostSynChsCarrierS1> + Send,
      PA: 'static + PassiveAcceptor<PostSynChsCarrierS1> + Send,
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
      PA: 'static + PassiveAcceptor<PostSynChsCarrierS1> + Send,
{
    fn respond(&mut self) {
        self.component.ffw_accepted().for_each(|s| self.component.feedforward(self.refine(s)));
    }
    
    fn running_passive_agents(&self) -> Vec<RunningSet<Broadcast, ()>> {
        self.component.running_passive_agents()
    }
}

impl<G, AA, PA> SimpleAcceptorS0 for SynapseS0S1<G, AA, PA>
where G: Generator<SimpleChsCarrierS0> + Send,
      AA: ActiveAcceptor<PostSynChsCarrierS1> + Send,
      PA: PassiveAcceptor<PostSynChsCarrierS1> + Send,
{}

impl<G, AA, PA> Acceptor<SimpleChsCarrierS0> for SynapseS0S1<G, AA, PA>
where G: Generator<SimpleChsCarrierS0> + Send,
      AA: ActiveAcceptor<PostSynChsCarrierS1> + Send,
      PA: PassiveAcceptor<PostSynChsCarrierS1> + Send,
{
    fn add(&mut self, pre: AcMx<dyn Generator<SimpleChsCarrierS0> + Send>, linker: AcMx<SimpleLinkerS0>) {
        panic!("should not call add() on Synapses!");
    }    
}

impl<G, AA, PA> SynapseGeneratorS1 for SynapseS0S1<G, AA, PA>
where G: Generator<SimpleChsCarrierS0> + Send,
      AA: ActiveAcceptor<PostSynChsCarrierS1> + Send,
      PA: PassiveAcceptor<PostSynChsCarrierS1> + Send,
{}


impl<G, AA, PA> Generator<PostSynChsCarrierS1> for SynapseS0S1<G, AA, PA>
where G: Generator<SimpleChsCarrierS0> + Send,
      AA: ActiveAcceptor<PostSynChsCarrierS1> + Send,
      PA: PassiveAcceptor<PostSynChsCarrierS1> + Send,
{
    fn add_active(&mut self, post: AcMx<dyn ActiveAcceptor<PostSynChsCarrierS1> + Send>, linker: AcMx<PostSynLinkerS1>) {
        panic!("should not call add_active() on Synapses!");
    }
    
    fn add_passive(&mut self, post: AcMx<dyn PassiveAcceptor<PostSynChsCarrierS1> + Send>, linker: AcMx<PostSynLinkerS1>) {
        panic!("should not call add_passive() on Synapses!");
    }
}

impl<G, AA, PA> SynapseS0S1<G, AA, PA>
where G: 'static + Generator<SimpleChsCarrierS0> + Send,
      AA: 'static + ActiveAcceptor<PostSynChsCarrierS1> + Send,
      PA: 'static + PassiveAcceptor<PostSynChsCarrierS1> + Send,
{
    pub fn new_on_active(pre: AcMx<G>, post: AcMx<AA>, value: i32) -> AcMx<SynapseS0S1<G, AA, PA>> {        
        let pre_linker = Linker::new();
        let post_linker = Linker::new();
        let syn = Arc::new(Mutex::new(SynapseS0S1 {
            component: SynapseComponentS0S1::new_on_active(pre.clone(), pre_linker.clone(), post.clone(), post_linker.clone()),
            value,
        }));
        pre.lock().unwrap().add_passive(syn.clone(), pre_linker);
        post.lock().unwrap().add(syn.clone(), post_linker);
        syn
    }
    
    pub fn new_on_passive(pre: AcMx<G>, post: AcMx<PA>, value: i32) -> AcMx<SynapseS0S1<G, AA, PA>> {
        let pre_linker = Linker::new();
        let post_linker = Linker::new();
        let syn =Arc::new(Mutex::new(SynapseS0S1 {
            component: SynapseComponentS0S1::new_on_passive(pre.clone(), pre_linker.clone(), post.clone(), post_linker.clone()),
            value,
        }));
        pre.lock().unwrap().add_passive(syn.clone(), pre_linker);
        post.lock().unwrap().add(syn.clone(), post_linker);
        syn
    }
    
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
}

impl<G, AA, PA> SynapseS0S1<G, AA, PA>
where G: Generator<SimpleChsCarrierS0> + Send,
      AA: ActiveAcceptor<PostSynChsCarrierS1> + Send,
      PA: PassiveAcceptor<PostSynChsCarrierS1> + Send,
{
    
    fn refine(&self, s: S0) -> S1 {
        S1 {
            msg_gen: s.msg_gen,
            msg_prop: self.value,
        }
    }    
}
