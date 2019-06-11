/// single_in S0, single_out S1.
use crossbeam_channel::Receiver as CCReceiver;
use crossbeam_channel::Sender as CCSender;
use std::sync::{Mutex, Arc};
use crate::{AcMx};
use crate::signals::s0::{S0, SynapseComponentS0S1, SimpleChsCarrierS0, SimpleAcceptorS0, SimpleLinkerS0};
use crate::signals::s1::{S1, PostSynChsCarrierS1, SynapseGeneratorS1, StdpBkwd0};
use crate::connectivity::{
    Generator, Acceptor,
    PassiveAcceptor, ActiveAcceptor,
    AppendableForeEnd, AppendableOneWayBackEnd, AppendableTwoWayBackEnd,
};
use crate::operation::{
    Configurable, Broadcast, RunMode, PassiveAgent, Passive,
    OpeChsGenCR, PassiveSyncChsSet,
    ActiveAgent,
};
use crate::operation::op_agent::{ConsecutivePassiveAgent};
use crate::connectivity::linker::Linker;
use crate::agents::synapses::{SynapseFlag};
use crate::agents::{Agent};
// use crate::populations::HoldAgents;

pub struct SynapseS0S1
{
    ope_chs_gen: OpeChsGenCR<()>,
    component: SynapseComponentS0S1,
    value: i32,
}

impl Configurable for SynapseS0S1
{
    fn config_mode(&mut self, mode: RunMode) {
        self.component.config_mode(mode);
    }
    
    fn config_channels(&mut self) {
        self.component.config_channels();
    }

    // fn mode(&self) -> RunMode {
    //     self.component.mode()
    // }
}

impl Agent for SynapseS0S1 {}

impl PassiveAgent for SynapseS0S1
{    
    fn report_sender(&self) -> CCSender<()> {
        self.ope_chs_gen.report_sender()
    }

    fn passive_sync_chs_set(&self) -> PassiveSyncChsSet {
        self.ope_chs_gen.passive_sync_chs_set()
    }
}

impl Passive for SynapseS0S1 {
    fn run(&mut self) {
        <Self as ConsecutivePassiveAgent>::run(self);
    }

    fn confirm_sender(&self) -> CCSender<Broadcast> {
        self.ope_chs_gen.confirm_sender()
    }
    
    fn confirm_receiver(&self) -> CCReceiver<Broadcast> {
        self.ope_chs_gen.confirm_receiver()
    }
}

// impl Runnable for SynapseS0S1
// {
//     type Confirm = Broadcast;
//     type Report = ();

//     fn run(&mut self, rx_confirm: CCReceiver<<Self as Runnable>::Confirm>, tx_report: CCSender<<Self as Runnable>::Report>) {
//         <Self as ConsecutivePassiveAgent>::run(self, rx_confirm, tx_report);
//     }
// }

impl ConsecutivePassiveAgent for SynapseS0S1
{
    fn respond(&mut self) {
        self.component.ffw_accepted().for_each(|s| self.component.feedforward(self.refine(s)));
        // fbw should be independent from ffw!!
        self.component.fbw_accepted().for_each(|StdpBkwd0 {msg: n}| println!("SynapseS0S1 get STDP signal: {}.", n) );
        // match self.component.fbw_accepted() {
        //     None => (),
        //     Some(StdpBkwd0 {msg: n}) => println!("SynapseS0S1 get STDP signal: {}.", n),
        // }
    }

    fn passive_sync_chs_sets(&self) -> Vec<PassiveSyncChsSet> {
        self.component.passive_sync_chs_sets()
    }
}

impl SimpleAcceptorS0 for SynapseS0S1 {}

impl Acceptor<SimpleChsCarrierS0> for SynapseS0S1{}

impl SynapseGeneratorS1 for SynapseS0S1 {}


impl Generator<PostSynChsCarrierS1> for SynapseS0S1 {}

impl SynapseS0S1
{
    pub fn config_syn_flag(&mut self, flag: SynapseFlag) {
        self.component.config_syn_flag(flag);
    }

    fn refine(&self, s: S0) -> S1 {
        S1 {
            msg_gen: s.msg_gen,
            msg_prop: self.value,
        }
    }    

    pub fn new_on_active<G, AA>(pre: AcMx<G>, post: AcMx<AA>, value: i32) -> AcMx<SynapseS0S1>
    where G: 'static + AppendableForeEnd<SimpleChsCarrierS0> + Send,
          AA: 'static + ActiveAgent + AppendableTwoWayBackEnd<PostSynChsCarrierS1> + Send,
    {
        let pre_linker = Linker::new();
        let post_linker = Linker::new();
        let syn = Arc::new(Mutex::new(SynapseS0S1 {
            ope_chs_gen: OpeChsGenCR::new(),
            component: SynapseComponentS0S1::new_on_active(pre.clone(), pre_linker.clone(), post.clone(), post_linker.clone()),
            value,
        }));
        pre.lock().unwrap().add_passive(syn.clone(), pre_linker);
        post.lock().unwrap().add_passive(syn.clone(), post_linker);
        syn
    }
    
    pub fn new_on_passive<G, PA>(pre: AcMx<G>, post: AcMx<PA>, value: i32) -> AcMx<SynapseS0S1>
    where G: 'static + AppendableForeEnd<SimpleChsCarrierS0> + Send,
          PA: 'static + PassiveAgent + AppendableTwoWayBackEnd<PostSynChsCarrierS1> + Send,
    {
        let pre_linker = Linker::new();
        let post_linker = Linker::new();
        let syn =Arc::new(Mutex::new(SynapseS0S1 {
            ope_chs_gen: OpeChsGenCR::new(),
            component: SynapseComponentS0S1::new_on_passive(pre.clone(), pre_linker.clone(), post.clone(), post_linker.clone()),
            value,
        }));
        pre.lock().unwrap().add_passive(syn.clone(), pre_linker);
        post.lock().unwrap().add_passive(syn.clone(), post_linker);
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
