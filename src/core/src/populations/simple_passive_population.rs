use std::sync::{Arc, Mutex};
use crossbeam_channel::Receiver as CCReceiver;
use crossbeam_channel::Sender as CCSender;
use crate::operation::{RunMode, PassiveAgent, Configurable, Passive, PassiveRunningSet, OpeChsGenC, Broadcast};
use crate::operation::op_population::PassivePopulation;
use crate::populations::{HoldAgents, Population};

pub struct SimplePassivePopulation<T>
where T: 'static + PassiveAgent + Send
{
    mode: RunMode,
    ope_chs_gen: OpeChsGenC,
    agents: Vec<Arc<Mutex<T>>>,
}

impl<T> Population for SimplePassivePopulation<T>
where T: 'static + PassiveAgent + Send,
{}

impl<T> Passive for SimplePassivePopulation<T>
where T: 'static + PassiveAgent + Send,
{
    fn run(&mut self) {
        <Self as PassivePopulation>::run(self);
    }

    fn confirm_sender(&self) -> CCSender<Broadcast> {
        self.ope_chs_gen.confirm_sender()
    }
    
    fn confirm_receiver(&self) -> CCReceiver<Broadcast> {
        self.ope_chs_gen.confirm_receiver()
    }
}
    
impl<T> Configurable for SimplePassivePopulation<T>
where T: 'static + PassiveAgent + Send
{
    fn config_mode(&mut self, mode: RunMode) {
        self.mode = mode;
        for agent in &self.agents {
            agent.lock().unwrap().config_mode(mode);
        }
    }

    fn config_channels(&mut self) {
        for agent in &self.agents {
            agent.lock().unwrap().config_channels();
        }
    }

    // fn mode(&self) -> RunMode {
    //     self.mode
    // }
}

impl<T> PassivePopulation for SimplePassivePopulation<T>
where T: 'static + PassiveAgent + Send
{
    fn recheck_mode(&mut self) {
        for agnt in &mut self.agents {
            agnt.lock().unwrap().recheck_mode();
        }
    }

    fn running_agents(&self) -> Vec<PassiveRunningSet> {
        self.agents.iter().map(|agent| PassiveRunningSet::new(Arc::clone(&agent))).collect()
    }
}

impl<T> HoldAgents<T> for SimplePassivePopulation<T>
where T: 'static + PassiveAgent + Send
{
    fn agent_by_id(&self, n: usize) -> Arc<Mutex<T>> {
        Arc::clone(&self.agents[n])
    }    
}

impl<T>  SimplePassivePopulation<T>
where T: 'static + PassiveAgent + Send
{
    pub fn new() -> Arc<Mutex<SimplePassivePopulation<T>>> {
        Arc::new(Mutex::new(SimplePassivePopulation{
            mode: RunMode::Idle,
            agents: Vec::new(),
            ope_chs_gen: OpeChsGenC::new(),
        }))
    }

    pub fn add(&mut self, agent: Arc<Mutex<T>>) {
        self.agents.push(agent);
    }
}
