use crossbeam_channel::Receiver as CCReceiver;
use crossbeam_channel::Sender as CCSender;
use std::sync::{Mutex, Arc};
use crate::operation::{RunMode, RunningSet, Broadcast, Fired, Configurable, Runnable};
use crate::operation::op_agent::FiringActiveAgent;
use crate::operation::op_population::FiringActivePopulation;
use crate::populations::HoldAgents;

pub struct SimpleFiringPopulation<T>
where T: 'static + FiringActiveAgent + Send,
{
    mode: RunMode,
    agents: Vec<Arc<Mutex<T>>>,
}

impl<T> Configurable for SimpleFiringPopulation<T>
where T: 'static + FiringActiveAgent + Send
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

impl<T> Runnable for SimpleFiringPopulation<T>
    where T: 'static + FiringActiveAgent + Send,
{
    type Confirm = Broadcast;
    type Report = Fired;
    
    fn run(&mut self, rx_confirm: CCReceiver<<Self as Runnable>::Confirm>, tx_report: CCSender<<Self as Runnable>::Report>) {
        <Self as FiringActivePopulation>::run(self, rx_confirm, tx_report);
    }
    
}

impl<T: 'static + FiringActiveAgent + Send> FiringActivePopulation for SimpleFiringPopulation<T> {
    fn running_agents(&self) -> Vec<RunningSet<Broadcast, Fired>> {
        self.agents.iter().map(|agent| RunningSet::<Broadcast, Fired>::new(Arc::clone(&agent))).collect()
    }
}

impl<T: FiringActiveAgent + Send> HoldAgents<T> for SimpleFiringPopulation<T> {
    fn agent_by_id(&self, n: usize) -> Arc<Mutex<T>> {
        Arc::clone(&self.agents[n])
    }    
}

impl<T: 'static + FiringActiveAgent + Send>  SimpleFiringPopulation<T> {
    pub fn new() -> Arc<Mutex<SimpleFiringPopulation<T>>> {
        Arc::new(Mutex::new(SimpleFiringPopulation{
            mode: RunMode::Idle,
            agents: Vec::new(),
        }))
    }

    pub fn add(&mut self, agent: Arc<Mutex<T>>) {
        self.agents.push(agent);
    }
}
