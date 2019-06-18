use crossbeam_channel::Receiver as CCReceiver;
use crossbeam_channel::Sender as CCSender;
use std::sync::{Mutex, Arc};
use crate::operation::{RunMode, ActiveRunningSet, Broadcast, Fired, Configurable, Active, OpeChs};
use crate::operation::op_agent::FiringActiveAgent;
use crate::operation::op_population::FiringActivePopulation;
use crate::populations::{HoldAgents, Population};
use crate::{Time};

pub struct SimpleFiringPopulation<T>
where T: 'static + FiringActiveAgent + Send,
{
    mode: RunMode,
    ope_chs_gen: OpeChs<Fired>,
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

    fn mode(&self) -> RunMode {
        self.mode
    }
}

impl<T> Active for SimpleFiringPopulation<T>
where T: 'static + FiringActiveAgent + Send,
{
    type Report = Fired;
    fn run(&mut self, dt: Time, time: Time) {
        <Self as FiringActivePopulation>::run(self, dt: Time, time: Time);
    }

    fn confirm_sender(&self) -> CCSender<Broadcast> {
        self.ope_chs_gen.confirm_sender()
    }
    
    fn confirm_receiver(&self) -> CCReceiver<Broadcast> {
        self.ope_chs_gen.confirm_receiver()
    }
    
    fn report_receiver(&self) -> CCReceiver<<Self as Active>::Report> {
        self.ope_chs_gen.report_receiver()
    }
    
    fn report_sender(&self) -> CCSender<<Self as Active>::Report> {
        self.ope_chs_gen.report_sender()
    }
}

impl<T> Population for SimpleFiringPopulation<T>
where T: 'static + FiringActiveAgent + Send,
{}

impl<T: 'static + FiringActiveAgent + Send> FiringActivePopulation for SimpleFiringPopulation<T> {
    fn running_agents(&self) -> Vec<ActiveRunningSet<Fired>> {
        self.agents.iter().filter_map(|agent| ActiveRunningSet::<Fired>::new(Arc::clone(&agent))).collect()
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
            ope_chs_gen: OpeChs::new(),
            agents: Vec::new(),
        }))
    }

    pub fn add(&mut self, agent: Arc<Mutex<T>>) {
        self.agents.push(agent);
    }
}
