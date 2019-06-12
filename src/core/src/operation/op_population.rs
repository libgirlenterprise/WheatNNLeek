extern crate crossbeam_channel;
use crate::utils::random_sleep;
use crossbeam_channel::Receiver as CCReceiver;
use crossbeam_channel::Sender as CCSender;
use crate::operation::{
    RunningSet, Runnable,
    Fired, Broadcast, Configurable, Passive, PassiveRunningSet, Active,
};
use crate::populations::{Population};

pub trait FiringActivePopulation: Active<Report = Fired> + Population + Configurable {
    fn running_agents(&self) -> Vec<RunningSet<Broadcast, Fired>>;
    fn run(&mut self) {
        let rx_confirm = self.confirm_receiver();
        let tx_report = self.report_sender();
        let running_agents = self.running_agents();

        let mut agents_with_event = Vec::new();
        loop {
            random_sleep();
            match rx_confirm.recv().unwrap() {

                Broadcast::Exit => {
                    for r_agent in &running_agents {
                        r_agent.confirm.send(Broadcast::Exit).unwrap();
                    }
                    for r_agent in running_agents {
                        r_agent.instance.join().expect("connection join error!");
                    }
                    break;
                },

                Broadcast::Evolve => {
                    random_sleep();
                    agents_with_event.clear();
                    for r_agent in &running_agents {
                        r_agent.confirm.send(Broadcast::Evolve).unwrap();
                    }
                    for r_agent in &running_agents {
                        if let Fired::Y = r_agent.report.recv().unwrap() {
                            agents_with_event.push((r_agent.confirm.clone(), r_agent.report.clone()));
                        }
                    }

                    match agents_with_event.len() {
                        0 => tx_report.send(Fired::N).unwrap(),
                        _ => tx_report.send(Fired::Y).unwrap(),
                    }
                    // println!("pp finished Evolve.");
                },

                Broadcast::Respond => {
                    random_sleep();
                    for agent_e in &agents_with_event {
                        agent_e.0.send(Broadcast::Respond).unwrap();
                    }
                    // println!("pp waiting agent report Respond.");
                    for agent_e in &agents_with_event {
                        match agent_e.1.recv().unwrap() {
                            Fired::N => (),
                            Fired::Y => panic!("agent report Event after Respond!")
                        }
                    }
                    // println!("pp get report from agent of Respond.")
                    tx_report.send(Fired::N).unwrap();
                }
            }
        }
    }
}

pub trait ConsecutiveActivePopulation: Active<Report = ()> + Population + Configurable {
    fn running_agents(&self) -> Vec<RunningSet<Broadcast, ()>>;
    fn run(&mut self, rx_confirm: CCReceiver<Broadcast>, tx_report: CCSender<()>) {
        // this version make all connections (only passive supported) into threads controlled by pre-agents.
        let running_agents = self.running_agents();
        loop {
            random_sleep();
            match rx_confirm.recv().unwrap() {

                Broadcast::Exit => {
                    for r_agent in &running_agents {
                        r_agent.confirm.send(Broadcast::Exit).unwrap();
                    }
                    for r_agent in running_agents {
                        r_agent.instance.join().expect("connection join error!");
                    }
                    break;
                },

                Broadcast::Evolve => {
                    for r_agent in &running_agents {
                        r_agent.confirm.send(Broadcast::Evolve).unwrap();
                    }
                    for r_agent in &running_agents {
                        r_agent.report.recv().unwrap();
                    }
                    tx_report.send(()).unwrap();
                },

                Broadcast::Respond => {
                    for r_agent in &running_agents {
                        r_agent.confirm.send(Broadcast::Respond).unwrap();
                    }
                    for r_agent in &running_agents {
                        r_agent.report.recv().unwrap();
                    }
                    tx_report.send(()).unwrap();
                }
            }
        }
    }
}

pub trait SilentActivePopulation: Active<Report = ()> + Population + Configurable {
    fn running_agents(&self) -> Vec<RunningSet<Broadcast, ()>>;
    fn run(&mut self, rx_confirm: CCReceiver<Broadcast>, tx_report: CCSender<()>) {
        // this version make all connections (only passive supported) into threads controlled by pre-agents.
        let running_agents = self.running_agents();
        loop {
            random_sleep();
            match rx_confirm.recv().unwrap() {

                Broadcast::Exit => {
                    for r_agent in &running_agents {
                        r_agent.confirm.send(Broadcast::Exit).unwrap();
                    }
                    for r_agent in running_agents {
                        r_agent.instance.join().expect("connection join error!");
                    }
                    break;
                },
                
                Broadcast::Evolve => {
                    for r_agent in &running_agents {
                        r_agent.confirm.send(Broadcast::Evolve).unwrap();
                    }
                    for r_agent in &running_agents {
                        r_agent.report.recv().unwrap();
                    }
                    tx_report.send(()).unwrap();
                },

                Broadcast::Respond => panic!("SilentActivePopulation should not recv Finishcycle!"),
            }
        }
    }
}

pub trait PassivePopulation: Passive + Population + Configurable {
    fn recheck_mode(&mut self);
    fn running_agents (&self) -> Vec<PassiveRunningSet>;
    
    fn run(&mut self) {
        let rx_confirm = self.confirm_receiver();
        let running_agents = self.running_agents();
        loop {
            random_sleep();
            match rx_confirm.recv().unwrap() {

                Broadcast::Exit => {
                    for r_agent in &running_agents {
                        r_agent.confirm.send(Broadcast::Exit).unwrap();
                    }
                    for r_agent in running_agents {
                        r_agent.instance.join().expect("connection join error!");
                    }
                    break;
                },

                b => panic!("PassivePopulation should only recv Exit, here: {:?}", b),
            }
        }        
    }
}
