/// used by Population.running_agents() or OutComponents.running_agents()

extern crate crossbeam_channel;
use crossbeam_channel::Receiver as CCReceiver;
use crossbeam_channel::Sender as CCSender;
use crate::operation::{
    Broadcast, Fired, PassiveAgent, ActiveAgent, Runnable,
    PassiveSyncChsSet, Active,
};
use crate::utils::random_sleep;

pub trait ConsecutivePassiveAgent: PassiveAgent {
    fn respond(&mut self);
    fn passive_sync_chs_sets(&self) -> Vec<PassiveSyncChsSet>;
    fn run(&mut self){
        let rx_confirm = self.confirm_receiver();
        let tx_report = self.report_sender();
        let passive_sync_sets = self.passive_sync_chs_sets();
        loop {
            random_sleep();
            match rx_confirm.recv().unwrap() {
                Broadcast::Exit => break,
                Broadcast::Evolve => panic!("ConsecutivePassiveagent confirmed by Evolve!"),

                Broadcast::Respond => {
                    self.respond();
                    for r_cn in &passive_sync_sets {
                        r_cn.send_confirm(Broadcast::Respond);
                    }
                    for r_cn in &passive_sync_sets {
                        r_cn.recv_report();
                    }
                    tx_report.send(()).unwrap();
                }
            }
        }
    }    
}

pub trait FiringPassiveAgent: PassiveAgent {
    fn respond(&mut self) -> Fired;
    fn passive_sync_chs_sets(&self) -> Vec<PassiveSyncChsSet>;
    fn run(&mut self, rx_confirm: CCReceiver<Broadcast>, tx_report: CCSender<()>){
        let passive_sync_sets = self.passive_sync_chs_sets();
        loop {
            random_sleep();
            match rx_confirm.recv().unwrap() {
                Broadcast::Exit => break,
                Broadcast::Evolve => panic!("FiringPassiveagent confirmed by Evolve!"),

                Broadcast::Respond => {
                    random_sleep();
                    // println!("conn wait recv signal.");
                    match self.respond() {
                        Fired::N => (),
                        Fired::Y => {
                            for r_cn in &passive_sync_sets {
                                r_cn.send_confirm(Broadcast::Respond);
                            }
                            for r_cn in &passive_sync_sets {
                                r_cn.recv_report();
                            }
                        },
                    }
                    tx_report.send(()).unwrap();
                }
            }
        }
    }
}

pub trait SilentPassiveAgent: PassiveAgent {
    fn respond(&mut self);

    fn run(&mut self, rx_confirm: CCReceiver<Broadcast>, tx_report: CCSender<()>){
        loop {
            random_sleep();
            match rx_confirm.recv().unwrap() {
                Broadcast::Exit => break,
                Broadcast::Evolve => panic!("Passiveagent confirmed by Evolve!"),
                Broadcast::Respond => {
                    // println!("conn wait recv signal.");
                    self.respond();
                    // println!("conn got & propagated signal.");
                    tx_report.send(()).unwrap();
                }
            }
        }
    }
}

pub trait ConsecutiveActiveAgent: ActiveAgent + Runnable<Confirm = Broadcast, Report = ()> {
    fn end(&mut self);
    fn evolve(&mut self);
    fn passive_sync_chs_sets(&mut self) -> Vec<PassiveSyncChsSet>;

    fn run(&mut self, rx_confirm: CCReceiver<Broadcast>, tx_report: CCSender<()>) {
        let passive_sync_sets = self.passive_sync_chs_sets();

        loop {
            random_sleep();
            match rx_confirm.recv().unwrap() {

                Broadcast::Exit => {
                    self.end();
                    break;
                },

                Broadcast::Evolve => {
                    self.evolve();
                    tx_report.send(()).unwrap();
                },

                Broadcast::Respond => {
                    for sync_set in &passive_sync_sets {
                        sync_set.send_confirm(Broadcast::Respond);
                    }
                    for sync_set in &passive_sync_sets {
                        sync_set.recv_report();
                    }
                    tx_report.send(()).unwrap();
                }
            }
        }
    }
}

pub trait FiringActiveAgent: ActiveAgent + Active<Report = Fired> {
    fn end(&mut self);
    fn evolve(&mut self) -> Fired;
    fn passive_sync_chs_sets(&mut self) -> Vec<PassiveSyncChsSet>;

    fn run(&mut self) {
        let rx_confirm = self.confirm_receiver();
        let tx_report = self.report_sender();
        let passive_sync_sets = self.passive_sync_chs_sets();
        let mut last_result = Fired::N;
        
        loop {
            random_sleep();
            match rx_confirm.recv().unwrap() {

                Broadcast::Exit => {
                    self.end();
                    break;
                },

                Broadcast::Evolve => {
                    match self.evolve() {
                        Fired::N => tx_report.send(Fired::N).unwrap(),
                        Fired::Y => {
                            random_sleep();
                            last_result = Fired::Y;
                            tx_report.send(Fired::Y).unwrap();
                            // println!("agnt finished Evolve.");
                        }
                    }
                },

                Broadcast::Respond => {
                    random_sleep();
                    match &mut last_result {
                        Fired::N => (),
                        Fired::Y => {
                            for sync_set in &passive_sync_sets {
                                sync_set.send_confirm(Broadcast::Respond);
                            }
                            for sync_set in &passive_sync_sets {
                                sync_set.recv_report();
                            }
                            tx_report.send(Fired::N).unwrap();
                        }
                    }
                    last_result = Fired::N;
                }
            }
        }
    }
}

pub trait SilentActiveAgent: ActiveAgent + Runnable<Confirm = Broadcast, Report = ()>{
    fn end(&mut self);
    fn evolve(&mut self);

    fn run(&mut self, rx_confirm: CCReceiver<Broadcast>, tx_report: CCSender<()>) {
        loop {
            random_sleep();
            match rx_confirm.recv().unwrap() {
                Broadcast::Exit => {
                    self.end();
                    break;
                },
                Broadcast::Evolve => {
                    self.evolve();
                    tx_report.send(()).unwrap();
                },
                Broadcast::Respond => panic!("SilentActivePopulation should not recv Finishcycle!"),
            }
        }
    }
}
