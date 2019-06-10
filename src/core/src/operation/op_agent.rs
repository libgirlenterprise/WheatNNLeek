/// used by Population.running_agents() or OutComponents.running_agents()

extern crate crossbeam_channel;
use crossbeam_channel::Receiver as CCReceiver;
use crossbeam_channel::Sender as CCSender;
use crate::operation::{
    RunningSet, Broadcast, Fired, PassiveAgent, ActiveAgent, Runnable,
    PassiveSyncChsSet,
};
use crate::utils::random_sleep;

pub trait ConsecutivePassiveAgent: PassiveAgent {
    fn respond(&mut self);
    fn running_passive_agents(&self) -> Vec<RunningSet<Broadcast, ()>>;

    fn run(&mut self){
        let rx_confirm = self.confirm_receiver();
        let tx_report = self.report_sender();
        let running_agents = self.running_passive_agents();
        loop {
            random_sleep();
            match rx_confirm.recv().unwrap() {
                Broadcast::Exit => {
                    for r_cn in &running_agents {
                        r_cn.confirm.send(Broadcast::Exit).unwrap();
                    }
                    for r_cn in running_agents {
                        r_cn.instance.join().expect("connection join error!");
                    }
                    break;
                },
                Broadcast::Evolve => panic!("ConsecutivePassiveagent confirmed by Evolve!"),

                Broadcast::Respond => {
                    // println!("conn wait recv signal.");
                    self.respond();
                    for r_cn in &running_agents {
                        r_cn.confirm.send(Broadcast::Respond).unwrap();
                    }
                    // println!("agnt waiting conn report finish Prop.");
                    for r_cn in &running_agents {
                        r_cn.report.recv().unwrap();
                    }
                    // println!("agnt get conn report finish Prop.");
                    tx_report.send(()).unwrap();
                }
            }
        }
    }    
}

pub trait FiringPassiveAgent: PassiveAgent {
    fn respond(&mut self) -> Fired;
    fn running_passive_agents(&self) -> Vec<RunningSet<Broadcast, ()>>;

    fn run(&mut self, rx_confirm: CCReceiver<Broadcast>, tx_report: CCSender<()>){
        let running_agents = self.running_passive_agents();
        loop {
            random_sleep();
            match rx_confirm.recv().unwrap() {
                Broadcast::Exit => {
                    for r_cn in &running_agents {
                        r_cn.confirm.send(Broadcast::Exit).unwrap();
                    }
                    for r_cn in running_agents {
                        r_cn.instance.join().expect("connection join error!");
                    }
                    break;
                },
                Broadcast::Evolve => panic!("FiringPassiveagent confirmed by Evolve!"),

                Broadcast::Respond => {
                    random_sleep();
                    // println!("conn wait recv signal.");
                    match self.respond() {
                        Fired::N => (),
                        Fired::Y => {
                            for r_cn in &running_agents {
                                r_cn.confirm.send(Broadcast::Respond).unwrap();
                            }
                            // println!("agnt waiting conn report finish Prop.");
                            for r_cn in &running_agents {
                                r_cn.report.recv().unwrap();
                            }
                        },
                    }
                    tx_report.send(()).unwrap();
                }
            }
        }
    }

    // fn run_passive(&mut self) {
    //     let running_agents = self.running_passive_agents();
    //     let super_confirm = self.super_confirm_receiver();
    //     let fore_ch_pairs = self.fore_ch_pairs();
    //     loop {
    //         match super_confirm.try_recv().unwrap() {
    //             Broadcast::Exit => {
    //                 for r_cn in &running_agents {
    //                     r_cn.confirm.send(Broadcast::Exit).unwrap();
    //                 }
    //                 for r_cn in running_agents {
    //                     r_cn.instance.join().expect("connection join error!");
    //                 }
    //                 break;
    //             },
    //             broadcast => panic!(
    //                 "PassiveAgent should only recv Broadcast::Exit from supervisor/population, here get {:?}.",
    //                 broadcast
    //             ),
    //         }

    //         for pair in fore_ch_pairs {
    //             match pair.0.try_recv().unwrap() {
    //                 Broadcast::Respond => {
    //                     random_sleep();
    //                     // println!("conn wait recv signal.");
    //                     match self.respond() {
    //                         Fired::N => (),
    //                         Fired::Y => {
    //                             for r_cn in &running_agents {
    //                                 r_cn.confirm.send(Broadcast::Respond).unwrap();
    //                             }
    //                             // println!("agnt waiting conn report finish Prop.");
    //                             for r_cn in &running_agents {
    //                                 r_cn.report.recv().unwrap();
    //                             }
    //                         },
    //                     }
    //                     pair.1.send(()).unwrap();
    //                 },
    //                 broadcast => panic!(
    //                     "PassiveAgent should only recv Broadcast::Respond from fore agents, here get {:?}.",
    //                     broadcast
    //                 ),
    //             }
    //         }
    //     }
    // }
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
    fn running_passive_agents(&self) -> Vec<RunningSet<Broadcast, ()>>;

    fn run(&mut self, rx_confirm: CCReceiver<Broadcast>, tx_report: CCSender<()>) {
        let running_agents = self.running_passive_agents();

        loop {
            random_sleep();
            match rx_confirm.recv().unwrap() {

                Broadcast::Exit => {
                    self.end();
                    for r_cn in &running_agents {
                        r_cn.confirm.send(Broadcast::Exit).unwrap();
                    }
                    for r_cn in running_agents {
                        r_cn.instance.join().expect("connection join error!");
                    }
                    break;
                },

                Broadcast::Evolve => {
                    self.evolve();
                    tx_report.send(()).unwrap();
                },

                Broadcast::Respond => {
                    for r_cn in &running_agents {
                        r_cn.confirm.send(Broadcast::Respond).unwrap();
                    }
                    // println!("agnt waiting conn report finish Prop.");
                    for r_cn in &running_agents {
                        r_cn.report.recv().unwrap();
                    }
                    // println!("agnt get conn report finish Prop.");
                    tx_report.send(()).unwrap();
                }
            }
        }
    }
}

pub trait FiringActiveAgent: ActiveAgent + Runnable<Confirm = Broadcast, Report = Fired> {
    fn end(&mut self);
    fn evolve(&mut self) -> Fired;
    fn passive_sync_chs_sets(&self) -> Vec<PassiveSyncChsSet>;

    fn run(&mut self, rx_confirm: CCReceiver<Broadcast>, tx_report: CCSender<Fired>) {
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
                            // println!("agnt waiting conn report finish Prop.");
                            for sync_set in &passive_sync_sets {
                                sync_set.recv_report();
                            }
                            // println!("agnt get conn report finish Prop.");
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
