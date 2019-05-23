/// used by Population.running_devices() or OutComponents.running_devices()

extern crate crossbeam_channel;
use crossbeam_channel::Receiver as CCReceiver;
use crossbeam_channel::Sender as CCSender;
use crate::operation::{RunningSet, Broadcast, Fired, PassiveDevice, ActiveDevice, Runnable};
use crate::utils::random_sleep;

pub trait ConsecutivePassiveDevice: PassiveDevice {
    fn respond(&mut self);
    fn running_passive_devices(&self) -> Vec<RunningSet<Broadcast, ()>>;

    fn run(&mut self, rx_confirm: CCReceiver<Broadcast>, tx_report: CCSender<()>){
        let running_devices = self.running_passive_devices();
        loop {
            random_sleep();
            match rx_confirm.recv().unwrap() {
                Broadcast::Exit => {
                    for r_cn in &running_devices {
                        r_cn.confirm.send(Broadcast::Exit).unwrap();
                    }
                    for r_cn in running_devices {
                        r_cn.instance.join().expect("connection join error!");
                    }
                    break;
                },
                Broadcast::Evolve => panic!("ConsecutivePassivedevice confirmed by Evolve!"),

                Broadcast::Respond => {
                    // println!("conn wait recv signal.");
                    self.respond();
                    for r_cn in &running_devices {
                        r_cn.confirm.send(Broadcast::Respond).unwrap();
                    }
                    // println!("agnt waiting conn report finish Prop.");
                    for r_cn in &running_devices {
                        r_cn.report.recv().unwrap();
                    }
                    // println!("agnt get conn report finish Prop.");
                    tx_report.send(()).unwrap();
                }
            }
        }
    }    
}

pub trait FiringPassiveDevice: PassiveDevice {
    fn respond(&mut self) -> Fired;
    fn running_passive_devices(&self) -> Vec<RunningSet<Broadcast, ()>>;

    fn run(&mut self, rx_confirm: CCReceiver<Broadcast>, tx_report: CCSender<()>){
        let running_devices = self.running_passive_devices();
        loop {
            random_sleep();
            match rx_confirm.recv().unwrap() {
                Broadcast::Exit => {
                    for r_cn in &running_devices {
                        r_cn.confirm.send(Broadcast::Exit).unwrap();
                    }
                    for r_cn in running_devices {
                        r_cn.instance.join().expect("connection join error!");
                    }
                    break;
                },
                Broadcast::Evolve => panic!("FiringPassivedevice confirmed by Evolve!"),

                Broadcast::Respond => {
                    random_sleep();
                    // println!("conn wait recv signal.");
                    match self.respond() {
                        Fired::N => (),
                        Fired::Y => {
                            for r_cn in &running_devices {
                                r_cn.confirm.send(Broadcast::Respond).unwrap();
                            }
                            // println!("agnt waiting conn report finish Prop.");
                            for r_cn in &running_devices {
                                r_cn.report.recv().unwrap();
                            }
                        },
                    }
                    tx_report.send(()).unwrap();
                }
            }
        }
    }
}

pub trait SilentPassiveDevice: PassiveDevice {
    fn respond(&mut self);

    fn run(&mut self, rx_confirm: CCReceiver<Broadcast>, tx_report: CCSender<()>){
        loop {
            random_sleep();
            match rx_confirm.recv().unwrap() {
                Broadcast::Exit => break,
                Broadcast::Evolve => panic!("Passivedevice confirmed by Evolve!"),
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

pub trait ConsecutiveActiveDevice: ActiveDevice + Runnable<Confirm = Broadcast, Report = ()> {
    fn end(&mut self);
    fn evolve(&mut self);
    fn running_passive_devices(&self) -> Vec<RunningSet<Broadcast, ()>>;

    fn run(&mut self, rx_confirm: CCReceiver<Broadcast>, tx_report: CCSender<()>) {
        let running_devices = self.running_passive_devices();

        loop {
            random_sleep();
            match rx_confirm.recv().unwrap() {

                Broadcast::Exit => {
                    self.end();
                    for r_cn in &running_devices {
                        r_cn.confirm.send(Broadcast::Exit).unwrap();
                    }
                    for r_cn in running_devices {
                        r_cn.instance.join().expect("connection join error!");
                    }
                    break;
                },

                Broadcast::Evolve => {
                    self.evolve();
                    tx_report.send(()).unwrap();
                },

                Broadcast::Respond => {
                    for r_cn in &running_devices {
                        r_cn.confirm.send(Broadcast::Respond).unwrap();
                    }
                    // println!("agnt waiting conn report finish Prop.");
                    for r_cn in &running_devices {
                        r_cn.report.recv().unwrap();
                    }
                    // println!("agnt get conn report finish Prop.");
                    tx_report.send(()).unwrap();
                }
            }
        }
    }
}

pub trait FiringActiveDevice: ActiveDevice + Runnable<Confirm = Broadcast, Report = Fired> {
    fn end(&mut self);
    fn evolve(&mut self) -> Fired;
    fn running_passive_devices(&self) -> Vec<RunningSet<Broadcast, ()>>;

    fn run(&mut self, rx_confirm: CCReceiver<Broadcast>, tx_report: CCSender<Fired>) {
        let running_devices = self.running_passive_devices();
        let mut last_result = Fired::N;
        
        loop {
            random_sleep();
            match rx_confirm.recv().unwrap() {

                Broadcast::Exit => {
                    self.end();
                    for r_cn in &running_devices {
                        r_cn.confirm.send(Broadcast::Exit).unwrap();
                    }
                    for r_cn in running_devices {
                        r_cn.instance.join().expect("connection join error!");
                    }
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
                            for r_cn in &running_devices {
                                r_cn.confirm.send(Broadcast::Respond).unwrap();
                            }
                            // println!("agnt waiting conn report finish Prop.");
                            for r_cn in &running_devices {
                                r_cn.report.recv().unwrap();
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

pub trait SilentActiveDevice: ActiveDevice + Runnable<Confirm = Broadcast, Report = ()>{
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
