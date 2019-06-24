use crate::{
    // utils::random_sleep;
    operation::{
        Broadcast, Fired, ActiveAgent, PassiveBackOpeChs, Active,
    },   
};
use uom::si::f64::Time;

pub trait FiringActiveAgent: ActiveAgent + Active<Report = Fired> {
    fn end(&mut self);
    fn evolve(&mut self, time: Time, dt: Time) -> Fired;
    fn passive_sync_chs_sets(&mut self) -> Vec<PassiveBackOpeChs>;
    fn serial_evolve(&mut self, begin: Time, dt: Time);
    
    fn run(&mut self, mut time: Time, dt: Time) {
        let rx_confirm = self.confirm_receiver();
        let tx_report = self.report_sender();
        let passive_sync_sets = self.passive_sync_chs_sets();
        let mut last_result = Fired::N;
        
        loop {
            // random_sleep();
            match rx_confirm.recv().unwrap() {

                Broadcast::Exit => {
                    self.end();
                    break;
                },

                Broadcast::Evolve => {
                    match self.evolve(time, dt) {
                        Fired::N => tx_report.send(Fired::N).unwrap(),
                        Fired::Y => {
                            // random_sleep();
                            last_result = Fired::Y;
                            tx_report.send(Fired::Y).unwrap();
                            // println!("agnt finished Evolve.");
                        }
                    }
                },

                Broadcast::Respond => {
                    // random_sleep();
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
            time += dt;
        }
    }
}
