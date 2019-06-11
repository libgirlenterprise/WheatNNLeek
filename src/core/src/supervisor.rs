use std::sync::{Mutex, Weak};
use std::collections::HashMap;
use crate::utils::random_sleep;
use uom::si::f64::Time;
use uom::si::time::millisecond;
use crate::operation::{Broadcast, RunMode, Fired, RunningSet, PassiveRunningSet};
use crate::operation::op_population::{ConsecutiveActivePopulation, FiringActivePopulation, SilentActivePopulation, PassivePopulation};

pub struct Supervisor {
    time_resolution: Time,
    start_time: Time,
    pub consecutive_populations: HashMap<String, Weak<Mutex<dyn ConsecutiveActivePopulation + Send>>>,
    pub firing_populations: HashMap<String, Weak<Mutex<dyn FiringActivePopulation + Send>>>,
    pub silent_populations: HashMap<String, Weak<Mutex<dyn SilentActivePopulation + Send>>>,
    pub passive_populations: HashMap<String, Weak<Mutex<dyn PassivePopulation + Send>>>,
}

impl Supervisor {
    pub fn new(time_resolution: Time) -> Supervisor {
        Supervisor {
            time_resolution,
            start_time: Time::new::<millisecond>(0.),
            consecutive_populations: HashMap::new(),
            firing_populations: HashMap::new(),
            silent_populations: HashMap::new(),
            passive_populations:HashMap::new(),
        }
    }

    pub fn add_consecutive<T>(&mut self, key: String, pp: Weak<Mutex<T>>)
    where T: 'static + ConsecutiveActivePopulation + Send
    {
        self.consecutive_populations.insert(key, pp);
    }

    pub fn add_firing<T>(&mut self, key: String, pp: Weak<Mutex<T>>)
    where T: 'static + FiringActivePopulation + Send
    {
        self.firing_populations.insert(key, pp);
    }

    pub fn add_silent<T>(&mut self, key: String, pp: Weak<Mutex<T>>)
    where T: 'static + SilentActivePopulation + Send
    {
        self.silent_populations.insert(key, pp);
    }
    
    pub fn add_passive<T>(&mut self, key: String, pp: Weak<Mutex<T>>)
    where T: 'static + PassivePopulation + Send
    {
        self.passive_populations.insert(key, pp);
    }
    
    pub fn run(&mut self, mode: RunMode, duration: Time) {
        let total_steps = (duration / self.time_resolution).value as usize;

        for (_, pp) in &self.consecutive_populations {
            pp.upgrade().unwrap().lock().unwrap().config_mode(mode);
        }
        for (_, pp) in &self.firing_populations {
            pp.upgrade().unwrap().lock().unwrap().config_mode(mode);
        }
        for (_, pp) in &self.silent_populations {
            pp.upgrade().unwrap().lock().unwrap().config_mode(mode);
        }
        for (_, pp) in &self.passive_populations {
            pp.upgrade().unwrap().lock().unwrap().config_mode(mode);
        }
        for (_, pp) in &self.passive_populations {
            pp.upgrade().unwrap().lock().unwrap().recheck_mode();
        }

        for (_, pp) in &self.consecutive_populations {
            pp.upgrade().unwrap().lock().unwrap().config_channels();
        }
        for (_, pp) in &self.firing_populations {
            pp.upgrade().unwrap().lock().unwrap().config_channels();
        }
        for (_, pp) in &self.silent_populations {
            pp.upgrade().unwrap().lock().unwrap().config_channels();
        }
        for (_, pp) in &self.passive_populations {
            pp.upgrade().unwrap().lock().unwrap().config_channels();
        }

        // println!("start making threads for populations.");
        let mut counter = 0;
        let running_consecutive_populations = self.running_consecutive_populations();
        let running_firing_populations = self.running_firing_populations();
        let running_silent_populations = self.running_silent_populations();
        let running_passive_populations = self.running_passive_populations();
        let mut fired_populations = Vec::new();
        loop {

            if counter >= total_steps {
                for r_pp in &running_consecutive_populations {
                    r_pp.confirm.send(Broadcast::Exit).unwrap();
                }
                for r_pp in &running_firing_populations {
                    r_pp.confirm.send(Broadcast::Exit).unwrap();
                }
                for r_pp in &running_silent_populations {
                    r_pp.confirm.send(Broadcast::Exit).unwrap();
                }
                for r_pp in &running_passive_populations {
                    r_pp.confirm.send(Broadcast::Exit).unwrap();
                }
                for r_pp in running_consecutive_populations {
                    r_pp.instance.join().expect("consecutive population join error!");
                }
                for r_pp in running_firing_populations {
                    r_pp.instance.join().expect("firing population join error!");
                }
                for r_pp in running_silent_populations {
                    r_pp.instance.join().expect("silent population join error!");
                }
                for r_pp in running_passive_populations {
                    r_pp.instance.join().expect("passive population join error!");
                }
                break;
            } else  {

                random_sleep();
                // println!("count: {}.", counter);
                fired_populations.clear();
                for r_pp in &running_consecutive_populations {
                    r_pp.confirm.send(Broadcast::Evolve).unwrap();
                }
                for r_pp in &running_firing_populations {
                    r_pp.confirm.send(Broadcast::Evolve).unwrap();
                }
                for r_pp in &running_silent_populations {
                    r_pp.confirm.send(Broadcast::Evolve).unwrap();
                }

                for r_pp in &running_consecutive_populations {
                    r_pp.report.recv().unwrap();
                }
                for r_pp in &running_firing_populations {
                    if let Fired::Y = r_pp.report.recv().unwrap() {
                        fired_populations.push((r_pp.confirm.clone(), r_pp.report.clone()));
                    }
                }
                for r_pp in &running_silent_populations {
                    r_pp.report.recv().unwrap();
                }

                for f_pp in &fired_populations {
                    f_pp.0.send(Broadcast::Respond).unwrap();
                }
                for r_pp in &running_consecutive_populations {
                    r_pp.confirm.send(Broadcast::Respond).unwrap();
                }
                
                // println!("sp waiting pp Respond.");
                for f_pp in &fired_populations {
                    match f_pp.1.recv().unwrap() {
                        Fired::N => (),
                        Fired::Y => panic!("pp report Event after Respond!")
                    }
                }
                for r_pp in &running_consecutive_populations {
                    r_pp.report.recv().unwrap();
                }
                // println!("sp get pp report Respond.");
                counter += 1;
            }
        }
        
        for (_, pp) in &self.consecutive_populations {
            pp.upgrade().unwrap().lock().unwrap().config_mode(RunMode::Idle);
        }
        for (_, pp) in &self.firing_populations {
            pp.upgrade().unwrap().lock().unwrap().config_mode(RunMode::Idle);
        }
        for (_, pp) in &self.silent_populations {
            pp.upgrade().unwrap().lock().unwrap().config_mode(RunMode::Idle);
        }
        for (_, pp) in &self.passive_populations {
            pp.upgrade().unwrap().lock().unwrap().config_mode(RunMode::Idle);
        }

        self.start_time += total_steps as f64 * self.time_resolution;
    }

    fn running_consecutive_populations(&self) -> Vec<RunningSet<Broadcast, ()>> {
        self.consecutive_populations.iter()
            .map(|(_, pp)| {
                RunningSet::<Broadcast, ()>::new(pp.upgrade().unwrap())
            }).collect()
    }

    fn running_firing_populations(&self) -> Vec<RunningSet<Broadcast, Fired>> {
        self.firing_populations.iter()
            .map(|(_, pp)| {
                RunningSet::<Broadcast, Fired>::new(pp.upgrade().unwrap())
            }).collect()
    }

    fn running_silent_populations(&self) -> Vec<RunningSet<Broadcast, ()>> {
        self.silent_populations.iter()
            .map(|(_, pp)| {
                RunningSet::<Broadcast, ()>::new(pp.upgrade().unwrap())
            }).collect()
    }

    fn running_passive_populations(&self) -> Vec<PassiveRunningSet> {
        self.passive_populations.iter()
            .map(|(_, pp)| {
                PassiveRunningSet::new(pp.upgrade().unwrap())
            }).collect()        
    }
}

