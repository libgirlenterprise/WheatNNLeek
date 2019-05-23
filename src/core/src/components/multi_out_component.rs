use std::sync::{Mutex, Weak, Arc};
use crate::operation::{RunningSet, RunMode, DeviceMode, Broadcast};
use crate::connectivity::{PassiveAcceptor, ActiveAcceptor};
use crate::components::{OutSet, Linker};

pub struct MultiOutComponent<AA, PA, S>
where AA: 'static + ActiveAcceptor<S> + Send + ?Sized,
      PA: 'static + PassiveAcceptor<S> + Send + ?Sized,
      S: Send + Copy,
{
    mode: RunMode,
    passive_out_sets: Vec<OutSet<PA, S>>,
    active_out_sets: Vec<OutSet<AA, S>>
}

impl<AA, PA, S> MultiOutComponent<AA, PA, S>
where AA: 'static + ActiveAcceptor<S> + Send + ?Sized,
      PA: 'static + PassiveAcceptor<S> + Send + ?Sized,
      S: Send + Copy,
{
    pub fn new() -> MultiOutComponent<AA, PA, S> {
        MultiOutComponent {
            mode: RunMode::Idle,
            passive_out_sets: Vec::new(),
            active_out_sets: Vec::new(),
        }
    }

    pub fn mode(&self) -> RunMode {
        self.mode
    }
    
    pub fn add_active_target(&mut self, target: Weak<Mutex<AA>>, linker: Arc<Mutex<Linker<S>>>) {
        match &mut self.mode {
            RunMode::Idle => self.active_out_sets.push(OutSet::new(target, linker)), 
            _ => panic!("can only add_active when DeviceMode::Idle!"),
        }
    }

    pub fn add_passive_target(&mut self, target: Weak<Mutex<PA>>, linker: Arc<Mutex<Linker<S>>>) {
        match &mut self.mode {
            RunMode::Idle => self.passive_out_sets.push(OutSet::new(target, linker)), 
            _ => panic!("can only add_active when DeviceMode::Idle!"),
        }
    }

    pub fn config_mode(&mut self, mode: RunMode) {
        match (mode, &self.mode) {
            (RunMode::Idle, RunMode::Idle) => println!("config_mode from Idle to Idle, no effect."),
            (RunMode::Idle, _) | (_, RunMode::Idle) => self.config_mode_to(mode),
            (_, _) => panic!("unhandled config_mode: from {:?} to {:?}.", self.mode(), mode),
        }
    }

    fn config_mode_to(&mut self, mode: RunMode) {
        self.mode = mode;
        for set in &mut self.active_out_sets {
            set.config_mode(mode);
        }
        for set in &mut self.passive_out_sets {
            set.config_mode(mode);
        }
    }
    
    pub fn config_channels(&mut self) {
        for set in &mut self.active_out_sets {
            set.config_channels();
        }
        for set in &mut self.passive_out_sets {
            set.config_channels();
        }
    }

    pub fn running_passive_devices(&self) -> Vec<RunningSet<Broadcast, ()>> {
        match &self.mode {
            RunMode::Idle => panic!("MultiOutComponent call running_passive_targets when agent Idle!"),
            RunMode::Feedforward => {
                self.passive_out_sets.iter()
                    .filter_map(|set| match set.channels {
                        DeviceMode::Idle => None,
                        DeviceMode::Feedforward(_) => Some(RunningSet::<Broadcast, ()>::new(set.target.upgrade().unwrap()))
                    }).collect()                
            }
        }
    }

    pub fn feedforward(&self, s: S) {
        match &self.mode {
            RunMode::Feedforward => {
                for set in &self.active_out_sets{
                    set.feedforward(s);
                }
                for set in &self.passive_out_sets {
                    set.feedforward(s);
                }
            },
            _ => panic!("PreAgentmodules1 is not Feedforward when feedforward called!"),
        }
    }
}
