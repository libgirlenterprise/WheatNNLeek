use std::sync::{Mutex, Weak, Arc};
use crate::operation::{RunMode};
use crate::connectivity::Generator;
use crate::components::{InSet, Linker};

pub struct MultiInComponent<C, S>
where C: 'static + Generator<S> + Send + ?Sized,
      S: Send,
{
    mode: RunMode,
    in_sets: Vec<InSet<C, S>>,
}

impl<C, S> MultiInComponent<C, S>
where C: 'static + Generator<S> + Send + ?Sized,
      S: Send,
{
    pub fn new() -> MultiInComponent<C, S> {
        MultiInComponent {
            mode: RunMode::Idle,
            in_sets: Vec::new(),
        }
    }

    pub fn mode(&self) -> RunMode {
        self.mode
    }
    
    pub fn ffw_accepted(&self) -> Vec<S> {
        match &self.mode {
            RunMode::Feedforward => {
                self.in_sets.iter()
                    .filter_map(|set| set.ffw_accepted_iter())
                    .flatten().collect()
            },
            RunMode::Idle => panic!("MultiInComponent is Idle when accepted() called!"),
        }
    }
    
    pub fn add_target(&mut self, target: Weak<Mutex<C>>, linker: Arc<Mutex<Linker<S>>>) {
        match &mut self.mode {
            RunMode::Idle => self.in_sets.push(InSet::new(target, linker)), 
            _ => panic!("can only add_conntion when DeviceMode::Idle!"),
        }
    }

    pub fn config_mode(&mut self, mode: RunMode) {
        match (mode, &self.mode) {
            (RunMode::Idle, RunMode::Idle) => println!("MultiInComponent config_mode from Idle to Idle, no effect."),
            (RunMode::Idle, _) => self.config_mode_to(mode),
            (_, RunMode::Idle) => self.config_mode_to(mode),
            (_, _) => panic!("MultiInComponent config_mode: from {:?} to {:?}.", self.mode(), mode),
        }
    }

    fn config_mode_to(&mut self, mode: RunMode) {
        self.mode = mode;
        for set in &mut self.in_sets {
            set.config_mode(mode);
        }
    }

    pub fn config_channels(&mut self) {
        for set in &mut self.in_sets {
            set.config_channels();
        }
    }
}
