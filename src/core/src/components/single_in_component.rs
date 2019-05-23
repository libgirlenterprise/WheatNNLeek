use std::sync::{Weak, Mutex, Arc};
use crate::operation::{RunMode};
use crate::components::{InSet, Linker};
use crate::connectivity::Generator;

pub struct SingleInComponent<G, S>
where G: Generator<S> + Send + ?Sized,
      S: Send,
{
    mode: RunMode,
    in_set: Option<InSet<G, S>>,
}

impl<G, S> SingleInComponent<G, S>
where G: Generator<S> + Send + ?Sized,
      S: Send,
{
    pub fn new() -> SingleInComponent<G, S> {
        SingleInComponent {
            mode: RunMode::Idle,
            in_set: None,
        }
    }

    pub fn add_target(&mut self, target: Weak<Mutex<G>>, linker: Arc<Mutex<Linker<S>>>) {
        match &mut self.mode {
            RunMode::Idle => match &mut self.in_set {
                None => self.in_set = Some(InSet::new(target, linker)),
                Some(_) => println!("SingleInComponent already connected!"),
            }
            _ => panic!("SingleInComponent can only add_conntion when DeviceMode::Idle!"),
        }
    }

    pub fn mode(&self) -> RunMode {
        self.mode
    }
    
    pub fn config_mode(&mut self, mode: RunMode) {
        match (mode, &self.mode) {
            (RunMode::Idle, RunMode::Idle) => println!("SingleInComponent config_mode from Idle to Idle, no effect."),
            (RunMode::Idle, _) => self.config_mode_to(mode),
            (_, RunMode::Idle) => self.config_mode_to(mode),
            (_, _) => panic!("config_mode on SingleIncomponent: from {:?} to {:?}.", self.mode(), mode),
        }
    }

    fn config_mode_to(&mut self, mode: RunMode) {
        self.mode = mode;
        match &mut self.in_set {
            None => (),
            Some(set) => set.config_mode(mode),
        }
    }
    
    pub fn config_channels(&mut self) {
        match &mut self.in_set {
            None => (),
            Some(set) => set.config_channels(),
        }
    }
    
    pub fn ffw_accepted(&self) -> Vec<S> {
        match &self.in_set {
            None => Vec::with_capacity(0),
            Some(set) => match &self.mode {
                RunMode::Feedforward => {
                    set.ffw_accepted_iter()
                        .map_or(Vec::with_capacity(0), |iter| iter.collect())
                },
                RunMode::Idle => panic!("SingleInComponent is Idle when accepted() called!"),
            },
        }   
    }
}
