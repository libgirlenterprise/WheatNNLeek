// not yet modified for neurons for handling STDP!

use std::sync::{Mutex, Weak, Arc};
use crate::operation::{RunMode};
use crate::connectivity::Generator;
use crate::connectivity::linker::{Linker};
use crate::connectivity::post_syn_joint::{PostSynBackJoint, PostSynChsCarrier};

pub struct NeuronPostSynComponent<G, SF, SB>
where G: Generator<PostSynChsCarrier<SF, SB>> + Send + ?Sized,
      SF: Send + Copy,
      SB: Send + Copy,
{
    mode: RunMode,
    in_sets: Vec<PostSynBackJoint<G, SF, SB>>,
}

impl<G, SF, SB> NeuronPostSynComponent<G, SF, SB>
    where G: Generator<PostSynChsCarrier<SF, SB>> + Send + ?Sized,
      SF: Send + Copy,
      SB: Send + Copy,
{
    pub fn new() -> NeuronPostSynComponent<G, SF, SB> {
        NeuronPostSynComponent {
            mode: RunMode::Idle,
            in_sets: Vec::new(),
        }
    }

    pub fn mode(&self) -> RunMode {
        self.mode
    }
    
    pub fn ffw_accepted(&self) -> impl Iterator<Item = SF> + '_ {
        match &self.mode {
            RunMode::ForwardStepping => {
                self.in_sets.iter()
                    .filter_map(|set| set.ffw_accepted_iter())
                    .flatten()
            },
            RunMode::ForwardRealTime => panic!("ForwardRealTime not yet implemented!"),
            RunMode::Idle => panic!("NeuronPostSynComponent is Idle when accepted() called!"),
        }
    }
    
    pub fn add_target(&mut self, target: Weak<Mutex<G>>, linker: Arc<Mutex<Linker<PostSynChsCarrier<SF, SB>>>>) {
        match &mut self.mode {
            RunMode::Idle => self.in_sets.push(PostSynBackJoint::new(target, linker)), 
            _ => panic!("can only add_conntion when DeviceMode::Idle!"),
        }
    }

    pub fn config_mode(&mut self, mode: RunMode) {
        match (mode, &self.mode) {
            (RunMode::Idle, RunMode::Idle) => println!("NeuronPostSynComponent config_mode from Idle to Idle, no effect."),
            (RunMode::Idle, _) => self.config_mode_to(mode),
            (_, RunMode::Idle) => self.config_mode_to(mode),
            (_, _) => panic!("NeuronPostSynComponent config_mode: from {:?} to {:?}.", self.mode(), mode),
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
