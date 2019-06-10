use crate::{AcMx};
use crate::operation::{RunMode, PassiveSyncChsSet};
use crate::connectivity::{PassiveAcceptor, ActiveAcceptor};
use crate::connectivity::linker::{Linker};
// use crate::connectivity::channels_sets::{SimpleForeChsFwd};
use crate::connectivity::simple_joint::{SimpleForeJoint, SimpleChsCarrier};

pub struct MultiOutComponent<AA, PA, S>
where AA: 'static + ActiveAcceptor<SimpleChsCarrier<S>> + Send + ?Sized,
      PA: 'static + PassiveAcceptor<SimpleChsCarrier<S>> + Send + ?Sized,
      S: Send + Copy,
{
    mode: RunMode,
    passive_out_sets: Vec<SimpleForeJoint<PA, S>>,
    active_out_sets: Vec<SimpleForeJoint<AA, S>>
}

impl<AA, PA, S> MultiOutComponent<AA, PA, S>
where AA: 'static + ActiveAcceptor<SimpleChsCarrier<S>> + Send + ?Sized,
      PA: 'static + PassiveAcceptor<SimpleChsCarrier<S>> + Send + ?Sized,
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
    
    pub fn add_active_target(&mut self, target: AcMx<AA>, linker: AcMx<Linker<SimpleChsCarrier<S>>>) {
        match &mut self.mode {
            RunMode::Idle => self.active_out_sets.push(SimpleForeJoint::new(target, linker)), 
            _ => panic!("can only add_active when DeviceMode::Idle!"),
        }
    }

    pub fn add_passive_target(&mut self, target: AcMx<PA>, linker: AcMx<Linker<SimpleChsCarrier<S>>>) {
        match &mut self.mode {
            RunMode::Idle => self.passive_out_sets.push(SimpleForeJoint::new(target, linker)), 
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

    pub fn passive_sync_chs_sets(&self) -> Vec<PassiveSyncChsSet> {
        match &self.mode {
            RunMode::Idle => panic!("MultiOutComponent call passive_sync_chs_sets when agent Idle!"),
            RunMode::ForwardStepping => {
                self.passive_out_sets.iter()
                    .filter_map(|set| set.passive_sync_chs_set()).collect()
            },
            RunMode::ForwardRealTime => panic!("ForwardRealTime not yet implemented!"),
        }
    }
    
    // pub fn running_passive_devices(&self) -> Vec<PassiveRunningSet> {
    //     match &self.mode {
    //         RunMode::Idle => panic!("MultiOutComponent call running_passive_targets when agent Idle!"),
    //         RunMode::ForwardStepping => {
    //             self.passive_out_sets.iter()
    //                 .filter_map(|set| set.running_target()).collect()                
    //         },
    //         RunMode::ForwardRealTime => panic!("ForwardRealTime not yet implemented!"),
    //     }
    // }

    pub fn feedforward(&self, s: S) {
        match &self.mode {
            RunMode::ForwardStepping => {
                for set in &self.active_out_sets{
                    set.feedforward(s);
                }
                for set in &self.passive_out_sets {
                    set.feedforward(s);
                }
            },
            _ => panic!("PreAgentmodules1 is not ForwardStepping when feedforward called!"),
        }
    }
}
