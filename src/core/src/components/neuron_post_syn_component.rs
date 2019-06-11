// not yet modified for neurons for handling STDP!

use std::sync::{Arc};
use crate::{AcMx};
use crate::operation::{RunMode, PassiveSyncChsSet};
use crate::connectivity::{Generator, PassiveGenerator, ActiveGenerator};
use crate::connectivity::linker::{Linker};
use crate::connectivity::post_syn_joint::{PostSynBackJoint, PostSynChsCarrier};

pub struct NeuronPostSynComponent<AG, PG, SF, SB>
where AG: ActiveGenerator<PostSynChsCarrier<SF, SB>> + Send + ?Sized,
      PG: PassiveGenerator<PostSynChsCarrier<SF, SB>> + Send + ?Sized,
      SF: Send + Copy,
      SB: Send + Copy,
{
    mode: RunMode,
    active_in_sets: Vec<PostSynBackJoint<AG, SF, SB>>,
    passive_in_sets: Vec<PostSynBackJoint<PG, SF, SB>>,
    tmp_passive_sync_chs_sets: Option<Vec<PassiveSyncChsSet>>,
}

impl<AG, PG, SF, SB> NeuronPostSynComponent<AG, PG, SF, SB>
where AG: ActiveGenerator<PostSynChsCarrier<SF, SB>> + Send + ?Sized,
      PG: PassiveGenerator<PostSynChsCarrier<SF, SB>> + Send + ?Sized,
      SF: Send + Copy,
      SB: Send + Copy,
{
    pub fn new() -> NeuronPostSynComponent<AG, PG, SF, SB> {
        NeuronPostSynComponent {
            mode: RunMode::Idle,
            active_in_sets: Vec::new(),
            passive_in_sets: Vec::new(),
            tmp_passive_sync_chs_sets: None,
        }
    }

    pub fn mode(&self) -> RunMode {
        self.mode
    }
    
    pub fn ffw_accepted(&self) -> impl Iterator<Item = SF> + '_ {
        match &self.mode {
            RunMode::ForwardStepping => {
                self.active_in_sets.iter()
                    .filter_map(|set| set.opt_ffw_accepted())
                    .chain(
                        self.passive_in_sets.iter()
                            .filter_map(|set| set.opt_ffw_accepted())
                    ).flatten()
            },
            RunMode::ForwardRealTime => panic!("ForwardRealTime not yet implemented!"),
            RunMode::Idle => panic!("NeuronPostSynComponent is Idle when accepted() called!"),
        }
    }

    pub fn feedbackward(&self, s: SB) {
        for set in &self.active_in_sets {
            set.feedbackward(s);
        }
        for set in &self.passive_in_sets {
            set.feedbackward(s);
        }
    }
    
    pub fn add_target(&mut self, target: AcMx<G>, linker: AcMx<Linker<PostSynChsCarrier<SF, SB>>>) {
        match &mut self.mode {
            RunMode::Idle => self.in_sets.push(PostSynBackJoint::new(Arc::downgrade(&target), linker)), 
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
        self.tmp_passive_sync_chs_sets = None;
    }

    fn config_mode_to(&mut self, mode: RunMode) {
        self.mode = mode;
        for set in &mut self.active_in_sets {
            set.config_mode(mode);
        }
        for set in &mut self.passive_in_sets {
            set.config_mode(mode);
        }
    }

    pub fn config_channels(&mut self) {
        for set in &mut self.active_in_sets {
            set.config_channels();
        }
        for set in &mut self.passive_in_sets {
            set.config_channels();
        }
        self.make_passive_sync_chs_sets();
    }

    pub fn make_passive_sync_chs_sets(&mut self) {
        match &self.mode {
            RunMode::Idle => panic!("NeuronPostSynComponent call passive_sync_chs_sets when agent Idle!"),
            RunMode::ForwardStepping => self.tmp_passive_sync_chs_sets = Some(
                self.passive_in_sets.iter()
                    .filter_map(|set| set.passive_sync_chs_set()).collect()  
            ),
            RunMode::ForwardRealTime => panic!("ForwardRealTime not yet implemented!"),
        }
    }

    pub fn passive_sync_chs_sets(&mut self) -> Vec<PassiveSyncChsSet> {
        self.tmp_passive_sync_chs_sets.take().map_or(Vec::with_capacity(0), |v| v)
    }
}
