use std::sync::{Arc, Mutex};
use crate::{AcMx};
// use crossbeam_channel::Receiver as CCReceiver;
// use crossbeam_channel::Sender as CCSender;
// // use crossbeam_channel::TryIter as CCTryIter;
use crate::operation::{RunMode, DeviceMode};
// use crate::connectivity::{Acceptor, Generator};

// pub use multi_in_component::MultiInComponent;
// pub use neuron_post_syn_component::NeuronPostSynComponent;
// pub use multi_out_component::MultiOutComponent;
// pub use multi_out_component::MultiOutComponent as NeuronPreSynComponent;
// pub use single_in_component::SingleInComponent;
// pub use single_out_component::SingleOutComponent;
// pub use synapse_component::SynapseComponent;
// pub use simple_joint::{InSet, OutSet};
// pub use simple_joint::Linker as PreSynLinker;
// pub use simple_joint::InSet as PreSynJointIn;
// pub use simple_joint::OutSet as PreSynJointOut;

// mod simple_joint;
mod post_syn_joint;
// mod multi_in_component;
// mod multi_out_component;
// mod single_in_component;
// mod single_out_component;
// mod neuron_post_syn_component;
// pub mod synapse_component;

pub trait ChannelsCarrier {
    // type ContentFWD;
    type ChsInFwd;
    type ChsOutFwd;
    
    fn new() -> Self;
    fn reset_idle(&mut self);
    fn mode(&self) -> RunMode;
    fn make_pre(&mut self, mode: RunMode) -> DeviceMode<<Self as ChannelsCarrier>::ChsOutFwd>;
    fn take_pre(&mut self) -> DeviceMode<<Self as ChannelsCarrier>::ChsOutFwd>;
    fn make_post(&mut self, mode: RunMode) -> DeviceMode<<Self as ChannelsCarrier>::ChsInFwd>;
    fn take_post(&mut self) -> DeviceMode<<Self as ChannelsCarrier>::ChsInFwd>;
}

pub struct Linker<C: ChannelsCarrier> {
    pre_mode: RunMode,
    post_mode: RunMode,
    tmp: C,
}

impl<C: ChannelsCarrier> Linker<C> {
    pub fn new() -> AcMx<Linker<C>> {
        Arc::new(Mutex::new(Linker {
            pre_mode: RunMode::Idle,
            post_mode: RunMode::Idle,
            tmp: C::new(),
        }))
    }

    pub fn config_post(&mut self, mode: RunMode) {
        match (mode, &mut self.post_mode) {
            (RunMode::Idle, _) => self.config_idle(),
            (_, RunMode::Idle) => self.post_mode = mode,
            (_, _) => panic!("config_post on Linker: from {:?} to {:?}.", self.post_mode, mode),
        }
    }

    pub fn config_pre(&mut self, mode: RunMode) {
        match (mode, &mut self.pre_mode) {
            (RunMode::Idle, _) => self.config_idle(),
            (_, RunMode::Idle) => self.pre_mode = mode,
            (_, _) => panic!("config_pre on Linker: from {:?} to {:?}.", self.pre_mode, mode),
        }
    }

    pub fn config_idle(&mut self) {
        self.pre_mode = RunMode::Idle;
        self.post_mode = RunMode::Idle;
        self.tmp.reset_idle();
    }
    
    pub fn make_pre(&mut self) -> DeviceMode<C::ChsOutFwd> {
        match (self.pre_mode, self.post_mode, self.tmp.mode()) {
            (RunMode::Idle, _, RunMode::Idle) | (_, RunMode::Idle, RunMode::Idle) => {
                self.config_idle();
                DeviceMode::Idle
            },
            (RunMode::Idle, _, ch_m) | (_, RunMode::Idle, ch_m) => panic!(
                "Linker.make_pre(): pre or post of Linker is Idle, but Channels is {:?}!", ch_m
            ),
            (pre_m, post_m, RunMode::Idle) if pre_m == post_m  => self.tmp.make_pre(pre_m),
            (pre_m, post_m, ch_m) if (pre_m == post_m) && (pre_m == ch_m) => self.tmp.take_pre(),
            (pre_m, post_m, ch_m) => panic!(
                "Linker.make_pre() error: pre: {:?}, post: {:?}, ch: {:?}",
                pre_m, post_m, ch_m
            ),
        }
    }

    pub fn make_post(&mut self) -> DeviceMode<C::ChsInFwd> {
        match (self.pre_mode, self.post_mode, self.tmp.mode()) {
            (RunMode::Idle, _, RunMode::Idle) | (_, RunMode::Idle, RunMode::Idle) => {
                self.config_idle();
                DeviceMode::Idle
            },
            (RunMode::Idle, _, ch_m) | (_, RunMode::Idle, ch_m) => panic!(
                "Linker.make_post(): pre or post of Linker is Idle, but Channels is {:?}!", ch_m
            ),
            (pre_m, post_m, RunMode::Idle) if pre_m == post_m  => self.tmp.make_post(pre_m),
            (pre_m, post_m, ch_m) if (pre_m == post_m) && (pre_m == ch_m) => self.tmp.take_post(),
            (pre_m, post_m, ch_m) => panic!(
                "Linker.make_post() error: pre: {:?}, post: {:?}, ch: {:?}",
                pre_m, post_m, ch_m
            ),
        }
    }
}

