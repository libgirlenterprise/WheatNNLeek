use std::sync::{Arc, Mutex};
use crate::{AcMx};
use crate::operation::{RunMode, DeviceMode};

mod tmp_contents;
// pub mod simple_joint;
pub mod post_syn_joint;
pub mod channels_sets;

pub trait ChannelsCarrier {
    // type ContentFWD;
    type ForeEndChs;
    type BackEndChs;
    
    fn new() -> Self;
    fn reset_idle(&mut self);
    fn mode(&self) -> RunMode;
    fn fore_chs(&mut self, mode: RunMode) -> DeviceMode<<Self as ChannelsCarrier>::ForeEndChs>;
    fn back_chs(&mut self, mode: RunMode) -> DeviceMode<<Self as ChannelsCarrier>::BackEndChs>;
    // fn take_post(&mut self) -> DeviceMode<<Self as ChannelsCarrier>::ChsInFwd>;
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
    
    pub fn fore_end_chs(&mut self) -> DeviceMode<C::ForeEndChs> {
        match (self.pre_mode, self.post_mode, self.tmp.mode()) {
            (RunMode::Idle, _, RunMode::Idle) | (_, RunMode::Idle, RunMode::Idle) => {
                self.config_idle();
                DeviceMode::Idle
            },
            (RunMode::Idle, _, ch_m) | (_, RunMode::Idle, ch_m) => panic!(
                "Linker.fore_chs(): pre or post of Linker is Idle, but Channels is {:?}!", ch_m
            ),
            (pre_m, post_m, _) if pre_m == post_m  => self.tmp.fore_chs(pre_m),
            // (pre_m, post_m, RunMode::Idle) if pre_m == post_m  => self.tmp.fore_chs(pre_m),
            // (pre_m, post_m, ch_m) if (pre_m == post_m) && (pre_m == ch_m) => self.tmp.take_pre(),
            (pre_m, post_m, ch_m) => panic!(
                "Linker.fore_chs() error: pre: {:?}, post: {:?}, ch: {:?}",
                pre_m, post_m, ch_m
            ),
        }
    }

    pub fn back_end_chs(&mut self) -> DeviceMode<C::BackEndChs> {
        match (self.pre_mode, self.post_mode, self.tmp.mode()) {
            (RunMode::Idle, _, RunMode::Idle) | (_, RunMode::Idle, RunMode::Idle) => {
                self.config_idle();
                DeviceMode::Idle
            },
            (RunMode::Idle, _, ch_m) | (_, RunMode::Idle, ch_m) => panic!(
                "Linker.back_chs(): pre or post of Linker is Idle, but Channels is {:?}!", ch_m
            ),
            (pre_m, post_m, _) if pre_m == post_m  => self.tmp.back_chs(pre_m),
            // (pre_m, post_m, RunMode::Idle) if pre_m == post_m  => self.tmp.back_chs(pre_m),
            // (pre_m, post_m, ch_m) if (pre_m == post_m) && (pre_m == ch_m) => self.tmp.take_post(),
            (pre_m, post_m, ch_m) => panic!(
                "Linker.back_chs() error: pre: {:?}, post: {:?}, ch: {:?}",
                pre_m, post_m, ch_m
            ),
        }
    }
}
