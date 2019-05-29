use crossbeam_channel::Receiver as CCReceiver;
use crossbeam_channel::Sender as CCSender;
use crate::components::joints::post_syn_joint::PostSynFlag;
use crate::operation::{DeviceMode};

pub type SimpleForeChs<S> = DeviceMode<SimpleForeChsFwd<S>>;

pub struct SimpleForeChsFwd<S: Send> {
    pub ch_ffw: CCSender<S>,
}

pub type SimpleBackChs<S> = DeviceMode<SimpleBackChsFwd<S>>;

pub struct SimpleBackChsFwd<S: Send> {
    pub ch_ffw: CCReceiver<S>,
}

pub type PostSynForeChs<SF, SB> =  DeviceMode<PostSynForeChsFwd<SF, SB>>;

pub struct PostSynForeChsFwd<SF: Send, SB: Send> {
    pub ch_ffw: CCSender<SF>,
    pub ch_fbw: PostSynFlag<(), CCReceiver<SB>>,
}

impl<SF: Send, SB: Send> PostSynForeChsFwd<SF, SB> {
    pub fn feedforward(&self, s: SF) {
        self.ch_ffw.send(s).unwrap();
    }
}

pub type PostSynBackChs<SF, SB> = DeviceMode<PostSynBackChsFwd<SF, SB>>;

pub struct PostSynBackChsFwd<SF: Send, SB: Send> {
    pub ch_ffw: CCReceiver<SF>,
    pub ch_fbw: PostSynFlag<(), CCSender<SB>>,
}

impl<SF: Send, SB: Send> PostSynBackChsFwd<SF, SB> {
    pub fn feedbackward(&self, s: SB) {
        match &self.ch_fbw {
            PostSynFlag::STDP(ch) => ch.send(s).unwrap(),
            _ => (),
        }
    }    
}

