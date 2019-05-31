use crossbeam_channel::Receiver as CCReceiver;
use crossbeam_channel::Sender as CCSender;
use crate::agents::synapses::{PostSynFlag};
use crate::operation::{AgentRunMode};

pub type SimpleForeChs<S> = AgentRunMode<SimpleForeChsFwd<S>>;

pub struct SimpleForeChsFwd<S: Send> {
    pub ch_ffw: CCSender<S>,
}

pub type SimpleBackChs<S> = AgentRunMode<SimpleBackChsFwd<S>>;

pub struct SimpleBackChsFwd<S: Send> {
    pub ch_ffw: CCReceiver<S>,
}

pub type PostSynForeChs<SF, SB> =  AgentRunMode<PostSynForeChsFwd<SF, SB>>;

pub struct PostSynForeChsFwd<SF: Send, SB: Send> {
    pub ch_ffw: CCSender<SF>,
    pub ch_fbw: PostSynFlag<(), CCReceiver<SB>>,
}

impl<SF: Send, SB: Send> PostSynForeChsFwd<SF, SB> {
    pub fn feedforward(&self, s: SF) {
        self.ch_ffw.send(s).unwrap();
    }
}

pub type PostSynBackChs<SF, SB> = AgentRunMode<PostSynBackChsFwd<SF, SB>>;

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

