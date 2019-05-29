use crossbeam_channel::Receiver as CCReceiver;
use crossbeam_channel::Sender as CCSender;
use crate::components::joints::post_syn_joint::PostSynFlag;

pub struct PostSynForeEndChs<SF: Send, SB: Send> {
    pub ch_ffw: CCSender<SF>,
    pub ch_fbw: PostSynFlag<(), CCReceiver<SB>>,
}

impl<SF: Send, SB: Send> PostSynForeEndChs<SF, SB> {
    pub fn feedforward(&self, s: SF) {
        self.ch_ffw.send(s).unwrap();
    }
}

pub struct PostSynBackEndChs<SF: Send, SB: Send> {
    pub ch_ffw: CCReceiver<SF>,
    pub ch_fbw: PostSynFlag<(), CCSender<SB>>,
}

impl<SF: Send, SB: Send> PostSynBackEndChs<SF, SB> {
    pub fn feedbackward(&self, s: SB) {
        match &self.ch_fbw {
            PostSynFlag::STDP(ch) => ch.send(s).unwrap(),
            _ => (),
        }
    }    
}
