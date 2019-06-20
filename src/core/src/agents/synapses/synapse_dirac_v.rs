use crossbeam_channel::Receiver as CCReceiver;
use crossbeam_channel::Sender as CCSender;
use crate::{Ratio, Time};
use crate::operation::{
    Configurable, RunMode, Broadcast,  PassiveAgent, Passive, OpeChs, PassiveBackOpeChs,
};
use crate::operation::op_agent::{ConsecutivePassiveAgent};
use crate::signals::dirac_delta_voltage::{
    SynapseComponentDiracV, DiracV, PostSynDiracV,
    SmplChsCarDiracV, PostSynChsCarDiracV,
};
use crate::agents::{Agent};
use crate::agents::synapses::{SynapseFlag};
use crate::signals::firing_time::FiringTime;
use crate::connectivity::{
    Generator, Acceptor,
};


pub struct SynapseModel
{
    w: Ratio, // weight
    delay: Time, // delay time
    ope_chs_gen: OpeChs<()>,
    component: SynapseComponentDiracV,
}

impl Configurable for SynapseModel
{
    fn config_mode(&mut self, mode: RunMode) {
        self.component.config_mode(mode);
    }
    
    fn config_channels(&mut self) {
        self.component.config_channels();
    }

    fn mode(&self) -> RunMode {
        self.component.mode()
    }
}

impl Agent for SynapseModel {}

impl PassiveAgent for SynapseModel
{    
    fn recheck_mode(&mut self) {
        // println!("SynapseS0S1 recheck_mode().");
        self.component.recheck_mode();
    }

    fn report_sender(&self) -> CCSender<()> {
        self.ope_chs_gen.report_sender()
    }

    fn passive_back_ope_chs(&self) -> PassiveBackOpeChs {
        self.ope_chs_gen.passive_back_ope_chs()
    }
}

impl Passive for SynapseModel {
    fn run(&mut self) {
        <Self as ConsecutivePassiveAgent>::run(self);
    }

    fn confirm_sender(&self) -> CCSender<Broadcast> {
        self.ope_chs_gen.confirm_sender()
    }
    
    fn confirm_receiver(&self) -> CCReceiver<Broadcast> {
        self.ope_chs_gen.confirm_receiver()
    }
}

impl ConsecutivePassiveAgent for SynapseModel
{
    fn respond(&mut self) {
        self.component.ffw_accepted().for_each(|s| self.component.feedforward(self.refine(s)));
        match self.component.flag() {
            SynapseFlag::Static => (),
            SynapseFlag::STDP => self.component.fbw_accepted().for_each(
                |s| self.stdp_update(s)
            ),
        }
    }

    fn passive_sync_chs_sets(&self) -> Vec<PassiveBackOpeChs> {
        self.component.passive_sync_chs_sets()
    }
}

// impl AcceptorDiracV for SynapseModel {}

impl Acceptor<SmplChsCarDiracV> for SynapseModel {}

// impl SynapseGeneratorDiracV for SynapseModel {}

impl Generator<PostSynChsCarDiracV> for SynapseModel {}


impl SynapseModel {
    fn refine(&self, s: DiracV) -> PostSynDiracV {
        PostSynDiracV {
            v: s.0,
            t: self.post_syn_act_time(),
            w: self.w,
        }
    }

    pub fn config_syn_flag(&mut self, flag: SynapseFlag) {
        self.component.config_syn_flag(flag);
    }
    
    fn stdp_update(&mut self, s: FiringTime) {
        panic!("stdp not yet implemented.");
    }
}
