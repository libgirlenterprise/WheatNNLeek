use crate::operation::{
    Configurable, RunMode,
    // Broadcast,  PassiveAgent, Passive,
    OpeChs,
//    PassiveBackOpeChs,    ActiveAgent,
};
use crate::signals::dirac_delta_voltage::SynapseComponentDiracV;
use crate::agents::{Agent};

pub struct SynapseModel
{
    ope_chs_gen: OpeChs<()>,
    component: SynapseComponentDiracV,
    value: i32,
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
