use crate::operation::{
//    Configurable, Broadcast, RunMode, PassiveAgent, Passive,
    OpeChs,
//    PassiveBackOpeChs,    ActiveAgent,
};
use crate::signals::dirac_delta_voltage::SynapseComponentDiracV;

pub struct SynapseModel
{
    ope_chs_gen: OpeChs<()>,
    component: SynapseComponentDiracV,
    value: i32,
}
