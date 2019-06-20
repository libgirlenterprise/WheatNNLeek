pub mod synapse_s0_s1;
pub use synapse_s0_s1::SynapseS0S1;
pub mod synapse_dirac_v;
pub use synapse_dirac_v::SynapseModel as SynapseDiracV;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum SynapseFlag {
    Static,
    STDP,
}

pub enum PostSynFlag<SI, ST> {
    Static(SI),
    STDP(ST),
}

impl<SI, ST> PostSynFlag<SI, ST> {
    pub fn variant(&self) -> SynapseFlag {
        match &self {
            PostSynFlag::Static(_) => SynapseFlag::Static,
            PostSynFlag::STDP(_) => SynapseFlag::STDP,
        }
    }
}

pub trait Synapse {}
