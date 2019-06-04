pub mod synapse_s0_s1;
pub use synapse_s0_s1::SynapseS0S1;


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
            PostSynFlag::Simple(_) => SynapseFlag::Simple,
            PostSynFlag::STDP(_) => SynapseFlag::STDP,
        }
    }
}

pub trait Synapse {}
