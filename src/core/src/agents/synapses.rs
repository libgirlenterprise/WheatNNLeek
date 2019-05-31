#[derive(Copy, Clone, Debug, PartialEq)]
pub enum SynapseFlag {
    Simple,
    STDP,
}

pub enum PostSynFlag<SI, ST> {
    Simple(SI),
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
