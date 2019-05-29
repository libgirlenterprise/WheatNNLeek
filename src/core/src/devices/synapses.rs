pub enum SynapseFlag {
    Simple,
    STDP,
}

pub enum PostSynFlag<SI, ST> {
    Simple(SI),
    STDP(ST),
}

pub trait Synapse {}
