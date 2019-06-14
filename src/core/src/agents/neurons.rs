mod neuron_t;
pub use neuron_t::NeuronT;
mod lif;
pub use lif::NeuronModel as LIF;

pub trait Neuron {}

