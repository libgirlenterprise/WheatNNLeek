pub mod neurons;
pub mod synapses;
// pub mod devices;

pub trait Device {}
pub use neurons::Neuron;
pub use synapses::Synapse;
