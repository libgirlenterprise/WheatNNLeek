use std::sync::{Arc};
use uom::si::f64::Time;
use uom::si::time::millisecond;
use wheatnnleek::supervisor::Supervisor;
use wheatnnleek::populations::{SimpleFiringPopulation, SimplePassivePopulation};
use wheatnnleek::agents::neurons::{NeuronT};
use wheatnnleek::agents::synapses::synapse_s0_s1::SynapseS0S1;

fn main() {
    let mut sp0 = Supervisor::new(Time::new::<millisecond>(1.0));

    let name_pp_neuron_t = String::from("NeuronT Population");
    let pp_neuron_t = SimpleFiringPopulation::<NeuronT>::new();
    sp0.add_firing(
        name_pp_neuron_t.clone(),
        Arc::downgrade(&pp_neuron_t) // should try to avoid Arc::clone.
    );

    pp_neuron_t.lock().unwrap().add(NeuronT::new(0, 100, Some(2)));
    pp_neuron_t.lock().unwrap().add(NeuronT::new(10, 100, Some(2)));
    pp_neuron_t.lock().unwrap().add(NeuronT::new(100, 100, None));

    let name_p_syn_s0s1 = String::from("SynapseS1 Population");
    let p_syn_s0s1 = SimplePassivePopulation::<SynapseS0S1<>>::new(0);
    sp0.add_passive(
        name_p_syn_s0s1.clone(),
        Arc::downgrade(&p_syn_s0s1) // should try to avoid Arc::clone.
    );

    p_syn_s0s1.lock().unwrap().add(SynapseS0S1::new(
        pp_neuron_t.lock().unwrap().device_by_id(0),
        pp_neuron_t.lock().unwrap().device_by_id(1),
        0));
    // p_syn_s0s1.lock().unwrap().add(SynapseS1::new(
    //     pp_neuron_t.lock().unwrap().device_by_id(2),
    //     pp_neuron_t.lock().unwrap().device_by_id(1),
    //     -50));
    
}
