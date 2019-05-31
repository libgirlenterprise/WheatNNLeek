use std::sync::{Arc};
use uom::si::f64::Time;
use uom::si::time::millisecond;
use wheatnnleek::supervisor::Supervisor;
use wheatnnleek::populations::{SimpleFiringPopulation, SimplePassivePopulation};
use wheatnnleek::agents::neurons::{NeuronT};

fn main() {
    let mut sp0 = Supervisor::new(Time::new::<millisecond>(1.0));

    let name_pp_neuron_t = String::from("NeuronT Population");
    let pp_neuron_t = SimpleFiringPopulation::<NeuronT>::new();
    sp0.add_firing(
        name_pp_neuron_t.clone(),
        Arc::downgrade(&pp_neuron_t) // should try to avoid Arc::clone.
    );

    // pp_neuron_t.lock().unwrap().add(NeuronT::new(0, 100, Some(2)));
    // pp_neuron_t.lock().unwrap().add(NeuronT::new(10, 100, Some(2)));
    // pp_neuron_t.lock().unwrap().add(NeuronT::new(100, 100, None));

    // let name_pp_syn_s1 = String::from("SynapseS1 Population");
    // let pp_syn_s1 = SimplePassivePopulation::<SynapseS1>::new();
    // sp0.add_passive(
    //     name_pp_syn_s1.clone(),
    //     Arc::downgrade(&pp_syn_s1) // should try to avoid Arc::clone.
    // );

    // pp_syn_s1.lock().unwrap().add(SynapseS1::new(
    //     pp_neuron_t.lock().unwrap().device_by_id(0),
    //     pp_neuron_t.lock().unwrap().device_by_id(1),
    //     0));
    // pp_syn_s1.lock().unwrap().add(SynapseS1::new(
    //     pp_neuron_t.lock().unwrap().device_by_id(2),
    //     pp_neuron_t.lock().unwrap().device_by_id(1),
    //     -50));
    
}
