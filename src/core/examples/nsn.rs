use std::sync::{Arc};
use uom::si::f64::Time;
use uom::si::time::millisecond;
use wheatnnleek::supervisor::Supervisor;
use wheatnnleek::populations::{SimpleFiringPopulation, SimplePassivePopulation, HoldAgents};
use wheatnnleek::agents::neurons::{NeuronT};
use wheatnnleek::agents::synapses::synapse_s0_s1::{SynapseS0S1};
use wheatnnleek::agents::synapses::{SynapseFlag};
use wheatnnleek::operation::{RunMode};

fn main() {
    let mut sp0 = Supervisor::new(Time::new::<millisecond>(1.0));

    let name_pp_neuron_t = String::from("NeuronT Population");
    let pp_neuron_t = SimpleFiringPopulation::<NeuronT>::new();
    sp0.add_firing(
        name_pp_neuron_t.clone(),
        Arc::downgrade(&pp_neuron_t) // should try to avoid Arc::clone.
    );

    pp_neuron_t.lock().unwrap().add(NeuronT::new(0, 0, Some(2)));
    pp_neuron_t.lock().unwrap().add(NeuronT::new(10, 0, Some(3)));
    pp_neuron_t.lock().unwrap().add(NeuronT::new(100, 0, None));

    let name_p_syn_s0s1 = String::from("SynapseS1 Population");
    let p_syn_s0s1 = SimplePassivePopulation::new();
    sp0.add_passive(
        name_p_syn_s0s1.clone(),
        Arc::downgrade(&p_syn_s0s1) // should try to avoid Arc::clone.
    );
    

    let n1 = pp_neuron_t.lock().unwrap().agent_by_id(0);
    let n2 = pp_neuron_t.lock().unwrap().agent_by_id(1);
    p_syn_s0s1.lock().unwrap().add(SynapseS0S1::new_on_active(n1, n2, 0));

    // commment this block to see STDP on/off.
    p_syn_s0s1.lock().unwrap()
        .agent_by_id(0)
        .lock().unwrap()
        .config_syn_flag(SynapseFlag::STDP);
        
    println!("start run.");
    sp0.run(RunMode::ForwardStepping, Time::new::<millisecond>(16.0));

    pp_neuron_t.lock().unwrap()
        .agent_by_id(1)
        .lock().unwrap()
        .show();
    
}
