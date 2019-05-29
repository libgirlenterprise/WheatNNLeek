use uom::si::f64::Time;
use uom::si::time::millisecond;
use wheatnnleek::supervisor::Supervisor;
use wheatnnleek::populations::{SimpleFiringPopulation, SimplePassivePopulation};
use wheatnnleek::devices::neurons::{TesterNeuronT};

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

    
}
