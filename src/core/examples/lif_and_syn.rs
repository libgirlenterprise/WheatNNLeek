use std::sync::{Arc};
use wheatnnleek::{
    Time, millisecond,
};
use wheatnnleek::supervisor::Supervisor;
use wheatnnleek::populations::{
    SimpleFiringPopulation,
    // SimplePassivePopulation, HoldAgents
};
use wheatnnleek::agents::neurons::{LIF};

fn main() {
    let mut sp0 = Supervisor::new(Time::new::<millisecond>(1.0));

    let name_pp_lif = String::from("LIF Population");
    let pp_lif = SimpleFiringPopulation::<LIF>::new();
    sp0.add_firing(
        name_pp_lif.clone(),
        Arc::downgrade(&pp_lif)
    );
}
