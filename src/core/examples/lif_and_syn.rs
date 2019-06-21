use std::sync::{Arc};
use wheatnnleek::{
    Time, millisecond,
    supervisor::Supervisor,
    populations::{
        SimpleFiringPopulation,
        // SimplePassivePopulation, HoldAgents
    },
    agents::neurons::{LIF, ParamsLIF},
};

fn main() {
    let mut sp0 = Supervisor::new(Time::new::<millisecond>(1.0));

    let name_pp_lif = String::from("LIF Population");
    let pp_lif = SimpleFiringPopulation::<LIF>::new();
    sp0.add_firing(
        name_pp_lif.clone(),
        Arc::downgrade(&pp_lif)
    );

    let lif_auto = ParamsLIF {
        v_rest: Voltage 0,   // Membrane resting potential
        r_m: Resistance 1,   // Membrane resistance
        tau_m: Time 30,       // Membrane time constant
        tau_refrac: Time 3,  // Refractory time
        v: Voltage 0,        // Membrane Voltage
        v_th: Voltage 15,     // Thresold Voltage of firing
        v_reset: 13.5
        i_e: Current 20,      // constant current injection
        gen_dirac_v: Voltage 1.4, // the generated signed DiracDelta Voltage signal.    
    }

    pp_lif.lock().unwrap().add();
}
