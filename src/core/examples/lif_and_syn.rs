use std::sync::{Arc};
use wheatnnleek::{
    Time, m_S,
    Resistance, M_Ohm,
    Current, n_A,
    Voltage, m_V,
    Ratio, ratio,
    supervisor::Supervisor,
    populations::{SimpleFiringPopulation, SimplePassivePopulation, HoldAgents},
    agents::neurons::{LIF, ParamsLIF},
    
    operation::RunMode,
};

fn main() {
    let mut sp0 = Supervisor::new(Time::new::<m_S>(1.0));

    // build population for LIF.
    let name_pp_lif = String::from("LIF Population");
    let pp_lif = SimpleFiringPopulation::<LIF>::new();
    sp0.add_firing(
        name_pp_lif.clone(),
        Arc::downgrade(&pp_lif)
    );

    // build automatic-firing LIF.
    let lif_auto = ParamsLIF {
        v_rest: Voltage::new::<m_V>(0.),
        v_reset: Voltage::new::<m_V>(13.5),
        r_m: Resistance::new::<M_Ohm>(1.),
        tau_m: Time::new::<m_S>(30.),
        tau_refrac: Time::new::<m_S>(3.),
        v: Voltage::new::<m_V>(0.),
        v_th: Voltage::new::<m_V>(15.),
        i_e: Current::new::<n_A>(20.),
        gen_dirac_v: Voltage::new::<m_V>(1.4),
    };

    pp_lif.lock().unwrap().add(lif_auto.build());

    // buil non-automatic-firing LIF.
    let lif_non_auto = ParamsLIF {
        v_rest: Voltage::new::<m_V>(0.),
        v_reset: Voltage::new::<m_V>(13.5),
        r_m: Resistance::new::<M_Ohm>(1.),
        tau_m: Time::new::<m_S>(30.),
        tau_refrac: Time::new::<m_S>(3.),
        v: Voltage::new::<m_V>(0.),
        v_th: Voltage::new::<m_V>(15.),
        i_e: Current::new::<n_A>(0.),
        gen_dirac_v: Voltage::new::<m_V>(1.4),
    };

    pp_lif.lock().unwrap().add(lif_non_auto.build());

    // build Synapse-Dirac-Delta-V
    let name_pp_syn = String::from("SynapseDiracV Population");
    let pp_syn = SimplePassivePopulation::new();
    sp0.add_passive(
        name_pp_syn.clone(),
        Arc::downgrade(&pp_syn) // should try to avoid Arc::clone.
    );

    // build Synapse-Dirac-Delta-V
    
    
    println!("start run.");
    sp0.run(RunMode::ForwardStepping, Time::new::<m_S>(100.0));
}
