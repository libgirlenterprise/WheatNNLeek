use std::sync::{Arc};
use wheatnnleek::{
    Time, m_S,
    Resistance, M_Ohm,
    Current, n_A,
    Voltage, m_V,
    Ratio, ratio,
    supervisor::Supervisor,
    populations::{
        SimpleFiringPopulation,
        // SimplePassivePopulation, HoldAgents
    },
    agents::neurons::{LIF, ParamsLIF},
};

fn main() {
    let mut sp0 = Supervisor::new(Time::new::<m_S>(1.0));

    let name_pp_lif = String::from("LIF Population");
    let pp_lif = SimpleFiringPopulation::<LIF>::new();
    sp0.add_firing(
        name_pp_lif.clone(),
        Arc::downgrade(&pp_lif)
    );

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

    pp_lif.lock().unwrap().add();
}
