use std::sync::{Arc};
use wheatnnleek::{
    Time, m_S,
    Resistance, M_Ohm,
    Current, n_A,
    Voltage, m_V,
    Ratio, ratio,
    supervisor::{Supervisor, Simulate},
    populations::{SimpleFiringPopulation, SimplePassivePopulation, HoldAgents},
    agents::{
        neurons::{ParamsLIF},
        synapses::{SynapseFlag, ParamsSynapseDiracV},
    },
    operation::RunMode,
};

fn main() {
    let mut sp0 = Supervisor::new(Time::new::<m_S>(0.5));

    // build population for LIF.
    let name_pp_lif = String::from("LIF Population");
    let pp_lif = SimpleFiringPopulation::new();
    sp0.add_firing(
        name_pp_lif.clone(),
        Arc::downgrade(&pp_lif)
    );

    // build automatic-firing LIF.
    let params_lif_auto = ParamsLIF {
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

    pp_lif.lock().unwrap().add(params_lif_auto.build());

    // buil non-automatic-firing LIF.
    let params_lif_non_auto = ParamsLIF {
        v_rest: Voltage::new::<m_V>(14.5),
        v_reset: Voltage::new::<m_V>(14.5),
        r_m: Resistance::new::<M_Ohm>(1.),
        tau_m: Time::new::<m_S>(30.),
        tau_refrac: Time::new::<m_S>(0.),
        v: Voltage::new::<m_V>(14.5),
        v_th: Voltage::new::<m_V>(15.),
        i_e: Current::new::<n_A>(0.),
        gen_dirac_v: Voltage::new::<m_V>(1.4),
    };

    pp_lif.lock().unwrap().add(params_lif_non_auto.build());

    // build Synapse-Dirac-Delta-V
    let name_pp_syn = String::from("SynapseDiracV Population");
    let pp_syn = SimplePassivePopulation::new();
    sp0.add_passive(
        name_pp_syn.clone(),
        Arc::downgrade(&pp_syn)
    );

    // build Synapse-Dirac-Delta-V
    let params_syn = ParamsSynapseDiracV {
        w: Ratio::new::<ratio>(0.5),
        w_max: Ratio::new::<ratio>(1.),
        w_min: Ratio::new::<ratio>(0.),
        delay: Time::new::<m_S>(1.),
        stdp_pre_amount: Ratio::new::<ratio>(-0.005), // be careful for +/- !!
        tau_stdp_pre: Time::new::<m_S>(20.),
        stdp_post_amount: Ratio::new::<ratio>(0.005),
        tau_stdp_post: Time::new::<m_S>(15.),
        synapse_flag: SynapseFlag::STDP, // decide use STDP or Static.
    };

    // n1 -> n2
    let n1 = pp_lif.lock().unwrap().agent_by_id(0);
    let n2 = pp_lif.lock().unwrap().agent_by_id(1);
    pp_syn.lock().unwrap().add(params_syn.build_to_active(n1, n2));

    // n2 -> n1
    let n1 = pp_lif.lock().unwrap().agent_by_id(0);
    let n2 = pp_lif.lock().unwrap().agent_by_id(1);
    pp_syn.lock().unwrap().add(params_syn.build_to_active(n2, n1));
    
    pp_syn.lock().unwrap().agent_by_id(0).lock().unwrap().print_w();
    pp_syn.lock().unwrap().agent_by_id(1).lock().unwrap().print_w();
    
    println!("start run.");
    sp0.run(Simulate::Serial, RunMode::ForwardStepping, Time::new::<m_S>(1000.0));

    pp_syn.lock().unwrap().agent_by_id(0).lock().unwrap().print_w();
    pp_syn.lock().unwrap().agent_by_id(1).lock().unwrap().print_w();
}
