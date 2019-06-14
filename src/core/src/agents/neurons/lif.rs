// Copyright (c) 2019 Libgirl
//
// Released under Apache 2.0 license as described in the file LICENSE.txt.

// Integrate-and-fire model

use uom::si::f64::Time;
use uom::si::f64::ElectricPotential as Voltage;
use uom::si::time::millisecond;
use uom::si::electric_potential::milivolt;


pub struct NeuronLIF {
    pub v_rest: Voltage,   // Membrane resting potential
    pub r_m: Double,   // Membrane resistance
    pub tau_m: Time, // Membrane time constant
    pub v: Voltage,     // Membrane Voltage
    pub v_th: Voltage,  // Spike thresold
    i_e: Double,
    spikes: Double,
}
