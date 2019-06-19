use std::sync::{Arc, Weak, Mutex};

// #[macro_use]
// extern crate crossbeam_channel;

pub mod supervisor;
pub mod populations;
pub mod agents;
pub mod components;
pub mod operation;
pub mod connectivity;
pub mod signals;
mod utils;
// pub mod ffi;

type AcMx<T> = Arc<Mutex<T>>;
type WkMx<T> = Weak<Mutex<T>>;

#[macro_use]
extern crate uom;

use uom::si::f64::Time;
// use uom::si::time::millisecond;
use uom::si::f64::ElectricalResistance as Resistance;
// use uom::si::electrical_resistance::megaohm;
use uom::si::f64::ElectricCurrent as Current;
// use uom::si::electric_current::nanoampere;
use uom::si::f64::ElectricPotential as Voltage;
// use uom::si::electric_potential::millivolt;
use uom::si::f64::Ratio;
use uom::si::ratio::ratio;

// #[macro_use]
// mod length {
//     quantity! {
//         /// Length (base unit meter, m<sup>1</sup>).
//         quantity: Length; "length";
//         /// Length dimension, m<sup>1</sup>.
//         dimension: Q<P1 /*length*/, Z0 /*mass*/, Z0 /*time*/>;
//         units {
//             @meter: 1.0E0; "m", "meter", "meters";
//             @foot: 3.048E-1; "ft", "foot", "feet";
//         }
//     }
// }
// mod voltage_rate {
//     quantity! {
//         quantity: VoltageRate; "voltage_rate";
//         dimension: Q<P2, P1, N3, N1, Z0, Z0, Z0>;
//         // units {

//         // }
//     }
// }
