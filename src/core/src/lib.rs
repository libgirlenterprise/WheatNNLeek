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

use uom::si::f64::Time;
// use uom::si::time::millisecond;
use uom::si::f64::ElectricalResistance as Resistance;
// use uom::si::electrical_resistance::megaohm;
use uom::si::f64::ElectricCurrent as Current;
// use uom::si::electric_current::nanoampere;
use uom::si::f64::ElectricPotential as Voltage;
// use uom::si::electric_potential::millivolt;
use uom::si::f64::Ratio;
