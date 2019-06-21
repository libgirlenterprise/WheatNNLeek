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

pub use uom::si::{
    f64::{
        Time,
        ElectricalResistance as Resistance,
        ElectricCurrent as Current,
        ElectricPotential as Voltage,
        Ratio,
    },
    time::millisecond as m_S,
    electrical_resistance::megaohm as M_Ohm,
    electric_current::nanoampere as n_A,
    electric_potential::millivolt as m_V,
    ratio::ratio,
};
