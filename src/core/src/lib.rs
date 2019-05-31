use std::sync::{Arc, Weak, Mutex};

// #[macro_use]
extern crate crossbeam_channel;

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
