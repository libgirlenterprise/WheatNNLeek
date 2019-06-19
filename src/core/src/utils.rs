// use crate::{Time, Voltage};
use uom::si::{Quantity, ISQ, Dimension, Units};
use uom::num::Num;
use uom::Conversion;
use core::ops::Div as OpsDiv;
use core::ops::Sub as OpsSub;
use uom::marker::{Mul, Div, Add};
use typenum::operator_aliases::Diff;
use crate::{Ratio};
// use std::thread;
// use std::time::Duration;
// use rand::Rng;

// pub fn random_sleep() {
//     thread::sleep(Duration::from_millis(rand::thread_rng().gen_range(1, 101)));
// }

// pub fn rk4(F: Fn(Voltage) -> Voltage / Time, y: Voltage, dt: Time) -> Voltage {
    
// }

pub fn rk4<Dl, Dr, Ul, Ur, V>(
    f: Fn(Quantity<Dl, Ul, V>) -> Quantity<ISQ<Diff<Dl::L, Dr::L>,
                                               Diff<Dl::M, Dr::M>,
                                               Diff<Dl::T, Dr::T>,
                                               Diff<Dl::I, Dr::I>,
                                               Diff<Dl::Th, Dr::Th>,
                                               Diff<Dl::N, Dr::N>,
                                               Diff<Dl::J, Dr::J>>,
                                           Ul,
                                           V>,
    y: Quantity<Dl, Ul, V>,
    dt: Quantity<Dr, Ur, V>,
) -> Quantity<Dl, Ul, V>
where Dl: Dimension + ?Sized,
      Dr: Dimension + ?Sized,
      Ul: Units<V> + ?Sized,
      Ur: Units<V> + ?Sized,
      V: Num + Conversion<V> + OpsDiv<V>,
      Dl::L: OpsSub<Dr::L>,
      Dl::M: OpsSub<Dr::M>,
      Dl::T: OpsSub<Dr::T>,
      Dl::I: OpsSub<Dr::I>,
      Dl::Th: OpsSub<Dr::Th>,
      Dl::N: OpsSub<Dr::N>,
      Dl::J: OpsSub<Dr::J>,
      Dl::Kind: Mul + Add + Div,
      Dr::Kind: Mul + Add + Div,
{
    let k1 = dt * f(y);
    let k2 = dt * f(y + k1 * Ratio::new::<ratio>(0.5));
    let k3 = dt * f(y + k2 * Ratio::new::<ratio>(0.5));
    let k4 = dt * f(y + k3);
    (k1 + 2.0 * k2 + 2.0 * k3 + k4) / Ratio::new::<ratio>(6.0)
}

// pub fn rk4<>(f: F, y: Quantity<D0, U, f64>, dt: Quantity<D2, U, f64>)

pub fn ref_rk4<F: Fn(f64) -> f64>(f: F, y: f64, dt: f64) -> f64 {
    let k1 = dt * f(y);
    let k2 = dt * f(y + k1 * 0.5);
    let k3 = dt * f(y + k2 * 0.5);
    let k4 = dt * f(y + k3);
    (k1 + 2.0 * k2 + 2.0 * k3 + k4) / 6.0
}
