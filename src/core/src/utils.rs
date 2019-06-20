// use crate::{Time, Voltage};
// use uom::si::{Quantity, ISQ, Dimension, Units};
// use uom::num::Num;
// use uom::Conversion;
// use core::ops::Div as OpsDiv;
use crate::{Ratio, ratio, Voltage, Time};
use uom::si::{Quantity, ISQ, SI};
use typenum::{P2, P1, N4, N1, Z0};
use uom::Kind;
// use std::thread;
// use std::time::Duration;
// use rand::Rng;

// pub fn random_sleep() {
//     thread::sleep(Duration::from_millis(rand::thread_rng().gen_range(1, 101)));
// }

// pub fn ref_rk4<F: Fn(f64) -> f64>(f: F, y: f64, dt: f64) -> f64 {
//     let k1 = dt * f(y);
//     let k2 = dt * f(y + k1 * 0.5);
//     let k3 = dt * f(y + k2 * 0.5);
//     let k4 = dt * f(y + k3);
//     (k1 + 2.0 * k2 + 2.0 * k3 + k4) / 6.0
// }

// dimensionless value
fn dmsls(v: f64) -> Ratio {
    Ratio::new::<ratio>(v)
}

type VoltageRate = Quantity<ISQ<P2, P1, N4, N1, Z0, Z0, Z0, dyn Kind>,
                            SI<f64>,
                            f64>;

use typenum::operator_aliases::Diff;
use typenum::operator_aliases::Sum;
use uom::si::Dimension;
use uom::marker::{Div, Mul, Sub, Add};
use uom::si::Units;
// use num_traits::Num;
// use uom::Conversion;
// use core::ops::Div as OpsDiv;
// use core::ops::Mul as OpsMul;
use core::ops::Sub as OpsSub;
use core::ops::Add as OpsAdd;
use typenum::marker_traits::Integer;

pub fn rk4<F: Fn(Voltage) -> VoltageRate>(f: F, y: Voltage, dt: Time) -> Voltage
{
    let k1 = dt * f(y);
    let k2 = dt * f(y + k1 * dmsls(0.5));
    let k3 = dt * f(y + k2 * dmsls(0.5));
    let k4 = dt * f(y + k3);
    (k1 + dmsls(2.0) * k2 + dmsls(2.0) * k3 + k4) / dmsls(6.0)
}




pub fn rk4_general<F, Dt: ?Sized, Dr: ?Sized, Ut: ?Sized, Ur: ?Sized>(
    f: F,
    y: Quantity<ISQ<Sum<Dt::L, Dr::L>,
                    Sum<Dt::M, Dr::M>,
                    Sum<Dt::T, Dr::T>,
                    Sum<Dt::I, Dr::I>,
                    Sum<Dt::Th, Dr::Th>,
                    Sum<Dt::N, Dr::N>,
                    Sum<Dt::J, Dr::J>>,
                Ut,
                f64>,
    dt: Quantity<Dt, Ut, f64>
) -> Quantity<ISQ<Sum<Dt::L, Dr::L>,
                  Sum<Dt::M, Dr::M>,
                  Sum<Dt::T, Dr::T>,
                  Sum<Dt::I, Dr::I>,
                  Sum<Dt::Th, Dr::Th>,
                  Sum<Dt::N, Dr::N>,
                  Sum<Dt::J, Dr::J>>,
              Ut,
              f64>
where F: Fn(
    Quantity<ISQ<Sum<Dt::L, Dr::L>,
                 Sum<Dt::M, Dr::M>,
                 Sum<Dt::T, Dr::T>,
                 Sum<Dt::I, Dr::I>,
                 Sum<Dt::Th, Dr::Th>,
                 Sum<Dt::N, Dr::N>,
                 Sum<Dt::J, Dr::J>>,
             Ut,
             f64>
) -> Quantity<Dr, Ur, f64>,
      Dt: Dimension,
      Dt::L: OpsAdd<Dr::L>,
      Dt::M: OpsAdd<Dr::M>,
      Dt::T: OpsAdd<Dr::T>,
      Dt::I: OpsAdd<Dr::I>,
      Dt::Th: OpsAdd<Dr::Th>,
      Dt::N: OpsAdd<Dr::N>,
      Dt::J: OpsAdd<Dr::J>,
      Dt::Kind: Mul + Sub + Add + Div,
      Dr: Dimension,
      Dr::Kind: Mul + Sub + Add + Div,
      Ut: Units<f64>,
      Ur: Units<f64>,
<Dt::L as OpsAdd<Dr::L>>::Output: Integer,
<Dt::M as OpsAdd<Dr::M>>::Output: Integer,
<Dt::T as OpsAdd<Dr::T>>::Output: Integer,
<Dt::I as OpsAdd<Dr::I>>::Output: Integer,
<Dt::Th as OpsAdd<Dr::Th>>::Output: Integer,
<Dt::N as OpsAdd<Dr::N>>::Output: Integer,
<Dt::J as OpsAdd<Dr::J>>::Output: Integer,
    
// pub fn rk4_general<F, Dy: ?Sized, Dt: ?Sized, Uy: ?Sized, Ut: ?Sized>
//     (f: F, y: Quantity<Dy, Uy, f64>, dt: Quantity<Dt, Ut, f64>)
//      ->  Quantity<(dyn Dimension<L = <Dy::L as OpsSub<Z0>>::Output, Kind = (dyn uom::Kind + 'static),
//                                  N = <Dy::N as OpsSub<Z0>>::Output,
//                                  Th = <Dy::Th as OpsSub<Z0>>::Output,
//                                  J = <Dy::J as OpsSub<Z0>>::Output,
//                                  T = <Dy::T as OpsSub<Z0>>::Output,
//                                  M = <Dy::M as OpsSub<Z0>>::Output,
//                                  I = <Dy::I as OpsSub<Z0>>::Output> + 'static),
//                   Uy,
//                   f64>
// where F: Fn(Quantity<Dy, Uy, f64>) -> Quantity<ISQ<Diff<Dy::L, Dt::L>, Diff<Dy::M, Dt::M>, Diff<Dy::T, Dt::T>, Diff<Dy::I, Dt::I>, Diff<Dy::Th, Dt::Th>, Diff<Dy::N, Dt::N>, Diff<Dy::J, Dt::J>>, Uy, f64>,
// // where F: Fn(MyQ<Ly, My, Ty, Iy, Thy, Ny, Jy>) -> MyQ<Diff<Lt, Ly>, Diff<Mt, My>, Diff<Tt, Ty>, Diff<It, Iy>, Diff<Tht, Thy>, Diff<Nt, Ny>, Diff<Jt, Jy>>,
//       Dy: Dimension,
//       Dy::Kind: Div + Mul + Sub + Add,
//       Dt: Dimension,
//       Dt::Kind: Div + Mul + Sub + Add,
//       Uy: Units<f64>,
//       Ut: Units<f64>,
// // V: Num + Conversion<V> + OpsDiv<V> + OpsMul<V>, 
//       Dy::L: OpsSub<Dt::L> + OpsSub<Z0>,
//       Dy::M: OpsSub<Dt::M> + OpsSub<Z0>,
//       Dy::T: OpsSub<Dt::T> + OpsSub<Z0>,
//       Dy::I: OpsSub<Dt::I> + OpsSub<Z0>,
//       Dy::Th: OpsSub<Dt::Th> + OpsSub<Z0>,
//       Dy::N: OpsSub<Dt::N> + OpsSub<Z0>,
//       Dy::J: OpsSub<Dt::J> + OpsSub<Z0>,
{
    let k1= dt * f(y);
    let x: () = k1;
    let k2= dt * f(y + k1 * dmsls(0.5));
  //  let y: () = k2;
    let k3= dt * f(y + k2 * dmsls(0.5));
//    let x3: () = k3;
    let k4= dt * f(y + k3);
    (k1 + dmsls(2.0) * k2 + dmsls(2.0) * k3 + k4) / dmsls(6.0)
        // dt * f(y)
}




// Quantity<(dyn Dimension<L = <<Dy as Dimension>::L as std::ops::Sub<Z0>>::Output, Kind = (dyn uom::Kind + 'static),
//                         N = <<Dy as Dimension>::N as std::ops::Sub<Z0>>::Output,
//                         Th = <<Dy as Dimension>::Th as std::ops::Sub<Z0>>::Output,
//                         J = <<Dy as Dimension>::J as std::ops::Sub<Z0>>::Output,
//                         T = <<Dy as Dimension>::T as std::ops::Sub<Z0>>::Output,
//                         M = <<Dy as Dimension>::M as std::ops::Sub<Z0>>::Output,
//                         I = <<Dy as Dimension>::I as std::ops::Sub<Z0>>::Output> + 'static),
//          _,
//          _>;


// type OutQ<Dy, Uy> = Quantity<(dyn Dimension<L = <Dy::L as OpsSub<Z0>>::Output, Kind = (dyn uom::Kind + 'static),
//                                             N = <Dy::N as OpsSub<Z0>>::Output,
//                                             Th = <Dy::Th as OpsSub<Z0>>::Output,
//                                             J = <Dy::J as OpsSub<Z0>>::Output,
//                                             T = <Dy::T as OpsSub<Z0>>::Output,
//                                             M = <Dy::M as OpsSub<Z0>>::Output,
//                                             I = <Dy::I as OpsSub<Z0>>::Output> + 'static),
//                              Uy,
//                              f64>;
