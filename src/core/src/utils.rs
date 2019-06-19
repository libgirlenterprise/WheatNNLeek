// use crate::{Time, Voltage};
// use uom::si::{Quantity, ISQ, Dimension, Units};
// use uom::num::Num;
// use uom::Conversion;
// use core::ops::Div as OpsDiv;
// use core::ops::Sub as OpsSub;
// use uom::marker::{Mul, Div, Add};
// use typenum::operator_aliases::Diff;
use crate::{Ratio, ratio, Voltage, Time};
use uom::si::{
    length,
    mass,
    thermodynamic_temperature,
    time,
    amount_of_substance,
    luminous_intensity,
    electric_current,
    Quantity,
    ISQ, Units,
};
use typenum::{P2, P1, N4, N1, Z0};
use uom::Kind;
use typenum::operator_aliases::Diff;
use std::ops::Sub;
// use uom::typenum::Integer;
use typenum::marker_traits::Integer;
// use std::thread;
// use std::time::Duration;
// use rand::Rng;

// pub fn random_sleep() {
//     thread::sleep(Duration::from_millis(rand::thread_rng().gen_range(1, 101)));
// }


pub fn rk4<F: Fn(Voltage) -> VoltageRate>(f: F, y: Voltage, dt: Time) -> Voltage
{
    let k1: MyQ<Ly, My, Ty, Iy, Thy, Ny, Jy> = dt * f(y);
    let k2: MyQ<Ly, My, Ty, Iy, Thy, Ny, Jy> = dt * f(y + k1 * new_ratio(0.5));
    let k3: MyQ<Ly, My, Ty, Iy, Thy, Ny, Jy> = dt * f(y + k2 * new_ratio(0.5));
    let k4: MyQ<Ly, My, Ty, Iy, Thy, Ny, Jy> = dt * f(y + k3);
    (k1 + new_ratio(2.0) * k2 + new_ratio(2.0) * k3 + k4) / new_ratio(6.0)
}

pub fn ref_rk4<F: Fn(f64) -> f64>(f: F, y: f64, dt: f64) -> f64 {
    let k1 = dt * f(y);
    let k2 = dt * f(y + k1 * 0.5);
    let k3 = dt * f(y + k2 * 0.5);
    let k4 = dt * f(y + k3);
    (k1 + 2.0 * k2 + 2.0 * k3 + k4) / 6.0
}

fn new_ratio(v: f64) -> Ratio {
    Ratio::new::<ratio>(v)
}

type VoltageRate = Quantity<ISQ<P2, P1, N4, N1, Z0, Z0, Z0, dyn Kind>,
                            dyn Units<f64,
                                      length = length::meter,
                                      mass = mass::kilogram,
                                      thermodynamic_temperature = thermodynamic_temperature::kelvin,
                                      time = time::second,
                                      amount_of_substance = amount_of_substance::mole,
                                      luminous_intensity = luminous_intensity::candela,
                                      electric_current = electric_current::ampere>,
                            f64>;

type VoltageRate = Quantity<(dyn Dimension<N = typenum::int::Z0,
                                           L = typenum::int::PInt<typenum::uint::UInt<typenum::uint::UInt<typenum::uint::UTerm,
                                                                                                          typenum::bit::B1>,
                                                                                      typenum::bit::B0>>,
                                           T = typenum::int::NInt<typenum::uint::UInt<typenum::uint::UInt<typenum::uint::UInt<typenum::uint::UTerm,
                                                                                                                              typenum::bit::B1>,
                                                                                                          typenum::bit::B0>,
                                                                                      typenum::bit::B0>>,
                                           Th = typenum::int::Z0,
                                           I = typenum::int::NInt<typenum::uint::UInt<typenum::uint::UTerm,
                                                                                      typenum::bit::B1>>,
                                           M = typenum::int::PInt<typenum::uint::UInt<typenum::uint::UTerm,
                                                                                      typenum::bit::B1>>,
                                           J = typenum::int::Z0,
                                           Kind = (dyn uom::Kind + 'static)> + 'static),
                            dyn Units<f64,
                                      length = length::meter,
                                      mass = mass::kilogram,
                                      thermodynamic_temperature = thermodynamic_temperature::kelvin,
                                      time = time::second,
                                      amount_of_substance = amount_of_substance::mole,
                                      luminous_intensity = luminous_intensity::candela,
                                      electric_current = electric_current::ampere>,
                            f64>;

pub fn rk4
    <F,
     Ly, My, Ty, Iy, Thy, Ny, Jy,
     Lt, Mt, Tt, It, Tht, Nt, Jt,>
    (f: F, y: MyQ<Ly, My, Ty, Iy, Thy, Ny, Jy>, dt: MyQ<Lt, Mt, Tt, It, Tht, Nt, Jt>) -> MyQ<Ly, My, Ty, Iy, Thy, Ny, Jy>
// where F: Fn(MyQ<Ly, My, Ty, Iy, Thy, Ny, Jy>) -> MyQ<Diff<Ly, Lt>, Diff<My, Mt>, Diff<Ty, Tt>, Diff<Iy, It>, Diff<Thy, Tht>, Diff<Ny, Nt>, Diff<Jy, Jt>>,
where F: Fn(MyQ<Ly, My, Ty, Iy, Thy, Ny, Jy>) -> MyQ<Diff<Lt, Ly>, Diff<Mt, My>, Diff<Tt, Ty>, Diff<It, Iy>, Diff<Tht, Thy>, Diff<Nt, Ny>, Diff<Jt, Jy>>,
      // Ly: Sub<Lt> + Integer + Sub<Z0>,
      // My: Sub<Mt> + Integer + Sub<Z0>,
      // Ty: Sub<Tt> + Integer + Sub<Z0>,
      // Iy: Sub<It> + Integer + Sub<Z0>,
      // Thy: Sub<Tht> + Integer + Sub<Z0>,
      // Ny: Sub<Nt> + Integer + Sub<Z0>,
      // Jy: Sub<Jt> + Integer + Sub<Z0>,
      // Lt: Integer,
      // Mt: Integer,
      // Tt: Integer,
      // It: Integer,
      // Tht: Integer,
      // Nt: Integer,
      // Jt: Integer,
      Ly: Integer,
      My: Integer,
      Ty: Integer,
      Iy: Integer,
      Thy: Integer,
      Ny: Integer,
      Jy: Integer,
      Lt: Sub<Ly> + Integer + Sub<Z0>,
      Mt: Sub<My> + Integer + Sub<Z0>,
      Tt: Sub<Ty> + Integer + Sub<Z0>,
      It: Sub<Iy> + Integer + Sub<Z0>,
      Tht: Sub<Thy> + Integer + Sub<Z0>,
      Nt: Sub<Ny> + Integer + Sub<Z0>,
      Jt: Sub<Jy> + Integer + Sub<Z0>,
{
    let k1: MyQ<Ly, My, Ty, Iy, Thy, Ny, Jy> = dt * f(y);
    let k2: MyQ<Ly, My, Ty, Iy, Thy, Ny, Jy> = dt * f(y + k1 * new_ratio(0.5));
    let k3: MyQ<Ly, My, Ty, Iy, Thy, Ny, Jy> = dt * f(y + k2 * new_ratio(0.5));
    let k4: MyQ<Ly, My, Ty, Iy, Thy, Ny, Jy> = dt * f(y + k3);
    (k1 + new_ratio(2.0) * k2 + new_ratio(2.0) * k3 + k4) / new_ratio(6.0)
}

type MyQ<L, M, T, I, Th, N, J> = Quantity<ISQ<L, M, T, I, Th, N, J, dyn Kind>,
                            dyn Units<f64,
                                      length = length::meter,
                                      mass = mass::kilogram,
                                      thermodynamic_temperature = thermodynamic_temperature::kelvin,
                                      time = time::second,
                                      amount_of_substance = amount_of_substance::mole,
                                      luminous_intensity = luminous_intensity::candela,
                                      electric_current = electric_current::ampere>,
                            f64>;

