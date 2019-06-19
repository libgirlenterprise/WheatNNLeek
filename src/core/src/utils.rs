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
    Quantity, ISQ, Units, SI,
};
use typenum::{P2, P1, N4, N1, Z0};
use uom::Kind;
// use std::thread;
// use std::time::Duration;
// use rand::Rng;

// pub fn random_sleep() {
//     thread::sleep(Duration::from_millis(rand::thread_rng().gen_range(1, 101)));
// }



pub fn rk4<F: Fn(Voltage) -> VoltageRate>(f: F, y: Voltage, dt: Time) -> Voltage
{
    let k1 = dt * f(y);
    let k2 = dt * f(y + k1 * dmsls(0.5));
    let k3 = dt * f(y + k2 * dmsls(0.5));
    let k4 = dt * f(y + k3);
    (k1 + dmsls(2.0) * k2 + dmsls(2.0) * k3 + k4) / dmsls(6.0)
}

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

// type VoltageRate = Quantity<ISQ<P2, P1, N4, N1, Z0, Z0, Z0, dyn Kind>,
//                             dyn Units<f64,
//                                       length = length::meter,
//                                       mass = mass::kilogram,
//                                       thermodynamic_temperature = thermodynamic_temperature::kelvin,
//                                       time = time::second,
//                                       amount_of_substance = amount_of_substance::mole,
//                                       luminous_intensity = luminous_intensity::candela,
//                                       electric_current = electric_current::ampere>,
//                             f64>;

// type VoltageRate = Quantity<(dyn Dimension<N = typenum::int::Z0,
//                                            L = typenum::int::PInt<typenum::uint::UInt<typenum::uint::UInt<typenum::uint::UTerm,
//                                                                                                           typenum::bit::B1>,
//                                                                                       typenum::bit::B0>>,
//                                            T = typenum::int::NInt<typenum::uint::UInt<typenum::uint::UInt<typenum::uint::UInt<typenum::uint::UTerm,
//                                                                                                                               typenum::bit::B1>,
//                                                                                                           typenum::bit::B0>,
//                                                                                       typenum::bit::B0>>,
//                                            Th = typenum::int::Z0,
//                                            I = typenum::int::NInt<typenum::uint::UInt<typenum::uint::UTerm,
//                                                                                       typenum::bit::B1>>,
//                                            M = typenum::int::PInt<typenum::uint::UInt<typenum::uint::UTerm,
//                                                                                       typenum::bit::B1>>,
//                                            J = typenum::int::Z0,
//                                            Kind = (dyn uom::Kind + 'static)> + 'static),
//                             dyn Units<f64,
//                                       length = length::meter,
//                                       mass = mass::kilogram,
//                                       thermodynamic_temperature = thermodynamic_temperature::kelvin,
//                                       time = time::second,
//                                       amount_of_substance = amount_of_substance::mole,
//                                       luminous_intensity = luminous_intensity::candela,
//                                       electric_current = electric_current::ampere>,
//                             f64>;
