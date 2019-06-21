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

pub fn rk4<F: Fn(Voltage) -> VoltageRate>(f: F, y: Voltage, dt: Time) -> Voltage
{
    let k1 = dt * f(y);
    let k2 = dt * f(y + k1 * dmsls(0.5));
    let k3 = dt * f(y + k2 * dmsls(0.5));
    let k4 = dt * f(y + k3);
    (k1 + dmsls(2.0) * k2 + dmsls(2.0) * k3 + k4) / dmsls(6.0)
}

pub trait Dimensionless {
    fn exp(self) -> Ratio;
}

impl Dimensionless for Ratio {
    fn exp(self) -> Ratio {
        Ratio::new::<ratio>(
            self.get::<ratio>().exp()            
        )

    }
}
