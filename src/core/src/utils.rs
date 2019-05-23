use std::thread;
use std::time::Duration;
use rand::Rng;

fn random_sleep() {
    thread::sleep(Duration::from_millis(rand::thread_rng().gen_range(1, 101)));
}
