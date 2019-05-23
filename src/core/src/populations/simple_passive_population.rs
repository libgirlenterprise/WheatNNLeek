use std::sync::{Arc, Mutex};
use crate::operation::{RunMode, PassiveDevice, Configurable};
use crate::operation::op_population::PassivePopulation;
use crate::populations::HoldDevices;

pub struct SimplePassivePopulation<T>
where T: 'static + PassiveDevice + Send
{
    mode: RunMode,
    devices: Vec<Arc<Mutex<T>>>,
}

impl<T> Configurable for SimplePassivePopulation<T>
where T: 'static + PassiveDevice + Send
{
    fn config_mode(&mut self, mode: RunMode) {
        self.mode = mode;
        for device in &self.devices {
            device.lock().unwrap().config_mode(mode);
        }
    }

    fn config_channels(&mut self) {
        for device in &self.devices {
            device.lock().unwrap().config_channels();
        }
    }

    fn mode(&self) -> RunMode {
        self.mode
    }
}

impl<T> PassivePopulation for SimplePassivePopulation<T>
where T: 'static + PassiveDevice + Send
{}

impl<T> HoldDevices<T> for SimplePassivePopulation<T>
where T: 'static + PassiveDevice + Send
{
    fn device_by_id(&self, n: usize) -> Arc<Mutex<T>> {
        Arc::clone(&self.devices[n])
    }    
}

impl<T>  SimplePassivePopulation<T>
where T: 'static + PassiveDevice + Send
{
    pub fn new() -> Arc<Mutex<SimplePassivePopulation<T>>> {
        Arc::new(Mutex::new(SimplePassivePopulation{
            mode: RunMode::Idle,
            devices: Vec::new(),
        }))
    }

    pub fn add(&mut self, device: Arc<Mutex<T>>) {
        self.devices.push(device);
    }
}
