// Copyright (c) 2019 Libgirl
//
// Released under Apache 2.0 license as described in the file LICENSE.txt.
extern crate serde_json;

use self::serde_json::Value;
use lazy_static::lazy_static;
use models::NeuronType;
use network::Network;
use std::ffi::{CStr, CString};
use std::os::raw::c_char;
use std::str::FromStr;
use std::sync::{Arc, Mutex};
use {Num, Parameters, Time};

use connections::{static_connection, stdp_connection, PostSynapticEffect};
use connectors::{all_to_all, all_to_all_except_diagonal, linear};

lazy_static! {
    static ref NETWORK: Arc<Mutex<Network>> = Arc::new(Mutex::new(Network::new()));
}

#[no_mangle]
pub extern "C" fn hello_world() -> *mut c_char {
    let ret = CString::new("Hello World").unwrap();
    ret.into_raw()
}

#[no_mangle]
pub extern "C" fn json_string_free(s: *mut c_char) {
    unsafe {
        if s.is_null() {
            return;
        }
        CString::from_raw(s)
    };
}

#[no_mangle]
pub extern "C" fn sum(a: i32, b: i32) -> i32 {
    a + b
}

#[no_mangle]
pub extern "C" fn Network_clear(
) {
    let network = NETWORK.clone();
    let mut network = network.lock().unwrap();
    (*network).clear();
}

#[no_mangle]
pub extern "C" fn Network_create(
    neuron_number: usize,
    neuron_type_buf: *const c_char,
    rests_buf: *const c_char,
) -> *mut c_char {
    let network = NETWORK.clone();
    let neuron_type_str: &CStr = unsafe { CStr::from_ptr(neuron_type_buf) };
    let neuron_type: NeuronType = neuron_type_str
        .to_str()
        .unwrap()
        .to_owned()
        .parse()
        .unwrap();
    let rests_str: &CStr = unsafe { CStr::from_ptr(rests_buf) };
    let rests_: String = rests_str.to_str().unwrap().to_owned().parse().unwrap();
    let rests: serde_json::Map<String, Value> = match rests_.as_ref() {
        "[]" => serde_json::Map::default(),
        _ => serde_json::from_str(&rests_).unwrap(),
    };

    let mut network_params = Parameters::new();

    for entry in rests.iter() {
        let key = entry.0;
        let value: std::string::String = entry.1.to_string();
        let value = f64::from_str(&value).unwrap();
        network_params.insert(key.to_string(), value);
    }

    let mut network = network.lock().unwrap();
    let population = (*network)
        .create(neuron_number, neuron_type, &network_params)
        .unwrap();
    let ret = CString::new(serde_json::to_string(&population).unwrap()).unwrap();
    ret.into_raw()
}

#[no_mangle]
pub extern "C" fn Network_connect(id0: usize, id1: usize) -> *mut c_char {
    let network = NETWORK.clone();

    let mut network = network.lock().unwrap();
    let population1 = (*network).get_population_by_id(id0);
    let population2 = (*network).get_population_by_id(id1);
    let result = (*network).connect(
        &population1,
        &population2,
        &all_to_all::Connector::default(),
        &static_connection::Connection::default(),
    );
    let ret = CString::new(serde_json::to_string(&result).unwrap()).unwrap();
    ret.into_raw()
}

#[no_mangle]
pub extern "C" fn static_connect(
    id0: usize,
    id1: usize,
    connection_delay: f64,
    connector_buf: *const c_char,
    post_syn_effect_buf: *const c_char,
) -> bool {

    let network = NETWORK.clone();
    let post_syn_effect_str: &CStr = unsafe { CStr::from_ptr(post_syn_effect_buf) };
    let post_syn_effect_: String = post_syn_effect_str
        .to_str()
        .unwrap()
        .to_owned()
        .parse()
        .unwrap();

    let post_syn_effect: PostSynapticEffect = match post_syn_effect_.as_ref() {
        "Inhibitory" => PostSynapticEffect::Inhibitory,
        _ => PostSynapticEffect::Excitatory,
    };

    let mut network = network.lock().unwrap();
    let population1 = (*network).get_population_by_id(id0);
    let population2 = (*network).get_population_by_id(id1);

    let mut params = Parameters::new();
    params.insert("weight".to_string(), 1.);
    params.insert("delay".to_string(), connection_delay);

    let connector_str: &CStr = unsafe { CStr::from_ptr(connector_buf) };
    let connector_: String = connector_str.to_str().unwrap().to_owned().parse().unwrap();

    match connector_.as_ref() {
        "linear" => {
            (*network).connect(
                &population1,
                &population2,
                &linear::Connector::default(),
                &static_connection::Connection::new(&params, post_syn_effect),
            );
        }
        "all_to_all_except_diagonal" => {
            (*network).connect(
                &population1,
                &population2,
                &all_to_all_except_diagonal::Connector::default(),
                &static_connection::Connection::new(&params, post_syn_effect),
            );
        }
        _ => {
            (*network).connect(
                &population1,
                &population2,
                &all_to_all::Connector::default(),
                &static_connection::Connection::new(&params, post_syn_effect),
            );
        }
    }
    true
}

#[no_mangle]
pub extern "C" fn stdp_connect(id0: usize, id1: usize, connection_delay: f64) -> bool {
    let network = NETWORK.clone();
    let mut network = network.lock().unwrap();
    let population1 = (*network).get_population_by_id(id0);
    let population2 = (*network).get_population_by_id(id1);
    let mut params = Parameters::new();
    params.insert("weight".to_string(), -1.);
    params.insert("delay".to_string(), connection_delay);
    (*network).connect(
        &population1,
        &population2,
        &all_to_all::Connector::default(),
        &stdp_connection::Connection::new(&params, PostSynapticEffect::Excitatory),
    );
    true
}

#[no_mangle]
pub extern "C" fn record_spikes(population_id: usize) -> bool {
    let network = NETWORK.clone();
    let mut network = network.lock().unwrap();
    (*network).record_spikes(population_id).unwrap();
    true
}

#[no_mangle]
pub extern "C" fn Network_run(t: Time) -> bool {
    let network = NETWORK.clone();
    let mut network = network.lock().unwrap();
    (*network).run(t);
    true
}

#[no_mangle]
pub extern "C" fn get_population_by_id(population_id: usize) -> *mut c_char {
    let network = NETWORK.clone();
    let network = network.lock().unwrap();
    let population = (*network).get_population_by_id(population_id);
    let ret = CString::new(serde_json::to_string(&population).unwrap()).unwrap();
    ret.into_raw()
}

#[no_mangle]
pub extern "C" fn set_static_poisson_freq(neuron_id: Num, freq: f64) -> bool {
    let network = NETWORK.clone();
    let mut params = Parameters::new();
    params.insert("freq".to_string(), freq);
    let mut network = network.lock().unwrap();
    (*network).set_neuron_params(neuron_id, &params);
    true
}
