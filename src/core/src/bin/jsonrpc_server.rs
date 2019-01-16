// Copyright (c) 2019 Libgirl
//
// Released under Apache 2.0 license as described in the file LICENSE.txt.

#[macro_use]
extern crate serde_json;

extern crate jsonrpc_http_server;
extern crate wheatnnleek;

use std::str::FromStr;
use std::sync::{Arc, Mutex};

use jsonrpc_http_server::jsonrpc_core::*;
use jsonrpc_http_server::{cors, DomainsValidation, ServerBuilder};

use wheatnnleek::connections::{static_connection, stdp_connection, PostSynapticEffect};
use wheatnnleek::connectors::{all_to_all, all_to_all_except_diagonal, linear};
use wheatnnleek::models::NeuronType;
use wheatnnleek::network::Network;
use wheatnnleek::{Num, Parameters, Time};

fn main() {
    let network = Arc::new(Mutex::new(Network::new()));

    let mut io = IoHandler::default();
    io.add_method("say_hello", |_params| {
        println!("say_hello called");
        futures::finished(Value::String("hello".to_owned()))
    });

    io.add_method("sum", |params: Params| {
        println!("sum called");
        println!("{:?}", params);

        let args: [i32; 2] = params.parse().unwrap();
        println!("{:?}", args);

        futures::finished(Value::String(format!("{}", args[0] + args[1]).to_owned()))
    });

    {
        let network = network.clone();
        io.add_method("Network::create", move |params: Params| {
            println!("Network::create");
            let args: [std::string::String; 3] = params.parse().unwrap();
            println!("{:?}", args);

            let neuron_number: usize = args[0].parse().unwrap();
            let neuron_type: NeuronType = args[1].parse().unwrap();
            let rests_str: String = args[2].parse().unwrap();
            let rests: serde_json::Map<String, Value> = match rests_str.as_ref() {
                "[]" => serde_json::Map::default(),
                _ => serde_json::from_str(&rests_str).unwrap(),
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

            futures::finished(json!({ "population": population }).to_owned())
        });
    }

    {
        let network = network.clone();
        io.add_method("Network::connect", move |params: Params| {
            println!("Network::connect");
            let ids: [usize; 2] = params.parse().unwrap();
            println!("{:?}", ids);

            let mut network = network.lock().unwrap();
            let population1 = (*network).get_population_by_id(ids[0]);
            let population2 = (*network).get_population_by_id(ids[1]);

            (*network).connect(
                &population1,
                &population2,
                &all_to_all::Connector::default(),
                &static_connection::Connection::default(),
            );

            futures::finished(Value::String("OK".to_owned()))
        });
    }

    {
        let network = network.clone();
        io.add_method("static-connect", move |params: Params| {
            println!("static-connect");
            let args: [String; 5] = params.parse().unwrap();
            let id0: usize = args[0].parse().unwrap();
            let id1: usize = args[1].parse().unwrap();
            let connection_delay: f64 = args[2].parse().unwrap();
            let post_syn_effect: PostSynapticEffect = match args[4].as_ref() {
                "Inhibitory" => PostSynapticEffect::Inhibitory,
                _ => PostSynapticEffect::Excitatory,
            };

            println!("{:?}", args);

            let mut network = network.lock().unwrap();
            let population1 = (*network).get_population_by_id(id0);
            let population2 = (*network).get_population_by_id(id1);

            let mut params = Parameters::new();
            params.insert("weight".to_string(), 1.);
            params.insert("delay".to_string(), connection_delay);

            match args[3].as_ref() {
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
            };

            futures::finished(Value::String("OK".to_owned()))
        });
    }

    {
        let network = network.clone();
        io.add_method("stdp-connect", move |params: Params| {
            println!("stdp-connect");
            let args: [String; 3] = params.parse().unwrap();
            let id0: usize = args[0].parse().unwrap();
            let id1: usize = args[1].parse().unwrap();
            let connection_delay: f64 = args[2].parse().unwrap();

            println!("{:?}", args);

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

            futures::finished(Value::String("OK".to_owned()))
        });
    }

    {
        let network = network.clone();
        io.add_method("Network::run", move |params: Params| {
            println!("Network::run");
            let args: [std::string::String; 1] = params.parse().unwrap();
            println!("{:?}", args);

            let t: Time = args[0].parse().unwrap();
            let mut network = network.lock().unwrap();

            (*network).run(t);

            futures::finished(Value::String("Finished".to_owned()))
        });
    }

    {
        let network = network.clone();
        io.add_method("get-population-by-id", move |params: Params| {
            println!("get-population-by-id");

            let args: [usize; 1] = params.parse().unwrap();
            let population_id: usize = args[0];
            let network = network.lock().unwrap();
            let population = (*network).get_population_by_id(population_id);

            futures::finished(json!({ "population": *population }).to_owned())
        });
    }

    {
        let network = network.clone();
        io.add_method("set-static-poisson-freq", move |params: Params| {
            println!("set-static-poisson-freq");

            let args: [String; 2] = params.parse().unwrap();
            let neuron_id: Num = args[0].parse().unwrap();
            let freq: f64 = args[1].parse().unwrap();
            let mut params = Parameters::new();

            params.insert("freq".to_string(), freq);

            let mut network = network.lock().unwrap();
            (*network).set_neuron_params(neuron_id, &params);

            futures::finished(Value::String("OK".to_owned()))
        });
    }

    let server = ServerBuilder::new(io)
        .cors(DomainsValidation::AllowOnly(vec![
            cors::AccessControlAllowOrigin::Null,
        ]))
        .start_http(&"127.0.0.1:3030".parse().unwrap())
        .expect("Unable to start RPC server");

    println!("Server running...");
    server.wait();
}
