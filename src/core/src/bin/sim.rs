// Copyright (c) 2019 Libgirl
//
// Released under Apache 2.0 license as described in the file LICENSE.txt.

use wheatnnleek::Parameters;

use wheatnnleek::connections::static_connection;
use wheatnnleek::connectors::all_to_all;
use wheatnnleek::models::NeuronType;
use wheatnnleek::network::Network;

fn main() {
    let mut network = Network::new();

    let mut params: Parameters = Parameters::new();
    params.insert("v_m".to_string(), -65.);
    params.insert("i_e".to_string(), 376.);

    let n1 = network.create(2, NeuronType::Izhikevich, &params).unwrap();
    let n2 = network
        .create(2, NeuronType::ConductionBasedAdaptiveThresholdLIF, &params)
        .unwrap();

    n1.print_status();
    n2.print_status();

    let variables: Vec<String> = vec!["v_m".to_string(), "spike".to_string()];
    n1.record(variables).unwrap();

    let population_id = n2.get_id();
    network.record_spikes(population_id).unwrap();
    network.connect(
        &n1,
        &n2,
        &all_to_all::Connector::default(),
        &static_connection::Connection::default(),
    );
    network.run(100.);
    println!("{:?}", network.get_spike_records());
}
