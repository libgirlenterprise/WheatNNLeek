# WheatNNLeek

[![Build Status](https://travis-ci.org/libgirlenterprise/WheatNNLeek.svg?branch=master)](https://travis-ci.org/libgirlenterprise/WheatNNLeek)
[![License](https://img.shields.io/badge/License-Apache%202.0-green.svg)](https://www.apache.org/licenses/LICENSE-2.0)

WheatNNLeek is a spiking neural network system.

## Getting started

### Building

WheatNNLeek is mainly developed using Rust.

Before running examples, you should install Rust toolchain first. Please see [instructions](https://www.rust-lang.org/tools/install) to install Rust.

After Rust is installed succesfully, you can build WheatNNLeek using ```make``` or ```make release```.

### Writing your own model and connection rule

Please see [izhikevich model](src/core/src/models/izhikevich.rs) and [static connection](src/core/src/connections/static_connection.rs) for details.

## Contributing

Your contributions are always welcome.

## License

Licensed under Apache License 2.0.

See [LICENSE.txt](LICENSE.txt) for details.
