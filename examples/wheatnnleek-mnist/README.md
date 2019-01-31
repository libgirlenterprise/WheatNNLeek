# wheatnnleek-mnist - handwritten digit recognizer example
## Introduction
wheatnnleek-mnist is an example project about how to use WheatNNLeek spiking neural network library.

This project is a reproduction of [Diehl PU and Cook M 2015](https://www.frontiersin.org/articles/10.3389/fncom.2015.00099/full).

## Prerequisite
Install [Roswell](https://github.com/roswell/roswell) first.
You can check the [installation guide in Roswell project](https://github.com/roswell/roswell/wiki/Installation)

Then install Common Lisp mnist-database library by roswell

```shell
$ ros install markcox80/mnist-database
```

## Installation
### From source code
Put project folder or make a symbolic link into ```~/.roswell/local-projects/```

## Usage

Before training and validating, use script to download mnist data from the internet:

```shell
$ cd examples/wheatnnleek-mnist
$ scripts/download_mnist.sh
```

### Training using 100 image data
```shell
wheatnnleek-mnist/scripts $ ros train100.ros [weight-save-filepath] [theta-save-filepath]
```

### labeling neuron classes after training using 100 image data

```shell
wheatnnleek-mnist/scripts $ ros label-neurons-100.ros [weight-save-filepath] [theta-save-filepath] [neuron-label-save-filepath]
```

### Validating

```shell
wheatnnleek-mnist/scripts $ ros validate.ros [weight-save-filepath] [theta-save-filepath] [neuron-label-save-filepath]
```

### Notice
100 training data is too small...... we use at least 400 training samples with 400 neuron numbers to perform better than random guess. 

Please help contribute to modify the training, labeling, and validation scripts. 
Besides, please help contribute your CPU time and find potential bugs......
