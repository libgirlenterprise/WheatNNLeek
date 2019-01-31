#!/bin/bash

HOST="http://yann.lecun.com/exdb/mnist"

mkdir data
for file in train-images-idx3-ubyte train-labels-idx1-ubyte t10k-images-idx3-ubyte t10k-labels-idx1-ubyte
do
    name="${file}.gz"
    curl "$HOST/$name" | gunzip > "data/$file"
done
