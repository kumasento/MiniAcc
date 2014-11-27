#!/bin/bash

# -- compile new Main.hs
echo "-- Compiling Main.hs ..."
ghc Main.hs

echo "-- Running Main.hs ... "
./Main > ExampleSolution.h

echo "-- Building New OpenCL Code ..."
make Example1

echo "-- Running OpenCL ..."
./Example1.o
