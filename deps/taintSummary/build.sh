#!/bin/bash
mkdir build && cd "$_"
CC=gcc CXX=g++ cmake .. -DCMAKE_PREFIX_PATH="$PWD/../../../llvm/bins-9.0" && make
