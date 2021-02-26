#!/bin/bash
mkdir build && cd "$_"
CC=gcc CXX=g++ cmake -DLLVM_PREBUILT_PATH="$PWD/../../../llvm/bins-9.0" .. && make
