#!/bin/bash
mkdir build && cd "$_"
CC=gcc CXX=g++ cmake .. -DCMAKE_EXPORT_COMPILE_COMMANDS=true -DCMAKE_BUILD_TYPE=Release -DCMAKE_PREFIX_PATH="$PWD/../../../llvm/bins-9.0" && make
