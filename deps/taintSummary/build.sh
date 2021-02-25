#!/bin/bash
mkdir build && cd "$_"
CC=gcc-5 CXX=g++-5 cmake .. -DCMAKE_EXPORT_COMPILE_COMMANDS=true -DCMAKE_BUILD_TYPE=Release -DCMAKE_PREFIX_PATH="$PWD/../../../llvm/bins-9.0" && make
