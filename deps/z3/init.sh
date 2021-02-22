#!/bin/bash

# paths
LLVM=/mnt/c/Users/lcm/Documents/llvm/bins-9.0/bin
echo $ROOT
ROOT=$(pwd)
SRCS=$ROOT/srcs
BINS=$ROOT/bins

# init paths
rm -rf $BINS
mkdir $BINS

# clone z3
git clone https://github.com/Z3Prover/z3.git $SRCS

# configure
cd $SRCS
CXX=$LLVM/clang++ CC=$LLVM/clang LDFLAGS="-static" python scripts/mk_make.py --prefix=$BINS --staticlib 

# build
cd build
make -j$(nproc)

# install
make install

