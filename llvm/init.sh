#!/bin/bash


# download pre-built package 

PKGN="clang+llvm-9.0.0-x86_64-linux-gnu-ubuntu-16.04"
TARF="$PKGN.tar.xz"
LINK="https://releases.llvm.org/9.0.0/$TARF"
wget $LINK

tar -xvf $TARF
mv $PKGN bins-9.0

rm $TARF


NATIVE_LLVM_DIR="kubo-bins-9.0"

LLVM_SRC_PKGN="llvm-9.0.0.src"
CLANG_SRC_PKGN="cfe-9.0.0.src"
LLVM_TARF="$LLVM_SRC_PKGN.tar.xz"
CLANG_TARF="$CLANG_SRC_PKGN.tar.xz"
LLVM_URL="https://releases.llvm.org/9.0.0/$LLVM_TARF"
CLANG_URL="https://releases.llvm.org/9.0.0/$CLANG_TARF"
wget $LLVM_URL
tar -xvf $LLVM_TARF
mv $LLVM_SRC_PKGN $NATIVE_LLVM_DIR
rm $LLVM_TARF

wget $CLANG_URL
tar -xvf $CLANG_TARF
mv $CLANG_SRC_PKGN ./$NATIVE_LLVM_DIR/tools/clang

## patch
cp ../patches/CGExpr.cpp ./$NATIVE_LLVM_DIR/tools/clang/lib/CodeGen/
cp -r ../patches/catch__user ./$NATIVE_LLVM_DIR/tools/clang/tools/ 
echo "add_clang_subdirectory(catch__user)" >> ./$NATIVE_LLVM_DIR/tools/clang/tools/CMakeLists.txt

cd ./$NATIVE_LLVM_DIR/ 
mkdir build
cd build

cmake ..
make
