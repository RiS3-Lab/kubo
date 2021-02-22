#!/usr/bin/env python

from sys import exit
from os import listdir
from os.path import abspath, join
from platform import system
from math import ceil
from multiprocessing import cpu_count

# platforms
HOST = system()
assert HOST in ("Linux", "Darwin")

# paths
PATH_ROOT = abspath(join(__file__, "..", ".."))

LINUX_GIT_URL = "git://git.kernel.org/pub/scm/linux/kernel/git/stable/linux-stable.git"
PATH_DEPS = join(PATH_ROOT, "deps")

PATH_LLVM = join(PATH_ROOT, "llvm")
PATH_PASS = join(PATH_ROOT, "pass")
PATH_TEST = join(PATH_ROOT, "unit")

PATH_CODE = join(PATH_ROOT, "code")
PATH_LOGS = join(PATH_CODE, "logs")
PATH_OUTS = join(PATH_CODE, "outs")
PATH_SRCS = join(PATH_CODE, "srcs")
PATH_OBJS = join(PATH_CODE, "objs")
PATH_BINS = join(PATH_CODE, "bins")
PATH_EXTS = join(PATH_CODE, "exts")
PATH_BCFS = join(PATH_CODE, "bcfs")
PATH_MODS = join(PATH_CODE, "mods")
PATH_TRAS = join(PATH_CODE, "tras")
PATH_SYMS = join(PATH_CODE, "syms")

PATH_WORK = join(PATH_ROOT, "work")


# deps
DEPS_PEX = join(PATH_DEPS,"pex")
DEPS_FMT = join(PATH_DEPS,"fmt")
DEPS_Z3 = join(PATH_DEPS, "z3")
DEPS_UTIL = join(PATH_DEPS, "util")
DEPS_PRINTFUNC = join(PATH_DEPS, "print_func")
PATH_CRIX = join(PATH_DEPS,"crix")
PATH_ENTRY = join(PATH_DEPS,"entry_analysis")
PATH_CALLCHAIN = join(PATH_DEPS,"callchain")
PATH_TAINT = join(PATH_DEPS,"taintSummary")
#taint analysis
PATH_TAINT_SO = join(PATH_TAINT,"build","SoundyAliasAnalysis","libSoundyAliasAnalysis.so")
#entry analysis
PATH_ENTRY_EXE = join(PATH_ENTRY,"build","entry_analysis")
#entry callchain
PATH_CALLCHAIN_SO = join(PATH_CALLCHAIN,"build","SoundyAliasAnalysis","libSoundyAliasAnalysis.so")
#call graph generation tool
PATH_PEX_EXE = join(DEPS_PEX,"exe.sh")
PATH_PEX_SO = join(DEPS_PEX,"build","gatlin","libgatlin.so")
PATH_CRIX_EXE = join(PATH_CRIX,"analyzer","build","lib","kanalyzer")
# print_func
PATH_PRINTFUNC = join(DEPS_PRINTFUNC,"build", "print_func")
# pass
PASS_BDIR = join(PATH_PASS, "build")
PASS_KSYM = join(PASS_BDIR, "KSym", "KSym.so")

# llvm-9.0
LLVM_PREP = join(PATH_LLVM, "kubo-bins-9.0","build")
LLVM_PRE_PREP = join(PATH_LLVM,"bins-9.0")
#LLVM_PREP = join(PATH_LLVM,"bins-9.0")

LLVM_BINS = join(LLVM_PREP,"bin")
LLVM_BIN_CPP = join(LLVM_BINS, "clang-cpp")
LLVM_BIN_CLA = join(LLVM_BINS, "clang")
LLVM_BIN_CATCH_USER = join(LLVM_BINS, "catch__user")
LLVM_BIN_CXX = join(LLVM_BINS, "clang++")
LLVM_BIN_LLD = join(LLVM_BINS, "ld.lld")
LLVM_BIN_BLD = join(LLVM_BINS, "llvm-link")
LLVM_BIN_OPT = join(LLVM_BINS, "opt")
LLVM_BIN_DIS = join(LLVM_BINS, "llvm-dis")
LLVM_BIN_AS = join(LLVM_BINS, "llvm-as")

LLVM_PRE_BINS = join(LLVM_PRE_PREP,"bin")
LLVM_PRE_BIN_CPP = join(LLVM_PRE_BINS, "clang-cpp")
LLVM_PRE_BIN_CLA = join(LLVM_PRE_BINS, "clang")
LLVM_PRE_BIN_CXX = join(LLVM_PRE_BINS, "clang++")
LLVM_PRE_BIN_LLD = join(LLVM_PRE_BINS, "ld.lld")
LLVM_PRE_BIN_BLD = join(LLVM_PRE_BINS, "llvm-link")
LLVM_PRE_BIN_OPT = join(LLVM_PRE_BINS, "opt")
LLVM_PRE_BIN_DIS = join(LLVM_PRE_BINS, "llvm-dis")
LLVM_PRE_BIN_AS = join(LLVM_PRE_BINS, "llvm-as")

LLVM_SYMS = join(PATH_LLVM, "syms")

# opts
OPTS_NCPU = cpu_count()
HALF_NCPU = ceil(OPTS_NCPU/2) 
OPTS_TIME = 43200
