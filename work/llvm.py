#!/usr/bin/env python

from os import symlink
from os.path import exists
from shutil import rmtree
from argparse import ArgumentParser

from conf import *
from util import *

# functions
def build(clean = True, debug = None):
    psrc = PATH_PASS
    if not exists(psrc):
        LOG_ERR("Src path %s does not exist" % psrc)
        return

    pobj = PASS_BDIR
    if clean:
        mkdirs(pobj, force = True)
    else:
        if not exists(pobj):
            LOG_ERR("Obj path %s does not exist" % pobj)
            return

    with cd(pobj):
        if clean:
            if debug is not None:
                btype = "Profile"
                ditem = debug.upper()
            else:
                btype = "Release"
                ditem = "NONE"
            
            cmd = ("CC=gcc CXX=g++ cmake " + \
                    "-G 'Unix Makefiles' " + \
                    "-DCMAKE_BUILD_TYPE=%s " % btype + \
                    "-DCMAKE_PREFIX_PATH=%s " % LLVM_PRE_PREP + \
                    "-DDEPS_FMT_DIR=%s " % DEPS_FMT + \
                    "-DDEPS_Z3_DIR=%s " % DEPS_Z3 + \
                    "-DDEPS_UTIL_DIR=%s " % DEPS_UTIL + \
                    "-DKSYM_DEBUG_ITEM=%s " % ditem + \
                    psrc)
            if shell(cmd) == 0:
                LOG_INF("Config done")
            else:
                LOG_ERR("Config failed")
                return

        cmd = "make -j%d" % HALF_NCPU
        # cmd = "make -j%d" % 1 

        if shell(cmd) == 0:
            LOG_INF("Build done")
        else:
            LOG_ERR("Build failed")
            return


# main
if __name__ == "__main__":
    # init
    parser = ArgumentParser()
    subs = parser.add_subparsers(dest = "cmd")

    sub_build = subs.add_parser("build")
    sub_build.add_argument("-c", "--clean", action="store_true")
    sub_build.add_argument("-d", "--debug", type=str, default=None)
    
    # parse
    args = parser.parse_args()

    # exec
    if args.cmd == "build":
        build(args.clean, args.debug)
    else:
        parser.print_usage()
