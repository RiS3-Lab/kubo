#!/usr/bin/env python

import os
import re
import json
import shlex
import signal
import shutil

from os import walk
from os.path import splitext
from collections import OrderedDict
from subprocess import Popen
from multiprocessing import Pool
from abc import ABCMeta, abstractmethod, abstractproperty
from IPython import embed

from cmd_util import *
from conf import *
from util import *

IRGEN_ERROR_FORMAT = re.compile("^(.+):(\d+):(\d+): error: (.*)$")
IRGEN_WARN_FORMAT = re.compile("^(.+):(\d+):(\d+): warning: (.*) \[(.*)\]$")

#TRANS_FLAGS = ["-std-link-opts", "-O2","-verify"]
TRANS_FLAGS = ["-std-link-opts", "-O2","-verify"]



# worker
class PoolWork(object):
    def __init__(self, redir, out, cmd):
        self.redir = redir
        self.out = out
        self.cmd = cmd

# helpers for multi-processing
def init_pool_worker():
    signal.signal(signal.SIGINT, signal.SIG_IGN)

def halt_pool_worker(signum, frame):
    raise RuntimeError()

# workers for subtasks
def irgen_worker(work):
    if not prepdn(work.out):
        LOG_ERR("Preparing %s failed" % work.out)
        return False

    with open(work.out, "w") as f:
        if shell(work.cmd, err = f) == 0:
            LOG_INF(work.cmd)
            LOG_INF("IRGen done")
            return True
        else:
            print(work.cmd)
            LOG_ERR("IRGen fail")
            return False

def gen_cg_worker(work):
    if shell(work.cmd) == 0:
        LOG_INF(work.cmd,"done")
        return True
    else:
        LOG_ERR(work.cmd,"failed")
        return False
def group_worker(work):
    if not prepdn(work.out):
        LOG_ERR("Preparing %s failed" % work.out)
        return False

    with open(work.out, "w") as f:
        if shell(work.cmd, err = f) == 0:
            LOG_INF("Group done")
            return True
        else:
            LOG_ERR("Group fail")
            return False

def entry_ana_worker(work):
    if not prepdn(work.out):
        LOG_ERR("Preparing %s failed" % work.out)
        return False

    with open(work.out, "w") as f:
        if shell(work.cmd, err = f) == 0:
            LOG_INF("entry analysis done")
            return True
        else:
            LOG_ERR(work.cmd,"entry analysis fail")
            return False
def callchain_worker(work):
    if not prepdn(work.out):
        LOG_ERR("Preparing %s failed" % work.out)
        return False

    with open(work.out, "w") as f:
        if shell(work.cmd, err = f) == 0:
            LOG_INF("callchain done")
            return True
        else:
            LOG_ERR(work.cmd,"callchain fail")
            return False

def taint_ana_worker(work):
    if not prepdn(work.out):
        LOG_ERR("Preparing %s failed" % work.out)
        return False

    with open(work.out, "w") as f:
        if shell(work.cmd, err = f) == 0:
            LOG_INF(work.cmd,"taint analysis done")
            return True
        else:
            LOG_ERR(work.cmd,"taint analysis fail")
            return False

def trans_worker(work):
    if not prepdn(work.out):
        LOG_ERR("Preparing %s failed" % work.out)
        return False

    with open(work.out, "w") as f:
        if shell(work.cmd, err = f) == 0:
            return True
        else:
            print(work.cmd,'fail')
            return False

def pre_trans_process(input):
    if (not os.path.isabs(input)):
        print("please use abs path to the problematic bc file")
        assert(False)
    bc_name = os.path.basename(input)
    dir_name = os.path.dirname(input)

    ll_path = os.path.join(dir_name,bc_name[:bc_name.rfind(".")]+".ll")
    cmd = "%s %s -o %s" %(LLVM_BIN_DIS, input, ll_path)
    os.system(cmd)

    tmp_path = os.path.join(dir_name,bc_name[:bc_name.rfind(".")]+".tmp")
    with open(ll_path,"r") as rfile:
        with open(tmp_path,"w") as wfile:
            cached_inline_asms = []
            for eachline in rfile.readlines():
                if eachline.startswith("module asm"):
                    if eachline in cached_inline_asms:
                        continue
                    cached_inline_asms.append(eachline)
                wfile.write(eachline)
    cmd = "%s %s -o %s" %(LLVM_BIN_AS, tmp_path, input)
    os.system(cmd)
    os.remove(tmp_path)
    os.remove(ll_path)

def executeone(s):
    label = s.split(">")[-1].strip()
    print(label," start")
    if(call(s,shell=True) == 0):
        print(label,colored(" done","green"))
        return True
    else:
        print(s)
        print(label,colored(" failed","red"))
        return False

def analyze_worker(pn):
    with open(pn, "r") as f:
        data = json.load(f, object_pairs_hook=OrderedDict)
        for fn in data:
            frec = data[fn]["result"]

            if frec["total"] != 0:
                print("%s: %d" % (fn, frec["total"]))

            if frec["error"] != 0:
                LOG_ERR("Error: %d" % frec["error"])

            if frec["udf"] != 0:
                LOG_WRN("UDF: %d" % frec["udf"])

            if frec["sat"] != 0:
                LOG_INF("SAT: %d" % frec["sat"])

# app base class
class App(object, metaclass=ABCMeta):
    def __init__(self, app, tag, builder, grouper):
        self.app = app
        self.tag = tag
        self.builder = builder
        self.grouper = grouper

    @property
    def path_repo(self):
        return LINUX_GIT_URL

    @property
    def full_name(self):
        return "%s-%s" % (self.app, self.tag)

    @property
    def path_logs(self):
        return resolve(PATH_LOGS, self.full_name)

    @property
    def path_outs(self):
        return resolve(PATH_OUTS, self.full_name)

    @property
    def path_srcs(self):
        return resolve(PATH_SRCS, self.full_name)

    @property
    def path_objs(self):
        return resolve(PATH_OBJS, self.full_name)

    @property
    def path_bins(self):
        return resolve(PATH_BINS, self.full_name)

    @property
    def path_exts(self):
        return resolve(PATH_EXTS, self.full_name)

    @property
    def path_bcfs(self):
        return resolve(PATH_BCFS, self.full_name)

    @property
    def path_mods(self):
        return resolve(PATH_MODS, self.full_name)

    @property
    def path_tras(self):
        return resolve(PATH_TRAS, self.full_name)

    @property
    def path_syms(self):
        return resolve(PATH_SYMS, self.full_name)

    @property
    def path_log_build(self):
        return resolve(self.path_logs, "build.log")

    @property
    def path_log_parse(self):
        return resolve(self.path_logs, "parse.log")
    @property
    def path_log_gen_cg(self):
        return resolve(self.path_logs,"gen_cg.log")
    @property
    def path_log_entry(self):
        return resolve(self.path_logs,"entry.log")
    @property
    def path_log_taint(self):
        return resolve(self.path_logs,"taint.log")
    @property
    def path_log_callchain(self):
        return resolve(self.path_logs,"callchain.log")
    @property
    def path_log_irgen(self):
        return resolve(self.path_logs, "irgen.log")
    @property
    def path_log_irgen_dbg(self):
        return resolve(self.path_logs, "irgen_dbg.log")
    @property
    def path_log_group(self):
        return resolve(self.path_logs, "group.log")

    @property
    def path_log_links(self):
        return resolve(self.path_logs, "links.log")
    @property
    def path_log_links_dbg(self):
        return resolve(self.path_logs, "links_dbg.log")

    @property
    def path_log_trans(self):
        return resolve(self.path_logs, "trans.log")
    @property
    def path_log_trans_dbg(self):
        return resolve(self.path_logs, "trans_dbg.log")

    @property
    def path_log_check(self):
        return resolve(self.path_logs, "check.log")

    @abstractmethod
    def convert(self):
        return

    def checkout(self):
        ver = self.convert()
        if ver is None:
            LOG_ERR("Tag parsing failed")
            return False

        psrc = self.path_srcs
        return gitclone(ver, self.path_repo, psrc)

    @abstractmethod
    def config_impl(self, psrc, pobj, pbin, pext):
        LOG_ERR("Should never reach here")
        return False

    def config(self):
        psrc = self.path_srcs
        if not exists(psrc):
            LOG_ERR("Src path %s does not exist" % psrc)
            return False

        pobj = self.path_objs
        if not mkdirs(pobj):
            LOG_WRN("Config canceled")
            return False

        pbin = self.path_bins
        pext = self.path_exts

        return self.config_impl(psrc, pobj, pbin, pext)

    @abstractmethod
    def build_impl(self, psrc, pobj, pbin, pext, plog):
        LOG_ERR("Should never reach here")
        return False

    def build(self):
        psrc = self.path_srcs
        if not exists(psrc):
            LOG_ERR("Src path %s does not exist" % psrc)
            return False

        pobj = self.path_objs
        if not exists(pobj):
            LOG_ERR("Obj path %s does not exist" % pobj)
            return False

        pbin = self.path_bins
        if not mkdirs(pbin):
            LOG_WRN("Build canceled")
            return False

        pext = self.path_exts
        if not mkdirs(pext):
            LOG_WRN("Build canceled")
            return False

        plog = self.path_log_build
        if not prepdn(plog):
            LOG_WRN("Log path %s cannot be prepared" % plog)
            return False

        return self.build_impl(psrc, pobj, pbin, pext, plog)

    @abstractmethod
    def parse_line(self, line):
        LOG_ERR("Should never reach here")
        return None

    @abstractmethod
    def parse_opts(self, opts):
        LOG_ERR("Should never reach here")
        return None

    def parse(self):
        log = self.path_log_build
        if not exists(log):
            LOG_ERR("Log path %s does not exist" % log)
            return False

        parser = CMDParser()
        runs = OrderedDict()
        dups = []

        # collect appeared flags
        collect = CMDCollect()

        with open(log) as f:
            for line in f:
                # analyze the line to extract argv section
                result = self.parse_line(line)
                if result is None or len(result) != 2:
                    LOG_ERR("Line parsing failed: %s" % line)
                    return False

                if result[0] == False:
                    continue

                try:
                    opts = CMDResult(parser, result[1])
                except:
                    print(result)
                    assert(False)
                # collect appeared flags
                collect.update(opts)

                # analyze the opts to get source files
                result = self.parse_opts(opts)
                if result is None or len(result) != 2:
                    LOG_ERR("Opt parsing failed: %s" % line)
                    return False

                if result[0] is False:
                    continue

                self.builder.build(opts)
                run = opts.organize()

                for src in result[1]:
                    if src in runs:
                        dups.append(src)

                    runs[src] = run

        out = self.path_log_parse
        with open(out, "w") as f:
            json.dump(runs, f, indent = 2)

        for i in dups:
            LOG_WRN("[duplicate] %s" % i)

        # output collected flags
        collect.show()

        return True

    @abstractmethod
    def group_line(self, line):
        LOG_ERR("Should never reach here")
        return None

    @abstractmethod
    def group_opts(self, opts):
        LOG_ERR("Should never reach here")
        return None

    @abstractmethod
    def group_mark(self, tops):
        LOG_ERR("Should never reach here")
        return None

    def group(self):
        log = self.path_log_build
        if not exists(log):
            LOG_ERR("Log path %s does not exist" % log)
            return False

        psrc = self.path_srcs
        pobj = self.path_objs

        pbcf = self.path_bcfs
        if not exists(pbcf):
            LOG_ERR("Bcf path %s does not exist" % pbcf)
            return False

        plog = self.path_log_irgen
        if not exists(plog):
            LOG_ERR("Log path %s does not exist" % plog)
            return False

        pout = self.path_outs
        if not exists(pout):
            LOG_ERR("Group cancelled")
            return False

        runs = OrderedDict()
        dups = []

        # collect included objs
        with open(log) as f:
            for line in f:
                # analyze the line to extract argv section
                result = self.group_line(line)
                if result is None or len(result) != 2:
                    LOG_ERR("Line grouping failed: %s" % line)
                    return False

                if result[0] == False:
                    continue
                print(line)
                '''
                if '&&' in result[1]:
                result = list(result)
                result[1] = result[1][:result[1].index('&&')]
                '''
                #print result[1]
                opts = CMDModule(self.grouper,result[1])

                opts.outs = opts.outs[:-2] + '.o'
                #print 'outs:',opts.outs
                for idx in range(len(opts.srcs)):
                    opts.srcs[idx] = opts.srcs[idx][:-2] + '.o'   
                result = self.group_opts(opts)
                if result is None or len(result) != 2:
                    LOG_ERR("Opt grouping failed: %s" % line)
                    return False

                if result[0] is False:
                    continue

                if opts.outs in runs:
                    dups.append(opts.outs)

                runs[opts.outs] = result[1]

        # ignore dups
        for goal in dups:
            del runs[goal]

        # build ld hierarchy
        arch = OrderedDict()

        for goal in runs:
            arch[goal] = CMDLink(goal)

        # establish the relationship between child and parent
        for goal in runs:
            for src in runs[goal]:
                if src in arch:
                    arch[goal].link(arch[src])

        # collect top level targets
        tops = []
        for goal in arch:
            if len(arch[goal].pars) == 0:
                tops.append(goal)

        marks = self.group_mark(tops)
        # find all available objs
        bcfs = set()
        stats = set()
        with open(plog, "r") as f:
            for l in f:
                toks = l.strip().split(" ")
                if toks[0] != "done":
                    continue
                if toks[1] == "bcf":
                    r = toks[2]
                    if r[0] == "/":
                        r = resolve(pbcf, r[len(psrc)+1:])
                    else:
                        r = resolve(pbcf, r)
                    r = splitext(r)[0] + ".bc"
                    bcfs.add(r)
                elif toks[1] == "__user":
                    r = toks[2]
                    if r[0] == "/":
                        r = resolve(pbcf, r[len(psrc)+1:])
                    else:
                        r = resolve(pbcf, r)
                    r = splitext(r)[0] + ".static"
                    stats.add(r)
                else:
                    assert(False and "what the heck?")

        # bottom up
        f = open(self.path_log_group, "w")

        count = 0
        coll = []
        failed = OrderedDict()

        while len(arch) != 0:
            f.write("==== iteration %d ====\n" % count)

            cmds = []
            outs = []
            reds = []
            srcs = []

            for goal in arch:
                if len(arch[goal].subs) != 0:
                    continue

                # define output
                if goal[0] == "/":
                    out = resolve(pbcf, goal[len(psrc)+1:])
                    red = resolve(pout, goal[len(psrc)+1:])
                else:
                    out = resolve(pbcf, goal)
                    red = resolve(pout, goal)
                merged_stat = splitext(out)[0] + ".static"
                out = splitext(out)[0] + ".bc"
                red = splitext(red)[0] + ".group"

                # define input
                stats_to_merge = []
                infs = []
                for r in runs[goal]:
                    if r[0] == "/":
                        inf = resolve(pbcf, r[len(psrc)+1:])
                    else:
                        inf = resolve(pbcf, r)

                    inf = splitext(inf)[0] + ".bc"
                    stat = splitext(inf)[0] + ".static"
                    if inf in bcfs:
                        infs.append(inf)
                    else:
                        if goal not in failed:
                            failed[goal] = set()

                        failed[goal].add(inf)
                    
                    if stat in stats:
                        stats_to_merge.append(stat)

                stat_lines = ""
                for each_stat_to_merge in stats_to_merge:
                    if(os.path.exists(each_stat_to_merge)):
                        #print each_stat_to_merge, "exists"
                        with open(each_stat_to_merge,"r") as rfile:
                            stat_lines += rfile.read()
                    #else:
                    #    print each_stat_to_merge,"missed"
                    #    raw_input()

                prepdn(merged_stat)
                with open(merged_stat,"w") as wfile:
                    wfile.write(stat_lines)
                print(merged_stat," merged finished\n")
                # create cmd
                if not prepdn(out):
                    LOG_ERR("Cannot prepare file path %s" % out)
                    return False

                cmd = PoolWork(True, red, "%s -o %s %s" % \
                        (LLVM_BIN_BLD, out, " ".join(infs)))

                cmds.append(cmd)
                outs.append(out)
                reds.append(red)
                srcs.append(goal)

            # run commands
            with cd(pbcf):
                pool = Pool(HALF_NCPU, init_pool_worker)

                try:
                    work = pool.map(group_worker, cmds)
                except KeyboardInterrupt:
                    pool.terminate()
                    pool.join()

            # consolidate output
            for i, r in enumerate(outs):
                if work[i]:
                    f.write("done %s\n" % srcs[i])
                    bcfs.add(r)
                else:
                    f.write("fail %s\n" % srcs[i])
                    coll.append(reds[i])

            # clean up
            for goal in srcs:
                del arch[goal]

            for goal in arch:
                for k in srcs:
                    if k in arch[goal].subs:
                        arch[goal].subs.remove(k)

            count += 1

        f.close()

        # output
        for i in dups:
            LOG_WRN("[duplicate] %s" % i)

        LOG_WRN("%d linking failures" % len(coll))
        for fn in coll:
            LOG_WRN(fn)
            with open(fn, "r") as f:
                print(f.read())

        LOG_WRN("%d goals have only partial linking" % len(failed))
        for obj in sorted(failed):
            print(obj)

        # move to mods folder
        with open(self.path_log_links, "w") as f:
            for m in marks:
                if m[0] == "/":
                    out = resolve(pbcf, m[len(psrc)+1:])
                else:
                    out = resolve(pbcf, m)

                out = splitext(out)[0] + ".bc"

                if exists(out):
                    f.write("done %s\n" % m)
                else:
                    f.write("fail %s\n" % m)

        return True
    def processOneUser(lines,eachIdx):
        pass
    def extract__user(src):
        lines = []
        with open(src,'r') as rfile:
            lines = rfile.readlines()
        __user_idx = []
        for idx,eachline in enumerate(lines):
            if '__user' in eachline:
                __user_idx.append(idx)
        for eachIdx in __user_idx:
            result = processOneUser(lines,eachIdx)
    
    def irgen(self, inputs, force):
        psrc = self.path_srcs
        if not exists(psrc):
            LOG_ERR("Src path %s does not exist" % psrc)
            return False

        pobj = self.path_objs
        if not exists(pobj):
            LOG_ERR("Obj path %s does not exist" % pobj)
            return False

        plog = self.path_log_parse
        if not exists(plog):
            LOG_ERR("Log path %s does not exist" % plog)
            return False

        pout = self.path_outs
        if not mkdirs(pout):
            LOG_WRN("IRGen cancelled")
            return False

        pbcf = self.path_bcfs

        f = open(plog, "r")
        data = json.load(f, object_pairs_hook=OrderedDict)
        f.close()

        if inputs is None:
            runs = data
        else:
            runs = OrderedDict()
            for i in inputs:
                if i[0] != "/":
                    new_i = resolve(psrc, i)
                else:
                    new_i = i
                runs[new_i] = data[i]
        cmds = []
        outs = []
        srcs = []
        to_delete = []
        runs = OrderedDict([(k,v) if k[0] =="/" else (resolve(pobj,k),v)  for k,v in list(runs.items())])
        for r in runs:
            if r[0] == "/":
                out = resolve(pbcf, r[len(psrc)+1:])
                red = resolve(pout, r[len(psrc)+1:])
            else:
                old_r = r
                out = resolve(pbcf, r)
                red = resolve(pout, r)
                
            out = splitext(out)[0] + ".bc"
            red = splitext(red)[0] + ".irgen"
            out_2 = splitext(out)[0] + ".static"
            red_2 = splitext(red)[0] + ".static1"
            
            if not prepdn(out):
                LOG_ERR("Cannot prepare file path %s" % out)
                return False
            #force = True
            if exists(out) and not force:
                cmd1 = PoolWork(True, red, "echo 'skip'")
                cmd2 = PoolWork(True, red_2,"echo 'skip'")
            else:
                ubsan_params = "-fsanitize=signed-integer-overflow,unsigned-integer-overflow,shift-exponent,integer-divide-by-zero,implicit-unsigned-integer-truncation,implicit-signed-integer-truncation,implicit-integer-sign-change,vla-bound,array-bounds,local-bounds"
                cm1str = "%s %s -emit-llvm %s -o %s %s" %(LLVM_BIN_CLA, ubsan_params, runs[r], out, r)
                cmd1 = PoolWork(True, red, cm1str)
                new_r = r[:-2]+'-kubo'+r[-2:]
                with open(r,'r') as rf:
                    with open(new_r,'w') as wf:
                        wf.write('''#define __user __attribute__((noderef))\n\n''')
                        wf.write(rf.read())
                cm2str = "%s %s -- %s %s" %(LLVM_BIN_CATCH_USER, new_r,runs[r],out_2) 
                to_delete.append(new_r)
                cmd2 = PoolWork(True,red_2,cm2str)
            #cmds.append(cmd1)
            cmds.append(cmd1)
            cmds.append(cmd2)
            outs.append(out)
            srcs.append(r)

        with cd(pobj):
            pool = Pool(HALF_NCPU, init_pool_worker)
            try:
                work = pool.map(irgen_worker, cmds)
            except KeyboardInterrupt:
                pool.terminate()
                pool.join()
        for eachDelete in to_delete:
            os.remove(eachDelete)
        inconsistency = []
        bcf_work = True
        __user_work = True
        with open(self.path_log_irgen, "w") as f:
            for i, r in enumerate(runs.keys()):
                i1 = i*2
                i2 = i*2 + 1
                if work[i1]:
                    bcf_work = True
                    f.write("done bcf %s\n" % srcs[i])
                else:
                    bcf_work = False
                    f.write("fail bcf %s\n" % srcs[i])
                if work[i2]:
                    __user_work = True
                    f.write("done __user %s\n" % srcs[i])
                else:
                    __user_work = False
                    f.write("fail __user %s\n" % srcs[i])
                if bcf_work != __user_work:
                    print(srcs[i],",inconsistency need to double check\n")
                    f.write("%s, inconsistency, need to double check\n")
        return True
    
    
    def gen_dbg_ir(self,inputs,force):
        psrc = self.path_srcs
        if not exists(psrc):
            LOG_ERR("Src path %s does not exist" % psrc)
            return False

        pobj = self.path_objs
        if not exists(pobj):
            LOG_ERR("Obj path %s does not exist" % pobj)
            return False

        plog = self.path_log_parse
        if not exists(plog):
            LOG_ERR("Log path %s does not exist" % plog)
            return False

        pout = self.path_outs
        if not exists(pout):
            LOG_WRN("IRGen cancelled")
            return False

        pbcf = self.path_bcfs
        if not exists(pbcf):
            LOG_ERR("Bcf path %s does not exist" % pbcf)
            return False

        f = open(plog, "r")
        data = json.load(f, object_pairs_hook=OrderedDict)
        f.close()

        if inputs is None:
            runs = data
        else:
            runs = OrderedDict()
            for i in inputs:
                if i[0] != "/":
                    new_i = resolve(psrc, i)
                else:
                    new_i = i
                runs[new_i] = data[i]
        
        cmds = [] 
        outs = []
        srcs = []

        runs = OrderedDict([(k,v) if k[0] =="/" else (resolve(pobj,k),v)  for k,v in list(runs.items())])
        for r in runs:
            if r[0] == "/":
                out = resolve(pbcf, r[len(psrc)+1:])
                red = resolve(pout, r[len(psrc)+1:])
            else:
                old_r = r
                out = resolve(pbcf, r)
                red = resolve(pout, r)
                
            out = splitext(out)[0] + "-dbg.bc"
            red = splitext(red)[0] + "-dbg.irgen"
            
            if not prepdn(out):
                LOG_ERR("Cannot prepare file path %s" % out)
                return False
            if exists(out) and not force:
                cmd1 = PoolWork(True, red, "echo 'skip'")
            else:
                ubsan_params = "-fsanitize=signed-integer-overflow,unsigned-integer-overflow,shift-exponent,integer-divide-by-zero,implicit-unsigned-integer-truncation,implicit-signed-integer-truncation,implicit-integer-sign-change,vla-bound,array-bounds,local-bounds"
                cm1str = "%s -g %s -emit-llvm %s -o %s %s" %(LLVM_BIN_CLA, ubsan_params, runs[r], out, r)
                cmd1 = PoolWork(True, red, cm1str)
            srcs.append(r)
            outs.append(out)
            cmds.append(cmd1)

        with cd(pobj):
            pool = Pool(HALF_NCPU, init_pool_worker)
            try:
                work = pool.map(irgen_worker, cmds)
            except KeyboardInterrupt:
                pool.terminate()
                pool.join()
        
        with open(self.path_log_irgen_dbg, "w") as f:
            for i, r in enumerate(runs.keys()):
                if work[i]:
                    bcf_work = True
                    f.write("done bcf %s\n" % srcs[i])
                else:
                    bcf_work = False
                    f.write("fail bcf %s\n" % srcs[i])
                

    def gen_dbg_group(self,inputs,force):
        log = self.path_log_build
        if not exists(log):
            LOG_ERR("Log path %s does not exist" % log)
            return False

        pbcf = self.path_bcfs
        if not exists(pbcf):
            LOG_ERR("Bcf path %s does not exist" % pbcf)
            return False
        
        pout = self.path_outs
        if not exists(pout):
            LOG_ERR("Group cancelled")
            return False
        
        
        psrc = self.path_srcs
        pobj = self.path_objs
        plog = self.path_log_irgen_dbg
        
        
        runs = OrderedDict()
        dups = []

        # collect included objs
        with open(log) as f:
            for line in f:
                # analyze the line to extract argv section
                result = self.group_line(line)
                if result is None or len(result) != 2:
                    LOG_ERR("Line grouping failed: %s" % line)
                    return False
                if result[0] == False:
                    continue
                print(line)

                opts = CMDModule(self.grouper,result[1])
                opts.outs = opts.outs[:-2] + '.o'
                for idx in range(len(opts.srcs)):
                    opts.srcs[idx] = opts.srcs[idx][:-2] + '.o'
                result = self.group_opts(opts)
                if result is None or len(result) != 2:
                    LOG_ERR("Opt grouping failed: %s" % line)
                    return False
                if result[0] is False:
                    continue
                if opts.outs in runs:
                    dups.append(opts.outs)
                runs[opts.outs] = result[1]

        # ignore dups
        for goal in dups:
            del runs[goal]

        # build ld hierarchy
        arch = OrderedDict()

        for goal in runs:
            arch[goal] = CMDLink(goal)

        # establish the relationship between child and parent
        for goal in runs:
            for src in runs[goal]:
                if src in arch:
                    arch[goal].link(arch[src])

        # collect top level targets
        tops = []
        for goal in arch:
            if len(arch[goal].pars) == 0:
                tops.append(goal)

        marks = self.group_mark(tops)
        # find all available objs
        bcfs = set()
        stats = set()
        with open(plog, "r") as f:
            for l in f:
                toks = l.strip().split(" ")
                if toks[0] != "done":
                    continue
                if toks[1] == "bcf":
                    r = toks[2]
                    if r[0] == "/":
                        r = resolve(pbcf, r[len(psrc)+1:])
                    else:
                        r = resolve(pbcf, r)
                    r = splitext(r)[0] + "-dbg.bc"
                    bcfs.add(r)
        # bottom up
        f = open(self.path_log_group, "w")

        count = 0
        coll = []
        failed = OrderedDict()

        while len(arch) != 0:
            f.write("==== iteration %d ====\n" % count)

            cmds = []
            outs = []
            reds = []
            srcs = []

            for goal in arch:
                if len(arch[goal].subs) != 0:
                    continue

                # define output
                if goal[0] == "/":
                    out = resolve(pbcf, goal[len(psrc)+1:])
                    red = resolve(pout, goal[len(psrc)+1:])
                else:
                    out = resolve(pbcf, goal)
                    red = resolve(pout, goal)
                out = splitext(out)[0] + "-dbg.bc"
                red = splitext(red)[0] + "-dbg.group"

                # define input
                infs = []
                for r in runs[goal]:
                    if r[0] == "/":
                        inf = resolve(pbcf, r[len(psrc)+1:])
                    else:
                        inf = resolve(pbcf, r)

                    inf = splitext(inf)[0] + "-dbg.bc"
                    if inf in bcfs:
                        infs.append(inf)
                    else:
                        if goal not in failed:
                            failed[goal] = set()

                        failed[goal].add(inf)
                    

                # create cmd
                if not prepdn(out):
                    LOG_ERR("Cannot prepare file path %s" % out)
                    return False

                cmd = PoolWork(True, red, "%s -o %s %s" % (LLVM_BIN_BLD, out, " ".join(infs)))

                cmds.append(cmd)
                outs.append(out)
                reds.append(red)
                srcs.append(goal)

            # run commands
            with cd(pbcf):
                pool = Pool(HALF_NCPU, init_pool_worker)

                try:
                    work = pool.map(group_worker, cmds)
                except KeyboardInterrupt:
                    pool.terminate()
                    pool.join()

            # consolidate output
            for i, r in enumerate(outs):
                if work[i]:
                    f.write("done %s\n" % srcs[i])
                    bcfs.add(r)
                else:
                    f.write("fail %s\n" % srcs[i])
                    coll.append(reds[i])

            # clean up
            for goal in srcs:
                del arch[goal]

            for goal in arch:
                for k in srcs:
                    if k in arch[goal].subs:
                        arch[goal].subs.remove(k)

            count += 1

        f.close()

        # output
        for i in dups:
            LOG_WRN("[duplicate] %s" % i)

        LOG_WRN("%d linking failures" % len(coll))
        for fn in coll:
            LOG_WRN(fn)
            with open(fn, "r") as f:
                print(f.read())

        LOG_WRN("%d goals have only partial linking" % len(failed))
        for obj in sorted(failed):
            print(obj)

        with open(self.path_log_links_dbg, "w") as f:
            for m in marks:
                if m[0] == "/":
                    out = resolve(pbcf, m[len(psrc)+1:])
                else:
                    out = resolve(pbcf, m)

                out = splitext(out)[0] + "-dbg.bc"

                if exists(out):
                    f.write("done %s\n" % m)
                else:
                    f.write("fail %s\n" % m)
    
    def gen_dbg_trans(self, inputs, force):
        psrc = self.path_srcs
        pbcf = self.path_bcfs
        plog = self.path_log_links_dbg
        pout = self.path_outs
        ptra = self.path_tras

        data = OrderedDict()
        with open(plog, "r") as f:
            for l in f:
                toks = l.strip().split(" ")
                data[toks[1]] = toks[0]

        runs = []
        if inputs is None:
            for i in data:
                if data[i] == "done":
                    runs.append(i)
        else:
            for i in inputs:
                if data[i] == "done":
                    runs.append(i)

        cmds = []
        outs = []
        reds = []
        srcs = []
        inputs = []

        for r in runs:
            if r[0] == "/":
                inf = resolve(pbcf, r[len(psrc)+1:])
                out = resolve(ptra, r[len(psrc)+1:])
                red = resolve(pout, r[len(psrc)+1:])
            else:
                inf = resolve(pbcf, r)
                out = resolve(ptra, r)
                red = resolve(pout, r)

            inf = splitext(inf)[0] + "-dbg.bc"
            out = splitext(out)[0] + "-dbg.ll"
            red = splitext(red)[0] + "-dbg.trans"

            #self.pre_trans_process(inf)
            if not prepdn(out):
                LOG_ERR("Cannot prepare file path %s" % out)
                return False
            if exists(out) and not force:
                if os.path.getsize(out) < 4:
                    cmd = PoolWork(True, red, "%s %s %s > %s" % \
                                (LLVM_BIN_OPT, " ".join(TRANS_FLAGS), inf, out))
                else:
                    cmd = PoolWork(True, red, "")
            else:
                cmd = PoolWork(True, red, "%s %s %s > %s" % \
                                (LLVM_BIN_OPT, " ".join(TRANS_FLAGS), inf, out))
            cmds.append(cmd)
            outs.append(out)
            reds.append(red)
            srcs.append(r)
            inputs.append(inf)

        with cd(pbcf):
            pool = Pool(HALF_NCPU, init_pool_worker)

            print("making sure no asm redefinition problem will happen...")
            try:
                work = pool.map(pre_trans_process,inputs)
            except KeyboardInterrupt:
                pool.terminate()
                pool.join()
                LOG_WRN("Interrupted")
                return False
            print("done")
            
            print("optimizing the bc files...")
            pool = Pool(HALF_NCPU, init_pool_worker)
            try:
                work = pool.map(trans_worker, cmds)
            except KeyboardInterrupt:
                pool.terminate()
                pool.join()
                LOG_WRN("Interrupted")
                return False
            print("done")

        with open(self.path_log_trans_dbg, "w") as f:
            for i, r in enumerate(runs):
                if work[i]:
                    f.write("done %s\n" % srcs[i])
                else:
                    f.write("fail %s\n" % srcs[i])

        for i, fn in enumerate(reds):
            with open(fn, "r") as f:
                content = f.read()
                if len(content) != 0:
                    LOG_WRN(srcs[i])
                    print(content)
    
    def gen_cg(self):
        ptra = self.path_tras
        if not exists(ptra):
            LOG_WRN("TRANS not exist")
            return False
        cmds = []
        srcs = []
        for root,_,files in os.walk(ptra):
            for eachfile in files:
                if eachfile == "built-in.ll":
                    inputfile = resolve(ptra,root,eachfile)
                    cgfile = resolve(ptra,root,"cg_pex.txt")
                    
                    srcs.append(inputfile)
                    cmd = PoolWork(False,cgfile,"%s %s %s %s %s" %(PATH_PEX_EXE,LLVM_BIN_OPT,PATH_PEX_SO,inputfile,cgfile))
                    cmds.append(cmd)
        pool = Pool(HALF_NCPU, init_pool_worker)
        try:
            work = pool.map(gen_cg_worker, cmds)
        except KeyboardInterrupt:
            pool.terminate()
            pool.join()
        print(self.path_log_gen_cg)
        prepdn(self.path_log_gen_cg)
        with open(self.path_log_gen_cg,"w") as wfile:
            for i,r in enumerate(srcs):
                if work[i]:
                    wfile.write("done %s\n" % srcs[i])
                else:
                    wfile.write("fail %s\n" % srcs[i])
        return True
    
    def entry_ana(self):
        ptra = self.path_tras
        if not exists(ptra):
            LOG_WRN("TRANS not exist")
            return False
        pout = self.path_outs
        if not exists(pout):
            LOG_ERR("Group cancelled")
            return False
        cmds = []
        srcs = []
        for root,_,files in os.walk(ptra):
            for eachfile in files:
                if eachfile == "built-in.ll":
                    inputfile = resolve(ptra,root,eachfile)
                    entryfile = resolve(ptra,root,"entry.txt")
                    relative_path = ''
                    if self.full_name in root:
                        relative_path = root[root.find(self.full_name)+len(self.full_name)+1:]
                    else:
                        relative_path = root
                    redfile = resolve(pout,relative_path,"entry.out")
                    force = True
                    if exists(entryfile) and not force:
                        pass
                    else:
                        srcs.append(inputfile)
                        cmd = PoolWork(True,redfile,"%s %s %s" %(PATH_ENTRY_EXE,inputfile,entryfile))
                        cmds.append(cmd)
        pool = Pool(HALF_NCPU, init_pool_worker)
        try:
            work = pool.map(entry_ana_worker, cmds)
        except KeyboardInterrupt:
            pool.terminate()
            pool.join()
        prepdn(self.path_log_entry)
        with open(self.path_log_entry,"w") as wfile:
            for i,r in enumerate(srcs):
                if work[i]:
                    wfile.write("done %s\n" % srcs[i])
                else:
                    wfile.write("fail %s\n" % srcs[i])
        #merge __user annotated arguments into entry functions and syscalls
        for root,_,files in os.walk(ptra):
            for eachfile in files:
                if eachfile == "built-in.ll":
                    inputfile = resolve(ptra,root,eachfile)
                    entryfile = resolve(ptra,root,"entry.txt")
                    kubofile = resolve(ptra,root,"built-in_kubo.txt")
                    kubolines = []
                    kubo_result = {}
                    with open(kubofile,'r') as rfile:
                        kubolines = rfile.readlines()
                    for idx,eachkuboline in enumerate(kubolines):
                        if eachkuboline.startswith("func:"):
                            funcName = eachkuboline.strip().split(":")[1]
                            paraIdx = int(kubolines[idx+1].strip().split("|")[0])
                            paraName = kubolines[idx+1].strip().split("|")[1]
                            if funcName not in kubo_result:
                                kubo_result[funcName] = {}
                            if paraIdx not in kubo_result[funcName]:
                                kubo_result[funcName][paraIdx] = paraName
                            else:
                                oldParaName = kubo_result[funcName][paraIdx]
                                if oldParaName == '' and paraName != '':
                                    kubo_result[funcName][paraIdx] = paraName
                    with open(entryfile,"a") as wfile:
                        for eachkubo in kubo_result:
                            arg_index = []
                            for each_ind in kubo_result[eachkubo]:
                                arg_index.append(str(each_ind))
                            wfile.write("%s:%s\n"%(",".join(arg_index),eachkubo))
        return True

    def taint_ana(self):
        ptra = self.path_tras
        if not exists(ptra):
            LOG_WRN("TRANS not exist")
            return False
        pout = self.path_outs
        if not exists(pout):
            LOG_ERR("Group cancelled")
            return False
        cmds = []
        srcs = []
        for root,_,files in os.walk(ptra):
            for eachfile in files:
                if eachfile == "built-in.ll":
                    inputfile = resolve(ptra,root,eachfile)
                    taintfile = resolve(ptra,root,"taint.txt")
                    entryfile = resolve(ptra,root,"entry.txt")
                    cgfile = resolve(ptra,root,"cg_pex.txt")
                    relative_path = ''
                    if self.full_name in root:
                        relative_path = root[root.find(self.full_name)+len(self.full_name)+1:]
                    else:
                        relative_path = root
                    redfile = resolve(pout,relative_path,"taint.out")
                    force = True
                    if exists(taintfile) and not force:
                        pass
                    else:
                        srcs.append(inputfile)
                        cmd = PoolWork(True,redfile,"%s -load %s -dr_checker -callgraphFile=%s -disable-output -outputFile=%s %s" %(LLVM_BIN_OPT,PATH_TAINT_SO,cgfile,taintfile,inputfile))
                        cmds.append(cmd)
        pool = Pool(OPTS_NCPU, init_pool_worker)
        try:
            work = pool.map(taint_ana_worker, cmds)
        except KeyboardInterrupt:
            pool.terminate()
            pool.join()
        prepdn(self.path_log_taint)
        with open(self.path_log_taint,"w") as wfile:
            for i,r in enumerate(srcs):
                if work[i]:
                    wfile.write("done %s\n" % srcs[i])
                else:
                    wfile.write("fail %s\n" % srcs[i])
        return True
    def trans(self, inputs, force):
        psrc = self.path_srcs

        pbcf = self.path_bcfs
        if not exists(pbcf):
            LOG_ERR("Bcf path %s does not exist" % pbcf)
            return False

        plog = self.path_log_links
        if not exists(plog):
            LOG_ERR("Log path %s does not exist" % pmod)
            return False

        pout = self.path_outs
        if not exists(pout):
            LOG_WRN("Trans cancelled")
            return False

        ptra = self.path_tras

        data = OrderedDict()
        with open(plog, "r") as f:
            for l in f:
                toks = l.strip().split(" ")
                data[toks[1]] = toks[0]

        runs = []
        if inputs is None:
            for i in data:
                if data[i] == "done":
                    runs.append(i)
        else:
            for i in inputs:
                if data[i] == "done":
                    runs.append(i)

        cmds = []
        outs = []
        reds = []
        srcs = []

        for r in runs:
            if r[0] == "/":
                inf = resolve(pbcf, r[len(psrc)+1:])
                out = resolve(ptra, r[len(psrc)+1:])
                red = resolve(pout, r[len(psrc)+1:])
            else:
                inf = resolve(pbcf, r)
                out = resolve(ptra, r)
                red = resolve(pout, r)

            inf = splitext(inf)[0] + ".bc"
            out = splitext(out)[0] + ".ll"
            red = splitext(red)[0] + ".trans"

            if not prepdn(out):
                LOG_ERR("Cannot prepare file path %s" % out)
                return False
            src_static = splitext(inf)[0] + ".static"
            dst_static = splitext(out)[0] + "_kubo.txt"
            shutil.copyfile(src_static,dst_static) 
            if exists(out) and not force:
                if os.path.getsize(out) < 4:
                    cmd = PoolWork(True, red,
                        "%s %s %s > %s" % \
                                (LLVM_BIN_OPT, " ".join(TRANS_FLAGS), inf, out))
                else:
                    cmd = PoolWork(True, red, "")
            else:
                cmd = PoolWork(True, red,
                        "%s %s %s > %s" % \
                                (LLVM_BIN_OPT, " ".join(TRANS_FLAGS), inf, out))
            cmds.append(cmd)
            outs.append(out)
            reds.append(red)
            srcs.append(r)

        with cd(pbcf):
            pool = Pool(HALF_NCPU, init_pool_worker)
            try:
                work = pool.map(trans_worker, cmds)
            except KeyboardInterrupt:
                pool.terminate()
                pool.join()

                LOG_WRN("Interrupted")
                return False

        with open(self.path_log_trans, "w") as f:
            for i, r in enumerate(runs):
                if work[i]:
                    f.write("done %s\n" % srcs[i])
                else:
                    f.write("fail %s\n" % srcs[i])

        for i, fn in enumerate(reds):
            with open(fn, "r") as f:
                content = f.read()
                if len(content) != 0:
                    LOG_WRN(srcs[i])
                    print(content)

    @abstractmethod
    def check_filter(self):
        LOG_ERR("Should never reach here")
        return None

    
    def run(self):
        ptra = self.path_tras
        cmds = []
        for root,_,files in os.walk(ptra):
            for eachfile in files:
                if eachfile=='built-in.ll':  
                    absRoot = os.path.abspath(root)
                    inputfile = os.path.join(absRoot,'built-in.ll')
                    outfile = os.path.join(absRoot,'result.txt')
                    errfile = os.path.join(absRoot,'err.txt')
                    cgfile = os.path.join(absRoot,'cg_pex.txt')
                    taintfile = os.path.join(absRoot,'taint.txt')
                    entryfile = os.path.join(absRoot,"entry.txt")
                    __userfile = os.path.join(absRoot,"built-in_kubo.txt")
                    if(not os.path.exists(cgfile)):
                        print("cg file does not exist")
                        continue
                    if(not os.path.exists(taintfile)):
                        print("taint file does not exist")
                        continue
                    if(not os.path.exists(entryfile)):
                        print("entry file doesn't exist")
                        continue
                    if(not os.path.exists(__userfile)):
                        print("__user file doesn't exist")
                        continue
                    cmd = "%s -load %s -KSym -cgf %s -outf %s -taintf %s -usrinputf %s -entryf %s \
                             -disable-verify -disable-output %s 2> %s" \
                            %(LLVM_PRE_BIN_OPT,PASS_KSYM,cgfile,outfile,taintfile,__userfile,entryfile,inputfile,errfile)
                    cmds.append(cmd)

        p = Pool(OPTS_NCPU,init_pool_worker)
        p.map(executeone,cmds)
        return True

    def __dump_irgen(self, fp):
        erec = dict()
        wrec = set()

        for l in fp:
            m = IRGEN_ERROR_FORMAT.match(l)
            if m is not None:
                src = m.group(1)
                if src not in erec:
                    erec[src] = []

                erec[src].append(m.group(4))
                continue

            m = IRGEN_WARN_FORMAT.match(l)
            if m is not None:
                wrec.add(m.group(5))
                continue

        for k in sorted(erec):
            LOG_INF(k)
            for i in erec[k]:
                LOG_ERR(i)

        return wrec

    def dump(self):
        pout = self.path_outs
        if not exists(pout):
            LOG_ERR("Out path %s does not exist" % pout)
            return False

        dset = set()
        for dname, dlist, flist in os.walk(pout):
            for fn in flist:
                if splitext(fn)[1] == ".irgen":
                    with open(resolve(dname, fn), "r") as f:
                        dset.update(self.__dump_irgen(f))

        for k in sorted(dset):
            print(k)

        return True

    def stat(self):
        class Func:
            def __init__(self):
                self.func_name = ''
                self.seeds = []
                self.times = []
                self.caller_nums = []
                self.timed_out_caller_num = 0
                self.confirmed_bugs = []

        start_of_function = "[info]dealing with"
        start_of_hop = "[info] hop"
        start_of_seed = "total seed num"

        bug_dir = "./bugs"

        def process_func(lines):
            assert(lines[0].startswith(start_of_function))
            f = Func()
            f.func_name = lines[0].strip().split(":")[1]
            hop_num = None
            for line_idx,eachline in enumerate(lines):
                if eachline.startswith("total seed num"):
                    total_seed = int(lines[line_idx].strip().split(":")[1])
                    true_seed = int(lines[line_idx+1].strip().split(":")[1])
                    unsure_seed = int(lines[line_idx+2].strip().split(":")[1])
                    false_seed = int(lines[line_idx+3].strip().split(":")[1])
                    f.seeds.append((total_seed,true_seed,unsure_seed,false_seed))
                elif eachline.startswith("[info] hop"):
                    splitted = eachline.strip().split(" ")
                    hop_num = int(splitted[2])
                    time = float(splitted[4][:-1])
                    f.times.append((hop_num,time))

                    if len(splitted) == 7:
                        ## zero hop, caller is 0
                        f.caller_nums.append((0,0))
                    else:
                        caller_num = int(splitted[7])
                        f.caller_nums.append((hop_num,caller_num))

                        if len(splitted) > 13:
                            timed_out_num = int(splitted[13])
                            if(timed_out_num > f.timed_out_caller_num):
                                f.timed_out_caller_num = timed_out_num
                elif eachline.startswith("confirmed UB:"):
                    indexes = eachline[eachline.find(":")+1:].strip().split(',')
                    func_id,blk_id,inst_id = int(indexes[0]),int(indexes[1]),int(indexes[2])

                    buggy_seed = lines[line_idx + 1]
                    if hop_num == None:
                        f.confirmed_bugs.append((0,buggy_seed,(func_id,blk_id,inst_id)))
                    else:
                        f.confirmed_bugs.append((hop_num + 1,buggy_seed,(func_id,blk_id,inst_id)))
            return f

        total_time = [0.0]*7
        total_seeds = [[0,0,0,0]for x in range(7)]
        total_timed_out = 0
        total_caller_nums = [0] * 7
        confirmed_bugs = []
        for each_root,_,files in os.walk(self.path_tras):
            if "result.txt" not in files:
                continue
            result_file = os.path.join(each_root,"result.txt")
            lines = []
            with open(result_file,"r") as rfile:
                lines = rfile.readlines()
            content_of_a_function = []
            front_cur = 0
            funcs = []
            assert(lines[0].startswith(start_of_function))
            for line_index in range(1,len(lines)):
                if lines[line_index].startswith(start_of_function):
                    f = process_func(lines[front_cur:line_index])
                    funcs.append(f)
                    front_cur = line_index
                line_index += 1
            f = process_func(lines[front_cur:line_index])
            funcs.append(f)
            for f in funcs:
                total_timed_out += f.timed_out_caller_num
                for hop_num,caller_num in f.caller_nums:
                    total_caller_nums[hop_num] += caller_num
                for hop_num,time in f.times:
                    total_time[hop_num] += time
                for hop_num,each_hop_seed in enumerate(f.seeds):
                    #embed()
                    total_seeds[hop_num][0] += each_hop_seed[0]
                    total_seeds[hop_num][1] += each_hop_seed[1]
                    total_seeds[hop_num][2] += each_hop_seed[2]
                    total_seeds[hop_num][3] += each_hop_seed[3]
                for each_confirmed_bug in f.confirmed_bugs:
                    confirmed_bugs.append((result_file[result_file.find("linux"):],f.func_name,each_confirmed_bug))
        print("time in second for each hop:",total_time)
        print("total seed for each hop:",total_seeds)
        print("timed_out callers:",total_timed_out)
        print("total callers:",total_caller_nums)

        if os.path.exists(bug_dir):
            shutil.rmtree(bug_dir) 
        os.mkdir(os.path.basename(bug_dir))
        cmds = []
        for idx,each_bug in enumerate(confirmed_bugs):
            result_file_name,func_name,(_,ub_handler,indexes) = each_bug 
            idx_str = str(idx)

            abs_path = os.path.join(PATH_TRAS,result_file_name)
            llvm_file_dir = os.path.dirname(abs_path)
            dbg_file_name = os.path.join(llvm_file_dir,"built-in-dbg.ll")
            normal_file_name = os.path.join(llvm_file_dir,"built-in.ll")

            module_name = llvm_file_dir.replace(self.path_tras,"").replace("/","-")
            cur_bug_dir = os.path.join(bug_dir,"{}.{}.{}".format(idx_str,module_name,func_name))
            os.mkdir(cur_bug_dir)
            bug_file = os.path.join(cur_bug_dir,"func-dbg.ll")
            cmd_dbg = "{} {} {} {} {} {} {}".format(PATH_PRINTFUNC,dbg_file_name,bug_file,func_name,indexes[0],indexes[1],indexes[2])
            cmds.append(cmd_dbg)

            bug_file = os.path.join(cur_bug_dir,"func.ll")
            cmd = "{} {} {} {} {} {} {}".format(PATH_PRINTFUNC,normal_file_name,bug_file,func_name,indexes[0],indexes[1],indexes[2])
            cmds.append(cmd)

        p = Pool(8,init_pool_worker)
        p.map(executeone,cmds)


        count = 0
        reported_bugs = []
        hash_funcs = ["__hash_32_generic","__hash_64_generic"]
        for bug_idx in range(len(os.listdir(bug_dir))):
            cur_bug_name = ''
            for each_bug_name in os.listdir(bug_dir):
                cur_id = each_bug_name.split(".")[0]
                if(int(cur_id) == bug_idx):
                    cur_bug_name = each_bug_name
            info_file = os.path.join(bug_dir,cur_bug_name,"func-dbg.ll")
            line = ''

            ## make sure no duplicate line are reported
            reported_before = False
            with open(info_file,"r") as rfile:
                line = rfile.readlines()[1]
                if(line in reported_bugs):
                    reported_before = True
                else:
                    reported_bugs.append(line)
            func_name = line.split("|")[1]
            if func_name in hash_funcs or reported_before:
                ##it's a hash function, so just supress it
                shutil.rmtree(os.path.join(bug_dir,cur_bug_name)) 
            else: 
                ## doesn't have to supress this, but needs to see if a new name is required
                if(bug_idx !=  count):
                    ## a new name is need
                    shutil.move(os.path.join(bug_dir,cur_bug_name), os.path.join(bug_dir,cur_bug_name.replace(str(bug_idx),str(count))))
                count += 1