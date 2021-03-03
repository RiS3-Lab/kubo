<img src="kubo.png" alt="drawing" width="200"/>

KUBO: precise and scalable static UB detector for the OS Kernel


Requirement:

- Ubuntu 16.04, 18.04, 20.04
- Python3
    - 3rd party packages:networkx, matplotlib, argparse, termcolor,ipython
- cmake

Init
----

- LLVM 9.0                           : cd llvm && ./init.sh 
    - this will prepare a pre-built as well as a natively built llvm 9.0 since we modify some of the source code
- All sorts of dependent projects    : cd deps && ./build.sh
- KUBO pass                          : cd work && python llvm.py build -c


Kernel
------

1. prepare

- Download Linux Source          : python main.py checkout(default to 5.4.1)
- Config Linux Source            : python main.py config
- Build the linux binary         : python main.py build
- Parse build procedure          : python main.py parse
- Build llvm bc                  : python main.py irgen
- Group into modules             : python main.py group
- Optimize and LTO               : python main.py trans
- Generate call graph            : python main.py gen_cg
- syscall/ioctl entry analysis   : python main.py entry_ana
- data summary generation        : python main.py taint_ana

2. generate bc with debug symbol so that we can map reported bugs to source code automatically when generating the bug reports

- Build llvm bc                  : python main.py gen_dbg_ir
- Group into modules             : python main.py gen_dbg_group
- Optimize and LTO               : python main.py gen_dbg_trans

3. the actual analysis

- Run kubo                       : python main.py run
- generate bug reports           : python main.py stat

4. see ./work/bugs for the bug reports


Credit:
----
This work is built on other amazing works
specifically

- For call graph analysis: https://github.com/lzto/pex
- For slicing and symbolic execution: https://github.com/sslab-gatech/deadline/
- For taint analysis: https://github.com/ucsb-seclab/dr_checker

Shout out to their amazing contributions that made this possible.

Q & A
----
Should you have any question, feel free to raise an issue in this repo or directly contact the author at liu.changm@northeastern.edu.
It's intended that this project to be actively maintained for a period of time, mainly for readability improvement and performance fine-tuning.
