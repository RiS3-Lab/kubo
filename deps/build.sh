#!/bin/bash
cd entry_analysis && ./build.sh && cd ..
cd fmt && ./build.sh && cd ..
cd pex && ./build.sh && cd ..
cd print_func && ./build.sh && cd ..
cd taintSummary && ./build.sh && cd ..
