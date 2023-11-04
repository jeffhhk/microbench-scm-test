#!/bin/bash
adirProj=$( cd $( dirname "$0" ) && pwd )
set -euo pipefail
gsc -target js \
    -exe \
    -nopreload \
    microbench/ \
    microbench/microbench/microbench.sld \
    app5_gambit/app5_gambit.sld
node app5_gambit/app5_gambit
