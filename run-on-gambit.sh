#!/bin/bash
adirProj=$( cd $( dirname "$0" ) && pwd )
set -euo pipefail
gsc -exe -nopreload \
    ../microbench/ \
    ../microbench/microbench/microbench.sld \
    app5_gambit/app5_gambit.sld
app5_gambit/app5_gambit
