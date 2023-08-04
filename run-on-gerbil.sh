#!/bin/bash
adirProj=$( cd $( dirname "$0" ) && pwd )
set -euo pipefail
gxc microbench/microbench_gerbil.ss
gxc -exe app5_gerbil.ss
./app5_gerbil
