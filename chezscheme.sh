#!/bin/bash
adirProj=$( cd $( dirname "$0" ) && pwd )
export adirProj
export CHEZSCHEMELIBDIRS="$adirProj/microbench:$adirProj:$CHEZSCHEMELIBDIRS"
# Run Chez
exec scheme "$@"
