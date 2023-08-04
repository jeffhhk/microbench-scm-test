
adirProj=$( cd $( dirname "$0" ) && pwd )
set -euo pipefail

gxi build_gerbil.ss
bin/app5_gerbil
