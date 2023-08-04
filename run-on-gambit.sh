#!/bin/bash
adirProj=$( cd $( dirname "$0" ) && pwd )
set -euo pipefail
gsc -prelude '(declare (standard-bindings) (block) (not safe))(define-macro (make-bytevector n) `(make-u8vector ,n))(define-macro (bytevector-u8-ref v i) `(u8vector-ref ,v ,i))(define-macro (bytevector-u8-set! v i x) `(u8vector-set! ,v ,i ,x))' \
    -exe -nopreload \
    microbench/ \
    microbench/microbench/microbench.sld \
    app5_gambit/app5_gambit.sld
app5_gambit/app5_gambit
