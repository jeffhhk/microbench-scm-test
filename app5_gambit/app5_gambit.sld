#! /usr/bin/scheme --script
#|
IIRC there is a way but I don't immediately recall how to get linux shebang to be 
agreeable to splitting command line arguments
  run with:
    env LD_LIBRARY_PATH=".:$LD_LIBRARY_PATH" /usr/bin/scheme --optimize-level 3 --script app3_chez.scm
|#

(import 
 (srfi 28)  ;; gambit
 (microbench))
(begin
  (define-macro (make-bytevector n) `(make-u8vector ,n))
  (define-macro (bytevector-u8-ref v i) `(u8vector-ref ,v ,i))
  (define-macro (bytevector-u8-set! v i x) `(u8vector-set! ,v ,i ,x))

  (define (mod x y) (modulo x y))
  (include "../app5_common.scm")
  )
