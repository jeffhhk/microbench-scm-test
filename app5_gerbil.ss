#! /usr/bin/scheme --script
#|
IIRC there is a way but I don't immediately recall how to get linux shebang to be 
agreeable to splitting command line arguments
  run with:
    env LD_LIBRARY_PATH=".:$LD_LIBRARY_PATH" /usr/bin/scheme --optimize-level 3 --script app3_chez.scm
|#

(import 
 :std/format  ;; gambit
 :microbench/microbench_gerbil
 ;; alternative ./microbenchmark
 )
(export
 main)
(begin
  (define (mod x y) (modulo x y))
  (define bytevector-length u8vector-length)
  (define bytevector-u8-ref u8vector-ref)
  (define bytevector-u8-set! u8vector-set!)
  (define make-bytevector make-u8vector)
  (define (main)
    (include "./app5_common.scm")
  ))
