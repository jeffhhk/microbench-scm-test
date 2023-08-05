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

  #|
  Replacing the defines above with these define-macros:

    (define-macro (make-bytevector n) `(make-u8vector ,n))
    (define-macro (bytevector-u8-ref v i) `(u8vector-ref ,v ,i))
    (define-macro (bytevector-u8-set! v i x) `(u8vector-set! ,v ,i ,x))
    (define-macro (bytevector-length v) `(u8vector-length ,v))


  causes compilation failure:

    *** ERROR IN gx#core-expand-ref% -- Syntax Error
    *** ERROR IN "app5_common.scm"@103.47
    --- Syntax Error: Reference to unbound identifier
    ... form:   (%#ref bytevector-length)
    ... detail: bytevector-length at "app5_common.scm"@103.47

  Adding (import :gerbil/gambit) does not help.

  Adding ## before right hand sides also does not help

  |#

  (define (main)
    (include "./app5_common.scm")
  ))
