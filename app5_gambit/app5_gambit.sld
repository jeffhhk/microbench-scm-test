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
  (define (mod x y) (modulo x y))
  (include "../app5_common.scm")
  )
