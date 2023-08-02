#lang racket
(provide
 dobench)

(define (mb-current-time)
  (* 1000000 (current-inexact-monotonic-milliseconds)))

(define (mb-time-from t0)
  (- (mb-current-time) t0))

;(define (vector-sort op v) (vector-sort v op))
#;(let ((vector-sort (lambda (op v) (vector-sort v op))))
    (include "microbench.scm"))

(define (mb-vector-sort op v) (vector-sort v op))

(include "microbench.scm")
  
