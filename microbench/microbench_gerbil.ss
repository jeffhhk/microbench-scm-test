(import :gerbil/gambit
        :std/error
        :std/misc/list
        :std/sugar
        :std/format)
(export
 dobench)

(define (mb-current-time)
  (time->seconds (current-time)))
(define (mb-time-from t0)
  (* 1000000000 (- (time->seconds (current-time)) t0)))
(define (mb-vector-sort op v)
  (list->vector
   (##list-sort op
              (vector->list v))))

(include "microbench.scm")
