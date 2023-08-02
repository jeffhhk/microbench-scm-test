(define-library (microbench)
  (import
    (gambit))
  (export dobench)
  (begin
    (define (hello-library)
      (display "hello gambit library\n"))
    
    (define (mb-current-time)
      (time->seconds (current-time)))
    (define (mb-time-from t0)
      (* 1000000000 (- (time->seconds (current-time)) t0)))
    (define (mb-vector-sort op v)
      (list->vector
        (list-sort op
          (vector->list v))))
    )
  (include "../microbench.scm")
  )
