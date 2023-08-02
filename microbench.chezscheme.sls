(library (microbench)
  (export
   dobench)
  (import
   (scheme))

  (define mb-current-time current-time)

  (define (mb-time-from t0)
    (let* ((dt (time-difference (current-time) t0))
	   (ns (+ (* (time-second dt) 1000000000) (time-nanosecond dt))))
      ns))

  (define mb-vector-sort vector-sort)
  
  (include "microbench.scm")
)  
  
