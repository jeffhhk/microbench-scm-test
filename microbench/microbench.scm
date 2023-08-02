;; BEGIN microbench.chezscheme.sls flattened for racket

#;(define (current-time-chez)
  (current-time))

#;(define (current-time)
  (* 1000000 (current-inexact-monotonic-milliseconds)))

#;(define (time-from-chez t0)
  (let ((dt (time-difference (current-time) t0))
        (ns (+ (* (time-second dt) 1000000000) (time-nanosecond dt))))
    ns))

#;(define (time-from t0)
  (- (current-time) t0))

(define (range n)
  (define (iter n l)
    (if (< n 0)
        l
        (iter (- n 1) (cons n l))))
  (iter (- n 1) '()))

(define (quantiles xs0 symq symt)
  (define xs (mb-vector-sort > xs0))
  ;; seriously?  chez: (vector-sort > xs0)
  ;; racket: vector-sort xs0 >)
  (define (iter n ys)
    (let* ((nHalf (floor (/ n 2)))
           (y `((,symq ,(exact->inexact (- 1 (/ nHalf (vector-length xs))))) (,symt ,(vector-ref xs nHalf)))))
      (if (equal? nHalf 0)
          (reverse (cons y ys))
          (iter (floor (/ n 10)) (cons y ys)))))
  (iter (vector-length xs) '()))

  ;;(quantiles (list->vector (range 1000000)))

(define (dobench nWarmup nRuns nData gen f g)
  (define (run f x)
    (let* ((t0 (mb-current-time))
           (junk (f x))
           (ns (mb-time-from t0)))
      (exact->inexact (/ ns nData))))
  (define (trial x)
    (- (run g x) (run f x)))
  (let* ((x (gen nData))
         (junk2 (map (lambda (junk) (trial x)) (range nWarmup)))
         (ys (make-vector nRuns 0.0)))
    (do ((i 0 (+ i 1)))
        ((= i nRuns))
      (vector-set! ys i (trial x)))
    ;(display (format "ys=~a\n" ys))
    (quantiles ys 'p 'ns)))

;; END microbench.chezscheme.sls
