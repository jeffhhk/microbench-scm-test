
(define (displayrun name nWarmup nRuns nData gen f g)
  ;;(display (format "about to run name=~a\n" name))
  (let ((qs (dobench nWarmup nRuns nData gen f g)))
    (display (format "~s\n"
		     `(,name ,qs)))))

;; Data generators for different types
(define (range n)
  (define (iter n l)
    (if (< n 0)
	l
	(iter (- n 1) (cons n l))))
  (iter (- n 1) '()))

(define (range-vector n)
  (define v (make-vector n))
  (define (iter n)
    (if (< n 0)
	v
	(begin
	  (vector-set! v n n)
	  (iter (- n 1)))))
  (iter (- n 1)))

(define (range-bytevector n)
  (define v (make-bytevector n))
  (define (iter n)
    (if (< n 0)
	v
	(begin
	  (bytevector-u8-set! v n (mod n 256))
	  (iter (- n 1)))))
  (iter (- n 1)))


;; bvcopy-*: Deconstructing what Chez calls bytevector-copy! (Similar to C memmove)
;; using bytevector primitives.
(define (bvcopy-loop bv-dst bv-src n)
  ;; simplified to not deal with overlaps
  (let loop ((n (- n 1)))
    (if (< n 0)
	#f
	(begin
	  (bytevector-u8-set! bv-dst n (bytevector-u8-ref bv-src n))
	  (loop (- n 1))))))

(define (bvcopy-loop-noop bv-dst bv-src n)
  ;; simplified to not deal with overlaps
  (let loop ((n (- n 1)))
    (if (< n 0)
	#f
	(begin
	  (loop (- n 1))))))

(define (bvcopy-fxloop bv-dst bv-src n)
  ;; simplified to not deal with overlaps
  (let loop ((n (fx- n 1)))
    (if (fx< n 0)
	#f
	(begin
	  (bytevector-u8-set! bv-dst n (bytevector-u8-ref bv-src n))
	  (loop (fx- n 1))))))

(define (bvcopy-fxloop-write bv-dst bv-src n)
  ;; simplified to not deal with overlaps
  (let loop ((n (fx- n 1)))
    (if (fx< n 0)
	#f
	(begin
	  (bytevector-u8-set! bv-dst n 0)
	  (loop (fx- n 1))))))

(define (bvcopy-fxloop-read bv-dst bv-src n)
  ;; simplified to not deal with overlaps
  (let loop ((n (fx- n 1)))
    (if (fx< n 0)
	#f
	(begin
	  (bytevector-u8-ref bv-src n)
	  (loop (fx- n 1))))))

(define (bvcopy-fxloop-noop bv-dst bv-src n)
  ;; simplified to not deal with overlaps
  (let loop ((n (fx- n 1)))
    (if (fx< n 0)
	#f
	(begin
	  (loop (fx- n 1))))))


(displayrun "bvcopy-loop"
	    2 3000 200000
	    (lambda (nData) (cons (range-bytevector nData) (range-bytevector nData)))
	    (lambda (data)
	      (define data-dst (car data))
	      (define data-src (cdr data))
	      #f
	      )
	    (lambda (data)
	      (define data-dst (car data))
	      (define data-src (cdr data))
	      (bvcopy-loop data-dst data-src (bytevector-length data-dst))
	      ))

(displayrun "bvcopy-loop-noop"
	    2 3000 200000
	    (lambda (nData) (cons (range-bytevector nData) (range-bytevector nData)))
	    (lambda (data)
	      (define data-dst (car data))
	      (define data-src (cdr data))
	      #f
	      )
	    (lambda (data)
	      (define data-dst (car data))
	      (define data-src (cdr data))
	      (bvcopy-loop-noop data-dst data-src (bytevector-length data-dst))
	      ))

(displayrun "bvcopy-fxloop-noop"
	    2 3000 200000
	    (lambda (nData) (cons (range-bytevector nData) (range-bytevector nData)))
	    (lambda (data)
	      (define data-dst (car data))
	      (define data-src (cdr data))
	      #f
	      )
	    (lambda (data)
	      (define data-dst (car data))
	      (define data-src (cdr data))
	      (bvcopy-fxloop-noop data-dst data-src (bytevector-length data-dst))
	      ))

(displayrun "bvcopy-fxloop-read"
	    2 3000 200000
	    (lambda (nData) (cons (range-bytevector nData) (range-bytevector nData)))
	    (lambda (data)
	      (define data-dst (car data))
	      (define data-src (cdr data))
	      #f
	      )
	    (lambda (data)
	      (define data-dst (car data))
	      (define data-src (cdr data))
	      (bvcopy-fxloop-read data-dst data-src (bytevector-length data-dst))
	      ))

(displayrun "bvcopy-fxloop-write"
	    2 3000 200000
	    (lambda (nData) (cons (range-bytevector nData) (range-bytevector nData)))
	    (lambda (data)
	      (define data-dst (car data))
	      (define data-src (cdr data))
	      #f
	      )
	    (lambda (data)
	      (define data-dst (car data))
	      (define data-src (cdr data))
	      (bvcopy-fxloop-write data-dst data-src (bytevector-length data-dst))
	      ))

(displayrun "bvcopy-fxloop-all"
	    2 3000 200000
	    (lambda (nData) (cons (range-bytevector nData) (range-bytevector nData)))
	    (lambda (data)
	      (define data-dst (car data))
	      (define data-src (cdr data))
	      #f
	      )
	    (lambda (data)
	      (define data-dst (car data))
	      (define data-src (cdr data))
	      (bvcopy-fxloop data-dst data-src (bytevector-length data-dst))
	      ))


;; consume generator: counting in the style of SRFI 158: Generators and Accumulators
(define (make-gen-count n)
  (lambda ()
    (define i 0)
    (lambda ()
      (cond
       ((< i n)
	(let ((j i))
	  (set! i (+ i 1))
	  j))
       (else
	#f)))))
	      

(displayrun "consume generator"
	    2 3000 25000
	    (lambda (nData) (make-gen-count nData))
	    (lambda (data)
	      #f)
	    (lambda (data)
	      (let ((gen (data)))
		(let loop ()
		  (let ((x (gen)))
		    (when x
		      (loop)))))))

#| Counting via streams as streams appear in:

    Jason Hemann and Dan Friedman.µkanren: A minimal
    functional core for relational programming. November 2013.
    http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf .

I do not know if said paper is the first use of streams in this
format or if they were taken from elsewhere |#

(define (make-s-count n)
  (let loop ((i 0))
    (lambda ()
      (if (>= i n)
	  '()
	  (cons i (loop (+ i 1)))))))

(displayrun "consume stream"
	    2 3000 25000
	    (lambda (nData) (make-s-count nData))
	    (lambda (data)
	      #f)
	    (lambda (data)
	      (let loop ((i 0)
			 (data data))
		(cond
		 ((procedure? data) (loop i (data)))
		 ((pair? data) (loop (+ i 1) (cdr data)))
		 ((null? data) i)
		 (else `(unrecognized stream ,data))))))
		 
(displayrun "consume stream 2"
	    2 3000 25000
	    (lambda (nData) (make-s-count nData))
	    (lambda (data)
	      #f)
	    (lambda (data)
	      (let loop ((data data))
		(cond
		 ((procedure? data) (loop (data)))
		 ((pair? data) (loop (cdr data)))
		 ;((null? data) #f)
		 (else #f)))))


;; Yield in call/cc from:
;;   https://matt.might.net/articles/programming-with-continuations--exceptions-backtracking-search-threads-generators-coroutines/
;;   https://matt.might.net/articles/programming-with-continuations--exceptions-backtracking-search-threads-generators-coroutines/generators.scm
(define (current-continuation) 
  (call-with-current-continuation
   (lambda (cc)
     (cc cc))))

(define (make-yield for-cc)
  (lambda (value)
    (let ((cc (current-continuation)))
      (if (procedure? cc)
          (for-cc (cons cc value))
          (void)))))

; (for v in generator body) will execute body 
; with v bound to successive values supplied
; by generator.
(define-syntax for
  (syntax-rules (in)
    ((_ v in iterator body ...)
     ; => 
     (let ((i iterator)
           (iterator-cont #f))
       (letrec ((loop (lambda ()
                        (let ((cc (current-continuation)))
                          (if (procedure? cc)
                              (if iterator-cont
                                  (iterator-cont (void))
                                  (iterator (make-yield cc)))
                              (let ((it-cont (car cc))
                                    (it-val  (cdr cc)))
                                (set! iterator-cont it-cont)
                                (let ((v it-val))
                                  body ...)
                                (loop)))))))
         (loop))))))

(define (make-coroutine-count n)
  (lambda (yield)
    (let loop ((i 0))
      (if (>= i n)
	  #f
	  (begin
	    (yield i)
	    (loop (+ i 1)))))))


(displayrun "consume coroutine"
	    2 3000 25000
	    (lambda (nData) (make-coroutine-count nData))
	    (lambda (data)
	      #f)
	    (lambda (data)
	      (for i in data
		   #f)))


;; Yield in call/cc from:
;;   https://stackoverflow.com/questions/30614788/implement-yield-and-send-in-scheme
;;   Chez-only call1/cc available as "consume coroutine4" in app5_chez.scm
(define (make-generator3 procedure)
  (define last-return values)
  (define last-value #f)
  (define (last-continuation _) 
    (let ((result (procedure yield))) 
      (last-return result)))

  (define (yield value)
    (call/cc (lambda (continuation)
               (set! last-continuation continuation)
               (set! last-value value)
               (last-return value))))

  (lambda args
    (call/cc (lambda (return)
               (set! last-return return)
               (if (null? args)
                   (last-continuation last-value)
                   (apply last-continuation args))))))



(define (make-gen3-count n)
  (make-generator3
   (lambda (yield)
     (let loop ((n (- n 1)))
       (if (>= n 0)
	   (begin
	     (yield n)
	     (loop (- n 1)))
	   #f)))))


(displayrun "consume coroutine3"
	    2 3000 25000
	    (lambda (nData) (lambda () (make-gen3-count nData)))
	    (lambda (data)
	      #f)
	    (lambda (data)
	      (let ((gen (data)))
		(let loop ()
		  (let ((x (gen)))
		    (if x
			(loop)
			#f))))))


