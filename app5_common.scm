;;(display "hello\n")

(define (displayrun name nWarmup nRuns nData gen f g)
  ;;(display (format "about to run name=~a\n" name))
  (let ((qs (dobench nWarmup nRuns nData gen f g)))
    (display (format "~s\n"
		     `(,name ,qs)))))

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

;; tested configuration:
;;   100,000 byte array
;;   3000 repetition
;;   50th percentile
;; driver code available upon request

#|
(load-shared-object "libc.so.6")
(define-ftype bvcopy_t (function (u8* u8* size_t) void))    ; make function type
(define bvcopy (ftype-ref
		bvcopy_t ()
		(make-ftype-pointer bvcopy_t "memcpy")))    ; bind function
;; average 0.015ns per byte
|#

(define (bvcopy-loop bv-dst bv-src n)
  ;; simplified to not deal with overlaps
  (let loop ((n (- n 1)))
    (if (< n 0)
	#f
	(begin
	  (bytevector-u8-set! bv-dst n (bytevector-u8-ref bv-src n))
	  (loop (- n 1))))))
;; average 3.5ns per byte

;;(/ 3.51027 0.01536)
;; => 228.53320312499997  ; times faster

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
;; average 3.3ns per byte

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

#;(define (bvcopy-fxloop-write64 bv-dst bv-src n)
  ;; simplified to not deal with overlaps
  (let loop ((n (fx- n 1)))
    (if (fx< n 0)
	#f
	(begin
	  (bytevector-u64-set! bv-dst n 0 (endianness little))
	  (loop (fx- n 8))))))

#;(define (bvcopy-fxloop-read64 bv-dst bv-src n)
  ;; simplified to not deal with overlaps
  (let loop ((n (fx- n 1)))
    (if (fx< n 0)
	#f
	(begin
	  (bytevector-u64-ref bv-src n (endianness little))
	  (loop (fx- n 8))))))


#;(displayrun "bvcopy"
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
	      (bvcopy data-dst data-src (bytevector-length data-dst))
	      ))

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

#;(displayrun "bvcopy-fxloop-read64"
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
	      (bvcopy-fxloop-read64 data-dst data-src (bytevector-length data-dst))
	      ))

#;(displayrun "bvcopy-fxloop-write64"
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
	      (bvcopy-fxloop-write64 data-dst data-src (bytevector-length data-dst))
	      ))

#|
(define-ftype bvcopy2_t (function (uptr uptr size_t) void))    ; make function type
(define bvcopy2 (ftype-ref
		bvcopy2_t ()
		(make-ftype-pointer bvcopy2_t "memcpy")))      ; bind function

(define (bytevector-copy2! src src-start dst dst-start n)
  ;; https://github.com/cisco/ChezScheme/issues/257
  (with-interrupts-disabled
   (let ((ptr-src (#%$object-address src (+ (foreign-sizeof 'ptr) 1 src-start)))
	 (ptr-dst (#%$object-address dst (+ (foreign-sizeof 'ptr) 1 dst-start))))
     (bvcopy2 ptr-dst ptr-src n))))

(define (vector-copy! src src-start dst dst-start n)
  ;; https://github.com/cisco/ChezScheme/issues/257
  (with-interrupts-disabled
   (let ((ptr-src (#%$object-address src (+ 1                     ; clear tag
					    (foreign-sizeof 'ptr)
					    (* (foreign-sizeof 'ptr) src-start))))
	 (ptr-dst (#%$object-address dst (+ 1                     ; clear tag
					    (foreign-sizeof 'ptr)
					    (* (foreign-sizeof 'ptr) dst-start)))))
     (bvcopy2 ptr-dst ptr-src (* (foreign-sizeof 'ptr) n)))))

;;     (display (format "~x vs ~x\n" (#%$object-address src 0) ptr-src))

(define (vector-copy-almost1! src src-start dst dst-start n)
  ;; https://github.com/cisco/ChezScheme/issues/257
  (with-interrupts-disabled
   (let ((ptr-src (#%$object-address src (+ 1                     ; clear tag
					    (foreign-sizeof 'ptr)
					    (* (foreign-sizeof 'ptr) src-start))))
	 (ptr-dst (#%$object-address dst (+ 1                     ; clear tag
					    (foreign-sizeof 'ptr)
					    (* (foreign-sizeof 'ptr) dst-start)))))
     (cons ptr-src ptr-dst))))

(define (vector-copy-almost2! src src-start dst dst-start n)
  ;; https://github.com/cisco/ChezScheme/issues/257
  (with-interrupts-disabled
   (let ((ptr-src (#%$object-address src (+ 1                     ; clear tag
					    (foreign-sizeof 'ptr)
					    (* (foreign-sizeof 'ptr) src-start))))
	 (ptr-dst (#%$object-address dst (+ 1                     ; clear tag
					    (foreign-sizeof 'ptr)
					    (* (foreign-sizeof 'ptr) dst-start)))))
     (bvcopy2 ptr-dst ptr-src 0))))


(define (pad-with-00-v1 bv)
  (define bv2 (make-bytevector (+ 4 (bytevector-length bv)) 0))
  (bytevector-copy! bv 0 bv2 2 (bytevector-length bv))
  bv2)

;;(pad-with-00-v1 (bytevector 1 2 3))

(define (pad-with-00-v2 bv)
  (define bv2 (make-bytevector (+ 4 (bytevector-length bv)) 0))
  (bytevector-copy2! bv 0 bv2 2 (bytevector-length bv))
  bv2)

;;(pad-with-00-v2 (bytevector 1 2 3))

(define (vector-pad-with-00 v)
  (define v2 (make-vector (+ 4 (vector-length v)) 0))
  (vector-copy! v 0 v2 2 (vector-length v))
  v2)

(vector-pad-with-00 (vector -1 -2 -3))


(displayrun "bytevector-copy!"
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
	      (let ((n (bytevector-length data-dst)))
		(bytevector-copy! data-dst 0 data-src 0 n))
	      ))

(displayrun "bytevector-copy2!"
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
	      (let ((n (bytevector-length data-dst)))
		(bytevector-copy2! data-dst 0 data-src 0 n))
	      ))

(displayrun "vector-copy!"
	    2 3000 25000
	    (lambda (nData) (cons (range-vector nData) (range-vector nData)))
	    (lambda (data)
	      (define data-dst (car data))
	      (define data-src (cdr data))
	      #f
	      )
	    (lambda (data)
	      (define data-dst (car data))
	      (define data-src (cdr data))
	      (let ((n (vector-length data-dst)))
		(vector-copy! data-dst 0 data-src 0 n))
	      ))
|#
#;(parameterize ([optimize-level 3]
               (#%$assembly-output #t))
  (compile
   '(define (bvcopy-fxloop bv-dst bv-src n)
      ;; simplified to not deal with overlaps
      (let loop ((n (fx- n 1)))
        (if (fx< n 0)
            #f
            (begin
              (bytevector-u8-set! bv-dst n (bytevector-u8-ref bv-src n))
              (loop (fx- n 1))))))))


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

(define (current-continuation1)
  (display "cc1\n")
  (call-with-current-continuation
   (lambda (cc)
     (cc cc))))

(define (current-continuation2) 
  (display "cc2\n")
  (call-with-current-continuation
   (lambda (cc)
     (cc cc))))

(define (make-yield2 for-cc)
  (lambda (value)
    (let ((cc (current-continuation1)))
      (if (procedure? cc)
          (for-cc (cons cc value))
          (void)))))

; (for v in generator body) will execute body 
; with v bound to successive values supplied
; by generator.
(define-syntax for2
  (syntax-rules (in)
    ((_ v in iterator body ...)
     ; => 
     (let (;(i iterator)
           (iterator-cont #f))
       (letrec ((loop (lambda ()
                        (let ((cc (current-continuation2)))
                          (if (procedure? cc)
                              (if iterator-cont
				  (begin
				    (display "have cc. about to iterator-cont\n")
				    (iterator-cont (void)))
				  (begin
				    (display "have cc. about to make-yield\n")
				    (iterator (make-yield2 cc))))
                              (let ((it-cont (car cc))
                                    (it-val  (cdr cc)))
				(display (format "for-cc: ~a iterator-cont=~a\n" it-val iterator-cont))
                                (set! iterator-cont it-cont)
                                (let ((v it-val))
                                  body ...)
                                (loop)))))))
         (loop))))))

#;(for2 i in (make-coroutine-count 3)
      (display (format "i=~a\n" i)))

(define-syntax for2b
  (syntax-rules (in)
    ((_ v in iterator body ...)
     ; => 
     (let (;(i iterator)
           (iterator-cont #f))
       (letrec ((loop (lambda ()
                        (let ((cc (current-continuation2)))
                          (if (procedure? cc)
                              (if (not iterator-cont)
				  (begin
				    (display "have cc. about to make-yield\n")
				    (iterator (make-yield2 cc)))
				  (begin
				    (display "have cc. about to iterator-cont\n")
				    (iterator-cont (void))))
                              (let ((it-cont (car cc))
                                    (it-val  (cdr cc)))
				(display (format "for-cc: ~a iterator-cont=~a\n" it-val iterator-cont))
                                (set! iterator-cont it-cont)
                                (let ((v it-val))
                                  body ...)
                                (loop)))))))
         (loop))))))

#;(for2b i in (make-coroutine-count 3)
      (display (format "i=~a\n" i)))


0

;; https://stackoverflow.com/questions/30614788/implement-yield-and-send-in-scheme
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


#;(begin
    (define test 
    (make-generator3
    (lambda (collect)
        (collect 1)
        (collect 5)
        (collect 10)
        #f)))

    (test)
    (test)
    (test)
    (test))

;; There is some kind of syntax error in this part of the stackoverflow answer.  We don't need it.
#;(define-syntax (define-coroutine stx)
  (syntax-case stx ()
    ((_ (name . args) . body )
     #`(define (name . args)
         (make-generator3 
          (lambda (#,(datum->syntax stx 'yield))
            . body))))))

#;(define-coroutine_try1 (countdown-from n)
  (let loop ((n n))
    (if (= n 0)
        0
        (loop (- (yield n) 1)))))

(define (make-gen3-count n)
  (make-generator3
   (lambda (yield)
     (let loop ((n (- n 1)))
       (if (>= n 0)
	   (begin
	     (yield n)
	     (loop (- n 1)))
	   #f)))))


#;(begin
    (define gen3-count-from-10 (make-gen3-count 3))
    (gen3-count-from-10)
    (gen3-count-from-10)
    (gen3-count-from-10)
    (gen3-count-from-10)
    (gen3-count-from-10))

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


#|
;; call/1cc

(define (make-generator4 procedure)
  (define last-return values)
  (define last-value #f)
  (define (last-continuation _) 
    (let ((result (procedure yield))) 
      (last-return result)))

  (define (yield value)
    (call/1cc (lambda (continuation)
               (set! last-continuation continuation)
               (set! last-value value)
               (last-return value))))

  (lambda args
    (call/1cc (lambda (return)
               (set! last-return return)
               (if (null? args)
                   (last-continuation last-value)
                   (apply last-continuation args))))))

(define (make-gen4-count n)
  (make-generator4
   (lambda (yield)
     (let loop ((n (- n 1)))
       (if (>= n 0)
	   (begin
	     (yield n)
	     (loop (- n 1)))
	   #f)))))

(displayrun "consume coroutine4"
	    2 3000 25000
	    (lambda (nData) (lambda () (make-gen4-count nData)))
	    (lambda (data)
	      #f)
	    (lambda (data)
	      (let ((gen (data)))
		(let loop ()
		  (let ((x (gen)))
		    (if x
			(loop)
			#f))))))
|#
