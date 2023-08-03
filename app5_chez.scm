#! /usr/bin/scheme --script
#|
IIRC there is a way but I don't immediately recall how to get linux shebang to be 
agreeable to splitting command line arguments
  run with:
    env LD_LIBRARY_PATH=".:$LD_LIBRARY_PATH" /usr/bin/scheme --optimize-level 3 --script app3_chez.scm
|#

(import (microbench))

(include "app5_common.scm")

(load-shared-object "libc.so.6")
(define-ftype bvcopy_t (function (u8* u8* size_t) void))    ; make function type
(define bvcopy (ftype-ref
                bvcopy_t ()
                (make-ftype-pointer bvcopy_t "memmove")))    ; bind function
;; average 0.015ns per byte

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

(define (bvcopy-fxloop-write64 bv-dst bv-src n)
  ;; simplified to not deal with overlaps
  (let loop ((n (fx- n 1)))
    (if (fx< n 0)
        #f
        (begin
          (bytevector-u64-set! bv-dst n 0 (endianness little))
          (loop (fx- n 8))))))

(define (bvcopy-fxloop-read64 bv-dst bv-src n)
  ;; simplified to not deal with overlaps
  (let loop ((n (fx- n 1)))
    (if (fx< n 0)
        #f
        (begin
          (bytevector-u64-ref bv-src n (endianness little))
          (loop (fx- n 8))))))


(displayrun "bvcopy"
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

(displayrun "bvcopy-fxloop-read64"
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

(displayrun "bvcopy-fxloop-write64"
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

(define-ftype bvcopy2_t (function (uptr uptr size_t) void))    ; make function type
(define bvcopy2 (ftype-ref
                 bvcopy2_t ()
                 (make-ftype-pointer bvcopy2_t "memmove")))      ; bind function

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

#;(begin
;;     (display (format "~x vs ~x\n" (#%$object-address src 0) ptr-src))

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
)

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

;; how to see almost assembler

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
