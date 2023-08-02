#lang racket
;(include "microbench.mzscheme.scm")
(require "../microbench/microbench.rkt")
(require racket/control)
(require racket/unsafe/ops)

(define make-bytevector make-bytes)
(define bytevector-u8-set! bytes-set!)
(define bytevector-u8-ref bytes-ref)
(define bytevector-length bytes-length)
(define (mod x y) (modulo x y))
(define fx< unsafe-fx<)
(define fx- unsafe-fx-)

(include "app5_common.scm")

; https://github.com/webyrd/mediKanren/blob/master/medikanren2/neo/dbKanren/dbk/enumerator.rkt#L32
(define ((enumerator->s en))
  (define tag (make-continuation-prompt-tag))
  (reset-at tag
            (en (lambda (x)
                  (shift-at tag k (cons x (lambda () (k (void)))))))
            '()))

(define (count-push n)
  (lambda (yield)
    (let loop ((i 0))
      (if (>= i n)
          #f
          (begin
            (yield i)
            (loop (+ i 1)))))))

(displayrun "consume enumerator->s"
	    2 3000 25000
	    (lambda (nData) (enumerator->s (count-push nData)))
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


  
