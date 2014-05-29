#lang racket

(provide (all-defined-out))


;; example in book seems more complicated.  Do they
;; just do it that way to get the calculations in the
;; same order as the recursive one, or am I missing
;; something important?
(define (fact-iter n sofar) 
  (if (= n 0) 
      sofar
      (fact-iter (- n 1) (* n sofar))))

(define (fact n) (fact-iter n 1))

;; 1.10: 
;; (A 0 n) = 2 * n
;; (A 1 n) = 2 ^ n
;; (A 2 n) = 2 ^ (A 2 (n-1)) 
;; If there's a nonrecursive way to describe that last 
;; one, other than 'bonkers', I don't know it.
;;

;; 1.11
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) 
         (* 2
            (f (- n 2)))
         (* 3
            (f (- n 3))))))

(define (f-iter a b c count)
  (if (<= count 0)
      a
      (f-iter b c (+ c
                     (* 2 b)
                     (* 3 a))
        (- count 1))))
                        
(define (fi n)
  (f-iter 0 1 2 n))

;;1.12
(define (pascal row column)
  (cond ((< row column) #f)
        ((or (= column 0) 
             (= column row))
         1)
        (else (+ (pascal (- row 1) 
                         column)
                 (pascal (- row 1)
                         (- column 1))))))


;;1.16: iterative and works, but blech.  I don't
;; understand the hint they gave.  Basically this
;; uses two log-time passes - base2list puts the
;; exponent into binary form, which power-iter
;; then uses for repeated squaring.
;; comments welcome.

(define (b2l-iter n acc)
  (cond ((= n 0) acc)
        ((even? n) (b2l-iter (/ n 2)
                             (cons 0 acc)))
        (else (b2l-iter (/ (- n 1) 2)
                        (cons 1 acc)))))

(define (base2list n)
  (b2l-iter n '()))

(define (square n) (* n n))

(define (power-iter b nlist acc)
  (cond ((null? nlist) acc)
        ((= (car nlist) 0) (power-iter b 
                                       (cdr nlist)
                                       (square acc)))
        (else (power-iter b
                          (cdr nlist)
                          (* b (square acc))))))

(define (power b n) 
  (power-iter b (base2list n) 1))


;; 1.18
(define (double n) (* n 2))
(define (halve n) (/ n 2))

(define (times-iter a blist acc)
  (cond ((null? blist) acc)
        ((= (car blist) 0) (times-iter a
                                       (cdr blist)
                                       (double acc)))
        (else (times-iter a
                          (cdr blist)
                          (+ a (double acc))))))


(define (times a b)
      (times-iter a (base2list b) 0))


;; 1.19
(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p)
                      (* q q))
                   (+ (* 2 p q)
                      (* q q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q)
                           (* a q)
                           (* a p))
                        (+ (* b p) 
                           (* a q))
                        p
                        q
                        (- count 1)))))
