#lang racket

(provide (all-defined-out))

(/ (+ 5 
      4
      (- 2
         (- 3
            (+ 6
               (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))


(define (sumsquare a b)
  (+ (* a a) (* b b)))


(define (proc13 a b c)
  (if (and (<= a b) (<= a c))
      (sumsquare b c)
      (if (< b c)
          (sumsquare a c)
          (sumsquare a b))))


;; 1.5 
;; It's an infinite loop with applicative order evaluation,
;; but works fine with normal order
;;
;; 1.6
;; Infinite loop again.  It keeps attempting to evaluate
;; the else terms before checking the predicates
;;

(define (improve guess x)
  (/ (+ (/ x
           (* guess guess))
        (* 2 guess))
      3))


;; tests whether guess^3 is within .1% of x
;; not what they specified in 1.7
(define (good-enough? guess x)
  (< (abs (/ (- (* guess guess guess) 
                x)
             x))
      0.001))


(define (cbrt-iter guess x)
  (if (good-enough? guess x)
   guess
   (cbrt-iter (improve guess x)
               x)))

(define (cbrt x) (cbrt-iter 1.0 x))
