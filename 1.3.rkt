#lang racket

;; stuff from the book

(provide (all-defined-out))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x) (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (inc x) (+ x 1))

;; 1.29 
;; getting ridiculously small answers, looks like
;; a pretty exact copy of their described algorithm
;; to me.  Come back to it later.
;; Later:  Still not seeing the difference.  Must be
;; something really simple.

(define (simpsons f a b maybe-odd-n)
  (define n (if (even? maybe-odd-n) 
                maybe-odd-n
                (+ maybe-odd-n 1)))
  (define h (+ 0.0 (/ (- b a) n)))
  (define (prefix k) (cond ((= k 0) 1.0) 
                           ((= k n) 1.0)
                           ((even? k) 2.0)
                           (else 4.0)))
  (define (subterm k) (f (+ a 
                            (* k h))))
  (define (term k) (* (prefix k) (subterm k)))
  (/ (* h (sum term a inc b)) 3.0))


;; 1.30
(define (sumi term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))


;; 1.31
(define (product term a next b)
  (if (> a b) 
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial n)
  (product ident 1 inc n))

(define (ident x) x)

(define (pi-approx n)
  (define (frac x) (if (even? x)
                       (/ (+ 2.0 x) (+ 1.0 x))
                       (/ (+ 1.0 x) (+ 2.0 x))))
  (* 4.0 (product frac 1 inc n)))

(define (producti term a next b)
  (product-iter term a next b 1))

(define (product-iter term a next b acc)
  (if (> a b)
      acc
      (product-iter term (next a) next b (* (term a) acc))))


;; 1.32
;;
(define (accumulate combiner null-value term a next b)
  (define (iter a acc)
    (if (> a b)
        acc
        (iter (next a) (combiner (term a) acc))))
  (iter a null-value))

(define (accumulate-r combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-r combiner
                              null-value
                              term
                              (next a)
                              next
                              b))))
                   
;; 1.33
;;
(define (filtered-accumulate combiner null-value predicate term a next b)
  (define (iter a acc)
    (cond ((> a b) acc)
          ((not (predicate a))
           (iter (next a) acc))
          (else (iter (next a) 
                      (combiner (term a) acc)))))
  (iter a null-value))

(define (fun133b n)
  (define (relatively-prime-n? x)
    (= 1 (gcd n x)))
  (filtered-accumulate *
                       1
                       relatively-prime-n?
                       ident
                       1
                       inc
                       n))


;; 1.36 this average-damping version uses 9 guesses as compared to
;; 30-some for the undamped one.

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess n)
    (let ((next (/ (+ (f guess) guess) 2)))
      (display "guess ")
      (display n)
      (display " - ")
      (display guess)
      (newline)
      (if (close-enough? guess next)
          next
          (try next (+ n 1)))))
  (try first-guess 1))


;; 1.37
;; k = 10 gives an answer that's within 0.00005 of correct;
;; if we're truncating then we need k = 12

(define (cont-frac n d k)
  (define (cont-help start)
    (if (= start k)
        0
        (/ (n start) 
           (+ (d start)
              (cont-help (+ start 1))))))
  (cont-help 1))

;; I originally wrote cont-frac as a call to (cont-help 0),
;; while cont-frac-i counts from one, so they gave different results
;; for more complicated n & d functions.  Caught it when I tried 
;; substituting cont-frac-i into my answer to 1.38.

;; iterative version

(define (cont-frac-i n d k)
  (define (cont-help end acc)
    (if (= 0 end)
        acc
        (cont-help (- end 1)
                   (/ (n end)
                      (+ (d end)
                         acc)))))
  (cont-help k 0))

;; 1.38

(define (approx-e k)
  (define (denom n)
    (if (= 2 (remainder n 3))
        (* 2.0
           (/ (+ n 1)
              3))
        1.0))
  (+ 2
     (cont-frac (lambda (x) 1)
                denom
                k)))

;; stuff from the book

(define (average a b) (/ (+ a b) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (my-sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (cube x) (* x x x))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x)
            ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g)
               guess))

(define (square x) (* x x))

(define (sqrt2 x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

;; 1.40

(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a x x)
       (* b x)
       c)))

;; 1.41 
;; (((double (double double)) inc) x) adds 16 to x
;; ((double double) inc) would add 4 because
(define (double f)
  (lambda (x) (f (f x))))

;; 1.42
(define (compose f g)
  (lambda (x)
    (f (g x))))


;; 1.43

(define (repeated f n)
  (define (rep-helper i acc)
    (if (= i 1)
        acc 
        (rep-helper (- i 1)
                    (compose f acc))))
  (rep-helper n f))


;; 1.44 - untested
(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
    3)))

(define (repeated-smooth f n)
  ((repeated smooth n) f))

;; 1.46

(define (iterative-improve test improve)
  (define (ii-helper guess)
    (if (test guess)
        guess
        (ii-helper (improve guess))))
  (lambda (x) (ii-helper x)))

;; I'd probably actually use another define for the test
;; but we're doing lambdas so what the hell
(define (fixed-point-improved f)
  (define (improve guess)
    (average guess (f guess)))
  (iterative-improve (lambda (x)
                       (< (abs (- x 
                                  (improve x)))
                          0.00001))
                     improve))
