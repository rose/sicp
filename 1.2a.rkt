#lang racket

(provide (all-defined-out))

;; 1.24 Using fermat's probabilistic test, 5 samples
;; 1009, 1013, 1019, about 20 microseconds each
;; 10007, 10009, 10037, about 24 microseconds each
;; 100003, 100019, 100043, about 27 microseconds each
;; 1000003, 1000033, 1000037, about 32 microseconds each
;; probably there's ~16 microseconds of overhead
;;

(define (search-for-primes start end)
  (if (even? start)
      (odd-search-for-primes (+ start 1) end)
      (odd-search-for-primes start end)))

(define (odd-search-for-primes start end)
  (if (>= start end) 
      (display "")
      (and (timed-prime-test start) 
           (search-for-primes (+ 2 start) end))))

(define (square n) (* n n))

(define (runtime) (current-inexact-milliseconds))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))
      (display "")))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))

(define (prime? n)
  (fast-prime? n 5))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (fermat-test n)
  (define (try-it a) (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))


;; 1.22
;;

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))
