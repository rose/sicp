#lang racket


;; 2.17 doesn't check for empty lists
(define (last-pair1 xs)
  (if (null? (cdr xs))
      xs
      (last-pair1 (cdr xs))))

;; 2.18
(define (reverse1 xs)
  (define (rev-helper remaining acc)
    (if (null? remaining)
        acc
        (rev-helper (cdr remaining) 
                    (cons (car remaining)
                          acc))))
  (rev-helper xs '()))

;; 2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else 
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define first-denomination car)
(define except-first-denomination cdr)
(define no-more? null?)

;; 2.20
(define (same-parity p . ps)
  (define r (remainder p 2))
  (define (sp-helper qs acc)
    (if (null? qs)
        acc
        (sp-helper (cdr qs)
                   (if (= r (remainder (car qs) 2))
                            (cons (car qs) acc)
                            acc))))
  (reverse (sp-helper (cons p ps) '())))


;; 2.23
(define (my-for-each f xs)
  (if (null? xs)
      #t
      (and (f (car xs))
           (my-for-each f (cdr xs)))))

;; 2.27
(define (deep-reverse xs)
  (define (rev-helper remaining acc)
    (cond ((null? remaining) acc)
          ((not (pair? remaining))
           remaining)
          (else 
           (rev-helper (cdr remaining)
                       (cons (rev-helper (car remaining) 
                                         '())
                             acc)))))
  (rev-helper xs '()))


;; 2.28
(define (fringe xs)
  (define (fringe-helper ys acc)
    (cond ((null? ys) acc)
          ((not (pair? ys))
           (cons ys acc))
          (else
           (append (fringe-helper (car ys) null)
                   (fringe-helper (cdr ys) acc)))))
  (fringe-helper xs null))

;; 2.29
(define (make-mobile left right)
  (list left right))
(define (make-branch len str)
  (list len str))

(define left-branch car)
(define (right-branch m) (car (cdr m)))
(define branch-length car)
(define (branch-structure b) 
  (if (pair? (cdr b))
      (car (cdr b))
      (cdr b)))

(define (total-weight mobile)
  (define (branch-weight b)
    (if (pair? (branch-structure b))
        (total-weight (branch-structure b))
        (branch-structure b)))
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))


(define (balanced? mobile)
  (define (branch-torque b)
    (if (pair? (branch-structure b))
        (* (branch-length b)
           (total-weight (branch-structure b)))
        (branch-structure b)))
  )
