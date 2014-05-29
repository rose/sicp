#lang racket

;; from book

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))


(define numer car)
(define denom cdr)

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x)))

;; 2.1
(define (make-rat x y)
  (define common (if (< 0 y)
                     (gcd x y)
                     (* -1 (gcd x y))))
  (cons (/ x common)
        (/ y common)))

;; 2.2
(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment start end) (cons start end))

(define (start-segment seg) (car seg))

(define (end-segment seg) (cdr seg))

(define (make-point x y) (cons x y))

(define (x-point p) (car p))

(define (y-point p) (cdr p))
  
(define (mids-pointegment seg)
  (make-point (/ (+ (x-point (start-segment seg))
                    (x-point (end-segment seg)))
                 2.0)
              (/ (+ (y-point (start-segment seg))
                    (y-point (end-segment seg)))
                 2.0)))

;; some helpers for 2.3

;; (mids-pointegment seg) == (end-segment (multiply-segment seg 0.5)) 

(define (delta-x seg)
  (- (x-point (end-segment seg))
     (x-point (start-segment seg))))
  
(define (delta-y seg)
  (- (y-point (end-segment seg))
     (y-point (start-segment seg))))

(define (multiply-segment seg n)
  (let* ((s (start-segment seg))
        (new-x (+ (* (delta-x seg)
                     n)
                  (x-point s)))
        (new-y (+ (* (delta-y seg)
                     n)
                  (y-point s)))
        (new-end (make-point new-x new-y)))
   (make-segment s new-end)))
                 
(define (len seg)
  (sqrt (+ (sqr (delta-x seg))
           (sqr (delta-y seg)))))

(define (unit seg)
  (multiply-segment seg (/ 1.0 (len seg))))

;; clockwise around starting point
(define (rotate seg)
  (define start (start-segment seg))
  (make-segment start
                (make-point (+ (x-point start) 
                               (delta-y seg))
                            (- (y-point start)
                               (delta-x seg)))))

;; 2.3 
;; I have a feeling they meant us to only make rectangles
;; perpendicular to the axes; this was getting pretty involved
;; and I didn't have time to come back to it

(define (area rect)
  (* (width rect)
     (height rect)))


(define (perimeter rect)
  (* 2 (+ (width rect)
          (height rect))))


(define (width a) 7)
(define (height a) 7)

;; 2.4

(define (cons1 x y)
  (lambda (m) (m x y)))

(define (car1 z)
  (z (lambda (p q) p)))

(define (cdr1 z)
  (z (lambda (p q) q)))


;; 2.5

(define (power b e) (if (= e 0) 1 (* b (power b (- e 1)))))

(define (cons2 x y)
  (* (power 2 x)
     (power 3 y)))

(define (car2 z)
  (if (= 0 (remainder z 2))
      (+ 1 (car2 (/ z 2)))
      0))

(define (cdr2 z)
  (if (= 0 (remainder z 3))
  (+ 1 (cdr2 (/ z 3)))
  0))

;; 2.6, which melted my brain 
;; I will need to look up how to do addition
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one 
  (lambda (f)
    (lambda (x) (f x))))

;; from book
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
  (make-interval (min p1 p2 p3 p4)
                 (max p1 p2 p3 p4))))

;; 2.10
(define (div-interval x y)
  (if (>= 0 (* (lower-bound y) (upper-bound y)))
      (display "crap that's not going to work")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                  (/ 1.0 (lower-bound y))))))

;; 2.7
(define (make-interval x y) 
  (if (< x y) 
      (cons x y)
      (cons y x)))

(define upper-bound cdr)
(define lower-bound car)

;; 2.8

(define (sub-interval x y)
  (add-interval x
                (make-interval (* -1.0 (upper-bound y))
                               (* -1.0 (lower-bound y)))))


;; 2.11 possibly more efficient but blech
(define (mul-interval2 x y)
  (let* ((lo-x (lower-bound x))
         (up-x (upper-bound x))
         (lo-y (lower-bound y))
         (up-y (upper-bound y))
         (both-neg-x (and (not (positive? lo-x))
                          (not (positive? up-x))))
         (both-pos-x (and (not (negative? lo-x))
                          (not (negative? up-x))))
         (both-neg-y (and (not (positive? lo-y))
                          (not (positive? up-y))))
         (both-pos-y (and (not (negative? lo-y))
                          (not (negative? up-y)))))
  (cond ((and both-neg-x both-neg-y)
          (make-interval (* up-x up-y) (* lo-x lo-y)))
        ((and both-pos-x both-pos-y)
          (make-interval (* lo-x lo-y) (* up-x up-y)))
        ((and both-pos-x both-neg-y)
          (make-interval (* up-x lo-y) (* lo-x up-y)))
        ((and both-neg-x both-pos-y)
          (make-interval (* lo-x up-y) (* up-x lo-y)))
        (both-neg-x
          (make-interval (* lo-x up-y) (* lo-x lo-y)))
        (both-neg-y
          (make-interval (* up-x up-y) (* lo-x up-y)))
        (both-pos-x
          (make-interval (* up-x lo-y) (* up-x up-y)))
        (both-pos-y
          (make-interval (* lo-x up-y) (* up-x up-y)))
        (#t
          (make-interval (min (* lo-x up-y)
                              (* up-x lo-y))
                         (max (* lo-x lo-y)
                              (* up-x up-y)))))))

;; 2.12
(define (make-center-percent c pt)
  (define w (* (abs c) (pt)))
  (cons (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

;; doesn't filter for center != 0
(define (percent i)
  (abs 
    (* 100 
       (/ (- (upper-bound i)
             (center i))
          (center i)))))

;; 2.14
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define (compar r1 r2)
  (display "comparing parallel resistance of:")
  (display r1)
  (display ", ")
  (display r2)
  (newline)
  (display "using division:  ")
  (display (par1 r1 r2))
  (newline)
  (display "  center = ")
  (display (center (par1 r1 r2)))
  (newline)
  (display "  tolerance = ")
  (display (percent (par1 r1 r2)))
  (newline)
  (display "using inverted form (1,1):  ")
  (display (par2 r1 r2))
  (newline)
  (display "  center = ")
  (display (center (par2 r1 r2)))
  (newline)
  (display "  tolerance = ")
  (display (percent (par2 r1 r2)))
  (newline))

(define (compar-list rlist)
  (if (null? rlist)
      '()
      (cons (compar (caar rlist) (cdar rlist))
            (compar-list (cdr rlist)))))

(define a (make-interval 99 101))
(define b (make-interval 997 1003))
(define c (make-interval 18 22))
(define d (make-interval 499 501))

(define rlist (list (cons a b)
                    (cons c d)
                    (cons a d)
                    (cons b c)))

;; run (compar-list rlist) to see the results
;; the two formulae give similar but not identical
;; centres, and sometimes very different tolerances

;; 2.15, 16
;; the different tolerances are because every use of
;; an interval adds uncertainty to the result - we know
;; that R1 == R1 even if we don't know R1's exact value,
;; but the functions we just made don't take that into 
;; account.  So par2 will give more precise results.

;; I don't understand the difference in centre values 
;; well enough to say which method is more accurate.

;; And I definitely am not going to try to make a better
;; package!
