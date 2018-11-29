#lang typed/racket

(require typed/test-engine/racket-tests)

;;Problem 1 

;;converting centimeters to inches
( : cm-in ( -> Exact-Rational Exact-Rational))
(define (cm-in centimeters) (* centimeters 100/245))
(cm-in 10)
(check-expect (cm-in 10) 200/49)

;;converting inches to centimeters
(: in-cm (-> Exact-Rational Exact-Rational))
(define (in-cm inches) (* inches 254/100))
(in-cm 4)
(check-expect (in-cm 4) 254/25)

;;molecular weight of hydrocarbon
(: molecular-weight-hydrocarbon (-> Integer Integer Integer))
;;compute the molecular weight of a hydrocarbon based on the number of carbon and hydrogen atoms
(define (molecular-weight-hydrocarbon carbon hydrogen) (+ (* carbon 12) (* hydrogen 1)))
(molecular-weight-hydrocarbon 2 4)
(check-expect (molecular-weight-hydrocarbon 2 4) 28)

;;quadratic equation
(: eval-quadratic (-> Real Real Real Real Real))
;;compute the value of f(x)
(define (eval-quadratic a b c x) 
  (+ (* a (* x x)) (* b x) c))
(eval-quadratic 1 2 3 4)
(check-expect (eval-quadratic 1 2 3 4) 27)

;;Problem 2

;;Valid Celcius
(: valid-celcius? (-> Real Boolean))
;;determine whether the value of C is a valid Celcius 
(define (valid-celcius? c) (> c -273.15))
(check-expect (valid-celcius? -270) #t)
(check-expect (valid-celcius? -280) #f)
(valid-celcius? -270)

;;Within
(: within? (-> Real Real Real Boolean))
;;determine whether x and y are within epsilon of each other
(define (within? E x y) (> E (abs (- x y))))
(check-expect (within? 0.1 1 2) #f)
(check-expect (within? 0.3 1.6 1.7) #t)
(within? 0.1 1 2)

;;on-quadratic?
(: on-quadratic? (-> Real Real Real Real Real Real Boolean))
;;determine whether a point is on the line or within epsilon
(define (on-quadratic? a b c x y E) 
  (> E (abs (- y (+ (* a (* x x)) (* b x) c)))))
(check-expect (on-quadratic? 1 2 3 4 27.01 .011) #t)
(on-quadratic? 1 2 3 4 28 1.01)

;;Problem 3
(: eggs-to-buy (-> Integer Integer))
;;compute the number of eggs to buy based on the number of people attending, n
(define (eggs-to-buy n)
  (max (* (exact-ceiling (/ (* n (/ 100 83)) 6)) 6) 12))
(eggs-to-buy 10)
(check-expect (eggs-to-buy 10) 18)
(check-expect (eggs-to-buy 20) 30)

;;Problem 4
(: g (-> Integer Exact-Rational))
;;compute the value of g(n)
(define (g n) (/ (* (+ (* 2 n) 1) n) (/ (* (+ n 1) n) 2)))
(g 2)
(check-expect (g 2) 10/3)
(check-expect (g 3) 21/6)
(check-expect (g 4) 36/10)

(test)

;; evaluation

;; You should use 'within?' and 'eval-quadratic' in your definition of
;; 'on-quadratic?'.  Writing the same code twice when you can write it
;; once and re-use it is bad style.

;; With just a little algebra, it is possible to significantly simplify your
;; code for the function 'g'.

;; Some of your lines are too long.  Keep them under 80 characters.

;; You should always put a return between the two parts of a definition, like
;; this:
;(define (valid-celcius? c)
;  (> c -273.15))
;(define (within? E x y)
;  (> E (abs (- x y))))
;(define (g n)
;  (/ (* (+ (* 2 n) 1) n) (/ (* (+ n 1) n) 2)))

;; All of your tests should be in 'check-expect' statements.  The extra times
;; you call your function should be removed (lines 10, 16, 23, 31, etc.).

;; The function 'cm-in' should be named 'cm->in'.  Similarly for 'in-cm'.

;; The funciton 'valid-celcius?' should be named 'valid-celsius?'.

;; === correctness ===

;; problem 1 (cm/in)                 5/ 6
;; problem 1 (molecular-w)           5/ 5
;; problem 1 (eval quad)             5/ 5

;; problem 2 (valid-celsius?)        4/ 5
;; problem 2 (within?)               5/ 5
;; problem 2 (on-quadratic?)         6/ 6

;; problem 3 (eggs-to-buy)           6/ 6

;; problem 4 (g)                     6/ 6

;; === style ===

;; code layout                       5/ 8
;; identifiers are well named        8/ 8
;; program decomposition (helpers)   3/ 4

;; contracts (types)                 8/ 8
;; well-written purposes             8/ 8
;; adequate tests                    8/ 8

;; clarity (clear logic)             6/ 6

;; svn used correctly                6/ 6

;; _total-score_                  94/ 100

;; grader: Nick Seltzer