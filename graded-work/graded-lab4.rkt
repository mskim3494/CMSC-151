#lang typed/racket
(require typed/test-engine/racket-tests)

;; ==== LAB 4 ====

(define-struct Point
  ([x : Real]
   [y : Real])
  #:transparent)

(define-struct Line
  ([m : Real]
   [b : Real])
  #:transparent)

(define-type Dataset (Listof Point))
;;make 2 datasets for simplicity (test engine)
;;dataset 1
(define p1 (make-Point 8 10))
(define p2 (make-Point 0 9))
(define p3 (make-Point 29 26))
(define p4 (make-Point 60 52))
(define p5 (make-Point 3 8))
(define DS1 (list p1 p2 p3 p4 p5))
(define xs1 (list 8 0 29 60 3))
(define xs2 (list 10 9 26 52 8))

;;dataset 2
(define l1 (make-Point 2 3))
(define l2 (make-Point 1 2))
(define l3 (make-Point 6 10))
(define l4 (make-Point 4 9))
(define l5 (make-Point -1 -2))
(define DS2 (list l1 l2 l3 l4 l5))
(define xs3 (list 2 1 6 4 -1))
(define xs4 (list 3 2 10 9 -2))

(: sum (-> (Listof Real) Real))
;;sum the components in the list
(define (sum xs)
  (cond
    [(empty? xs) 0]
    [(cons? xs)
     (+
      (first xs)
      (sum (rest xs)))]))
(check-expect (sum xs3) 12)
(check-expect (sum xs4) 22)

(: x-value (-> Dataset (Listof Real)))
;;create a list with the x-component of the list
(define (x-value xs)
  (cond
    [(empty? xs) empty]
    [(cons? xs) 
     (cons
       (Point-x (first xs))
       (x-value (rest xs)))]))
(check-expect (x-value DS1) xs1)
(check-expect (x-value DS2) xs3)

(: y-value (-> Dataset (Listof Real)))
;;create a list with the y-component of the list
(define (y-value ys)
  (cond
    [(empty? ys) empty]
    [(cons? ys) 
     (cons
       (Point-y (first ys))
       (y-value (rest ys)))]))
(check-expect (y-value DS1) xs2)
(check-expect (y-value DS2) xs4)

(: square (-> Dataset (Listof Real)))
;;compute the square of x-component
(define (square x)
  (cond
    [(empty? x) empty]
    [(cons? x) 
     (cons
       (sqr (Point-x(first x)))
       (square (rest x)))]))
(check-expect (square DS1) (list 64 0 841 3600 9))
(check-expect (square DS2) (list 4 1 36 16 1))

(: xy (-> Dataset (Listof Real)))
;;compute the product of xy and return a list 
(define (xy l)
  (cond
    [(empty? l) empty]
    [(cons? l) 
     (cons
      (*
       (Point-x (first l))
       (Point-y (first l)))
      (xy (rest l)))]))
(check-expect (xy DS1) (list 80 0 754 3120 24))
(check-expect (xy DS2) (list 6 2 60 36 2))

(: slope (-> Dataset Real))
;;compute the best-fit slope of the linear regression
(define (slope A)
  (cond
    [( < (length A) 2) (error "not enough elements")]
    [else
     (/
      {-
       (* (length A) (sum (xy A)))
       (* (sum (x-value A)) (sum (y-value A)))}
      {-
       (* (length A) (sum(square A)))
       (sqr (sum (x-value A)))})]))
(check-within (slope DS1) 0.74701 0.001)
(check-within (slope DS2) 1.82192 0.001)

(: intercept (-> Dataset Real))
;;compute the y-intercept for the linear model
(define (intercept A)
  (cond
    [(< (length A) 2) (error "not enough elements")]
    [else
     (/
      {-
       (* (sum (y-value A)) (sum (square A)))
       (* (sum (x-value A)) (sum (xy A)))}
      {-
       (* (length A) (sum (square A)))
       (sqr (sum (x-value A)))})]))
(check-within (intercept DS1) 6.0597 0.001)
(check-within (intercept DS2) 0.02740 0.001)

(: linreg (-> Dataset Line))
;;compute the linear regression of a given dataset
(define (linreg B)
  (make-Line (slope B) (intercept B)))
(check-within (linreg DS1) (make-Line 0.74701 6.0597) 0.001)
(check-within (linreg DS2) (make-Line 1.82192 0.02740) 0.001)
(test)

;;GRADER TESTS

(check-expect (linreg (list (Point 0 0) (Point 1 1)))
              (Line 1 0))

(check-expect (linreg (list (Point 0 0) (Point 1 1) (Point 2 2)))
              (Line 1 0))

(check-within (linreg (list (Point 0 0) (Point 1 1) (Point 2 3)))
              (Line 1.5 -0.166666) ;; calculated this result on a google sheet
              0.0001)

;;GRADER COMMENTS 

;; evaluation

;; === correctness ===

;; correctness of slope              20/20
;; correctness of intercept          20/20
;; correctness of linreg             4/ 4

;; === style ===

;; code layout                       8/ 8
;; identifiers are well named        8/ 8
;; program organization              4/ 4

;; contracts (types)                 8/ 8
;; well-written purposes             8/ 8
;; adequate tests                    8/ 8

;; clarity (clear logic)             6/ 6

;; svn used correctly                6/ 6

;; _total-score_                   100/ 100

;; graded by Benjamin Rohrer (bjr)
