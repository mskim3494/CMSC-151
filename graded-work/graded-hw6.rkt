#lang typed/racket
(require typed/test-engine/racket-tests)

;; ======== HOMEWORK 6 ==========

(define-struct Literal   ([val : Integer]) #:transparent)
(define-type   Operator  (U '+ '*))
(define-struct Operation ([op : Operator] [es : (Listof Exp)] [en : Exp]) #:transparent)
(define-type   Exp       (U Literal Operation Local BoundId))
(define-type   Identifier String)
(define-struct Local     ([id  : Identifier] [val : Exp] [in  : Exp]) #:transparent)
(define-struct BoundId   ([id : Identifier]) #:transparent)

;; PROBLEM 1

(define tree1
(Operation '+ (list (Literal 1))
  (Local "x" (Operation '* (list (Literal 2)) (Literal 3)) (BoundId "x"))))
(define tree2
(Local "x" (Operation '+ (list (Literal 5)) (Literal 2))
  (Local "y" (Operation '* (list (Literal 2)) (Literal 3))
    (Operation '* (list (BoundId "x")) (BoundId "y")))))
(define tree3
  (Operation '+ (list (Literal 2) (Literal 4))
             (Local "x" (Operation '* (list (Literal 5)) (Literal 2))
                    (Operation '+ (list (Literal 3) (Literal 2)) (BoundId "x")))))

(define-type Bindings (Listof Bind))
(define-struct Bind
  ([x : Identifier]
   [y : Integer])
  #:transparent)

(: eval-tree : Bindings Exp -> Integer)
;;evaluate the operation in the tree
(define (eval-tree b t)
  (match t
    [(Literal t) t]
    [(Operation _ '() en) (eval-tree b en)]
    [(Operation '+ es en) 
     (+ (eval-tree b (first es)) (eval-tree b (make-Operation '+ (rest es) en)))]
    [(Operation '* es en) 
     (* (eval-tree b (first es)) (eval-tree b (make-Operation '* (rest es) en)))]
    [(Local id val in)
     (match (eval-tree b val)
       [v (eval-tree (cons (Bind id v) b) in)])]
    [(BoundId id)
     (Bind-y (first (filter (lambda ([z : Bind]) (eqv? (Bind-x z) id)) b)))]))
(check-expect (eval-tree '() tree1) 7)
(check-expect (eval-tree '() tree2) 42)
(check-expect (eval-tree '() tree3) 21)


;; PROBLEM 2

(define-type Integrator (-> (-> Real Real) Real Real Integer Real))

(: integrate-list : Integrator)
;;integrate using Reimann sums
(define (integrate-list f x0 xf n)
  (local
    {(define w (/ (abs(- xf x0)) n))
     (: x-w : Real Real -> (Listof Real))
     ;;make a list with x values
     (define (x-w x1 x2)
       (cond
         [(>= x1 x2) '()]
         [else (cons (+ x1 w) (x-w (+ x1 w) x2))]))
     (define rec-hights (map f (x-w x0 xf)))}
    (define areas (map (lambda ([x : Real]) (* x w)) rec-hights))
    (foldr + 0 areas)))
;;GRADER: Incorrect math. -2
(check-expect (integrate-list sdf 0 10 10) 130)
(check-expect (integrate-list sdf 0 10 100) 121)
(check-expect (integrate-list quadratic -2 5 7) 140)
(check-expect (integrate-list quadratic -10 0 70) (+ 169 (/ 4 49)))

(: sdf : Real -> Real)
(define (sdf n) (* 2 (+ n 1)))
(: quadratic : Real -> Real)
(define (quadratic x)
  (+ (sqr x) (* 4 x) 4))

(: integrate-acc : Integrator)
;;integrate using tail-recursion
(define (integrate-acc f x0 xf n)
  (local
    {(define w (/ (abs(- xf x0)) n))
     (: reimann : (-> Real Real) Real Real Real -> Real)
     ;;use accumulator to calculate area under curve
     (define (reimann f x1 x2 w)
       (cond
         [(>= x1 x2) 0] 
         [else (+ (* w (f (+ x1 w))) (reimann f (+ x1 w) x2 w))]))}
    (reimann f x0 xf w))) ;; i know that the function works simply with (* w (f x1))
                          ;; but I did it this way for consistency (using right side of
                          ;; rectangle to calculate area)
;;GRADER: Incorrect math.
(check-expect (integrate-acc sdf -2 10 12) 132)
(check-within (integrate-acc sdf 5 12 14) 136.5 0.000000001)
(check-expect (integrate-list quadratic -2 5 7) 140)
(check-within (integrate-list quadratic 0 4 8) 77.5 0.00000001)

;; PROBLEM 3

(: is-palindrome? : (Vectorof Integer) -> Boolean)
;; determines if a given vector is palindromic
(define (is-palindrome? v)
  (local
    {(define l (- (vector-length v) 1))
     (: palindrome? : (Vectorof Integer) Integer Integer -> Boolean)
     ;;determine whether the vector is palindromic
     (define (palindrome? b n1 n2)
       (cond
         [(= n1 n2) #t]
         [(= (+ n1 1) n2) #t]
         [(= (vector-ref b n1) (vector-ref b n2))
          (palindrome? b (+ n1 1) (- n2 1))]
         [else #f]))}
    (palindrome? v 0 l)))
;;GRADER: Incorrect handling of empty vector. -1
(check-expect (is-palindrome? '#(1 2 3 2 1)) true)
(check-expect (is-palindrome? '#(1 2 2 1)) true)
(check-expect (is-palindrome? '#(17 24 36 24 17)) true)
(check-expect (is-palindrome? '#(1 2 3 2 4)) false)
(check-expect (is-palindrome? '#(1 2 3 2 0 1)) false)

(test)

;; ===== GRADER TESTS

"Problem 1"

(define test-exp
(Operation '+
  (list (Literal 1)
  (Operation '* (list (Literal 4) (Literal 5) (Literal 6)) (Literal 7)))
  (Operation '* (list (Literal 10)) (Operation '+ (list (Literal 1)) (Literal 2)))
))

(define test-bind1
  (Operation '+ (list (Literal 1))
    (Local "x" (Literal 2) (BoundId "x"))))

(define test-lexscope
  (Operation '+ (list (Literal 1))
    (Local "x" (Literal 2)
      (Local "x" (Literal 3)
        (BoundId "x")))))

(define test-ortho
  (Operation '+ (list (Literal 1))
    (Local "x" (Literal 2)
      (Local "y" (Literal 3)
        (BoundId "x")))))

(define test-outofscope
  (Operation '+ (list (Literal 1)
    (Local "x" (Literal 2)
        (BoundId "x")))
    (BoundId "x")))

(define test-recursive
  (Operation '+ (list (Literal 1))
    (Local "x" (BoundId "x")
        (BoundId "x"))))

(define test-propagate
  (Operation '+ (list (Literal 1))
    (Local "x" (Literal 2)
        (Operation '+ (list (Literal 3)) (BoundId "x")))))

(check-expect (eval-tree '() test-bind1) 3)

(check-expect (eval-tree '() test-lexscope) 4)

(check-expect (eval-tree '() test-ortho) 3)

(check-expect (eval-tree '() test-propagate) 6)

(check-expect (eval-tree '() (Operation '+ (list (Operation '* (list (Literal 3) (Literal 4)) (Operation '+ (list (Literal 12)) (Literal 6))) (Operation '* (list (Literal 3)) (Literal 4))) (Literal 5))) 233)

(check-expect (eval-tree '() (Operation '+ (list (Literal 3)) (Literal 4))) 7)

(check-expect (eval-tree '() (Operation '+ (list (Literal 4) (Literal 10) (Literal -11)) (Operation '* (list (Literal 12) (Operation '* (list (Literal 4) (Literal 3)) (Literal -4))) (Operation '+ (list (Literal -3) (Literal 6)) (Literal -5))))) 1155)

(check-expect (eval-tree '() (Operation '+ (list (Literal 1)) (Local "x" (Operation '* (list (Literal 2)) (Literal 3)) (BoundId "x")))) 7)

(check-expect (eval-tree '() (Local "x" (Operation '+ (list (Literal 5)) (Literal 2)) (Local "y" (Operation '* (list (Literal 2)) (Literal 3)) (Operation '* (list (BoundId "x")) (BoundId "y"))))) 42)

"Problem 2"

(check-within (integrate-list sqr 0 7 7) 91 .1)
(check-within (integrate-list add1 0 7 7) 28 .1)
(check-within (integrate-list sqr 0 7 10000) 114.3 .1)
(check-within (integrate-list sin 0 pi 10000) 2 .1)

(check-within (integrate-acc sqr 0 7 7) 91 .1)
(check-within (integrate-acc add1 0 7 7) 28 .1)
(check-within (integrate-acc sqr 0 7 10000) 114.3 .1)
(check-within (integrate-acc sin 0 pi 10000) 2 .1)

"Problem 3"

(check-expect (is-palindrome? '#(1 2 3 2 1)) #t)
(check-expect (is-palindrome? '#(1 2 2 1)) #t)
(check-expect (is-palindrome? '#(17 24 36 24 17)) #t)
(check-expect (is-palindrome? '#(1 2 3 4)) #f)
(check-expect (is-palindrome? '#(1 2 3 2)) #f)
(check-expect (is-palindrome? '#(1 2 3 2 1 0)) #f)
(check-expect (is-palindrome? '#(0)) #t)
(check-expect (is-palindrome? '#()) #t)

(test)

;; ====== correctness

;; === correctness ===

;; problem 1 (eval-tree)             -/ 20

;; problem 2 (integrade-list)        6/ 8
;; problem 2 (integrate-acc)         6/ 8

;; problem 3 (is-palindrome?)        7/ 8

;; === style ===

;; code layout                       -/ 8
;; identifiers are well named        -/ 8
;; program decomposition (helpers)   -/ 4

;; contracts (types)                 -/ 8
;; well-written purposes             -/ 8
;; adequate tests                    6/ 8 ;;GRADER: Should test integration more.

;; clarity (clear logic)             -/ 6

;; svn used correctly                -/ 6

;; _total-score_                   93/ 100

;; grader: olsona
