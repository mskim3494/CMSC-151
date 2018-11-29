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
(check-expect (is-palindrome? '#(1 2 3 2 1)) true)
(check-expect (is-palindrome? '#(1 2 2 1)) true)
(check-expect (is-palindrome? '#(17 24 36 24 17)) true)
(check-expect (is-palindrome? '#(1 2 3 2 4)) false)
(check-expect (is-palindrome? '#(1 2 3 2 0 1)) false)

(test)