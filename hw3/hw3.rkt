#lang typed/racket
(require typed/test-engine/racket-tests)
;;Worked with Chanwool Kim, Won Jae Noh, Teresa Kim, Jamie Lee

;;Problem 2

(define-type Nat (U 'Zero Succ))
(define-struct Succ ([prev : Nat]) #:transparent)

;;nat->int
(: nat->int : Nat -> Integer)
;;find the integer representing the nat that is input
(define (nat->int x)
  (match x
    ('Zero 0)
    ((Succ p) (+ 1 (nat->int p)))))
(check-expect (nat->int (Succ'Zero)) 1)
(check-expect (nat->int (Succ(Succ'Zero))) 2)

;;int->nat
(: int->nat : Integer -> Nat)
;;find the Nat that represents the Integer output
(define (int->nat x)
  (match x
    (0 'Zero)
    (x (Succ(int->nat (- x 1))))))
(check-expect (int->nat 2) (Succ(Succ'Zero)))
(check-expect (int->nat 1) (Succ'Zero))
(check-expect (int->nat 3) (Succ(Succ(Succ'Zero))))

;;nat+
(: nat+ : Nat Nat -> Nat)
;;compute the sum of the two input Nats 
(define (nat+ x y)
  (match y
    ('Zero x)
    ((Succ n) (nat+ (Succ x) n))))
(check-expect (nat+ (Succ'Zero) 'Zero) (Succ'Zero))
(check-expect (nat+ (Succ'Zero) (Succ'Zero)) (Succ(Succ'Zero)))

;;nat-
(: nat- : Nat Nat -> Nat)
;;compute the difference of two input Nats
(define (nat- x y)
  (match (cons x y)
    ((cons 'Zero y) 'Zero)
    ((cons x 'Zero) x)
    ((cons (Succ x) (Succ y)) (nat- x y))))
(check-expect (nat- (Succ(Succ(Succ'Zero))) (Succ'Zero)) (Succ (Succ'Zero)))
(check-expect (nat- (Succ'Zero) (Succ(Succ(Succ'Zero)))) 'Zero)
(check-expect (nat- (Succ'Zero) (Succ'Zero)) 'Zero)

;;nat<
(: nat< : Nat Nat -> Boolean)
;;evaluate whether the first is strictly less than the second
(define (nat< x y)
  (cond
    [(not (eq? 'Zero (nat- y x))) true]
    [else false]))
(check-expect (nat< (Succ'Zero) (Succ'Zero)) false)
(check-expect (nat< (Succ(Succ(Succ'Zero))) (Succ'Zero)) false)
(check-expect (nat< (Succ'Zero) (Succ(Succ(Succ'Zero)))) true)

;;nat*
(: nat* : Nat Nat -> Nat)
;;computes the multiplication of the two inputs in Nat
(define (nat* x y)
  (match (cons x y)
    [(cons 'Zero y) 'Zero]
    [(cons x 'Zero) 'Zero]
    [(cons x (Succ'Zero)) x]
    [(cons (Succ'Zero) y) y]
    [(cons x (Succ y)) (nat+ x (nat* x y))]))
(check-expect (nat* (Succ(Succ(Succ(Succ'Zero)))) (Succ(Succ(Succ'Zero)))) (int->nat 12)) ;; 4*3=12

;;nat/
(: nat/ : Nat Nat -> Nat)
;;computes the quotient of the division of the two inputs in Nat
(define (nat/ x y)
  (cond
    [(nat< x y) 'Zero]
    [(eq? x 'Zero) 'Zero]
    [(eq? 'Zero y) (error "undefined")]
    [(and (eq? x 'Zero) (eq? y 'Zero)) (error "undefined")]
    [else (nat+ (Succ'Zero) (nat/ (nat- x y) y))]))
(check-expect (nat/ (int->nat 0) (int->nat 2)) 'Zero)
(check-expect (nat/ (int->nat 12) (int->nat 3)) (int->nat 4))
(check-expect (nat/ (int->nat 7) (int->nat 2)) (int->nat 3))
(check-expect (nat/ (int->nat 2) (int->nat 3)) 'Zero)

;;nat-remainder
(: nat-remainder : Nat Nat -> Nat)
;;computes the remainder of the two inputs in Nat
(define (nat-remainder x y)
  (nat- x (nat* y (nat/ x y))))
(check-expect (nat-remainder (int->nat 7) (int->nat 2)) (int->nat 1))
(check-expect (nat-remainder (int->nat 8) (int->nat 3)) (int->nat 2))
(check-expect (nat-remainder (int->nat 18) (int->nat 5)) (int->nat 3))

;;Problem 3

;;creating structs and data type

(define-struct cartesian
  ([x : Real]
   [y : Real])
   #:transparent)

(define-struct polar-pair
  ([r : Real]
   [th : Real])
   #:transparent)

(define-type Point2D (U cartesian Complex polar-pair))

(: p2d-from-x-y : Real Real -> Point2D)
;;represent the x y values in cartesian coordinate as a Point2D
(define (p2d-from-x-y x y)
  (make-cartesian x y))
(check-expect (p2d-from-x-y 2 3) (cartesian 2 3))
(check-expect (p2d-from-x-y 4 5) (cartesian 4 5))
(check-expect (p2d-from-x-y 10 2) (cartesian 10 2))

(: p2d-from-complex : Complex -> Point2D)
;;represent the complex number as a Point2D
(define (p2d-from-complex foo)
  foo)
(check-expect (p2d-from-complex 2+2i) 2+2i)
(check-expect (p2d-from-complex 5) 5)
(check-expect (p2d-from-complex 0+4i) 0+4i)

(: p2d-from-polar : Real Real -> Point2D)
;;represent a radius and an angle as a Point2D
(define (p2d-from-polar r A)
  (make-polar-pair r A))
(check-within (p2d-from-polar 5 (* 2 pi)) (polar-pair 5 6.2831) 0.01)
(check-within (p2d-from-polar -5 (* 3 pi)) (polar-pair -5 9.425) 0.01)

(: p2d-as-cartesian : Point2D -> Point2D)
;;represent a given Point2D as a Point2D in cartesian coordinates
(define (p2d-as-cartesian p)
  (cond
    [(cartesian? p) p]
    [(complex? p) (make-cartesian (real-part p) (imag-part p))]
    [(polar-pair? p) 
     (make-cartesian 
      (* (polar-pair-r p) (cos(polar-pair-th p))) 
      (* (polar-pair-r p) (sin(polar-pair-th p))))]
    [else (error "something went wrong")]))
(check-within (p2d-as-cartesian (make-polar-pair 5 (/ pi 2))) (cartesian 0 5) .01)
(check-expect (p2d-as-cartesian 2+5i) (cartesian 2 5))
(check-expect (p2d-as-cartesian (make-cartesian 4 6)) (cartesian 4 6))

(: p2d-as-complex : Point2D -> Point2D)
;;represent a given Point2D as a Point2D using complex number
(define (p2d-as-complex p)
  (cond
    [(cartesian? p) (make-rectangular (cartesian-x p) (cartesian-y p))]
    [(complex? p) p]
    [(polar-pair? p) 
     (make-rectangular 
      (* (polar-pair-r p) (cos(polar-pair-th p))) 
      (* (polar-pair-r p) (sin(polar-pair-th p))))]
    [else (error "something went wrong")]))
(check-expect (p2d-as-complex (make-cartesian 4 6)) 4+6i)
(check-expect (p2d-as-complex 2+6i) 2+6i)
(check-within (p2d-as-complex (make-polar-pair 5 (/ pi 2))) 0+5i 0.01)

(: p2d-as-polar : Point2D -> Point2D)
;;represent a given Point2D as a Point2D in polar coordinates
(define (p2d-as-polar p)
  (cond
    [(cartesian? p) 
     (make-polar-pair 
      (sqrt (+ (sqr (cartesian-x p)) (sqr (cartesian-y p)))) 
      (atan (/ (cartesian-y p) (cartesian-x p))))]
    [(complex? p)
     (make-polar-pair 
      (sqrt (+ (sqr (real-part p)) (sqr (imag-part p)))) 
      (atan (/ (imag-part p) (real-part p))))]
    [(polar-pair? p) p]))
(check-within 
 (p2d-as-polar (make-cartesian 3 4)) 
 (polar-pair 5 1) 0.1)
(check-within 
 (p2d-as-polar (make-polar-pair 3 pi)) 
 (polar-pair 3 3.1316) 0.01)
(check-within 
 (p2d-as-polar 4+5i) 
 (polar-pair 6.403 0.896) 
 0.01)

(: p2d-x : Point2D -> Real)
;;find the value of x in cartesian coordinates of the given point 
(define (p2d-x p)
  (cond
    [(cartesian? p) (cartesian-x p)]
    [(complex? p) (real-part p)]
    [(polar-pair? p) (* (polar-pair-r p) (cos(polar-pair-th p)))]))
(check-expect (p2d-x (make-cartesian -3 4)) -3)
(check-expect (p2d-x 10+9i) 10)
(check-within (p2d-x (make-polar-pair 3 (/ pi 3))) 1.5 0.0001)

(: p2d-y : Point2D -> Real)
;;find the value of x in cartesian coordinates of the given point 
(define (p2d-y p)
  (cond
    [(cartesian? p) (cartesian-y p)]
    [(complex? p) (imag-part p)]
    [(polar-pair? p) (* (polar-pair-r p) (sin(polar-pair-th p)))]))
(check-expect (p2d-y (make-cartesian -3 4)) 4)
(check-expect (p2d-y 10+9i) 9)
(check-within (p2d-y (make-polar-pair 3 (/ pi 3))) 2.6 0.1)

(: p2d-n : Point2D -> Complex)
;;find the value of Point2D represented as a complex number
(define (p2d-n p)
  (make-rectangular
   (cond
        [(cartesian? p) (cartesian-x p)]
        [(complex? p) (real-part p)]
        [(polar-pair? p)  
         (* (polar-pair-r p) (cos(polar-pair-th p)))])
   (cond
        [(cartesian? p) (cartesian-y p)]
        [(complex? p) (imag-part p)]
        [(polar-pair? p) 
         (* (polar-pair-r p) (sin(polar-pair-th p)))])))
(check-expect (p2d-n (make-cartesian 5 9)) 5+9i)
(check-expect (p2d-n 4+2i) 4+2i)
(check-within 
 (p2d-n (make-polar 5 (/ pi 2))) 
 (make-polar 5 1.571) 0.01)

(: p2d-r : Point2D -> Real)
;;find the radius of given Point2D
(define (p2d-r p)
  (cond
   [(cartesian? p) 
    (sqrt (+ (sqr (cartesian-x p)) (sqr (cartesian-y p))))]
   [(complex? p) 
    (sqrt (+ (sqr (real-part p)) (sqr (imag-part p))))]
   [(polar-pair? p) (polar-pair-r p)]))
(check-expect (p2d-r (make-cartesian -3 4)) 5)
(check-within (p2d-r 4+8i) 8.944 0.001)
(check-expect (p2d-r (make-polar-pair -2 pi)) -2)

(: p2d-theta : Point2D -> Real)
;;find the radius of given Point2D
(define (p2d-theta p)
  (cond
   [(cartesian? p)  
    (atan (/ (cartesian-y p) (cartesian-x p)))]
   [(complex? p)
    (atan (/ (imag-part p) (real-part p)))]
   [(polar-pair? p) (polar-pair-th p)]))
(check-within (p2d-theta (make-cartesian -3 4)) -0.9273 0.001)
(check-within (p2d-theta 4+8i) 1.107 0.001)
(check-within (p2d-theta (make-polar-pair -3 pi)) 3.1416 0.01)

(: p2d-dist : Point2D Point2D -> Real)
;;find the distance between two points
(define (p2d-dist A B)
  (sqrt
    (+ 
     (sqr (- (p2d-x A) (p2d-x B)))
     (sqr (- (p2d-y A) (p2d-y B))))))
(check-expect 
 (p2d-dist
  (make-cartesian 0 0)
  (make-cartesian 3 4))
5)
(check-within
 (p2d-dist
  10+20i
  (make-polar-pair 3 pi))
 23.8537 0.001)

;;Problem 4

(define-struct Point
  ([x : Real]
   [y : Real])
  #:transparent)

(define-struct Rectangle
  ([blc : Point] ;;blc -> bottom left corner
   [length : Real]
   [width : Real])
  #:transparent)

(: throw-dart : Rectangle -> Point)
;;produce a point chosen uniformly at random within the rectangle given
(define (throw-dart board)
  (make-Point 
   (+ (Point-x (Rectangle-blc board)) 
      (* (random) (Rectangle-length board))) 
   (+ (Point-y (Rectangle-blc board)) 
      (* (random) (Rectangle-width board)))))

(check-range 
 (Point-x 
  (throw-dart 
   (make-Rectangle (make-Point 2 3) 4 3))) 2 6)
(check-range 
 (Point-y 
  (throw-dart 
   (make-Rectangle (make-Point 2 3) 4 3))) 3 6)

(define-struct Circle
  ([center : Point]
   [radius : Real])
  #:transparent)

;;distance helper function
(: distance : Point Point -> Real)
;;compute the distance between two Points
(define (distance A B)
  (sqrt
    (+ 
     (sqr (- (Point-x A) (Point-x B)))
     (sqr (- (Point-y A) (Point-y B))))))
(check-expect 
 (distance
  (make-Point 0 0)
  (make-Point 3 4))
5)
(check-expect 
 (distance 
  (make-Point -3 3) 
  (make-Point 0 -1)) 5)

(: within-circle? : Circle Point -> Boolean)
;;checks to see if the circle is within the circle
(define (within-circle? whatup notmuch)
  (cond 
    [(> (Circle-radius whatup) 
        (distance (Circle-center whatup) notmuch)) true]
    [else false]))
(check-expect 
 (within-circle?
  (make-Circle
   (make-Point 2 2) 3)
  (make-Point 2 3))
 true)
(check-expect 
 (within-circle?
  (make-Circle
   (make-Point 2 2) 3)
  (make-Point 6 3))
 false)

(: bounding-box : Circle Circle -> Rectangle)
;;find the smallest rectangle enclosing the two circles
(define (bounding-box one two)
  (make-Rectangle
   (make-Point 
      (min 
       (- (Point-x (Circle-center one)) (Circle-radius one))
       (- (Point-x (Circle-center two)) (Circle-radius two)))
      (min 
       (- (Point-y (Circle-center one)) (Circle-radius one))
       (- (Point-y (Circle-center two)) (Circle-radius two))))
   (-
    (max
     (+ (Point-x (Circle-center one)) (Circle-radius one))
     (+ (Point-x (Circle-center two)) (Circle-radius two)))
    (min
     (- (Point-x (Circle-center one)) (Circle-radius one))
     (- (Point-x (Circle-center two)) (Circle-radius two))))
   (-
    (max
     (+ (Point-y (Circle-center one)) (Circle-radius one))
     (+ (Point-y (Circle-center two)) (Circle-radius two)))
    (min
     (- (Point-y (Circle-center one)) (Circle-radius one))
     (- (Point-y (Circle-center two)) (Circle-radius two))))
   )
 )
(check-expect (bounding-box 
               (make-Circle (make-Point 2 2) 3)
               (make-Circle (make-Point 5 3) 2))
              (make-Rectangle
               (make-Point -1 -1) 8 6))
(check-expect (bounding-box 
               (make-Circle (make-Point 3 4) 2)
               (make-Circle (make-Point 5 3) 3))
              (make-Rectangle
               (make-Point 1 0) 7 6))

(: foo : Circle Circle Integer -> Real)
;;find the number of times that the random point succesfully 
;;lies in the intersection between the two circles
(define (foo a b m)
  (local {(: x : Circle Circle -> Point)
          (define (x a b) (throw-dart (bounding-box a b)))}
  (cond
    [(= m 0) 0]
    [(and (within-circle? a (x a b)) (within-circle? b (x a b)))
     (+ 1 (foo a b (- m 1)))]
    [else (foo a b (- m 1))])))
;;I know there's a problem here regarding the fact that the inputs 
;;in the within-circle? functions are not the same. However, I could
;;not find a way to make them equal (by using functions let( and set!( )
(check-range
 (foo
  (make-Circle (make-Point 2 2) 3)
  (make-Circle (make-Point 5 3) 3)
  10000) 0 (* 64 10000))


(: area-of-intersection-monte-carlo : Circle Circle Integer -> Real)
;;approximation of the area of the intersection between two circles
(define (area-of-intersection-monte-carlo A B n)
  (*
   (Rectangle-length (bounding-box A B))
   (Rectangle-width (bounding-box A B))
   (/ (foo A B n) n)))
(check-range 
 (area-of-intersection-monte-carlo
  (make-Circle (make-Point 0 0) 1)
  (make-Circle (make-Point 0 0) 5) 1000)
 0 100)
(check-range
 (area-of-intersection-monte-carlo
  (make-Circle (make-Point 2 2) 3)
  (make-Circle (make-Point 4 4) 3) 1000)
 0 100)

(: left-circle : Real Real -> Circle)
;;produce the left circle of a venn-diagram
(define (left-circle r d)
  (make-Circle (make-Point 0 0) r))
(check-expect 
 (left-circle 3 10) 
 (make-Circle (make-Point 0 0) 3))
(check-expect
 (left-circle 20 50)
 (make-Circle (make-Point 0 0) 20))

(: right-circle : Real Real -> Circle)
;;produce the right circle of a venn-diagram
(define (right-circle r d)
  (make-Circle (make-Point d 0) r))
(check-expect 
 (right-circle 5 10) 
 (make-Circle (make-Point 10 0) 5))
(check-expect
 (right-circle 25 40)
 (make-Circle (make-Point 40 0) 25))

(: venn-monte-carlo : Real Real Integer -> Real)
;;find the approximation of the area of the intersection between the two circles
(define (venn-monte-carlo r d n)
  (area-of-intersection-monte-carlo 
   (left-circle r d) 
   (right-circle r d)
   n))
(check-range (venn-monte-carlo 3 0 10000) 0 36)
(check-range (venn-monte-carlo 6 12 1000) 0 576)

(test)