#lang typed/racket
(require typed/test-engine/racket-tests)
(require/typed 2htdp/image
  [#:opaque Image image?]
  [overlay (-> Image Image Image)]
  [empty-image Image]
  [circle (-> Integer String String Image)]
  [square (-> Integer String String Image)]
  [rectangle (-> Integer Integer String String Image)]
  [triangle (-> Integer String String Image)]
  [star(-> Integer String String Image)]
  [radial-star (-> Integer Integer Integer String String Image)])

;; ==== HOMEWORK 4 ====

;; == PROBLEM 1 ==

(: concentric-circles (-> String Integer Integer Image))
;;create concentric circles
(define (concentric-circles color r d)
  (cond
    [(< (- r d) 0) empty-image]
    [else
     (overlay
      (circle r "outline" color)
      (concentric-circles color (- r d) d))]))
(concentric-circles "red" 100 10)

(: concentric (-> (-> Integer Image) Integer Integer Image))
;;create concentric image of any shape
(define (concentric shape size delta)
  (cond
    [(< (- size delta) 0) empty-image]
    [else
     (overlay
      (shape size) 
      (concentric shape (- size delta) delta))]))

(: blue-star (-> Integer Image))
;;define blue-star
(define (blue-star x)
  (star x "outline" "blue"))
(concentric blue-star 100 20)

;; == PROBLEM 2 ==

;;from hw2
(define-struct Food
 ([name : String]
  [size : Exact-Rational]
  [fat : Exact-Rational]
  [carbs : Exact-Rational]
  [protein : Exact-Rational])
  #:transparent)
(define f1 (make-Food "Pepperoni Cheese Steak" 201 30 40 30))
(define f2 (make-Food "French Fries" 134 12 43 4))
(define f3 (make-Food "Ice Cream" 107 8 27 4))
(define f4 (make-Food "Steak" 182 20 0 46))
(define f5 (make-Food "Mashed Potatoes" 240 10 37 5))
(define f6 (make-Food "Slice of Cake" 80 10 43 4))

(define-type Meal (Listof Food))
(define Meal1 (list f1 f2 f3))
(define Meal2 (list f2 f4 f5 f6))

(: meal-fat (-> Meal Exact-Rational))
;;calculate the grams of fat in the food
(define (meal-fat fatty)
  (cond
    [(empty? fatty) 0]
    [else 
     (+
      (Food-fat (first fatty))
      (meal-fat (rest fatty)))]))
(check-expect (meal-fat Meal1) 50)
(check-expect (meal-fat Meal2) 52)

(: fatty-food-in-meal? (-> Meal Boolean))
;;determine if a food in the meal has more than 25 gr of fat
(define (fatty-food-in-meal? m)
  (cond
    [(empty? m) false]
    [(cons? m)
     (cond
       [(> (Food-fat (first m)) 25) true]
       [else (fatty-food-in-meal? (rest m))])]))
(check-expect (fatty-food-in-meal? Meal1) true)
(check-expect (fatty-food-in-meal? Meal2) false)

(: eliminate-grease (-> Meal Meal))
;;eliminate foods that contain more than 25gr of fat
(define (eliminate-grease m)
  (cond
    [(empty? m) empty]
    [else
      (if (< (Food-fat (first m)) 25) 
          (cons (first m) (eliminate-grease (rest m)))
          (eliminate-grease (rest m)))]))
(check-expect (eliminate-grease Meal1) (list f2 f3))
(check-expect (eliminate-grease Meal2) Meal2)

;; = Helper Function =

(: food-scale (-> Food Exact-Rational Food))
;;scale the numbers of the food (serving size, fat, protein, and carbs) to the number provided
(define (food-scale foo x)
  (make-Food 
   (string-append "x" (Food-name foo)) 
   (* x (Food-size foo)) 
   (* x (Food-fat foo)) 
   (* x (Food-carbs foo)) 
   (* x (Food-protein foo))))
(check-expect 
 (food-scale f5 4) 
 (Food "xMashed Potatoes" 960 40 148 20))

(: meal-scale (-> Meal Exact-Rational Meal))
;;scale the numbers of the food (serving size, fat, protein, and carbs) to the number provided
(define (meal-scale foo x)
  (cond
    [(empty? foo) empty]
    [(cons? foo)
     (cons
      (food-scale (first foo) x)
      (meal-scale (rest foo) x))]))
(check-expect
 (meal-scale Meal1 2)
 (list
  (Food "xPepperoni Cheese Steak" 402 60 80 60)
  (Food "xFrench Fries" 268 24 86 8)
  (Food "xIce Cream" 214 16 54 8)))

(: conform-to-diet (-> Meal Meal))
;;scale meal down so that cumulative fat is 25gr
(define (conform-to-diet m)
  (cond
    [(empty? m) empty]
    [(cons? m)
     (meal-scale m (/ 25 (meal-fat m)))]))
(define x1 (meal-scale Meal1 (/ 1 2)))
(check-expect (conform-to-diet Meal1) x1)

;; = Helper function =
(: food-kcal (-> Food Exact-Rational))
;;compute the kilocalories in the food
(define (food-kcal foo) 
  (+ (* 9 (Food-fat foo))
     (* 4 (Food-carbs foo))
     (* 4 (Food-protein foo))))
(check-expect (food-kcal f2) 296)
(check-expect (food-kcal f6) 278)

(: meal-kcal (-> Meal Exact-Rational))
;;calculate the calories of a whole meal
(define (meal-kcal mymeal)
  (cond
   [(empty? mymeal) 0]
   [(cons? mymeal)
    (+
     (food-kcal (first mymeal))
     (meal-kcal (rest mymeal)))]))
(check-expect (meal-kcal Meal1) 1042)

(: meal-healthy? (-> Meal Exact-Rational Boolean))
;;determine whether the meal is healthy based on criteria from hw
(define (meal-healthy? diet quota)
  (cond
   [(empty? diet) (error "no input data")]
   [(cons? diet)
    (cond
      [(and
        (< (meal-kcal diet) (* 0.5 quota))
        (< (meal-fat diet) 50)
        (< (meal-fat diet) (/ (meal-kcal diet) 3)))
       true]
      [else false])]))
(check-expect (meal-healthy? Meal1 2000) false)
(check-expect (meal-healthy? Meal2 3000) false)

;; == PROBLEM 3 ==

(define-struct Point
  ([x : Real]
   [y : Real])
  #:transparent)

(define-struct Rectangle
  ([blc : Point] ;;blc -> bottom left corner
   [length : Real]
   [width : Real])
  #:transparent)

(define-struct Circle
  ([center : Point]
   [radius : Real])
  #:transparent)

(define-type circles (Listof Circle))
(define A (make-Circle (make-Point 0 0) 1))
(define B (make-Circle (make-Point 0.5 0) 1))
(define C (make-Circle (make-Point 2.5 0) 1))
(define D (make-Circle (make-Point 0.25 0.4330127018922193) 1))
(define E (make-Circle (make-Point 0.25 -0.4330127018922193) 1))
(define F (make-Circle (make-Point 0.5 0) 0.75))
(define G (make-Circle (make-Point 0.25 0) 1.25))
(define L1 (list A B C D E F G))
(define no-circles '())
(define one-circle (list A))
(define two-circles (list A B))
(define no-overlap (list A B C))
(define three-circles (list A B D))
(define four-circles (list A B D E))
(define two-diff-radii (list A F))
(define two-plus-total-overlap (list A B G))

;;distance helper function
(: distance : Point Point -> Real)
;;compute the distance between two Points
(define (distance A B)
  (sqrt
    (+ 
     (sqr (- (Point-x A) (Point-x B)))
     (sqr (- (Point-y A) (Point-y B))))))
(check-expect (distance (make-Point 0 0) (make-Point 3 4)) 5)
(check-expect (distance (make-Point -3 3) (make-Point 0 -1)) 5)

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

(: within-circle? : Circle Point -> Boolean)
;;checks to see if the circle is within the circle
(define (within-circle? whatup notmuch)
  (cond 
    [(> (Circle-radius whatup) 
        (distance (Circle-center whatup) notmuch)) true]
    [else false]))
(check-expect (within-circle? C (make-Point 3 0.3)) true)
(check-expect (within-circle? E (make-Point 0 0)) true)

(: x-center : circles -> (Listof Real))
;;extract the x-value of the center of circles
(define (x-center c)
  (cond
    [(empty? c) empty]
    [(cons? c)
     (cons
      (Point-x (Circle-center (first c)))
      (x-center (rest c)))]))
(check-expect (x-center two-circles) (list 0 0))
(check-expect (x-center three-circles) (list 0 0 0.25))

(: y-center : circles -> (Listof Real))
;;extract the x-value of the center of circles
(define (y-center c)
  (cond
    [(empty? c) empty]
    [(cons? c)
     (cons
      (Point-y (Circle-center (first c)))
      (y-center (rest c)))]))
(check-expect (y-center two-circles) (list 0 0))
(check-expect (y-center three-circles) (list 0 0 0.4330127018922193))

(: radius : circles -> (Listof Real))
;;extract the x-value of the center of circles
(define (radius c)
  (cond
    [(empty? c) empty]
    [(cons? c)
     (cons
       (Circle-radius (first c))
       (radius (rest c)))]))
(check-expect (radius four-circles) (list 1 1 1 1))
(check-expect (radius two-diff-radii) (list 1 0.75))

(: min-x : circles -> Real)
;;find the x-coordinate for the lower-left corner
(define (min-x hello)
  (cond 
    [(empty? hello) (error "empty list")]
    [else
     (apply min (map - (x-center hello) (radius hello)))]))
(check-within (min-x L1) -1 0.01)
(check-within (min-x no-overlap) -1 0.01)
(check-within (min-x three-circles) -1 0.01)

(: min-y : circles -> Real)
;;find the y-coordinate for the lower-left corner
(define (min-y bye)
  (cond 
    [(empty? bye) (error "empty list")]
    [else
     (apply min (map - (y-center bye) (radius bye)))]))
(check-within (min-y L1) -1.433013 0.0001)

(: max-x : circles -> Real)
;;find the max x-value
(define (max-x q)
  (cond 
    [(empty? q) (error "empty list")]
    [else
     (apply max (map + (x-center q) (radius q)))]))
(check-within (max-x L1) 3.5 0.01)

(: max-y : circles -> Real)
;;find the max y-value
(define (max-y q)
  (cond 
    [(empty? q) (error "empty list")]
    [else
     (apply max (map + (y-center q) (radius q)))]))
(check-within (max-y L1) 1.433013 0.001)

(: bounding-box : circles -> Rectangle)
;;find the smallest rectangle enclosing the two circles
(define (bounding-box many)
  (make-Rectangle
   (make-Point 
    (min-x many)
    (min-y many))
   (-
    (max-x many)
    (min-x many))
   (-
    (max-y many)
    (min-y many))))
(check-expect (bounding-box (list (make-Circle (make-Point 5 10) 1) (make-Circle (make-Point 20 5) 5))) (Rectangle (Point 4 0) 21 11))

(: within-intersection? : circles Point -> Boolean)
;;check to see if point lies within intersection of two circles
(define (within-intersection? list p)
  (cond 
    [(empty? list) true]
    [else (if
           (and 
            (within-circle? (first list) p)
            (within-intersection? (rest list) p)) true false)]))
(check-expect (within-intersection? L1 (make-Point 0 5)) false)

(: foo : circles Integer -> Real)
;;find the number of times that the random point succesfully 
;;lies in the intersection between the two circles
(define (foo a m)
  (cond
    [(= m 0) 0]
    [(within-intersection? a (throw-dart (bounding-box a)))
     (+ 1 (foo a (- m 1)))]
    [else (foo a (- m 1))]))
(check-range
 (foo L1 1000) 0 (* 64 1000))

(: area-of-intersection-monte-carlo : circles Integer -> Real)
;;approximation of the area of the intersection between two circles
(define (area-of-intersection-monte-carlo A n)
  (cond
    [(empty? A) 0]
    [else
     (*
      (Rectangle-length (bounding-box A))
      (Rectangle-width (bounding-box A))
      (/ (foo A n) n))]))
(check-within (area-of-intersection-monte-carlo no-circles 100000) 0 0.1)
(check-within (area-of-intersection-monte-carlo one-circle 100000) 3.141996 0.1)
(check-within (area-of-intersection-monte-carlo two-circles 100000) 2.152585 0.1)
(check-within (area-of-intersection-monte-carlo three-circles 100000) 1.76309 0.1)
(check-within (area-of-intersection-monte-carlo four-circles 100000) 1.38566 0.1)
(check-within (area-of-intersection-monte-carlo no-overlap 100000) 0 0.0001)
(check-within (area-of-intersection-monte-carlo two-diff-radii 100000) 1.4765 0.1)
(check-within (area-of-intersection-monte-carlo two-plus-total-overlap 100000) 2.1558 0.1)


;; === Problem 4 ===

(define-struct (T) Rose-Tree
  ([value : T]
   [kids : (Listof (Rose-Tree T))])
  #:transparent)

(: int-tree : (Rose-Tree Integer))
(define int-tree 
  (make-Rose-Tree
   4
   (list (make-Rose-Tree -1 empty)
         (make-Rose-Tree 12 empty)
         (make-Rose-Tree 42 empty))))

(: string-tree : (Rose-Tree String))
(define string-tree
  (make-Rose-Tree
   "AAA"
   (list (make-Rose-Tree "B" 
                         (list (make-Rose-Tree "CC" empty)
                               (make-Rose-Tree "D" empty)))
         (make-Rose-Tree "E"
                         (list (make-Rose-Tree "FFF" empty)))
         (make-Rose-Tree "G" empty))))

(: rose-size : (All (T) (Rose-Tree T) -> Natural))
;;count the number of nodes in the tree
(define (rose-size  tree)
  (cond 
    [(empty? (Rose-Tree-kids tree)) 1]
    [else 
     (+
      (rose-size (first (Rose-Tree-kids tree)))
      (rose-size
       (make-Rose-Tree
        (Rose-Tree-value tree)
        (rest (Rose-Tree-kids tree)))))]))
(check-expect (rose-size int-tree) 4)
(check-expect (rose-size string-tree) 7)

(: rose-height : (All (T) (Rose-Tree T) -> Natural))
;;determine the height of a Rose Tree
(define (rose-height tree)
  (cond
    [(empty? (Rose-Tree-kids tree)) 1]
    [else
     (max
     (if
       (>= (length(Rose-Tree-kids tree)) 1)
       (+ 1 (rose-height (first (Rose-Tree-kids tree))))
       (rose-height 
         (make-Rose-Tree
          (Rose-Tree-value tree)
          (rest (Rose-Tree-kids tree))))))]))
(check-expect (rose-height int-tree) 2)
(check-expect (rose-height string-tree) 3)

(test)