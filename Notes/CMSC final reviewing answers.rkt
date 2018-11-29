#lang typed/racket

;; FINAL QUESTIONS REVIEW
;; PART 1
(: curry : (All (A B C) (A B -> C) -> (A -> (B -> C))))
;;
(define (curry f)
  (lambda ([x : A])
    (lambda ([y : B]) (f x y))))

(: less : Integer Integer -> Boolean)
;;
(define (less x y) (<= x y))
(filter ((curry less) 3) (list 1 2 3 4))

(foldl string-append "0" (list "w" "x" "y" "z"))
(define v (vector 1 2 3 4 ))

;; gets you error
;;(foldl (lambda ([x : Integer] [acc : Integer]) (+ acc (vector-ref v x))) 0 (list 2 3 4))

(define-struct (A) Node
  ([node : A]
   [left : B-Tree]
   [right : B-Tree])
  #:transparent)

(define-type (B-Tree A) (U 'nil Node))

(: count-nodes : (All (A) B-Tree -> Integer))
;;
(define (count-nodes btree)
  (match btree
    ['nil 0]
    [(Node v l r)
     (+ 1 (count-nodes l) (count-nodes r))]))

(: tree->vector : (All (A) B-Tree -> (Vectorof A)))
;;
(define (tree->vector btree)
  (match btree
    (define v (make-vector (count-nodes btree) (
    ['nil ]
    [(Node n l r)
     (

;;roll dice
(: histogram : Integer -> (Vectorof Integer))
;;
(define (histogram n)
  (local
    {(define v (make-vector 11 0))
     (: thrown-dice : Integer -> Void)
     ;;
     (define (thrown-dice n)
       (if (= n 0) (void)
           (local
             {(define roll (+ (random 6) (random 6)))}
              (begin
                (vector-set! v roll (add1 (vector-ref v roll)))
                (thrown-dice (sub1 n))))))}
    (thrown-dice n) v))
(histogram 8)




















