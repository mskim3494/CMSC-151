#lang typed/racket

(define-struct rtree
  ([node : Integer]
   [kids : (Listof rtree)])
  #:transparent)

(: rose-map : (Integer -> Integer) rtree -> rtree)
;;
(define (rose-map f rt)
  (match rt
    [(rtree n '()) (rtree (f n) '())]
    [(rtree n k)
     (rtree (f n) 
            (map (lambda ([z : rtree]) (rose-map f z)) k))]))

(: max-depth : rtree -> Integer)
;;
(define (max-depth rt)
  (local
    {(: counting : rtree Integer -> Integer)
     ;;
     (define (counting rt x)
       (match rt
         [(rtree n '()) x]
         [(rtree n kids)
          (argmax (lambda ([z : Integer]) z)
          (map (lambda ([k : rtree]) (counting k (+ x 1))) kids))]))}
    (counting rt 1)))

(max-depth (rtree 1 '()))
(max-depth (rtree 1 (list 
                     (rtree 2 
                            (list (rtree 3 '())))
                     (rtree 4 
                            (list 
                             (rtree 5 (list (rtree 6 '()))))))))

(define-struct Vertex
  ([label : Symbol]
   [mark : Boolean]
   [succ : (Listof Vertex)])
  #:transparent)

(define-struct tree 
  ([node : Vertex]
   [kids : (Listof tree)]) #:transparent)

(define-type Graph (Listof Vertex))

(: spanning-tree : Vertex -> tree)
;;
(define (spanning-tree v)
  (match v
    [(Vertex l #t s)
     (tree (Vertex l #t s) '())]
    [(Vertex l #f s)
     (tree
      (Vertex l #t s)
      (map (lambda ([z : Vertex]) (spanning-tree z)) s))]))

;(: A1 Vertex)
;;(define A1 (Vertex 'A #f (list (Vertex 'B #f (list  E1)))
;(: B1 Vertex)
;;(define B1 (Vertex 'B #f (list F1 E1)))
;(: C1 Vertex)
;(: D1 Vertex)
;(: E1 Vertex)
;(: F1 Vertex)
;(: G1 Vertex)
;
;(define C1 (Vertex 'C #f (list D1)))
;(define D1 (Vertex 'D #f (list E1)))
;(define E1 (Vertex 'E #f (list 
;                          (Vertex 'F #f '())
;                          (Vertex 'C #f (list (Vertex 'D #f (list (Vertex 'E #f )))
;(define F1 (Vertex 'F #f (list G1 D1)))
;(define G1 (Vertex 'G #f '()))
;(define graph1 (list A1 B1 C1 D1 E1 F1 G1))

(define-struct Int ([z : Integer]) #:transparent #:mutable)

(: x : Int -> Void)
(define (x y)
  (begin
    (set-Int-z! y (+ (Int-z y) 1))
    (set-Int-z! y (* (Int-z y) 2))))

(define v1 (vector-immutable (vector 1 2 3 4)))
(define v2 (vector 9 8 7 6))
(define v3 v1)

(: quicksort : (Listof Integer) -> (Listof Integer))
(define (quicksort l)
  (match l
    ['() '()]
    [(cons a '()) (list a)]
    [(cons a b)
     (append
      (quicksort
       (filter (lambda ([z : Integer]) (<= z a)) b))
      (quicksort
       (filter (lambda ([z : Integer]) (> z a)) b )))]))






