#lang typed/racket
(require typed/test-engine/racket-tests)

;; =========== HOMEWORK 7 ===========

;; PROBLEM 1 

(define-struct Counter 
  ([ctr : Integer])
  #:transparent
  #:mutable)

(define ctr1 (Counter 1))
(define ctr2 (Counter 2))

(: make-increment : Counter -> (Integer -> Void))
;; curried function increments the Counter by the specified
;; amount when it is called
(define (make-increment c)
  (lambda ([z : Integer])
    (set-Counter-ctr! c (+ (Counter-ctr c) z))))
(check-expect (begin ((make-increment ctr1) 2) ctr1) (Counter 3))
(check-expect (begin ((make-increment ctr1) 5) ctr1) (Counter 8))
(check-expect (begin ((make-increment ctr2) -2) ctr2) (Counter 0))

;; PROBLEM 2

(define-type (VTree A) (Vectorof A))

(define v1 (vector 0 1 2 3 4 5 6 7 8))
(define v2 (vector 8 3 5 2 9 0 1))

(: in-tree? : (All (A) (VTree A) Integer -> Boolean))
;; function to test if an index is in the tree
(define (in-tree? vt n)
  (> (vector-length vt) n))
(check-expect (in-tree? v1 10) #f)
(check-expect (in-tree? v2 3) #t)

(: left-kid : Integer -> Integer)
;; compute the index of left kid
(define (left-kid n)
  (+ 1 (* 2 n)))
(check-expect (left-kid 0) 1)
(check-expect (left-kid 3) 7)
(check-expect (left-kid 4) 9)

(: right-kid : Integer -> Integer)
;; compute the index of right kid
(define (right-kid n)
  (+ 2 (* 2 n)))
(check-expect (right-kid 0) 2)
(check-expect (right-kid 2) 6)
(check-expect (right-kid 5) 12)

(: parent : Integer -> Integer)
;; compute the index of parent node
(define (parent n)
  (cond
    [(odd? n) (exact-floor (/ (- n 1) 2))]
    [(even? n) (exact-floor (/ (- n 2) 2))]
    [else n]))
(check-expect (parent 12) 5)
(check-expect (parent 6) 2)
(check-expect (parent 9) 4)
(check-expect (parent 1) 0)

(: vtree->list : (All (A) (VTree A) -> (Listof A)))
;; return a list of vtree in-order traversal
(define (vtree->list vt)
  (local
    {(: tolist : (All (A) (VTree A) Integer -> (Listof A)))
     ;; execution of vtree->list on vtree
     (define (tolist vt n)
       (if
         (not (in-tree? vt n)) '()
          (append
           (tolist vt (left-kid n))
           (list (vector-ref vt n))
           (tolist vt (right-kid n)))))}
    (tolist vt 0)))
(check-expect (vtree->list v1) '(7 3 8 1 4 0 5 2 6))
(check-expect (vtree->list v2) '(2 3 9 8 0 5 1))

(: bst-vtree-member? : (VTree Integer) Integer -> Boolean)
;; checks to see if a key is in a Binary Search Tree
(define (bst-vtree-member? bst n)
  (local
    {(: binary-search : (VTree Integer) Integer Integer -> Boolean)
     ;; uses binary search property to find the key
     (define (binary-search vt n i)
       (cond
         [(not (in-tree? vt i)) #f]
         [(= (vector-ref vt i) n) #t]
         [(< n (vector-ref vt i)) (binary-search vt n (left-kid i))]
         [(> n (vector-ref vt i)) (binary-search vt n (right-kid i))]
         [else #f]))}
    (binary-search bst n 0)))
(define v3 '#(5 1 8 0 3 6 9))
(check-expect (bst-vtree-member? v3 3) #t)
(check-expect (bst-vtree-member? v3 10) #f)

;; PROBLEM 3

(: rotate-right : (All (A) (Vectorof A) Integer Integer -> Void))
;; rotate a contiguous sub-sequence of 
;; vector elements to the right by one position
(define (rotate-right v n m)
  (define nn (vector-ref v n))
  (define mm (vector-ref v m))
  (if
   (or (not (in-tree? v n)) (not (in-tree? v m)))
   (error "invalid index")
   (begin
     (vector-copy! v (+ n 1) v n m)
     (vector-set! v n mm))))
(define v (vector 0 1 2 3 4 5 6 7 8 9))
(check-expect (begin (rotate-right v 2 5) v) '#(0 1 5 2 3 4 6 7 8 9))
(check-expect (begin (rotate-right v1 2 6) v1) '#(0 1 6 2 3 4 5 7 8))

;; PROBLEM 4

(define-struct (A) Vertex 
  ([id : Integer]
   [attr : A]
   [succs : (Listof Integer)])
  #:transparent
  #:mutable)

(define-type (Graph A) (Vectorof (Vertex A)))

(: valid-edge? : (All (A) (Graph A) -> (Integer Integer -> Boolean)))
;; function for testing if an edge (specified as two integers) 
;; exists in the graph
(define (valid-edge? g)
  (lambda ([n : Integer] [m : Integer])
    (local
      {(define id 
         (vector-filter (lambda ([z : (Vertex A)])
                       (= (Vertex-id z) n)) g))}
      (if (= (vector-length id) 0) #f
          (local
            {(define idd 
               (vector->list
               (vector-map (lambda ([y : (Vertex A)]) (Vertex-succs y)) id)))
             (define ids
              (filter (lambda ([k : Integer]) (= m k)) 
                      (foldr (inst append Integer) '() idd)))}
            (not (= (length ids) 0)))))))
(define graph
  (vector (Vertex 0 'A '(1 2 3))
          (Vertex 3 'B '(2 4))
          (Vertex 0 'C '(3 4))
          (Vertex 5 'D '(2 0))
          (Vertex 2 'C '(3 5))
          (Vertex 3 'C '(6 5))))
(check-expect ((valid-edge? graph) 3 2) #t)
(check-expect ((valid-edge? graph) 2 3) #t)
(check-expect ((valid-edge? graph) 5 7) #f)
(check-expect ((valid-edge? graph) 0 3) #t)

(: valid-path? : (All (A) (Graph A) (Listof Integer) -> Boolean))
; check to see whether list of vertices represent a valid path 
; from the first vertex to the last vertex in the list
(define (valid-path? g lst)
  (match lst
    [(cons hd '()) #t]
    [(cons hd tl)
     (if ((valid-edge? g) hd (car tl))
         (valid-path? g tl)
         #f)]))
(check-expect (valid-path? graph (list 0 2 3 5 0 4)) #t)
(check-expect (valid-path? graph (list 0 2 3 5 1 4)) #f)
(check-expect (valid-path? graph (list 0 3 5 2 5 0 1)) #t)
(check-expect (valid-path? graph (list 0 3 5 2 5 0 5)) #f)

(test)