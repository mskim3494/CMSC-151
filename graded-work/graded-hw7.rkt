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

;; === automated test code follows

;; == some type abbreviations for readability

(define-type Int Integer)
(define-type Bool Boolean)
(define-type (Vec α) (Vectorof α))

(: check-points-handler : String -> (Any -> Void))
;; custom exception handler for check-points, given problem number
;; - displays message about the exception raised
(define ((check-points-handler problem-name) x)
   (display (string-append "\ncheck-points: "
                           "error raised while testing problem "
                           problem-name
                           (if (exn? x)
                               (local {(define msg (exn-message x))}
                                 (if (string=? msg "")
                                     ": <empty error message>"
                                     (string-append ":\n" msg)))
                               "")
                           "\n")))
  
(: check-points : (Vec String) (Vec Int) -> (Int Int (-> Bool) -> Void))
;; run the test, record points earned in points vector
(define ((check-points problem-names scores) prob pts run-test)
  (local {(define prob-name (vector-ref problem-names prob))}
    (with-handlers 
        ([exn? (check-points-handler prob-name)])
      (match (run-test)
        [#t (vector-set! scores prob (+ pts (vector-ref scores prob)))]
        [result 
         (begin
           (display (string-append "\ncheck-points: "
                                   "unexpected result testing problem "
                                   prob-name
                                   "\n"))
           (display "unexpected result: ")
           (display result)
           (newline))]))))


(: problem : (Vec String) (Vec Int) -> (Int String Int -> Void))
;; register problem in problem name and max scores vectors
(define ((problem problem-names max-scores) i s pts)
  (begin
    (vector-set! problem-names i s)
    (vector-set! max-scores i pts)))

(: grade-report : (Vec String) (Vec Int) (Vec Int) -> Void)
;; display information about results of all tests
(define (grade-report problem-names scores max-scores)
  (local 
    {(define as-list (vector->list scores))
     (define total (foldl + 0 as-list))
     (: print-result : Integer -> Void)
     (define (print-result i)
       (display (string-append
                 ";; problem " (vector-ref problem-names i) " : "
                 (number->string (vector-ref scores i))
                 "/" (number->string (vector-ref max-scores i)) "\n")))}
    (begin
      (newline)
      (display ";; === evaluation\n\n")
      (build-list 10 print-result)
      (display (string-append "\n;; _total-" "score_ " ;; so grep isn't fooled
                              (number->string total) "/50\n")))))

(display (string-append "\n\n;; =========================================\n"
                        ";; === begin automated evaluation of hw7 ===\n"
                        ";; =========================================\n"))

(define problem-names-vec (make-vector 10 ""))
(define grading-scores-vec (make-vector 10 0))
(define max-scores-vec (make-vector 10 0))

(define prob (problem problem-names-vec max-scores-vec))
(define check (check-points problem-names-vec grading-scores-vec))

;;;;;;;;;;; PROBLEM 1 ;;;;;;;;;;

(prob 0 "1 (mutable counter)" 6)
(check 0 6
 (lambda ()
 (local
   {(define cntr (Counter 0))
    (define inc-cntr (make-increment cntr))}
   (begin
     (inc-cntr 5)
     (inc-cntr -3)
     (inc-cntr 4)
     (match cntr
       [(Counter 6) #t]
       [else #f])))))

;;;;;;;;;;; PROBLEM 2 ;;;;;;;;;;

(define gr_test-tree : (VTree Integer) (vector 12 9 16 5 10 14))

;;;;; Part a

(prob 1 "2a (in-tree?)" 2)
(check 1 1
  (lambda ()
    (in-tree? gr_test-tree 3))) ;; also 0
(check 1 1
  (lambda ()
   (not (in-tree? gr_test-tree -1))))

;;;;; Part b

(prob 2 "2b (left-kid)" 2)
(prob 3 "2b (right-kid)" 2)
(prob 4 "2b (parent)" 3)
(check 2 1
  (lambda ()
    (= (left-kid 3) 7)))
(check 3 1
  (lambda ()
    (= (right-kid 3) 8)))
(check 2 1
  (lambda ()
    (= (left-kid 4) 9)))
(check 3 1
  (lambda ()
    (= (right-kid 4) 10)))
(check 4 1
  (lambda ()
    (= (parent (left-kid 3)) 3)))
(check 4 1
  (lambda ()
    (= (parent (right-kid 3)) 3)))
(check 4 1
  (lambda ()
    (= (parent (left-kid 4)) 4))) ;; also right

;;;;; Part c

(prob 5 "2c (vtree->list)" 8)
(check 5 8
  (lambda ()
    (equal? (vtree->list gr_test-tree) (list 5 9 10 12 14 16))))

;;;;; Part d

(prob 6 "2d (bst-vtree-member?)" 8)
(check 6 1
  (lambda ()
    (not (bst-vtree-member? gr_test-tree 0))))
(check 6 1
  (lambda ()
    (bst-vtree-member? gr_test-tree 5)))
(check 6 1
  (lambda ()
    (bst-vtree-member? gr_test-tree 12)))
(check 6 1
  (lambda ()
    (not (bst-vtree-member? gr_test-tree 13))))
(check 6 2
  (lambda ()
    (bst-vtree-member? gr_test-tree 16)))
(check 6 2
  (lambda ()
    (not (bst-vtree-member? gr_test-tree 18))))

;;;;;;;;;; PROBLEM 3 ;;;;;;;;;;

(prob 7 "3 (rotate-right)" 10)
(check 7 5
  (lambda ()
    (local {(define v (vector 0 1 2 3 4 5 6 7 8 9))}
      (begin (rotate-right v 0 9)
        (equal? v (vector 9 0 1 2 3 4 5 6 7 8))))))
(check 7 5
  (lambda ()
    (local {(define v (vector 0 1 2 3 4 5 6 7 8 9))}
      (begin (rotate-right v 2 3)
        (equal? v (vector 0 1 3 2 4 5 6 7 8 9))))))

;;;;;;;;;; PROBLEM 4 ;;;;;;;;;;

(define gr_test-g : (Graph Symbol)
  (vector
   (Vertex 0 'A '(1 4))  ;; A -> B; E
   (Vertex 1 'B '(4 5))  ;; B -> E; F
   (Vertex 2 'C '(3))    ;; C -> D
   (Vertex 3 'D '())
   (Vertex 4 'E '(2 5))  ;; E -> C; F
   (Vertex 5 'F '(3 6))  ;; F -> D; G
   (Vertex 6 'G '())))

(prob 8 "4 (valid-edge?)" 3)
(prob 9 "4 (valid-path?)" 6)

;;;;; Part a

(check 8 1
  (lambda ()
    (not ((valid-edge? gr_test-g) 1 0))))
(check 8 1
  (lambda ()
    ((valid-edge? gr_test-g) 2 3)))
(check 8 1
  (lambda ()
    (not ((valid-edge? gr_test-g) 3 4))))
;; also 0-1, 0-2 (not), 0-4

;;;;; Part b

(check 9 1
  (lambda ()
    (valid-path? gr_test-g '())))
(check 9 1
  (lambda ()
    (valid-path? gr_test-g '(1)) #t))
(check 9 1
  (lambda ()
    (valid-path? gr_test-g '(0 4 2 3)) #t))
(check 9 3
  (lambda ()
    (not (valid-path? gr_test-g '(0 4 2 3 6)))))

(grade-report problem-names-vec grading-scores-vec max-scores-vec)

All 29 tests passed!


;; =========================================
;; === begin automated evaluation of hw7 ===
;; =========================================

check-points: unexpected result testing problem 2a (in-tree?)
unexpected result: #f

check-points: error raised while testing problem 4 (valid-path?):
match: no matching clause for '()

;; === evaluation

;; problem 1 (mutable counter) : 6/6
;; problem 2a (in-tree?) : 1/2
;; problem 2b (left-kid) : 2/2
;; problem 2b (right-kid) : 2/2
;; problem 2b (parent) : 3/3
;; problem 2c (vtree->list) : 8/8
;; problem 2d (bst-vtree-member?) : 8/8
;; problem 3 (rotate-right) : 10/10
;; problem 4 (valid-edge?) : 3/3
;; problem 4 (valid-path?) : 5/6

;; _total-score_ 48/50
