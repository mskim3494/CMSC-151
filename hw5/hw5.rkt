#lang typed/racket
(require typed/test-engine/racket-tests)

;; ===== HOMEWORK 5 ======


;; === PROBLEM 1 ====

(: catw : String (Listof String) -> String)
;;concactenate given list into a single string with given separator
(define (catw x y)
  (cond
    [(empty? y) ""]
    [(cons? y) 
     (if 
      (empty? (rest y))
      (string-append (first y))
      (string-append (first y) x (catw x (rest y))))]))
(check-expect (catw "<*>" '("A" "B" "C")) "A<*>B<*>C")
(check-expect (catw ":" '("A" "B" "C")) "A:B:C")
(check-expect (catw ":" '("A")) "A")
(check-expect (catw ":" '()) "")

(: every : (All (a) (a -> Boolean) (Listof a) -> Boolean))
;;test if every item in the list passes given test
(define (every x y)
  (cond
    [(empty? y) true]
    [(cons? y) 
     (if 
      (x (first y))
      (every x (rest y))
      false)]))
(check-expect (every even? '(1 2 3)) #f)
(check-expect (every positive? '(1 2 3)) #t)
(check-expect (every positive? '()) #t)

(: discard : (All (a) (a -> Boolean) (Listof a) -> (Listof a)))
;;discard all items that pass the test
(define (discard x y)
  (cond
    [(empty? y) empty]
    [(cons? y)
     (if
      (not (x (first y)))
      (cons (first y) (discard x (rest y)))
      (discard x (rest y)))]))
(check-expect (discard even? '(1 2 3)) '(1 3))
(check-expect (discard positive? '(1 2 3)) '())


;; === PROBLEM 2 ===
(define-type   Exp       (U Literal Operation))
(define-struct Literal   ([val : Integer]) #:transparent)
(define-type   Operator  (U '+ '*))
(define-struct Operation ([op : Operator] [es : (Listof Exp)] [en : Exp]) #:transparent)

(define tree1 
  (make-Operation 
   '+
   (list (Literal 1) (make-Operation '* (list (Literal 4) (Literal 5) (Literal 6)) (Literal 7)))
   (make-Operation '* (list (Literal 10)) (make-Operation '+ (list (Literal 1)) (Literal 2)))))
      

(: eval-tree : Exp -> Integer)
;;evaluate the operation in the tree
(define (eval-tree t)
  (match t
    [(Literal t) t]
    [(Operation _ '() en) (eval-tree en)]
    [(Operation '+ es en) (+ (eval-tree (first es)) (eval-tree (make-Operation '+ (rest es) en)))]
    [(Operation '* es en) (* (eval-tree (first es)) (eval-tree (make-Operation '* (rest es) en)))]))
(check-expect (eval-tree tree1) 871)

(: unparse : Exp -> String)
;;turn the expression tree into a string
(define (unparse t)
  (local
    {(: y : (Listof String) -> String)
     (define (y l)
       (cond
         [(empty? l) ""]
         [(cons? l) (string-append (first l) " " (y (rest l)))]))}
  (match t
    [(Literal t) (number->string t)]
    [(Operation _ '() en) (string-append (unparse en))]
    [(Operation op es en) (string-append "(" (symbol->string op) " " (y (map unparse es)) (unparse en) ")")])))
(check-expect (unparse tree1) "(+ 1 (* 4 5 6 7) (* 10 (+ 1 2)))")

(eval (read (open-input-string (unparse tree1))))

(test)