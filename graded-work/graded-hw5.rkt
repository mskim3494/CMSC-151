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
     (if ;; grader: nesting if inside of cond. -1 layout
      (empty? (rest y))
      (string-append (first y)) ;; grader: string-append here is unnecessary.
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
   (list (Literal 1) (make-Operation '* (list (Literal 4) (Literal 5) (Literal 6)) (Literal 7))) ;; grader: lines over 80 chars. -1 layout
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

; grader: inadequate testing. -1 tests

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
; grader: does not account for es empty operators -> e.g. grader test (unparse tt3) should be "(* 3)", not "3". -2
(check-expect (unparse tree1) "(+ 1 (* 4 5 6 7) (* 10 (+ 1 2)))")
; grader: inadequate testing. -1 tests

(eval (read (open-input-string (unparse tree1))))

;(test)
;; === Grader's Tests ===

;; catw
(check-expect (catw "<*>" '("A" "B" "C")) "A<*>B<*>C")
(check-expect (catw ":" '("A" "B" "C")) "A:B:C")
(check-expect (catw ":" '("A" "B" "C" "D")) "A:B:C:D")
(check-expect (catw "+" '("A")) "A")
(check-expect (catw ":" '()) "")

;; every
(check-expect (every even? '(1 2 3)) #f)
(check-expect (every positive? '(1 2 3)) #t)
(check-expect (every positive? '(1 -2 3)) #f)
(check-expect (every positive? '()) #t)
(check-expect (every (lambda ([x : Integer]) (> x 5)) '(6 7 8)) #t)
(check-expect (every (lambda ([x : Integer]) (> x 5)) '(5 6 7)) #f)

;; discard
(check-expect (discard even? '(1 2 3)) '(1 3))
(check-expect (discard positive? '(1 2 3)) '())
(check-expect (discard (lambda ([s : String]) (> (string-length s) 2))
	      	       '("A" "B" "AB" "C" "ABC" "D" "ABCD" "E"))
	      '("A" "B" "AB" "C" "D" "E"))
(check-expect (discard even? '()) '())

;; eval-tree, unparse
(define tt1
  (Operation '+
             (list (Literal 1)
                   (Operation '* (list (Literal 4) (Literal 5) (Literal 6))
		   	      	 (Literal 7)))
             (Operation '* (list (Literal 10)) (Operation '+ (list (Literal 1))
	     		   (Literal 2)))))
(define tt2
  (Literal 3))
(define tt3
  (Operation '* '() (Literal 3)))
(define tt4
  (Operation '+ (list (Literal 1) (Literal 2)
  	     	      (Operation '* (list (Literal 1) (Literal 2)) (Literal 3))
		      (Literal 4))
		(Literal 5)))

(check-expect (eval-tree tt1) 871)
(check-expect (eval-tree tt2) 3)
(check-expect (eval-tree tt3) 3)
(check-expect (eval-tree tt4) 18)

(printf "=== Eyeball Check for unparse ===~n")
(printf "tt1 -> \"(+ 1 (* 4 5 6 7) (* 10 (+ 1 2)))\"~n       ")
(unparse tt1)
(printf "tt2 -> \"3\"~n       ")
(unparse tt2)
(printf "tt3 -> \"(* 3)\"~n       ")
(unparse tt3)
(printf "tt4 -> \"(+ 1 2 (* 1 2 3) 4 5)\"~n       ")
(unparse tt4)

(test)

;; ====== correctness

;; === correctness ===

;; problem 1 (catw)                  4 / 4
;; problem 1 (every)                 4 / 4
;; problem 1 (discard)               3 / 3

;; problem 2 (eval-tree)             6 / 6
;; problem 2 (unparse)               3 / 5

;; === style ===

;; code layout                       2 / 4 (lines over 80 chars, nesting if inside cond)
;; identifiers are well named        4 / 4
;; program decomposition (helpers)   2 / 2

;; contracts (types)                 4 / 4
;; well-written purposes             4 / 4
;; adequate tests                    4 / 4

;; clarity (clear logic)             3 / 3

;; svn used correctly                3 / 3

;; _total-score_                    46 / 50

;; grader: Mark Maskeri