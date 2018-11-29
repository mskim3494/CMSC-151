#lang typed/racket
(require typed/test-engine/racket-tests)


;; === polymorphic data structures (broadly useful)

(define-struct (a b) Pair
  ([first  : a]
   [second : b])
  #:transparent)

(define-type (Maybe a) (U 'Nothing (Just a)))

(define-struct (a) Just
  ([x : a])
  #:transparent)

(define-type Order (U 'Less 'Equal 'Greater))

(define-type (Cmp a) (a a -> Order))

;; === BST map data structures

(define-struct (key val) BSTMap
  ([compare : (Cmp key)]
   [data    : (BST key val)])
  #:transparent)

(define-type (BST key val) (U 'E (Node key val)))
;; note the symbol 'E is used for the empty tree

(define-struct (key val) Node
  ([root : key]
   [v    : val]
   [lsub : (BST key val)]
   [rsub : (BST key val)])
  #:transparent)

(define tree (Node 5 "Regenstein"
                    (Node 1 "Ryerson"
                          (Node 0 "Harper" 'E 'E)
                          (Node 3 "Crerar" 'E 'E))
                    (Node 8 "Pick"
                          (Node 6 "Kent" 'E 'E)
                          (Node 9 "Ratner" 'E 'E))))

(: cmp-int (Cmp Integer))
(define (cmp-int m n)
  (cond
    [(< m n) 'Less]
    [(= m n) 'Equal]
    [else 'Greater]))

(: map-apply : (All (key val) (BSTMap key val) key -> (Maybe val)))
;; if the key is present in the tree, return the associated value in a Just;
;; otherwise, return 'Nothing
(define (map-apply m k)
  (match m
    [(BSTMap cmp t)
     (local
       {(: lp : (BST key val) -> (Maybe val))
        ;; search for k in t
        (define (lp t)
          (match t
            ['E 'Nothing]
            [(Node r v tl tr)
             (match (cmp k r)
               ['Less (lp tl)]
               ['Equal (Just v)]
               ['Greater (lp tr)])]))}
       (lp t))]))
(check-expect (map-apply (BSTMap cmp-int tree) 0) (Just "Harper"))
(check-expect (map-apply (BSTMap cmp-int tree) 2) 'Nothing)


(: insert : (All (key val) (BSTMap key val) key val -> (BSTMap key val)))
;;insert the key/value in the correct position in the tree
(define (insert m k c)
  (match m
    [(BSTMap cmp t)
     (local
       {(: in : (BST key val) -> (BST key val))
        (define (in t)
          (match t
            ['E (Node k c 'E 'E)]
            [(Node r v tl tr)
             (match (cmp k r)
               ['Less 
                (Node r v (in tl) tr)]
               ['Equal
                (Node r c tl tr)] 
               ['Greater 
                (Node r v tl (in tr))])]))}
       (BSTMap cmp (in t)))]))
(check-expect (BSTMap-data (insert (BSTMap cmp-int tree) 8 "Bart Mart"))
              (Node 5 "Regenstein"
                    (Node 1 "Ryerson"
                          (Node 0 "Harper" 'E 'E)
                          (Node 3 "Crerar" 'E 'E))
                    (Node 8 "Bart Mart"
                          (Node 6 "Kent" 'E 'E)
                          (Node 9 "Ratner" 'E 'E))))
(check-expect (BSTMap-data (insert (BSTMap cmp-int tree) 13 "Bart Mart"))
              (Node 5 "Regenstein"
                    (Node 1 "Ryerson"
                          (Node 0 "Harper" 'E 'E)
                          (Node 3 "Crerar" 'E 'E))
                    (Node 8 "Pick"
                          (Node 6 "Kent" 'E 'E)
                          (Node 9 "Ratner" 'E
                                (Node 13 "Bart Mart" 'E 'E)))))
              
(: remove-max : (All (key val) (BST key val) -> (Pair (Pair key val) (BST key val))))
;;remove the biggest number from the tree
(define (remove-max t)
  (local
    {(: x : (BST key val) -> (Pair key val))
     (define (x n)
       (match n
         ['E (error "error")]
         [(Node r v 'E 'E) (Pair r v)]
         [(Node r v tl tr) (x tr)]))
     (: new-t : (BST key val) -> (BST key val))
     (define (new-t t)
      (match t
        ['E (error "error")]
        [(Node r v tl 'E) 'E]
        [(Node r v tl tr) (Node r v tl (new-t tr))]))}
    (Pair (x t) (new-t t))))
(check-expect (remove-max tree)
              (Pair (Pair 9 "Ratner")
                    (Node 5 "Regenstein"
                          (Node 1 "Ryerson"
                                (Node 0 "Harper" 'E 'E)
                                (Node 3 "Crerar" 'E 'E))
                          (Node 8 "Pick"
                                (Node 6 "Kent" 'E 'E) 'E))))

(: remove-root : (All (key val) (BST key val) -> (BST key val)))
;;remove the root of a tree
(define (remove-root t)
  (match t
    ['E 'E]
    [(Node r v 'E tr) tr]
    [(Node r v tl 'E) tl]
    [(Node r v tl tr)
     (local
       {(define x (remove-max tl))
        (define y (Pair-first (Pair-first x)))
        (define z (Pair-second (Pair-first x)))}
       (Node y z (Pair-second x) tr))]))
(check-expect (remove-root tree)
              (Node 3 "Crerar"
                    (Node 1 "Ryerson"
                          (Node 0 "Harper" 'E 'E) 'E)
                    (Node 8 "Pick"
                          (Node 6 "Kent" 'E 'E)
                          (Node 9 "Ratner" 'E 'E))))

(: remove : (All (key val) (BSTMap key val) key -> (BSTMap key val)))
;;remove the desired key from the tree
(define (remove m k)
  (match m
    [(BSTMap cmp t)
     (local
       {(: rem : (BST key val) -> (BST key val))
        (define (rem t)
          (match t
            ['E t]
            [(Node r v tl tr)
             (match (cmp k r)
               ['Less (Node r v (rem tl) tr)]
               ['Equal (remove-root t)]
               ['Greater (Node r v tl (rem tr))])]))}
     (BSTMap cmp (rem t)))]))
(check-expect (BSTMap-data (remove (BSTMap cmp-int tree) 2))
              (Node 5 "Regenstein"
                    (Node 1 "Ryerson"
                          (Node 0 "Harper" 'E 'E)
                          (Node 3 "Crerar" 'E 'E))
                    (Node 8 "Pick"
                          (Node 6 "Kent" 'E 'E)
                          (Node 9 "Ratner" 'E 'E))))
(check-expect (BSTMap-data (remove (BSTMap cmp-int tree) 3))
              (Node 5 "Regenstein"
                    (Node 1 "Ryerson"
                          (Node 0 "Harper" 'E 'E) 'E)
                    (Node 8 "Pick"
                          (Node 6 "Kent" 'E 'E)
                          (Node 9 "Ratner" 'E 'E))))
(check-expect (BSTMap-data (remove (BSTMap cmp-int tree) 9))
              (Node 5 "Regenstein"
                    (Node 1 "Ryerson"
                          (Node 0 "Harper" 'E 'E)
                          (Node 3 "Crerar" 'E 'E))
                    (Node 8 "Pick"
                          (Node 6 "Kent" 'E 'E)
                          'E)))

(test)

 