#lang typed/racket
(require typed/test-engine/racket-tests)
;; ========= PROJECT B =========
;;                by: Min Su Kim


;;First brought code from project A
(define-type Player (U 'black 'white))

(define-struct Pos
  ([row : Integer]  ;; an integer on the interval [0,7]
   [col : Integer]) ;; an integer on the interval [0,7]
  #:transparent)

(define-struct Board
  ([squares : (Listof (U Player 'none))]) ;; a list of length 64
  #:transparent)

(define-struct Game
  ([board : Board]
   [next  : Player])
  #:transparent)

(: new-game : Game)
;;starts a new game
(define new-game
  (Game 
   (Board (list 'none 'none 'none 'none 'none 'none 'none 'none
                'none 'none 'none 'none 'none 'none 'none 'none
                'none 'none 'none 'none 'none 'none 'none 'none
                'none 'none 'none 'white 'black 'none 'none 'none
                'none 'none 'none 'black 'white 'none 'none 'none
                'none 'none 'none 'none 'none 'none 'none 'none
                'none 'none 'none 'none 'none 'none 'none 'none
                'none 'none 'none 'none 'none 'none 'none 'none))
               'black))
(define test-board
  (Board (list 'none 'none 'none 'none 'none 'none 'none 'none
               'none 'none 'none 'none 'none 'none 'black 'none
               'none 'none 'white 'none 'white 'black 'none 'none
               'none 'none 'white 'black 'white 'none 'none 'none
               'none 'none 'white 'black 'none 'none 'none 'none
               'none 'none 'none 'none 'none 'none 'none 'none
               'none 'none 'none 'none 'none 'none 'none 'none
               'none 'none 'none 'none 'none 'none 'none 'none)))

(define test-board2
  (Board (list 'white 'none 'none 'none 'none 'none 'black 'black
               'none 'white 'none 'none 'none 'none 'black 'none
               'none 'none 'white 'none 'white 'black 'none 'white
               'black 'black 'black 'black 'black 'none 'none 'none
               'none 'none 'white 'black 'none 'none 'none 'none
               'none 'none 'none 'none 'none 'none 'none 'none
               'none 'none 'none 'none 'none 'none 'none 'none
               'none 'none 'none 'none 'none 'none 'none 'none)))
(define new-game1
  (Game 
   (Board (list 'none 'none 'none 'none 'none 'none 'none 'none
                'none 'none 'none 'none 'none 'none 'none 'none
                'none 'none 'none 'none 'none 'none 'none 'none
                'none 'none 'none 'black 'white 'none 'none 'none
                'none 'none 'none 'white 'black 'none 'none 'none
                'none 'none 'none 'none 'none 'none 'none 'none
                'none 'none 'none 'none 'none 'none 'none 'none
                'none 'none 'none 'none 'none 'none 'none 'none))
   'black))
(define game-state1 (Game test-board 'white))
(define game-state2 (Game test-board2 'black))
(define game-state3 (Game test-board2 'white))

(: pos->list : Pos -> Integer)
;;find the position in terms of the list of the board
(define (pos->list p)
  (cond
    [(< (or (Pos-row p) (Pos-col p)) 0) (error "invalid")]
    [else (+ (* 8 (Pos-row p)) (Pos-col p))]))
(check-expect (pos->list (Pos 4 4)) 36)

(: board-ref : Board Pos -> (U Player 'none))
;;Return the piece at the given square, or 'none.
(define (board-ref board xy)
  (list-ref (Board-squares board) (pos->list xy)))
(check-expect (board-ref test-board (Pos 3 3)) 'black)

(: int->pos : Integer -> Pos)
;;turns integer into a position in the board
(define (int->pos n)
  (Pos (quotient n 8) (remainder n 8)))
(check-expect (int->pos 36) (Pos 4 4))

(: op : Player -> Player)
;;find opposing player
(define (op p) 
  (cond
    [(eq? p 'white) 'black]
    [else 'white]))
(check-expect (op 'white) 'black)
(check-expect (op 'black) 'white)

;;list of functions that move in desired direction
(: up : Pos -> Pos)
;;move one row up
(define (up pos) (Pos (- (Pos-row pos) 1) (Pos-col pos)))
(check-expect (up (Pos 4 4)) (Pos 3 4))

(: down : Pos -> Pos)
;;move one row down
(define (down pos) (Pos (+ (Pos-row pos) 1) (Pos-col pos)))
(check-expect (down (Pos 4 4)) (Pos 5 4))

(: left : Pos -> Pos)
;;move one column left
(define (left pos) (Pos (Pos-row pos) (- (Pos-col pos) 1)))
(check-expect (left (Pos 4 4)) (Pos 4 3))

(: right : Pos -> Pos)
;;move one column right
(define (right pos) (Pos (Pos-row pos) (+ 1 (Pos-col pos))))
(check-expect (right (Pos 4 4)) (Pos 4 5))

(: nw : Pos -> Pos)
;;move one in the north-west direction
(define (nw pos) (Pos (- (Pos-row pos) 1) (- (Pos-col pos) 1)))
(check-expect (nw (Pos 4 4)) (Pos 3 3))

(: se : Pos -> Pos)
;;move one in the south-east direction
(define (se pos) (Pos (+ (Pos-row pos) 1) (+ (Pos-col pos) 1)))
(check-expect (se (Pos 4 4)) (Pos 5 5))

(: ne : Pos -> Pos)
;;move one in the north-east direction
(define (ne pos) (Pos (- (Pos-row pos) 1)(+ (Pos-col pos) 1)))
(check-expect (ne (Pos 4 4)) (Pos 3 5))

(: sw : Pos -> Pos)
;;move one in the south-west direction
(define (sw pos) (Pos (+ (Pos-row pos) 1) (- (Pos-col pos) 1)))
(check-expect (sw (Pos 4 4)) (Pos 5 3))

(: next-to : Pos Integer -> Pos)
;; returns the position in desired direction
;;I have assigned values (1-8) to the direction change of pos
;;this is done for convenience
(define (next-to pos direction)
  (cond
    [(eqv? direction 1) (up pos)]
    [(eqv? direction 2) (down pos)]
    [(eqv? direction 3) (left pos)]
    [(eqv? direction 4) (right pos)]
    [(eqv? direction 5) (nw pos)]
    [(eqv? direction 6) (se pos)]
    [(eqv? direction 7) (ne pos)]
    [(eqv? direction 8) (sw pos)]
    [else (error "N/A direction")]))
(check-expect (next-to (Pos 4 4) 1) (Pos 3 4))

(: check : Board Player Pos Integer -> Boolean)
;;returns true if player can outflank 
(define (check t q pos direction)
    (cond
      [(or
        (> (Pos-col pos) 7)
        (< (Pos-col pos) 0)
        (> (Pos-row pos) 7)
        (< (Pos-row pos) 0)) false]
      [else 
       (local
         {(define qos (next-to pos direction))}
         (cond
           [(< (Pos-row qos) 0) #f]
           [(< (Pos-col qos) 0) #f]
           [(> (Pos-row qos) 7) #f]
           [(> (Pos-col qos) 7) #f]
           [(eqv? (board-ref t qos) 'none) false]
           [(eqv? (board-ref t qos) (op q)) (check t q qos direction)]
           [(eqv? (board-ref t qos) q) true]
           [else (error "error")]))]))
(check-expect (check test-board 'white (Pos 4 4) 1) #t)
(check-expect (check test-board 'white (Pos 4 4) 2) #f)
(check-expect (check test-board 'white (Pos 4 4) 5) #t)

(: doublecheck : Board Player Pos Integer -> Boolean)
;;determine whether the first adjacent tile in the desired direction is same color
(define (doublecheck b p pos direction)
  (define c (next-to pos direction))
  (cond
    [(< (Pos-row c) 0) #f]
    [(< (Pos-col c) 0) #f]
    [(> (Pos-row c) 7) #f]
    [(> (Pos-col c) 7) #f]
    [else (eqv? (board-ref b (next-to pos direction)) p)]))
(check-expect (doublecheck test-board2 'black (Pos 3 5) 3) #t)

(define-struct num-bool 
  ([num : Integer]
   [bool : Boolean])
   #:transparent)
;;for use in outflanks

(define directions (list 1 2 3 4 5 6 7 8))
;;for use in outflanks

(: outflanks? : Board Player Pos -> Boolean)
;;returns true if that player can outflank the opponent by placing a piece at that position
(define (outflanks? b p pos)
  (cond
    [(not (eqv? 'none (board-ref b pos))) #f]
    [else
     (local
       {(define k
          (map (lambda ([z : num-bool]) (num-bool-num z))
               (filter (lambda ([z : num-bool]) (eqv? (num-bool-bool z) #f)) 
                       (map (lambda ([xx : Integer]) 
                              (num-bool xx (doublecheck b p pos xx))) directions))))
        (: run-outflank : Board Player Pos (Listof Integer) -> Boolean)
        ;;use check on directions that could possibly be outflanked
        (define (run-outflank b p pos a)
          (cond
            [(empty? a) #f]
            [(cons? a)
             (if
              (check b p pos (first a))
              #t
              (run-outflank b p pos (rest a)))]))}
       (run-outflank b p pos k))]))
(check-expect (outflanks? test-board 'white (Pos 4 4)) true)
(check-expect (outflanks? (Game-board new-game) 'black (Pos 2 3)) true)
(check-expect (outflanks? (Game-board new-game) 'black (Pos 3 2)) true)
(check-expect (outflanks? (Game-board new-game) 'black (Pos 4 5)) true)
(check-expect (outflanks? (Game-board new-game) 'black (Pos 5 4)) true)

(: flips-1d : Board Player Pos Integer -> (Listof Pos))
;;determine the positions to be flipped in the specified direction
(define (flips-1d b p pos direction)
  (define next (next-to pos direction))
  (cond
    [(or
      (> (Pos-col pos) 7)
      (< (Pos-col pos) 0)
      (> (Pos-row pos) 7)
      (< (Pos-row pos) 0)) '()]
    [(eqv? #f (check b p pos direction)) '()]
    [(eqv? (board-ref b next) p) '()]
    [(eqv? (board-ref b next) 'none) '()]
    [(eqv? (board-ref b next) (op p)) 
     (cons next (flips-1d b p next direction))]
    [else (error "flips-1d error")]))
(check-expect (flips-1d test-board2 'white (Pos 4 4) 1) (list (Pos 3 4)))
(check-expect (flips-1d test-board2 'white (Pos 4 4) 3) (list (Pos 4 3)))
(check-expect (flips-1d test-board2 'white (Pos 4 4) 5) (list(Pos 3 3)))

(: flips : Board Player Pos -> (Listof Pos))
;; determine the positions to be flipped given a move
(define (flips b p pos)
  (cond
    [(not (outflanks? b p pos)) '()]
    [else
     (foldr (inst append Pos) '() ;;found use of inst append on piazza, question answered by Prof. Reppy
            (map (lambda ([z : Integer]) (flips-1d b p pos z)) directions))]))
(check-expect 
 (flips test-board 'white (Pos 4 4)) 
 (list (Pos 4 3) (Pos 3 3)))

(: change : Board Player Pos -> (Listof (U Player 'none)))
;;change desired position to the given player
(define (change b p pos)
   (append
    (reverse-ref (Board-squares b) (- 64 (pos->list pos)))
    (list p)
    (ref1 (Board-squares b) (+ 1 (pos->list pos)))))

(: ref1 : (Listof (U Player 'none)) Integer -> (Listof (U Player 'none)))
;;reduce given length of board to the integer given
(define (ref1 board n)
  (cond
    [(= n 0) board]
    [else (ref1 (rest board) (- n 1))]))

(: reverse-ref : (Listof (U Player 'none)) Integer -> (Listof (U Player 'none)))
;;reduce given length of board by the integer given (from last to first)
(define (reverse-ref board n)
  (cond
    [(= n 0) board]
    [else 
     (reverse-ref 
      (reverse (cdr (reverse board)))
      (- n 1))]))

(: apply-change : Board Player (Listof Pos) -> Board)
;; apply change on the board effected by the list
(define (apply-change b p a)
  (cond
    [(empty? a) b]
    [else
     (apply-change (Board(change b p (first a))) p (rest a))]))

(: apply-move : Game Player Pos -> Game)
;;Given a game, a player and a position, apply the move 
;;to the game board and return the subsequent game state
(define (apply-move game p pos)
  (cond
    [(not (eq? (Game-next game) p)) (error "illegal")]
    [(not (outflanks? (Game-board game) p pos)) (error "illegal move")]
    [else
     (local
       {(define to-flip (cons pos (flips (Game-board game) p pos)))}
       (Game
        (apply-change (Game-board game) p to-flip)
        (op p)))]))
(define m1 (apply-move new-game1 'black (Pos 2 4)))
(define m2 (apply-move m1 'white (Pos 2 3)))
(define m3 (apply-move m2 'black (Pos 2 2)))
(define m4 (apply-move m3 'white (Pos 2 5)))
(define m5 (apply-move m4 'black (Pos 4 2)))
(define m6 (apply-move m5 'white (Pos 3 2)))
(define m7 (apply-move m6 'black (Pos 4 5)))
(define m8 (apply-move m7 'white (Pos 5 2)))
(define m9 (apply-move m8 'black (Pos 4 1)))
;;eyeball test from looking at each one (both in list and image)
;;compared to actual gameplay

(define white-wins
  (Game
   (Board
    (list 'black 'white 'white 'white 'white 'white 'white 'white
          'black 'black 'white 'black 'black 'black 'black 'white
          'black 'black 'black 'white 'white 'white 'white 'white
          'white 'white 'black 'black 'black 'black 'white 'white
          'white 'white 'white 'black 'black 'black 'white 'white
          'white 'white 'white 'black 'black 'black 'white 'white
          'white 'black 'black 'white 'white 'white 'white 'white
          'white 'white 'white 'white 'white 'white 'white 'black))
   'black))

(: game-over? : Game -> Boolean)
;; is the game over?
(define (game-over? game)
  (local
    {(define gb (Game-board game))
     (define gp (Game-next game))
     (: checking : Board Player Integer -> Boolean)
     ;;apply outflanks? to each tile in the board
     (define (checking b p n)
       (cond
         [(= n 64) #t]
         [else
          (if
           (outflanks? b p (int->pos n))
           #f
           (checking b p (+ n 1)))]))}
  (and (checking gb gp 0) (checking gb (op gp) 0))))
(check-expect (game-over? white-wins) #t)

(: score : Game Player -> Integer)
;;finds the score of wanted player
;;separated from outcome on purpose for future use in game-image
(define (score game p)
  (length (filter (lambda 
                      ([x : (U Player 'none)]) (eqv? x p)) 
                  (Board-squares (Game-board game)))))
(check-expect (score m1 'black) 4)
(check-expect (score white-wins 'white) 41)
(check-expect (score white-wins 'black) 23)

(: outcome : Game -> (U Player 'tie))
;;determines the winner of the match
(define (outcome game)
  (local
    {(define a (score game 'white))
     (define b (score game 'black))}
  (cond
    [(= a b) 'tie]
    [(< a b) 'black]
    [(> a b) 'white]
    [else (error "outcome error")])))
(check-expect (outcome white-wins) 'white)

(require/typed 2htdp/image
   [#:opaque Image image?]
   [empty-image Image]
   [rectangle (-> Number Number String String Image)]
   [circle (-> Integer String String Image)]
   [square (-> Integer String String Image)]
   [text (-> String Number String Image)] 
   [beside (-> Image * Image)]
   [beside/align (-> String Image * Image)]
   [above (-> Image * Image)]
   [overlay (-> Image * Image)])

(: blank-tile : Integer -> Image)
;; returns image with a blank tile
(define (blank-tile size)
  (overlay (square size "outline" "black")
           (square size "solid" "tan")))

(: tile : Any Integer -> Image)
;; returns tile image with corresponding player
(define (tile p size)
  (cond
    [(eqv? p 'none) (blank-tile size)]
    [(eqv? p 'white) (overlay (circle (exact-floor(/ size 4)) "solid" "white")
                              (circle (exact-floor(/ size 4)) "outline" "black")
                              (blank-tile size))]
    [(eqv? p 'black) (overlay (circle (exact-floor(/ size 4)) "solid" "black") 
                              (blank-tile size))]
    [(integer? p) (overlay (text (number->string p) (exact-floor(/ size 1.5)) "white")
                           (square size "outline" "black")
                           (square size "solid" "darkgreen"))]
    [(string? p) (overlay (square size "outline" "black")
                          (square size "solid" "darkgreen"))]
    [else (error "error tile")]))

(define nums (build-list 8 (lambda ([x : Integer]) x)))

(: build-row : Any Integer (Listof Any) -> Image)
;;create a subsequent row with corresponding player positions
(define (build-row num size l)
  (local
    {(: rowhelper : Integer (Listof Any) -> Image)
     ;;create a row using recursion
     (define (rowhelper size l)
       (cond
         [(empty? l) empty-image]
         [(cons? l)
          (beside
           (tile (first l) size)
           (rowhelper size (rest l)))]))}
  (beside
   (tile num size)
   (rowhelper size l))))

(: extract-row : Board Integer Integer -> Image)
;;extract desired row from a board
(define (extract-row board row size)
  (local
  {(define r1 (* 8 row))
   (: posns : Board Integer Integer -> (Listof (U Player 'none)))
   ;;extracts the corresponding positions in the board
   (define (posns b first last)
     (cond
       [(>= first last) '()]
       [(cons (board-ref b (int->pos first)) (posns b (+ 1 first) last))]))
   (define poss (posns board r1 (+ r1 8)))}
    (build-row row size poss))) 

(: board-image : Board Integer -> Image)
;;create an image of the board
(define (board-image board w)
  (local
    {(define width (exact-floor (/ w 9)))
     (define rows (list 0 1 2 3 4 5 6 7))
     (define l
       (map (lambda ([z : Integer]) (extract-row board z width)) rows))
     (: m : (Listof Image) -> Image)
     ;;make a list of images into a single one
     (define (m l) 
       (cond
         [(empty? l) empty-image]
         [(cons? l) (above (first l) (m (rest l)))]))}
    (above
     (build-row "first" width nums)
     (m l))))
;;eyeball test
;(board-image test-board 180)
  
(: game-image : Game Integer -> Image)
;;show an image of the current game state
(define (game-image g w)
  (local
    {(define width (exact-floor (/ w 9)))
     (: ef : Integer -> Integer)
     ;;to simplify code, divides width by desired number
     (define (ef n) (exact-floor (/ width n))) }
    (beside/align "middle"
                  (above
                   (board-image (Game-board g) w)
                   (beside
                    (overlay (text "Turn" 20 "black")
                             (rectangle 50 25 "outline" "black"))
                    (overlay (text (symbol->string (Game-next g)) 20 "black")
                             (rectangle 50 25 "outline" "black"))))
                   (above 
                    (text "Score" 20 "black")
                    (beside
                     (circle 8 "solid" "black")
                     (text (number->string (score g 'black)) 20 "black"))
                    (beside
                     (circle 8 "outline" "black")
                     (text (number->string (score g 'white)) 20 "black"))))))
;eyeball test
;(game-image m1 180)
;(game-image white-wins 180)

;; ========================================================================

;; ======================== STARTING PROJECT B ============================

(define-type Strategy (Game -> Pos))

(define-struct (a b) Pair
  ([x1 : a]
   [x2 : b])
  #:transparent)

(: checking : Board Player Integer -> Pos)
;; apply outflanks? to each tile in the board
(define (checking b p n)
  (cond
    [(= n 64) (error "error")]
    [else
     (if
      (outflanks? b p (int->pos n))
      (int->pos n)
      (checking b p (+ n 1)))]))

(: first-move Strategy)
;; player that chooses the first available legal move
(define (first-move g)
    (checking (Game-board g) (Game-next g) 0))
(check-expect (first-move new-game) (Pos 2 3))
(check-expect (first-move new-game1) (Pos 2 4))
(check-expect (first-move m4) (Pos 2 6))
(check-expect (first-move m7) (Pos 1 1))
(check-expect (first-move m8) (Pos 2 6))

(: parse-pos : String -> (U Pos String))
;; function that parses strings
(define (parse-pos s)
  (define p
    (map
     (lambda ([x : Char]) (char->integer x))
     (string->list s)))
  (cond
    [(eqv? #f (string->number s)) "unparseable"]
    [(not (= (length (string->list s)) 2)) s]
    [else (Pos (- (first p) 48) (- (last p) 48))]))
(check-expect (parse-pos "10") (Pos 1 0))
(check-expect (parse-pos "68") (Pos 6 8))
(check-expect (parse-pos "whatever") "unparseable")

(: human Strategy)
;; evaluate read-line and return given position (if legal)
(define (human g)
  (local
    {(define rl (read-line))
     (: type-matcher : (U Pos String) -> Pos)
     ;; changes the type
     (define (type-matcher s)
       (cond
         [(string? s) (human g)]
         [else s]))}
  (cond
    [(eof-object? rl) (error "EOF")]
    [(string? rl)
     (local
       {(define pp (type-matcher (parse-pos rl)))}
       (cond
         [(> (Pos-row pp) 7) (human g)]
         [(> (Pos-col pp) 7) (human g)]
         [else pp]))])))

(: can-move? : Game -> Boolean)
;; can player move?
(define (can-move? game)
  (local
    {(define gb (Game-board game))
     (define gp (Game-next game))
     (: checking : Board Player Integer -> Boolean)
     ;; apply outflanks? to each tile in the board
     (define (checking b p n)
       (cond
         [(= n 64) #f]
         [else
          (if
           (outflanks? b p (int->pos n))
           #t
           (checking b p (+ n 1)))]))}
  (checking gb gp 0)))
(check-expect (can-move? white-wins) #f)
(check-expect (can-move? new-game) #t)
(check-expect (can-move? m2) #t)
(check-expect (can-move? m4) #t)

(: play-loop : Game Strategy Strategy -> (Pair (Listof (Pair Player Pos)) Game))
;; runs a game based on the strategy inputs, and returns a list of historical
;; moves and the final state of the game
(define (play-loop g b w)
  (display (game-image g 180))
  (newline)
  (local
     {(: loop : Game Strategy Strategy Integer -> (Listof (Pair Player Pos)))
     ;; loops the game for continuous play
     (define (loop g b w x)
        (if
          (game-over? g) '()
          (if
          (eqv? (can-move? g) #f) 
           (loop (Game (Game-board g) (op (Game-next g))) b w x)
           (local 
             {(define strategy (if (eqv? (Game-next g) 'black) (b g) (w g)))}
             (cond
               [(eqv? x 3) '()]
               [(or (> (Pos-row strategy) 7) (< (Pos-row strategy) 0)
                    (> (Pos-col strategy) 7) (< (Pos-col strategy) 0))
                (display "unparseable")
                (loop g b w (+ x 1))]
               [(outflanks? (Game-board g) (Game-next g) strategy)
                (display (game-image (apply-move g (Game-next g) strategy) 180))
                (newline)
                (cons (Pair (Game-next g) strategy)
                      (loop (apply-move g (Game-next g) strategy) b w 0))]
               [else
                (display "error, try again")
                (loop g b w (+ x 1))])))))
     (define l (loop g b w 0))}
    (Pair l (apply-moves g l)))) 

(: apply-moves : Game (Listof (Pair Player Pos)) -> Game)
;; applies the moves specified in the list
(define (apply-moves g l)
  (match l
    ['() g]
    [(cons hd tl)
     (cond
       [(eqv? (Game-next g) (Pair-x1 hd))
        (apply-moves 
         (apply-move g (Pair-x1 hd) (Pair-x2 hd))
         tl)]
       [(eqv? (op (Game-next g)) (Pair-x1 hd)) 
         (apply-moves 
              (apply-move 
               (Game (Game-board g) (op (Game-next g))) (Pair-x1 hd) (Pair-x2 hd))
              tl)]
       [else (error 'apply-moves "wrong")])]))

(: play-loop-forgetful : Game Strategy Strategy -> (U Player 'tie))
;; "forgets" the history of the game; good for quick and easy console play
(define (play-loop-forgetful g b w)
  (match (play-loop g b w)
    [(Pair history game-at-end) (outcome game-at-end)]))

(: pass-and-play : -> (Pair (Listof (Pair Player Pos)) Game))
(define (pass-and-play)
  (play-loop new-game human human))

(: best-pos : Game (Listof Pos) -> Pos)
;; finds the Pos that can flip the most
(define (best-pos g lst)
  (match lst
    ['() (error "empty list")]
    [(cons hd '()) hd]
    [(cons hd tl)
     (local
       {(define length-pos 
          (map 
           (lambda ([z : Pos]) 
             (Pair (length (flips (Game-board g) (Game-next g) z)) z)) 
           lst))}
       (Pair-x2 (argmax (lambda ([x : (Pair Real Pos)]) (Pair-x1 x)) length-pos)))]))

(check-expect 
 (best-pos (Game test-board 'white) (list (Pos 0 7) (Pos 4 4) (Pos 2 6)))
 (Pos 0 7))

;; DEFINITIONS FOR USE IN FUNCTIONS
(define corners (list (Pos 0 0)(Pos 0 7)(Pos 7 0)(Pos 7 7)))
(define l2 (build-list 6 (lambda ([x : Integer]) (+ x 1))))
(define left-edge (map (lambda ([x : Integer]) (Pos x 0)) l2))
(define right-edge (map (lambda ([x : Integer]) (Pos x 7)) l2))
(define upper-edge (map (lambda ([x : Integer]) (Pos 0 x)) l2))
(define lower-edge (map (lambda ([x : Integer]) (Pos 7 x)) l2))
(define edges (foldl (inst append Pos) '() 
                     (list left-edge right-edge upper-edge lower-edge)))
(define edges-and-corners (foldl (inst append Pos) '()
                                 (list corners edges)))
(define all-pos (map (lambda ([x : Integer]) (int->pos x)) 
                          (build-list 64 (lambda ([z : Integer]) z))))
(define interior (remove* corners (remove* edges all-pos)))

(: immediate-tactics : Strategy)
;; a rules-based strategy
(define (immediate-tactics g)
  (local
    {(define gb (Game-board g))
     (define gp (Game-next g))}
    (if
     (ormap (lambda ([z : Pos]) (outflanks? gb gp z)) corners)
     (best-pos g corners)
     (if (ormap (lambda ([z : Pos]) (outflanks? gb gp z)) edges)
         (best-pos g edges)
         (best-pos g interior)))))
(check-expect (immediate-tactics game-state1) (Pos 0 7))
(check-expect (immediate-tactics game-state2) (Pos 1 2))
(check-expect (immediate-tactics game-state3) (Pos 0 5))
(check-expect (immediate-tactics m8) (Pos 4 1))

(define-type Heuristic (Game -> Integer))

(: piece-counting : Heuristic)
;; number of black pieces minus number of white pieces
(define (piece-counting g)
  (- (score g 'black) (score g 'white)))
(check-expect (piece-counting m1) 3)
(check-expect (piece-counting m4) 2)
(check-expect (piece-counting m8) -2)
(check-expect (piece-counting white-wins) -18)
(check-expect (piece-counting new-game) 0)

(: list-positions : Game Player -> (Listof Pos))
;; lists positions that have player p pieces on them
(define (list-positions g p)
  (local
    {(define brd (Game-board g))
     (: posns : Board Player Integer -> (Listof Pos))
     ;; find positions that have p pieces on them
     (define (posns b p n)
       (cond
         [(= n 64) '()]
         [else
          (if (eqv? p (board-ref b (int->pos n)))
              (cons (int->pos n) (posns b p (+ n 1)))
              (posns b p (+ n 1)))]))}
    (posns brd p 0)))
(check-expect (list-positions new-game 'black) (list (Pos 3 4) (Pos 4 3)))
(check-expect (list-positions new-game 'white) (list (Pos 3 3) (Pos 4 4)))
(check-expect (list-positions m2 'black) (list (Pos 2 4) (Pos 3 4) (Pos 4 4)))

(: piece-value : (Listof Pos) Integer -> Integer)
;; determines the value of the piece, depending on
;; whether it is an edge or not
(define (piece-value l n)
  (local
    {(define lst 
       (map 
        (lambda ([z : Pos]) 
          (if  (ormap (lambda ([x : Pos])
                        (equal? z x)) edges-and-corners) n 1)) l))}
     (foldr + 0 lst)))
(check-expect (piece-value (list-positions game-state2 'black) 2) 13)
(check-expect (piece-value (list-positions game-state2 'white) 4) 12)
(check-expect (piece-value (list-positions white-wins 'white) 2) 65)

(: piece-value2 : (Listof Pos) Integer Integer -> Integer)
;; determines the value of the piece, depending on
;; whether it is an edge, corner, or inside
(define (piece-value2 l edge corner)
  (local
    {(define lst 
       (map 
        (lambda ([z : Pos]) 
          (if (ormap (lambda ([x : Pos]) (equal? z x)) edges) edge
              (if (ormap (lambda ([x : Pos]) (equal? z x)) corners) corner 1))) l))}
     (foldr + 0 lst)))
(check-expect (piece-value2 (list-positions game-state2 'black) 2 1) 12)
(check-expect (piece-value2 (list-positions game-state2 'black) 2 3) 14)
(check-expect (piece-value2 (list-positions white-wins 'black) 2 3) 29)
(check-expect (piece-value2 (list-positions white-wins 'white) 2 3) 67)

(: prefer-edges : Integer -> Heuristic)
;; returns a function that behaves like piece-counting above, 
;; but counts every edge piece as being worth that many interior pieces
(define (prefer-edges n)
  (lambda ([z : Game])
     (- (piece-value (list-positions z 'black) n)
        (piece-value (list-positions z 'white) n))))
(check-expect ((prefer-edges 2) game-state2) 5)
(check-expect ((prefer-edges 2) white-wins) -38)
(check-expect ((prefer-edges 5) game-state2) 8)

(: prefer-edges-and-corners : Integer Integer -> Heuristic)
;; returns a function that behaves like piece-counting above, 
;; but counts every edge and corner piece as being worth n, m as many interior pieces
(define (prefer-edges-and-corners edge corner)
  (lambda ([z : Game])
    (- (piece-value2 (list-positions z 'black) edge corner)
       (piece-value2 (list-positions z 'white) edge corner))))
(check-expect ((prefer-edges-and-corners 2 1) white-wins) -38)
(check-expect ((prefer-edges-and-corners 2 1) game-state2) 5)
(check-expect ((prefer-edges-and-corners 2 5) game-state2) 5)

;;;;;;;;;; HELPER FUNCTIONS FOR MINI/MONTYMAX ;;;;;;;;;;;;;;;

(: assign-score : Heuristic (Listof Game) -> (Listof Integer))
;; assigns scores to games based on the heuristic funtion provided
(define (assign-score h lst)
  (map (lambda ([z : Game]) (h z)) lst))
(define listof-testgames (list m1 m2 m3 white-wins))
(check-expect (assign-score piece-counting listof-testgames) (list 3 0 5 -18))
(check-expect (assign-score (prefer-edges 2) listof-testgames) (list 3 0 5 -38))
(check-expect (assign-score (prefer-edges-and-corners 2 4) listof-testgames) (list 3 0 5 -38))

(: next-pos : Game -> (Listof Pos))
;; creates a list of all the possile game moves
(define (next-pos g)
    (filter 
     (lambda ([z : Pos]) (outflanks? (Game-board g) (Game-next g) z)) 
     all-pos))
(check-expect (next-pos new-game) (list (Pos 2 3) (Pos 3 2) (Pos 4 5) (Pos 5 4)))
(check-expect (next-pos m1) (list (Pos 2 3) (Pos 2 5) (Pos 4 5)))
(check-expect (next-pos white-wins) '())

(: next-games : Game (Listof Pos) -> (Listof Game))
;; extracts the list of games from a list of positions
(define (next-games g list)
  (map (lambda ([p : Pos]) (apply-move g (Game-next g) p)) list))
(check-expect (next-games m1 (list (Pos 2 3))) (list m2))

;;; MINIMAX ;;;

(: minimax-eval : Heuristic Integer Game -> Integer)
;; strategy that consumes the ply (the number of moves to look ahead) and
;; a game, and assign a score using the given heuristic function
(define (minimax-eval heur ply g)
  (if (or (game-over? g) (= ply 0)) (heur g)
      (if (eqv? (can-move? g) #f) 
          (minimax-eval heur (sub1 ply) (Game (Game-board g) (op (Game-next g))))
          (local
            {(define next-ply (next-games g (next-pos g)))
             (define max/min (if (eqv? (Game-next g) 'black) argmax argmin))}
            (cond
              [(> ply 1) 
               (local
                 {(define mini 
                    (map (lambda ([k : Game]) (minimax-eval heur (sub1 ply) k)) next-ply))}
                 (max/min (lambda ([i : Integer]) i) mini))]
              [(= ply 1)
               (if (empty? next-ply)
                 (error "error")
                 (max/min (lambda ([i : Integer]) i) (assign-score heur next-ply)))]
              [else (error "something's wrong")])))))
(check-expect (minimax-eval piece-counting 1 new-game) 3)
(check-expect (minimax-eval piece-counting 2 new-game) 0)
(check-expect (minimax-eval piece-counting 3 new-game) 3)
(check-expect (minimax-eval piece-counting 4 new-game) -2)

(: minimax : Heuristic Integer -> Strategy)
;; strategy that looks a certain number of ply ahead 
;; and choose the best possible move based on heuristic
(define (minimax heur ply)
  (lambda ([g : Game])
    (if (game-over? g)
        (error "game over")
        (local
          {(define next (next-pos g))
           (define arg (if (eqv? 'black (Game-next g)) argmax argmin))}
              (Pair-x2
               (arg (lambda ([x : (Pair Integer Pos)]) 
                      (Pair-x1 x))
                    (map (lambda ([k : Pos]) 
                           (Pair (minimax-eval heur (sub1 ply) (apply-move g (Game-next g) k)) k)) 
                         next)))))))
;; eyeball test (play-loop new-game (minimax piece-counting 3) (minimax piece-counting 3))
;; results in 'black 46 'white 18, consistent with example provided by AMS in piazza
;; additionally, every run has a consistent outcome

;;; MONTYMAX ;;;

(: pick-upto : (All (a) Integer (Listof a) -> (Listof a)))
;; randomly selects the number of elements in a list
(define (pick-upto n lst)
  (if (>= n (length lst))
      lst
      (local
        {(: random-select : (All (a) (Listof a) Integer (Listof Integer) -> (Listof a)))
         ;; recursively picks the elements from the list
         (define (random-select l n xs)
           (define index (random (length lst)))
           (if (eqv? #t (ormap (lambda ([i : Integer]) (eqv? i index)) xs))
               (random-select l n xs)
               (if (= 0 n) '()
                   (cons (list-ref l index) (random-select l (sub1 n) (cons index xs))))))}
        (random-select lst n '()))))
;; eyeball test (pick-upto 2 (list 'a 'b 'c 'd 'e))
;; series of tests give different random results, same element does not repeat

(: montymax-eval : Heuristic Integer Integer Game -> Integer)
;; similar to minimax, but limits and randomizes the branches to be explored
(define (montymax-eval heur ply max-b g)
  (if (or (game-over? g) (= 0 ply)) (heur g)
      (if (eqv? (can-move? g) #f) 
          (montymax-eval heur (sub1 ply) max-b (Game (Game-board g) (op (Game-next g))))
          (local
            {(define next-ply (next-games g (pick-upto max-b (next-pos g))))
             (define max/min (if (eqv? (Game-next g) 'black) argmax argmin))}
            (cond
              [(> ply 1) 
               (local
                 {(define mini 
                    (map (lambda ([k : Game]) (montymax-eval heur (sub1 ply) max-b k)) next-ply))}
                 (max/min (lambda ([i : Integer]) i) mini))]
              [(= ply 1)
               (if (empty? next-ply)
                 (error "error")
                 (max/min (lambda ([i : Integer]) i) (assign-score heur next-ply)))]
              [else (error "something's wrong")])))))
;; eyeball test (montymax-eval piece-counting 5~10 2 new-game)
;; consistently returns differing values

(: montymax : Heuristic Integer Integer -> Strategy)
;; Monte Carlo version of minimax (using monotymax-eval)
;; given a maximum number of branch searching for each level of the tree
(define (montymax heur ply max-b)
  (lambda ([g : Game])
    (if (game-over? g)
        (error "game over")
        (local
          {(define next (pick-upto max-b (next-pos g)))
           (define arg (if (eqv? 'black (Game-next g)) argmax argmin))}
          (Pair-x2
           (arg (lambda ([x : (Pair Integer Pos)]) 
                  (Pair-x1 x))
               (map (lambda ([k : Pos]) 
                           (Pair (montymax-eval heur (sub1 ply) max-b (apply-move g (Game-next g) k)) k)) 
                         next)))))))
;; eyeball test
;; returns different results every run

(test)