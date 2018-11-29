#lang typed/racket
(require typed/test-engine/racket-tests)
(require/typed 2htdp/image
   [#:opaque Image image?]
   [text (-> String Number String Image)]
   [beside (-> Image * Image)]
   [above (-> Image * Image)]
   [bitmap/url (-> String Image)])

;;brought definitions from lab 5, so skip down ahead till I say stop
(define-type State 
  (U 'AL 'AK 'AZ 'AR 'CA 'CO 'CT 'DE 'DC 'FL 'GA 'HI 'ID 'IL 'IN 'IA 'KS
     'KY 'LA 'ME 'MD 'MA 'MI 'MN 'MS 'MO 'MT 'NE 'NV 'NH 'NJ 'NM 'NY 'NC
     'ND 'OH 'OK 'OR 'PA 'RI 'SC 'SD 'TN 'TX 'UT 'VT 'VA 'WA 'WV 'WI 'WY))
;; 51 state-like entities -- includes Washington, DC

(define-type Party
  (U 'D 'R)) ;; Democrats, Republicans
             ;; apologies to third parties! they're not represented.

(define-struct EV
  ([s  : State]    ;; a state symbol
   [ev : Integer]) ;; electoral votes per this state
  #:transparent)

(define-struct DemProb
  ([s    : State]
   [demp : Real])   ;; probability of Democratic victory on [0.0,1.0] 
  #:transparent)

(define-struct StateResult
  ([s  : State]
   [p  : Party]     ;; winning party
   [ev : Integer])  ;; number of electoral votes for victor
  #:transparent)

(define-struct USAResult
  ([dems : (Listof StateResult)]  ;; states won by Democrats
   [reps : (Listof StateResult)]) ;; states won by Republicans
  #:transparent)

(define-struct Tally
  ([demv : Integer]  ;; simulations where D candidate wins
   [repv : Integer]  ;; simulations where R candidate wins
   [ties : Integer]) ;; simulations where candidates tie
  #:transparent)

(define-type Outcome (U Party 'tie))

;;; === data === 

(: ev-map (Listof EV))
(define ev-map 
  (list
   (EV 'AL 9)
   (EV 'AK 3)
   (EV 'AZ 11)
   (EV 'AR 6)
   (EV 'CA 55)
   (EV 'CO 9)
   (EV 'CT 7)
   (EV 'DE 3)
   (EV 'DC 3)
   (EV 'FL 29)
   (EV 'GA 16)
   (EV 'HI 4)
   (EV 'ID 4)
   (EV 'IL 20)
   (EV 'IN 11)
   (EV 'IA 6)
   (EV 'KS 6)
   (EV 'KY 8)
   (EV 'LA 8)
   (EV 'ME 4)
   (EV 'MD 10)
   (EV 'MA 11)
   (EV 'MI 16)
   (EV 'MN 10)
   (EV 'MS 6)
   (EV 'MO 10)
   (EV 'MT 3)
   (EV 'NE 5)
   (EV 'NV 6)
   (EV 'NH 4)
   (EV 'NJ 14)
   (EV 'NM 5)
   (EV 'NY 29)
   (EV 'NC 15)
   (EV 'ND 3)
   (EV 'OH 18)
   (EV 'OK 7)
   (EV 'OR 7)
   (EV 'PA 20)
   (EV 'RI 4)
   (EV 'SC 9)
   (EV 'SD 3)
   (EV 'TN 11)
   (EV 'TX 38)
   (EV 'UT 6)
   (EV 'VT 3)
   (EV 'VA 13)
   (EV 'WA 12)
   (EV 'WV 5)
   (EV 'WI 10)
   (EV 'WY 3)))

(: prob-map (Listof DemProb))
;; These probabilities are fabricated. They are loosely modeled on the 
;; Obama/Romney predictions prior to 2012 elections.
(define prob-map
  (list
   (DemProb 'AL 0)
   (DemProb 'AK 0)
   (DemProb 'AZ 0.02)
   (DemProb 'AR 0)
   (DemProb 'CA 1)
   (DemProb 'CO 0.50)
   (DemProb 'CT 1)
   (DemProb 'DE 1)
   (DemProb 'DC 1)
   (DemProb 'FL 0.30)
   (DemProb 'GA 0)
   (DemProb 'HI 1)
   (DemProb 'ID 0)
   (DemProb 'IL 1)
   (DemProb 'IN 0)
   (DemProb 'IA 0.73)
   (DemProb 'KS 0)
   (DemProb 'KY 0)
   (DemProb 'LA 0)
   (DemProb 'ME 0.89)
   (DemProb 'MD 1)
   (DemProb 'MA 1)
   (DemProb 'MI 0.80)
   (DemProb 'MN 0.94)
   (DemProb 'MS 0)
   (DemProb 'MO 0.23)
   (DemProb 'MT 0)
   (DemProb 'NE 0)
   (DemProb 'NV 0.65)
   (DemProb 'NH 0.70)
   (DemProb 'NJ 1)
   (DemProb 'NM 0.87)
   (DemProb 'NY 1)
   (DemProb 'NC 0.20)
   (DemProb 'ND 0)
   (DemProb 'OH 0.50)
   (DemProb 'OK 0)
   (DemProb 'OR 0.90)
   (DemProb 'PA 0.72)
   (DemProb 'RI 1)
   (DemProb 'SC 0)
   (DemProb 'SD 0)
   (DemProb 'TN 0)
   (DemProb 'TX 0.01)
   (DemProb 'UT 0)
   (DemProb 'VT 1)
   (DemProb 'VA 0.50)
   (DemProb 'WA 1)
   (DemProb 'WV 0)
   (DemProb 'WI 0.68)
   (DemProb 'WY 0.02)))
  
(: all-states (Listof State))
(define all-states
  (list 'AL 'AK 'AZ 'AR 'CA 'CO 'CT 'DE 'DC 'FL 'GA 'HI 'ID 'IL 'IN 'IA 'KS
        'KY 'LA 'ME 'MD 'MA 'MI 'MN 'MS 'MO 'MT 'NE 'NV 'NH 'NJ 'NM 'NY 'NC
        'ND 'OH 'OK 'OR 'PA 'RI 'SC 'SD 'TN 'TX 'UT 'VT 'VA 'WA 'WV 'WI 'WY))

;;; === simulation code ===

(: find-prob : State (Listof DemProb) -> Real)
;;find prob of corresponding state
(define (find-prob x l)
  (DemProb-demp 
   (first 
    (filter (lambda ([y : DemProb]) (eq? x (DemProb-s y))) l))))

(: find-ev : State (Listof EV) -> Integer)
;;find the corresponding electoral votes per the state
(define (find-ev x l)
  (EV-ev (first (filter (lambda ([y : EV]) (eq? x (EV-s y))) l))))

(: sim-state : State -> StateResult)
;; given a state, choose a random number on [0,1] and consult 
;; the probability in prob-map above to determine victorious party
;; and look up the number of electoral votes in ev-map
(define (sim-state s)
  (if
   (<= (random) (find-prob s prob-map))
   (StateResult s 'D (find-ev s ev-map))
   (StateResult s 'R (find-ev s ev-map))))

(: list-dems : (Listof StateResult) -> (Listof StateResult))
;;create a list with the states in which dems win
(define (list-dems l)
  (filter (lambda ([x : StateResult]) (eq? 'D (StateResult-p x))) l))

(: list-reps : (Listof StateResult) -> (Listof StateResult))
;;create a list with the states in which reps win
(define (list-reps l)
  (filter (lambda ([x : StateResult]) (eq? 'R (StateResult-p x))) l))

(: sim-USA : -> USAResult)
;; run simulation on all states (plus Washington, DC)
(define (sim-USA)
  (USAResult 
   (list-dems (map sim-state all-states)) 
   (list-reps (map sim-state all-states))))

(: sum-ev : (Listof StateResult) -> Integer)
;;sum the electoral votes in a given list
(define (sum-ev l)
  (cond
    [(empty? l) 0]
    [(cons? l) (+ (StateResult-ev (first l)) (sum-ev (rest l)))]))

(: outcome : USAResult -> Outcome)
;; Add the electoral votes of each candidate to determine outcome.
;; Assume no state splits its electoral votes (in actuality, some do).
(define (outcome r)
  (cond
    [(> (sum-ev (USAResult-dems r)) (sum-ev (USAResult-reps r))) 'D]
    [(< (sum-ev (USAResult-dems r)) (sum-ev (USAResult-reps r))) 'R]
    [else 'tie])) 


(: results : Integer -> (Listof Outcome))
;; create a list of length x with the outcomes of each simulation
(define (results x)
  (cond
    [(= x 0) '()]
    [else (cons (outcome (sim-USA)) (results (- x 1)))]))

(: run-sims : Integer -> Tally)
;; given a number of trials to run, run the simulation that
;; number of times, and tally the results over the trials
(define (run-sims n)
  (local
    {(define y (results n))}
    (Tally
     (length (filter (lambda ([x : Outcome]) (eqv? x 'D)) y))
     (length (filter (lambda ([x : Outcome]) (eqv? x 'R)) y))
     (length (filter (lambda ([x : Outcome]) (eqv? x 'tie)) y)))))

;; ======= STOP ======
;; Lab 7 starts from here on

;;list without DC, HI, or AK
(define reduced-list
    (list 'AL 'AZ 'AR 'CA 'CO 'CT 'DE 'FL 'GA 'ID 'IL 'IN 'IA 'KS
        'KY 'LA 'ME 'MD 'MA 'MI 'MN 'MS 'MO 'MT 'NE 'NV 'NH 'NJ 'NM 'NY 'NC
        'ND 'OH 'OK 'OR 'PA 'RI 'SC 'SD 'TN 'TX 'UT 'VT 'VA 'WA 'WV 'WI 'WY))

(: list-states : (Listof State) -> String)
;;return part of URL with states
(define (list-states l)
  (match l
    ['() ""]
    [(cons hd '())
     (string-append
      "US-"
      (symbol->string hd))]
    [(cons hd tl) 
     (string-append
      "US-"
      (symbol->string hd)
      "|"
      (list-states tl))]))
(define map-states (list-states reduced-list))

(: blue? : State USAResult -> String)
;;returns blue if democrat, red otherwise
(define (blue? st r)
  (define x (USAResult-dems r))
  (cond
    [(empty? x) "FF0000"]
    [(cons? x)
     (if
      (eqv? st (StateResult-s (first x)))
      "0000FF"
      (blue? st (USAResult (rest x) (USAResult-reps r))))]))

(: color-of-map : USAResult (Listof State) -> String)
;;find the corresponding colors of each state
(define (color-of-map r l)
  (match l
    ['() ""]
    [(cons hd '())
     (blue? hd r)]
    [(cons hd tl)
     (string-append
      (blue? hd r)
      "|"
      (color-of-map r tl))]))

(: sim-image-URL : USAResult -> String)
;;consumes a USAResult and produces a Google Image URL to display that result
(define (sim-image-URL r)
  (string-append
   "http://chart.googleapis.com/chart?cht=map:auto=20,20,40,40&chs=240x240&chld="
   map-states
   "&chco=ABABAB|"
   (color-of-map r all-states)
   "&chtt=Simulated%202016%20Election"))

(bitmap/url (sim-image-URL (sim-USA)))
(sim-image-URL (sim-USA))

(: state-outcome : State USAResult -> String)
;;produces the outcome of a desired state
(define (state-outcome st r)
  (cond
    [(eqv? (blue? st r) "0000FF") "Democrat"]
    [(eqv? (blue? st r) "FF0000") "Republican"]
    [else (error "not found")]))

(: sim-image : USAResult -> Image)
;;consumes a USAResult and produces a Google Image URL to display that result (advanced)
(define (sim-image r)
  (above
   (bitmap/url (sim-image-URL r))
   (text (string-append "DC - " (state-outcome 'DC r)) 20 "blue")
   (text (string-append "HI - " (state-outcome 'HI r)) 20 "blue")
   (text (string-append "AK - " (state-outcome 'AK r)) 20 "red")
   (text (string-append "Democrats : " (number->string (sum-ev (USAResult-dems r)))) 20 "blue")
   (text (string-append "Republicans : " (number->string (sum-ev (USAResult-reps r)))) 20 "red")
   (cond
     [(eqv? (outcome r) 'D) 
      (text (string-append (symbol->string 'D) " " "victory") 30 "blue")]
     [(eqv? (outcome r) 'R) 
      (text (string-append (symbol->string 'R) " " "victory") 30 "red")]
     [(eqv? (outcome r) 'tie) 
      (text (string-append (symbol->string 'tie)) 30 "black")]
     [else (error "error")])))
(sim-image (sim-USA))

(: iterate-until-tie : -> Image)
;; Run simulations repeatedly until the outcome of the simulation is a tie, then return an image of the tie scenario
(define (iterate-until-tie)
  (define r (sim-USA))
  (cond
    [(eqv? 'tie (outcome r))
     (sim-image r)]
    [else (iterate-until-tie)]))
(iterate-until-tie)

(test)

;; === graders' tests

(define split-along-mississippi-river
  (USAResult
   (list
    (StateResult 'CT 'D 7)  (StateResult 'DE 'D 3)  (StateResult 'DC 'D 3)
    (StateResult 'IL 'D 20) (StateResult 'ME 'D 4)  (StateResult 'MD 'D 10)
    (StateResult 'MA 'D 11) (StateResult 'MI 'D 16) (StateResult 'NH 'D 4)
    (StateResult 'NJ 'D 14) (StateResult 'NY 'D 29) (StateResult 'OH 'D 18)
    (StateResult 'PA 'D 20) (StateResult 'RI 'D 4)  (StateResult 'VT 'D 3)
    (StateResult 'WI 'D 10) (StateResult 'AL 'D 9)  (StateResult 'AR 'D 6)
    (StateResult 'FL 'D 29) (StateResult 'GA 'D 16) (StateResult 'IN 'D 11)
    (StateResult 'KY 'D 8)  (StateResult 'LA 'D 8)  (StateResult 'MS 'D 6)
    (StateResult 'MO 'D 10) (StateResult 'NC 'D 15) (StateResult 'TN 'D 11)
    (StateResult 'VA 'D 13) (StateResult 'SC 'D 9)  (StateResult 'WV 'D 5))
   (list
    (StateResult 'SD 'R 3)  (StateResult 'WA 'R 12) (StateResult 'OR 'R 7)
    (StateResult 'NM 'R 5)  (StateResult 'MN 'R 10) (StateResult 'CA 'R 55)
    (StateResult 'CO 'R 9)  (StateResult 'HI 'R 4)  (StateResult 'AK 'R 3)
    (StateResult 'AZ 'R 11) (StateResult 'ID 'R 4)  (StateResult 'IA 'R 6)
    (StateResult 'KS 'R 6)  (StateResult 'MT 'R 3)  (StateResult 'NE 'R 5)
    (StateResult 'NV 'R 6)  (StateResult 'ND 'R 3)  (StateResult 'OK 'R 7)
    (StateResult 'TX 'R 38) (StateResult 'UT 'R 6)  (StateResult 'WY 'R 3))))

"sim-image"
(sim-image split-along-mississippi-river)

"tie"
(iterate-until-tie)

;; evaluation

;; GRADER: incorrect test sim image; should be "split along Mississippi"
;;
;;         sim-image-URL -2

;; === correctness ===

;; sim-image-URL                     6/ 8
;; sim-image                         8/ 8
;; iterate-until-tie                 6/ 6

;; === style ===

;; code layout                       4/ 4
;; program organization              4/ 4

;; contracts (types)                 4/ 4
;; well-written purposes             4/ 4
;; adequate tests                    4/ 4

;; clarity (clear logic)             6/ 6

;; svn used correctly                2/ 2

;; _total-score_                   48/  50

;; graded by Jonathan Jin
