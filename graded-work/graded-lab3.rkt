#lang typed/racket

(require/typed 2htdp/image
   [#:opaque Image image?]
   [rectangle (-> Number Number String String Image)]
   [image-width (-> Image Number)]
   [image-height (-> Image Number)]
   [text (-> String Number String Image)] 
   [beside (-> Image * Image)]
   [beside/align (-> String Image * Image)]
   [above (-> Image * Image)]
   [above/align (-> String Image * Image)]
   [overlay (-> Image * Image)]
   [crop (-> Number Number Number Number Image Image)]
   [flip-vertical (-> Image Image)]
   [flip-horizontal (-> Image Image)]
   [freeze (-> Image Image)]
   [rotate (-> Number Image Image)])

;; medium-sized icons for each suit

(: hearts-med Image)
(define hearts-med 
  (crop -2 10 32 36 (text "♥" 40 "red")))

(: spades-med Image)
(define spades-med 
  (crop -2 10 32 36 (text "♠" 40 "black")))

(: clubs-med Image)
(define clubs-med 
  (crop 0 10 32 36 (text "♣" 40 "black")))

(: diamonds-med Image)
(define diamonds-med 
  (crop -2 10 32 36 (text "♦" 40 "red")))

;; small icons for each suit

(: hearts-sm Image)
(define hearts-sm 
  (crop -2 4 18 20 (text "♥" 20 "red")))

(: spades-sm Image)
(define spades-sm 
  (crop -2 4 18 20 (text "♠" 20 "black")))

(: clubs-sm Image)
(define clubs-sm 
  (crop -1 4 18 20 (text "♣" 20 "black")))

(: diamonds-sm Image)
(define diamonds-sm 
  (crop -2 4 18 20 (text "♦" 20 "red")))

;; suit structures

(define-struct Suit
  ([name : String]
   [color : String]
   [small-icon : Image]
   [medium-icon : Image])
  #:transparent)

(define hearts   (make-Suit "hearts" "red" hearts-sm hearts-med))
(define diamonds (make-Suit "diamonds" "red" diamonds-sm diamonds-med))
(define clubs    (make-Suit "clubs" "black" clubs-sm clubs-med))
(define spades   (make-Suit "spades" "black" spades-sm spades-med))

;; some useful operations

(: frame (-> Integer Image Image))
;; given padding in pixels and an image, draw a thin
;; black rectangle around the image
(define (frame padding i)
  (overlay i (rectangle (+ padding (image-width i)) 
                        (+ padding (image-height i)) 
                        "outline" 
                        "black")))

(: spacer-v (-> Number Image))
;; construct a tall, thin, white rectangle for vertical space
(define (spacer-v n)
  (rectangle 1 n "solid" "white"))

(: flip-v (-> Image Image))
;; flip an image vertically, even if image includes text
;; (this is why "freeze" is called)
(define (flip-v i)
  (flip-vertical (freeze i)))

(: mirror-v (-> Image Image))
;; "vertical mirroring" -- show image above its own reflection
(define (mirror-v i)
  (above i (flip-v i)))

;; === student's code below ===
;; (: nine-of (-> Suit Image))
(: nine-of (-> Suit Image))
(define (nine-of suit)
   (frame 9
       (above/align "right"
         (above/align "left" (text "9" 20 (Suit-color suit)) 
          (beside/align "top" (Suit-small-icon suit)
             (beside/align "middle" 
                           (above/align "middle" (Suit-medium-icon suit) 
                                                 (Suit-medium-icon suit) 
                                                 (flip-v (Suit-medium-icon suit))
                                                 (flip-v (Suit-medium-icon suit)))
                           (Suit-medium-icon suit))
             (above/align "left" (Suit-medium-icon suit) 
                                   (Suit-medium-icon suit) 
                                   (flip-v (Suit-medium-icon suit))
                                   (beside/align "bottom" (flip-v (Suit-medium-icon suit))
                                                          (flip-v (Suit-small-icon suit))))))
         (flip-horizontal (flip-v (text "9" 20 (Suit-color suit)))))))

(nine-of hearts)
(nine-of clubs)

;; (: ten-of (-> Suit Image))
(: ten-of (-> Suit Image))
(define (ten-of suit)
 (frame 9
  (above/align "right"
   (above/align "left" (text "10" 20 (Suit-color suit)) 
          (beside/align "bottom"
             (beside/align "top" (Suit-small-icon suit)
               (mirror-v (beside/align "middle" 
                           (above/align "middle" (Suit-medium-icon suit) 
                                                 (Suit-medium-icon suit))
                           (Suit-medium-icon suit)
                           (above/align "middle" (Suit-medium-icon suit) 
                                                 (Suit-medium-icon suit)))))
               (flip-v (Suit-small-icon suit))))
   (flip-horizontal (flip-v (text "10" 20 (Suit-color suit)))))))
(ten-of spades)
(ten-of diamonds)

;GRADER:TESTS
(nine-of hearts)
(nine-of diamonds)
(nine-of spades)
(nine-of clubs)
(ten-of hearts)
(ten-of diamonds)
(ten-of spades)
(ten-of clubs)

;; evaluation

;; === correctness ===

;; helper functions                  16/16
;; nine-of                           14/14
;; ten-of                            14/14

;; === style ===

;; code layout                       8/ 8
;; identifiers are well named        8/ 8
;; program decomposition (helpers)   0/ 4 ;GRADER: You need to include helper functions when

;; contracts (types)                 8/ 8
;; well-written purposes             8/ 8
;; adequate tests                    8/ 8

;; clarity (clear logic)             6/ 6

;; svn used correctly                6/ 6

;; _total-score_                   96/ 100

;; graded by Benjamin Rohrer (bjr)