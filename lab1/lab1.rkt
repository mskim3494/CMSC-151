#lang racket
9

(+ 9 1)

(+ 1 9)

(- 9 1)

(- 1 9)


(* 9 1)

(* 1 9)

(/ 9 1)

(/ 1 9)

 
(expt 9 1)

(expt 9 2)

(expt 1 9)

(expt 2 9)

(+ (expt 9 1) 1)

(+ (expt 9 2) 2)

(- (expt 9 2) (expt 9 1))
(= (expt 9 1) (expt 9 2))

(< (expt 9 1) (expt 9 2))

(> (expt 9 1) (expt 9 2))

;; a : Integer
(define a 7)

;; b : Integer
(define b 8)

(* a b)

(- (* b b) (* a a))

