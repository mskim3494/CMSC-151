#lang typed/racket
(require typed/test-engine/racket-tests)

;;Part 1
(: leap? (-> Integer Boolean))
(define (leap? y)
  (cond
  ((or 
    (and (= (remainder y 4) 0) (not (eq? (remainder y 100) 0)))
    (= (remainder y 400) 0)) true)
    (else false)))
(check-expect (leap? 400) true)
(check-expect (leap? 1920) true)
(leap? 2014)


;;Part 2
(: date-valid? (-> Integer Integer Integer Boolean))
(define (date-valid? m d y)
 (cond
    [(or (> 1900 y) (<= 2100 y)) false]
    [else (cond
            [(and (= 0 m) (> m 13)) false]
            [(and (= m 2) (<= d 29) (leap? y)) true]
            [(and (= 2 m) (<= d 28)) true]
            [(and (= 1 m) (<= d 31)) true]
            [(and (= 3 m) (<= d 31)) true]
            [(and (= 4 m) (<= d 30)) true]
            [(and (= 5 m) (<= d 31)) true]
            [(and (= 6 m) (<= d 30)) true]
            [(and (= 7 m) (<= d 31)) true]
            [(and (= 8 m) (<= d 31)) true]
            [(and (= 9 m) (<= d 30)) true]
            [(and (= 10 m) (<= d 31)) true]
            [(and (= 11 m) (<= d 30)) true]
            [(and (= 12 m) (<= d 31)) true]
            [else false])]))
(date-valid? 2 29 2014)
(date-valid? 10 7 2014)
(check-expect (date-valid? 9 32 2014) false)
(check-expect (date-valid? 2 20 2100) false)
(check-expect (date-valid? 2 20 1899) false)

;;Part 3
;;first define month adjustment
;;month adjustment
(: adjustment (-> Integer Integer Integer))
(define (adjustment m y)
  (cond
    [(and (= m 1) (leap? y)) 0]
    [(and (= m 1) (not(leap? y))) 1]
    [(and (= m 2) (leap? y)) 3]
    [(and (= m 2) (not(leap? y))) 4]
    [(= m 3) 4]
    [(= m 4) 0]
    [(= m 5) 2]
    [(= m 6) 5]
    [(= m 7) 0]
    [(= m 8) 3]
    [(= m 9) 6]
    [(= m 10) 1]
    [(= m 11) 4]
    [(= m 12) 6]
    [else 10]))
(check-expect (adjustment 3 2000) 4)
(check-expect (adjustment 6 2010) 5)
(check-expect (adjustment 1 2020) 0)
(check-expect (adjustment 2 2011) 4)

;;now find day of week
(: day-of-week (-> Integer Integer Integer String))
(define (day-of-week m d y)
  (: n (-> Integer Integer Integer Integer))
   (define (n m d y)
     (+ (- y 1900) (adjustment m y) d (exact-floor (/ y 4))))
  (: w (-> Integer Integer))
   (define (w n) (remainder n 7))
  (cond
    [(eq? 0 (w (n m d y))) "Sunday"]
    [(eq? 1 (w (n m d y))) "Monday"]
    [(eq? 2 (w (n m d y))) "Tuesday"]
    [(eq? 3 (w (n m d y))) "Wednesday"]
    [(eq? 4 (w (n m d y))) "Thursday"]
    [(eq? 5 (w (n m d y))) "Friday"]
    [(eq? 6 (w (n m d y))) "Saturday"]
    [else "error"]))

(day-of-week 10 9 2014)
(check-expect (day-of-week 10 10 2014) "Friday")
(check-expect (day-of-week 4 3 2015) "Friday")
(check-expect (day-of-week 4 3 1994) "Sunday")

;;Part 4
(: date->string (-> Integer Integer Integer String))
(define (date->string m d y)
  (: month (-> Integer String))
  (define (month m)
    (cond
      [(= m 1) "Jan"]
      [(= m 2) "Feb"]
      [(= m 3) "Mar"]
      [(= m 4) "Apr"]
      [(= m 5) "May"]
      [(= m 6) "Jun"]
      [(= m 7) "Jul"]
      [(= m 8) "Aug"]
      [(= m 9) "Sep"]
      [(= m 10) "Oct"]
      [(= m 11) "Nov"]
      [(= m 12) "Dec"]
      [else "invalid"]))
  (cond
    [(not (date-valid? m d y)) "[invalid date]"]
    [else (string-append 
           (day-of-week m d y) " " (number->string d) " " (month m) " " (number->string y))]))
(date->string 10 9 2014)
(check-expect (date->string 4 3 1994) "Sunday 3 Apr 1994")
(check-expect (date->string 4 8 1992) "Wednesday 8 Apr 1992")

(test)