 #lang typed/racket
(require typed/test-engine/racket-tests)
;;Worked with Chanwool Kim and Jamie Lee

;;Problem 1
(: area-of-intersection (-> Real Real Number))
;; compute the area of intersection between two circles with radii r and distance d
(define (area-of-intersection r d)
  (-
   (* 2 (* r r) (acos(/ d (* 2 r))))
   (* 0.5 d (sqrt (- (* 4 (sqr r)) (sqr d))))))
(area-of-intersection 5 3)
(check-within (area-of-intersection 1 0.8079455) (* 0.5 pi) 0.00001)
(check-within (area-of-intersection 10 4) 234.6958 0.0001)
(check-within (area-of-intersection 9 4) 183.0661 0.001)

;;Problem 2
(: mobile-broadband-cost (-> Integer Integer))
;;compute the amount of money to pay based on broadband usage
(define (mobile-broadband-cost MB)
  (cond
       [(<= MB 300) 20]
       [(and (> MB 300) (<= MB 3072)) 30]
       [(and (> MB 3072) (<= MB 4096)) 45]
       [else (+ 30 (* 15 (- (exact-ceiling (/ MB 1024)) 3)))]
      )
  )
(mobile-broadband-cost 2000)
(mobile-broadband-cost 5500)
(check-expect (mobile-broadband-cost 6000) 75)
(check-expect (mobile-broadband-cost 3500) 45)

;;Problem 3
(define-struct Food
 ([name : String]
  [size : Exact-Rational]
  [fat : Exact-Rational]
  [carbs : Exact-Rational]
  [protein : Exact-Rational])
  #:transparent)
(define cheese-steak (make-Food "Pepperoni Cheese Steak" 201 30 40 30))
(define fries (make-Food "French Fries" 134 12 43 4))
(define ice-cream (make-Food "Ice Cream" 107 8 27 4))
(define steak (make-Food "Steak" 182 20 0 46))
(define potatoes (make-Food "Mashed Potatoes" 240 10 37 5))
(define cake (make-Food "Slice of Cake" 80 10 43 4))

(define-struct Meal 
 ([entree : Food]
  [side : Food]
  [dessert : Food])
  #:transparent)
(define Meal1 (make-Meal cheese-steak fries ice-cream))
(define Meal2 (make-Meal steak potatoes cake))

(: food-kcal (-> Food Exact-Rational))
;;compute the kilocalories in the food
(define (food-kcal foo) 
  (+ (* 9 (Food-fat foo))
     (* 4 (Food-carbs foo))
     (* 4 (Food-protein foo))))
(food-kcal cheese-steak)
(check-expect (food-kcal fries) 296)
(check-expect (food-kcal cake) 278)

(: foods-fat (-> Food Food Exact-Rational))
;;compute the total grams of fat in two foods
(define (foods-fat fat fatty)
  (+ 
   (Food-fat fatty)
   (Food-fat fat)))
(foods-fat cheese-steak fries)
(check-expect (foods-fat cheese-steak potatoes) 40)
(check-expect (foods-fat steak ice-cream) 28)

(: food-scale (-> Food Exact-Rational Food))
;;scale the numbers of the food (serving size, fat, protein, and carbs) to the number provided
(define (food-scale foo x)
  (make-Food (string-append "x" (Food-name foo)) (* x (Food-size foo)) (* x (Food-fat foo)) (* x (Food-carbs foo)) (* x (Food-protein foo))
               ))
(food-scale steak 2)
(check-expect (food-scale cake 3) (Food "xSlice of Cake" 240 30 129 12))
(check-expect (food-scale potatoes 4) (Food "xMashed Potatoes" 960 40 148 20))

(: food-daily-pct-kcal (-> Food Integer Exact-Rational))
;;compute the percent of calories consumed from the daily quota
(define (food-daily-pct-kcal foodie quota)
  (/ (food-kcal foodie) quota))
(food-daily-pct-kcal cheese-steak 2000)
(check-expect (food-daily-pct-kcal ice-cream 2000) 49/500)
(check-expect (food-daily-pct-kcal fries 2000) 37/250)

(: meal-kcal (-> Meal Exact-Rational))
;;calculate the calories of a whole meal
(define (meal-kcal mymeal)
  (+
   (food-kcal (Meal-entree mymeal))
   (food-kcal (Meal-side mymeal))
   (food-kcal (Meal-dessert mymeal))))
(meal-kcal Meal1)
(check-expect (meal-kcal Meal2) 900)

;;Helper functions for the meal-healthy? function
(: kcal (-> Meal Exact-Rational))
;;helper function to return the amount of kcal in a given meal
(define (kcal meal) (meal-kcal meal))
(kcal Meal1)
(check-expect (kcal Meal1) 1042)
(check-expect (kcal Meal2) 900)


(: gr-fat (-> Meal Exact-Rational))
;;helper function to compute the number of grams of fat in a given meal
(define (gr-fat meal) 
    (+ (Food-fat (Meal-entree meal)) (Food-fat (Meal-side meal)) (Food-fat (Meal-dessert meal))))
(gr-fat Meal2)
(check-expect (gr-fat Meal1) 50)
(check-expect (gr-fat Meal2) 40)

(: meal-healthy? (-> Meal Integer Boolean))
;;determine whether the meal is healthy or not
(define (meal-healthy? meal quota)
  (cond
    [(and 
      (< (kcal meal) (* 0.5 quota)) 
      (< (gr-fat meal) 50)
      (< (* 9 (gr-fat meal)) (kcal meal))) true]
    [else false]))
(meal-healthy? Meal1 2000)
(check-expect (meal-healthy? Meal2 2000) true)
(check-expect (meal-healthy? Meal1 2500) false)


;;Problem 4
(define-struct Time
  ([hour : Integer]
   [min : Integer]
   [sec : Integer]
   [zone : Integer])
  #:transparent)
(define time1 (make-Time 12 00 00 0))
(define time2 (make-Time 21 45 00 -6))
(define time3 (make-Time 8 20 49 6))
(define time4 (make-Time 1 00 00 -8))
(define time5 (make-Time 23 59 00 0))

(: UTC (-> Time Time))
;;helper function: change time to Coordinated Universal Time UTC
;;if value of hour is -1, there was an error in the operation
  (define (UTC z)
    (make-Time 
     (cond
          [(< (- (Time-hour z) (Time-zone z)) 24) (- (Time-hour z) (Time-zone z))]
          [(> (- (Time-hour z) (Time-zone z)) 24) (- (- (Time-hour z) (Time-zone z)) 24)]
          [else -1])
      (Time-min z) (Time-sec z) 0))
(UTC time2)
(UTC time1)
(check-expect (UTC time3) (Time 2 20 49 0))

(: before? (-> Time Time Boolean))
;;determine whether time x is strictly before time y
(define (before? x y)
  (cond
    [(> (Time-hour (UTC x)) (Time-hour (UTC y))) true]
    [(< (Time-hour (UTC x)) (Time-hour (UTC y))) false]
    [else (cond
            [(> (Time-min (UTC x)) (Time-min (UTC y))) true]
            [(< (Time-min (UTC x)) (Time-min (UTC y))) false]
            [else (cond
                    [(> (Time-sec (UTC x)) (Time-sec (UTC y))) true]
                    [(< (Time-sec (UTC x)) (Time-sec (UTC y))) false]
                    [else true])])]))
(before? time3 time2)
(before? time2 time3)
(check-expect (before? time2 time1) false)
(check-expect (before? time1 time3) true)
(check-expect (before? (make-Time 1 0 0 -5) (make-Time 22 0 0 -6)) true)

;;creating a helper function for add-minutes
(: min-add (-> Time Integer Integer))
;;find the resulting number from adding the mins of the time (Time-min T) and the minutes to be added (y)
(define (min-add T y)
  (+ (Time-min T) y))
(min-add time1 43)
(check-expect (min-add time2 15) 60)
(check-expect (min-add time3 21) 41)

(: add-minutes (-> Time Integer Time))
;;to find the resulting time from adding minutes to a given time
;;a value of -1 for hour value denotes an error
(define (add-minutes time x)
  (make-Time (cond
               [(>= (min-add time x) 60) (cond 
                                          [(>= (+ (Time-hour time) (quotient (min-add time x) 60)) 24) 
                                           (- (+ (Time-hour time) (quotient (min-add time x) 60)) 24)]
                                          [(< (+ (Time-hour time) (quotient (+ (Time-min time) x) 60)) 24) 
                                           (+ (Time-hour time) (quotient (min-add time x) 60))]
                                          [else -1])]
               [(< (min-add time x) 0) (cond
                                         [(< (+ (Time-hour time) (- (quotient (min-add time x) 60) 1)) 0) 
                                           (+ 24 (+ (Time-hour time) (- (quotient (min-add time x) 60) 1)))]
                                          [(> (+ (Time-hour time) (quotient (min-add time x) 60)) 0) 
                                           (+ (Time-hour time) (quotient (min-add time x) 60))]
                                          [else -2])]
               [else (Time-hour time)])
             (cond
               [(= (remainder (min-add time x) 60) 0) 0]
               [(> (min-add time x) 60) (remainder (min-add time x) 60)]
               [(< (min-add time x) 0) (+ 60 (remainder (min-add time x) 60))]
               [else (min-add time x)])
             (Time-sec time)
             (Time-zone time)))
(add-minutes time1 60)
(check-expect (add-minutes time2 200) (Time 1 5 0 -6))
(check-expect (add-minutes time4 -70) (Time 23 50 0 -8))
(check-expect (add-minutes (make-Time 1 30 0 -2) -151) (make-Time 22 59 0 -2))
(check-expect (add-minutes (make-Time 3 50 0 -2) -170) (make-Time 1 0 0 -2))

;;HELPER FUNCTION
(: time->sec (-> Time Integer))
;;turn the time given in number of seconds at UTC
(define (time->sec x)
  (+
   (* (Time-hour (UTC x)) 3600)
   (* (Time-min (UTC x)) 60)
   (Time-sec (UTC x))))
(time->sec time1)
(check-expect (time->sec time2) 13500)
(check-expect (time->sec time3) 8449)
(check-expect (time->sec time4) 32400)
(check-expect (time->sec time5) 86340)

(: diff-sec (-> Time Time Integer))
;;compute the time difference between two given times in seconds
(define (diff-sec x y)
  (abs 
   (- (time->sec x) (time->sec y))))
(diff-sec time1 time2)
(check-expect (diff-sec time2 time1) 29700)
(check-expect (diff-sec time2 time3) 5051)
(check-expect (diff-sec (make-Time 1 15 15 -11) (make-Time 1 15 15 -11)) 0)

(test)