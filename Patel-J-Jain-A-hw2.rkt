;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Patel-J-Jain-A-hw2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Jai Patel
;; Adish Jain
;; jcpatel@wpi.edu
;; ajain5@wpi.edu


;; 1.

(define-struct hurricane (name category maximum-winds velocity direction))
;; A hurricane is A. make-hurricane (String Natural Natural Natural String)
;; interp: represents a hurricane where
;; name is the name of the hurricane
;; catgeory is the category of the hurricane between 1 and 5 inclusive
;; maximum-winds is the maximum sustained winds in miles per hour
;; velocity is the velocity of the storm in miles per hour
;; direction is the hurricane's heading (for example NNW)
(define HURRICANE (make-hurricane "DAVE" 2 20 25 "SW"))

(define-struct thunderstorm (rainfall wind-gust velocity direction))
;; A thunderstorm is A. make-thunderstorm (Natural Natural Natural String)
;; interp: represents a thunderstorm where
;; rainfall is the number of inches of rainfall
;; wind-gust is the maximum wind gust in miles per hour
;; velocity is the velocity of the storm in miles per hour
;; direction is the thunderstorm's heading
(define THUNDERSTORM (make-thunderstorm 20 30 40 "W"))

(define-struct fire (coverage days people))
;; A fire is A. make-fire (Natural Natural Natural)
;; interp: represents a fire where
;; coverage is the number of square miles it covers
;; days is the number of days it has been raging
;; people is the number of people displaced by the fire
(define FIRE (make-fire 27 42 200))


;; 2.

;; A Storm is one of
;; Hurricane
;; Thunderstorm
;; Fire


;;     storm-fcn:  Storm ->  ...
;; 
;; (define (storm-fcn a-storm)
;;   (cond [(hurricane? a-storm) (... (hurricane-name a-storm)
;;                               (hurricane-category a-storm)
;;                               (hurricane-maximum-winds a-storm)
;;                               (hurricane-velocity a-storm)
;;                               (hurricane-direction a-storm))]
;;         [(thunderstorm? a-storm)  (... (thunderstorm-rainfall a-storm)
;;                                (thunderstorm-wind-gust a-storm)
;;                                (thunderstorm-velocity a-storm)
;;                                (thunderstorm-direction a-storm))]
;;         [(fire? a-storm)  (... (fire-coverage a-storm)
;;                                (fire-days a-storm)
;;                                (fire-people a-storm))]                               


;; make-hurricane: String Natural Natural Natural String -> Hurricane
;; hurricane-name: Hurricane -> String
;; hurricane-category: Hurricane -> Natural
;; hurricane-maximum-winds: Hurricane -> Natural
;; hurricane-velocity: Hurricane -> Natural
;; hurricane-direction: Hurricane -> String

;; make-thunderstorm: Natural Natural Natural String -> Thunderstorm
;; thunderstorm-rainfall: Thunderstorm -> Natural
;; thunderstorm-wind-gust: Thunderstorm -> Natural
;; thunderstorm-velocity: Thunderstorm -> Natural
;; thunderstorm-direction: Thunderstorm -> String

;; make-fire: Natural Natural Natural -> Fire
;; fire-coverage: Fire -> Natural
;; fire-days: Fire -> Natural
;; fire-people: Fire -> Natural


;; 3.

;; Storm -> Boolean
;; Consume a storm and produce a boolean that returns true if the storm is a category 4 or 5 hurricane,
;; a thunderstorm with more than 3 inches of rainfall and winds exceeding 60mph,
;; or a fire covering at least 50 square miles.

(define (high-impact? a-storm)
   (cond [(hurricane? a-storm) (or (equal? (hurricane-category a-storm) 4)
                                   (equal? (hurricane-category a-storm) 5))]
         [(thunderstorm? a-storm) (and (> (thunderstorm-rainfall a-storm) 3)
                                       (> (thunderstorm-wind-gust a-storm) 60))]
         [(fire? a-storm) (>= (fire-coverage a-storm) 50)]))

(check-expect (high-impact? (make-hurricane "Dave" 4 20 30 "W")) #true)
(check-expect (high-impact? (make-hurricane "Joe" 5 20 30 "W")) #true)
(check-expect (high-impact? (make-hurricane "Sam" 2 20 30 "W")) #false)
(check-expect (high-impact? (make-thunderstorm 3 59 61 "W")) #false)
(check-expect (high-impact? (make-thunderstorm 3 60 77 "W")) #false)                            
(check-expect (high-impact? (make-thunderstorm 3 61 3 "W")) #false)
(check-expect (high-impact? (make-thunderstorm 2 59 3 "W")) #false)
(check-expect (high-impact? (make-thunderstorm 2 60 3 "W")) #false)
(check-expect (high-impact? (make-thunderstorm 2 61 3 "W")) #false)
(check-expect (high-impact? (make-thunderstorm 4 59 3 "W")) #false)
(check-expect (high-impact? (make-thunderstorm 4 60 3 "W")) #false)
(check-expect (high-impact? (make-thunderstorm 4 61 3 "W")) #true)
(check-expect (high-impact? (make-fire 80 7 20)) #true)
(check-expect (high-impact? (make-fire 10 7 20)) #false)
(check-expect (high-impact? (make-fire 50 7 20)) #true)


;; 4.

;; Signature: Storm -> Storm
;; Purpose: Consumes a storm and a heading and produces a storm

(define ADISH (make-hurricane "Adish" 4 169 30 "E"))
(define ADISH2 (make-hurricane "Adish" 4 169 30 "N"))
(define ADISH3 (make-hurricane "Adish" 4 169 30 "E"))
(define STORMY (make-thunderstorm 20 40 80 "W"))
(define STORMY2 (make-thunderstorm 20 40 80 "N"))

(define (change-heading a-storm storm-direction)
  (cond [(hurricane? a-storm) (make-hurricane (hurricane-name a-storm)
                                              (hurricane-category a-storm)
                                              (hurricane-maximum-winds a-storm)
                                              (hurricane-velocity a-storm) storm-direction)]
        [(thunderstorm? a-storm) (make-thunderstorm (thunderstorm-rainfall a-storm)
                                    (thunderstorm-wind-gust a-storm)
                                    (thunderstorm-velocity a-storm) storm-direction)]
        [(fire? a-storm) (make-fire (fire-coverage a-storm)
                                    (fire-days a-storm)
                                    (fire-people a-storm))]))
                                    

(check-expect (change-heading ADISH "N") ADISH2)
(check-expect (change-heading ADISH "E") ADISH3)
(check-expect (change-heading STORMY "N") STORMY2)
(check-expect (change-heading STORMY "W") STORMY)
(check-expect (change-heading FIRE "N") FIRE)


;; 5.

;; Signature: ListOfString -> Natural
;; Purpose: Consume a ListOfString and produce the total number of characters in all the strings in the list.

(define (character-count a-los)
  (cond [(empty? a-los) 0]
        [(cons? a-los) (+ (string-length (first a-los)) (character-count (rest a-los)))]))               
                                                            

(check-expect (character-count empty) 0)
(check-expect (character-count (cons "steve" (cons "bell" (cons "the" empty)))) 12)
(check-expect (character-count (cons "george" empty)) 6)
(check-expect (character-count (cons "abcdefghijklmnopqrstuvwxyz" empty)) 26)
(check-expect (character-count (cons "bbb" (cons "sgab" (cons "this" (cons "a" empty))))) 12) 
(check-expect (character-count (cons "" empty)) 0)

;; 6.

;; Signature: ListOfString -> ListOfString
;; Purpose: Consumes a ListOfString and produces a list that contains only
;; the strings from the original list that consist of entirely numeric characters.

(define (numeric-strings a-los)
  (cond [(empty? a-los) empty]
        [(cons? a-los) (if (string-numeric? (first a-los))
                            (cons (first a-los)(numeric-strings (rest a-los)))
                            (numeric-strings(rest a-los)))]))

  
(check-expect (numeric-strings empty) empty)
(check-expect (numeric-strings (cons "1" empty)) (cons "1" empty))
(check-expect (numeric-strings (cons "cat" (cons "12345" (cons "the" (cons "123" empty))))) (cons "12345" (cons "123" empty)))
(check-expect (numeric-strings (cons "1" (cons "2" (cons "a" (cons "3" empty))))) (cons "1" (cons "2" (cons "3" empty))))


;; 7.

;; Signature: ListOfString -> ListOfNatural
;; Purpose: Consumes a ListOfString and produces a list of the lengths of each of the strings in the given ListOfString called ListOfNatural.

;; A ListOfNatural is one of:
;; - '()
;; - (cons String ListOfNatural)
;; a list of natural numbers

  
(define (lengths-of-strings a-los)
  (cond [(empty? a-los) empty]
        [(cons? a-los)
         (cons (string-length (first a-los)) (lengths-of-strings (rest a-los)))
                       ]))

(check-expect (lengths-of-strings (cons "12345678910ert" empty)) (cons 14 empty))
(check-expect (lengths-of-strings (cons "abc" (cons "tttttt" (cons "a" empty)))) (cons 3 (cons 6 (cons 1 empty))))
(check-expect (lengths-of-strings (cons "the" empty)) (cons 3 empty))
(check-expect (lengths-of-strings (cons "pear" (cons "apple" empty))) (cons 4 (cons 5 empty)))
(check-expect (lengths-of-strings empty) empty)