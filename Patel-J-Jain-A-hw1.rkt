;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Patel-J-Jain-A-hw1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Jai Patel
;; Adish Jain
;; jcpatel@wpi.edu
;; ajain5@wpi.edu

;; 1. 

(define-struct film (title genre rating running-time opening-date receipts-collected))
;; A film is. A (make-film String String String Natural Date Natural)
;; interp: represents a film where
;; title is the title of the film
;; genre is the film's genre (drama, comedy, family, etc.)
;; rating is the film's rating where the rating can be one of G, PG, PG-13, R, NC-17, NR.
;; running-time is the running time of the film, in minutes
;; opening-date is the date the film opened at the theater that includes the year, month, and day
;; receipts-collected is the total box office receipts collected so far for the film (in millions of dollars)

(define-struct date (year month day))
;; Natural Natural Natural -> Date
;; A date is. A (make-date Natural Natural Natural)
;; interp: represents a date where
;; year is the year the film opened
;; month is the month the film opened
;; day is the day the film opened

(define DATE1 (make-date 1929 2 4))
(define DATE2 (make-date 2003 2 2))
(define DATE3 (make-date 1973 2 17))

(define TITANIC (make-film "Titanic" "Drama" "PG-13" 200 (make-date 2000 10 4) 200 ))
(define TITANIC2 (make-film "Titanic" "Drama" "R" 200 (make-date 2000 10 4) 200))
(define UP (make-film "Up" "Comedy" "PG" 300 (make-date 2001 12 3) 300 ))
(define UP2 (make-film "Up" "Comedy" "R" 300 (make-date 2001 12 3) 300))
(define MOVIE (make-film "Movie" "Family" "R" 400 (make-date 2005 12 3) 400))
(define MOVIE2 (make-film "Movie" "Family" "PG" 400 (make-date 2005 12 3) 400))
(define WALL (make-film "Wall" "Family" "G" 700 (make-date 2008 9 2) 600))
(define WALL2 (make-film "Wall" "Family" "PG-13" 700 (make-date 2008 9 2) 600))



;; 2.

;; Signatures:

;; make-Film: String String String Natural Date Natural -> Film

;; film-title: Film -> String
;; film-genre: Film -> String
;; film-rating: Film -> String
;; film-running-time: Film -> Natural
;; film-opening-date: Film -> Date
;; film-receipts-collected: Film -> Natural

;; Film?: Data Structure -> Boolean



;; 3.

;; Signature: Film -> Boolean
;; Consumes film and returns true if rating is G, PG, or PG-13 and returns false otherwise

(define (suitable-for-children? film)
  (cond 
    [(equal? "G" (film-rating film)) #true]
    [(equal? "PG" (film-rating film)) #true]
    [(equal? "PG-13" (film-rating film)) #true]
    [else #false]))

(check-expect (suitable-for-children? TITANIC) #true) ;Checks if TITANIC is suitable for children -> True
(check-expect (suitable-for-children? MOVIE) #false) ;Checks if MOVIE is suitable for children -> False
(check-expect (suitable-for-children? UP) #true) ;Checks if UP is suitable for children -> True
(check-expect (suitable-for-children? WALL) #true) ;Checks if WALL is suitable for children -> True
(check-expect (suitable-for-children? WALL2) #true) ;Checks if WALL2 is suitable for children -> True

;; 4.

 ; Signature: Film Film -> Natural
 ; Consumes two films and produces the absolute value of the difference between the box office receipts for the two films.

(define (difference-in-receipts film film2)
  (abs (- (film-receipts-collected film) (film-receipts-collected film2))))
    
(check-expect (difference-in-receipts MOVIE TITANIC) 200) ;Checks whether the difference in receipts of MOVIE and TITANIC is equal to 200
(check-expect (difference-in-receipts TITANIC UP) 100) ;Checks whether the difference in receipts of TITANIC and UP is equal to 100
(check-expect (difference-in-receipts MOVIE UP) 100) ;Checks whether the difference in receipts of MOVIE and UP is equal to 100
(check-expect (difference-in-receipts MOVIE2 UP2) 100) ;Checks whether the difference in receipts of MOVIE2 and UP2 is equal to 100
(check-expect (difference-in-receipts TITANIC2 UP2) 100) ;Checks whether the difference in receipts of TITANIC2 and UP2 is equal to 100
(check-expect (difference-in-receipts TITANIC2 TITANIC2) 0) ;Checks whether the difference in receipts of TITANIC2 and TITANIC2 is equal to 0
(check-expect (difference-in-receipts MOVIE MOVIE) 0) ;Checks whether the difference in receipts of MOVIE AND MOVIE is equal to 0

               
;; 5.

;;; Signature: Film String -> Film
;;; Consumes a film and a string and produces a film that is the same as the original except that its rating has been replaced by the given rating

(define (modify-rating film user-input-rating)
  (make-film (film-title film) (film-genre film) user-input-rating (film-running-time film) (film-opening-date film) (film-receipts-collected film)))

(check-expect (modify-rating TITANIC "R") TITANIC2) ;Checks if TITANIC2 is equivalent to TITANIC after the rating of TITANIC has been modified. 
(check-expect (modify-rating UP "R") UP2) ;Checks if UP2 is equivalent to UP after the rating of UP has been modified.
(check-expect (modify-rating MOVIE "PG") MOVIE2) ;Checks if MOVIE2 is equivalent to MOVIE after the rating of MOVIE has been modified.
(check-expect (modify-rating WALL "PG-13") WALL2) ;Checks if WALL2 is equivalent to WALL after the rating of WALL has been modified.


;; 6.

;; Signature: Film Date -> Boolean
;; Consumes a film and a date and produces a boolean that provides true if the given film opens before the given date and returns false otherwise

(define (opens-before? film date)
  (cond
    [(< (date-year (film-opening-date film)) (date-year date)) #true]
    [(and (= (date-year (film-opening-date film)) (date-year date)) (< (date-month (film-opening-date film)) (date-month date))) #true]
    [(and (= (date-year (film-opening-date film)) (date-year date)) (= (date-month (film-opening-date film)) (date-month date)) (< (date-day (film-opening-date film)) (date-day date))) #true]
    [(> (date-year (film-opening-date film)) (date-year date)) #false]
    [(> (date-month (film-opening-date film)) (date-month date)) #false]
    [(> (date-day (film-opening-date film)) (date-day date)) #false]
    [else #false]))


(check-expect (opens-before? TITANIC (make-date 2002 12 4)) #true) ;Checks if TITANIC opens before the given date -> True
(check-expect (opens-before? TITANIC (make-date 1999 3 6)) #false) ;Checks if TITANIC opens before the given date -> False
(check-expect (opens-before? TITANIC (make-date 2000 3 2)) #false) ;Checks if TITANIC opens before the given date -> False
(check-expect (opens-before? TITANIC (make-date 2000 10 1)) #false) ;Checks if TITANIC opens before the given date -> False
(check-expect (opens-before? TITANIC (make-date 2000 10 5)) #true) ;Checks if TITANIC opens before the given date -> True
(check-expect (opens-before? TITANIC (make-date 2000 11 5)) #true) ;Checks if TITANIC opens before the given date -> True
(check-expect (opens-before? TITANIC (make-date 2000 10 4)) #false) ;Checks if TITANIC opens before the given date -> False
(check-expect (opens-before? WALL (make-date 2009 9 2)) #true) ;Checks if TITANIC opens before the given date -> True
(check-expect (opens-before? WALL (make-date 2008 10 4)) #true) ;Checks if TITANIC opens before the given date -> True

    