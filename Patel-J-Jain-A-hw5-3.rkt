;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Patel-J-Jain-A-hw5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; Jai Patel
;; Adish Jain
;; jcpatel@wpi.edu
;; ajain5@wpi.edu

;; 1.

(define-struct river (name ph do tributaries))
;; a river is a (make-river String Number Number ListOfRivers)
;; interp: represents a river where
;; name is the name of the river -> String
;; pH is the pH of the water -> Number
;; do is the dissolved oxygen in mg/L of the river -> Number
;; ListOfTributaries is the list of rivers that feed into the river -> ListOfTributaries

;; a ListOfRivers is one of
;;    empty
;;    (cons River ListOfRivers)


;; 2.


(define RIVERS2LOL
   (make-river "Susan" 5.3 23
               (list (make-river "Joe" 5.2 0.8 empty)
                     (make-river "Helen" 9.3 17
                                  (list (make-river "Beth" 4.2 3.2 empty)
                                        (make-river "Sam" 6.6 4.8 empty)))
                     (make-river "Ricky" 1.7 4.0 empty))))

(define RIVERS3LOL
   (make-river "Susan" 6.7 23
               (list (make-river "Joe" 6.8 9 empty)
                     (make-river "Helen" 6.9 17
                                  (list (make-river "Beth" 7.0 89 empty)
                                        (make-river "Sam" 7.1 67 empty)))
                     (make-river "Ricky" 7.2 65 empty))))

(define RIVERS4LOL
   (make-river "Susan" 6.7 23
               (list (make-river "Joe" 6.8 9 empty)
                     (make-river "Helen" 6.9 17
                                  (list (make-river "Beth" 7.0 89 empty)
                                        (make-river "Sam" 7.1 67 empty)))
                     (make-river "Ricky" 7.2 5 empty))))

(define RIVERSLOL
  (make-river "Susan" 7.2 23
               (list (make-river "Joe" 5.2 0.8 empty)
                     (make-river "Helen" 9.4 17
                                  (list (make-river "Beth" 7.9 3.2 empty)
                                        (make-river "Sam" 6.6 4.8 empty)))
                     (make-river "Ricky" 9.2 4.0 empty))))

(define RIVERS5LOL
  (make-river "Susan" 7 23
               (list (make-river "Joe" 5 4 empty)
                     (make-river "Helen" 9 17
                                  (list (make-river "Beth" 7 3 empty)
                                        (make-river "Sam" 6 4 empty)))
                     (make-river "Ricky" 9 4 empty))))

(define RIVERSLOLTEST1
  (make-river "Susan" 6.7 23
               (list (make-river "Joe" 4.7 4 empty)
                     (make-river "Helen" 8.7 17
                                  (list (make-river "Beth" 6.7 3 empty)
                                        (make-river "Sam" 5.7 4 empty)))
                     (make-river "Ricky" 8.7 4 empty))))

(define RIVERS2LOLTEST2
  (make-river "Susan" 5 23
               (list (make-river "Joe" 4.9 0.8 empty)
                     (make-river "Helen" 9.0 17
                                  (list (make-river "Beth" 3.9 3.2 empty)
                                        (make-river "Sam" 6.3 4.8 empty)))
                     (make-river "Ricky" 1.4 4.0 empty))))

(define RIVERS3LOLTEST
   (make-river "Susan" 6.4 23
               (list (make-river "Joe" 6.5 9 empty)
                     (make-river "Helen" 6.6 17
                                  (list (make-river "Beth" 6.7 89 empty)
                                        (make-river "Sam" 6.8 67 empty)))
                     (make-river "Ricky" 6.9 65 empty))))


;; 3.

;; river-fcn: River ->
;;
;; (define (river-fcn a-river)
;;    (... (river-name a-river)
;;         (river-ph a-river)
;          (river-do a-river)
;          (lor-fcn (river-tributaries a-river))))


;; lor-fcn: ListOfRiver ->
;;
;; (define (lor-fcn alor)
;    (cond [(empty? alor) (...)]
;          [(cons? alor) (... (river-fcn (first alos))
;                             (lor-fcn (rest alos)))]))


;; 4.

;; Signature: list-acidic-rivers: River -> ListOfString
;; Purpose: Consumes a river system and produces a list of string that consists
;; of the names of the rivers in the system with a pH level lower than 6.5.


(define (list-acidic-rivers a-river)
  (if (valid a-river)
      (cons (river-name a-river) (fn-for-ListOfRiver (river-tributaries a-river)))
       (fn-for-ListOfRiver (river-tributaries a-river))))


;; Signature: fn-for-ListOf-River: ListOfRiver -> ListOfString
;; Purpose: Consumes a ListOfRiver and produces a list of string that consists
;; of the names of the rivers in the system with a pH level lower than 6.5.

(define (fn-for-ListOfRiver a-ListOfRiver)
  (cond [(empty? a-ListOfRiver) empty]
        [(cons?  a-ListOfRiver) (append (list-acidic-rivers (first a-ListOfRiver))
                                      (fn-for-ListOfRiver (rest a-ListOfRiver)))]))


(check-expect (list-acidic-rivers RIVERSLOL) (list "Joe"))
(check-expect (list-acidic-rivers RIVERS2LOL) (list "Susan" "Joe" "Beth" "Ricky"))
(check-expect (list-acidic-rivers RIVERS2LOLTEST2) (list "Susan" "Joe" "Beth" "Sam" "Ricky"))
(check-expect (list-acidic-rivers RIVERS4LOL) empty)


;; Signature: valid: River -> Boolean
;; Consumes a river and produces a boolean true or false.

(define (valid river)
  (if (< (river-ph river) 6.5)
      #true
      #false))

(check-expect (valid (make-river "Joe" 5.2 0.8 empty)) #true)
(check-expect (valid (make-river "Helen" 9.3 17 empty)) #false)


;; 5.

;; Signature: unhealthy: River -> Boolean
;; Purpose: Consumes a river system and produces a boolean true or false if
;; any river in the system has a pH below 6.5 or over 8.5 or a DO under 6ppm.

(define (unhealthy? a-river)
  (if (valid2 a-river)
      #true
       (fn-for-ListOfRiver2 (river-tributaries a-river))))


;; Signature: fn-for-ListOfRiver2: ListOfRiver -> Boolean
;; Purpose: Consumes a ListOfRiver and produces a boolean true or false if any
;; river in the ListOfRiver has a pH below 6.5 or over 8.5 or a DO under 6ppm.

(define (fn-for-ListOfRiver2 a-ListOfRiver)
  (cond [(empty? a-ListOfRiver) #false]
        [(cons?  a-ListOfRiver) (if (unhealthy? (first a-ListOfRiver))
                                    #true
                                      (fn-for-ListOfRiver2 (rest a-ListOfRiver)))]))


;; Signature: valid2: River -> Boolean
;; Consumes a river and produces a boolean true or false.

  (define (valid2 a-river)
    (or (< (river-ph a-river) 6.5)
        (> (river-ph a-river) 8.5)
        (< (river-do a-river) 6)))

(check-expect (valid2 (make-river "Joe" 5.2 0.8 empty)) #true)
(check-expect (valid2 (make-river "Helen" 7.2 0.8 empty)) #true)
(check-expect (valid2 (make-river "Sam" 7.2 8 empty)) #false)

(check-expect (unhealthy? RIVERSLOL) #true)
(check-expect (unhealthy? RIVERS2LOL) #true)
(check-expect (unhealthy? RIVERS3LOL) #false)
(check-expect (unhealthy? RIVERS4LOL) #true)

;; 6.

;; Signature: lower-all-ph: River -> River
;; Purpose: Consumes a river system and produces a river system
;; that is the same as the original, except the pH of all the rivers
;; in the system have been lowered by 0.3.


(define (new-river a-ListOfRiver)
  (cond [(empty? a-ListOfRiver) empty]
        [(cons?  a-ListOfRiver) (cons (lower-all-ph (first a-ListOfRiver))
                                      (new-river (rest a-ListOfRiver)))]))

(define (lower-all-ph a-river)
  (make-river
   (river-name a-river)
   (- (river-ph a-river) 0.3)
   (river-do a-river)
   (new-river (river-tributaries a-river))))


(check-expect (lower-all-ph RIVERS5LOL) RIVERSLOLTEST1)
(check-expect (lower-all-ph RIVERS2LOL) RIVERS2LOLTEST2)
(check-expect (lower-all-ph RIVERS3LOL) RIVERS3LOLTEST)
             

;; 7.

;; Signature: find-subsystem: String River -> River or False
;; Purpose: Consumes the name of a river and a river system and produces
;; either a river system or #false.


 (define (find-subsystem user-river a-river)
    (if (equal? (river-name a-river) user-river)
        a-river  
          (find-subsystem-list user-river (river-tributaries a-river))))


;; Signature: find-subsystem-list: String ListOfRiver -> River or False
;; Purpose: Consumes a string and a list of river and produces either a river system
;; or false.


 (define (find-subsystem-list user-river alor)
    (cond [(empty? alor) #false]
          [(cons? alor) (if (river? (find-subsystem user-river (first alor)))
                            (find-subsystem user-river (first alor))
                            (find-subsystem-list user-river (rest alor)))]))


(check-expect (find-subsystem "Susan" RIVERS3LOL) RIVERS3LOL)
(check-expect (find-subsystem "Beth" RIVERS3LOL) (make-river "Beth" 7 89 empty))
(check-expect (find-subsystem "David" RIVERS3LOL) #false)



;; 8.

(define-struct menu-item (name kind vegetarian? quantity price))
;; a MenuItem is a (make-menu-item String String Boolean Natural Number)
;; interp:
;;   MenuItem represents an item for an electronic menu system in a restaurant, where
;;   name is the name of the menu item
;;   kind indicates whether the item is a beverage, entree, appetizer, dessert
;;   vegetarian?  is true if the item is vegetarian
;;   qty is the number of that item that has been ordered
;;   price is the cost of a single item

;; an Order (ListOfMenuItem) is either
;; empty, or
;; (cons MenuItem Order)

(define PASTA (make-menu-item "Pasta" "entree" #true 20 10))
(define SPRITE (make-menu-item "Sprite" "beverage" #true 2 4))
(define COOKIE (make-menu-item "Cookie" "dessert" #true 3 6))
(define APP (make-menu-item "App" "appetizer" #false 4 20))
(define GUY (make-menu-item "Guy" "snack" #false 4 0.6))
(define DUDE (make-menu-item "Dude" "appetizer" #false 4 0.69))
(define MAN (make-menu-item "Man" "entree" #false 4 0.420))

(define ORDER (cons PASTA (cons COOKIE (cons SPRITE empty))))
(define ORDER2 (cons SPRITE (cons COOKIE empty)))
(define ORDER3 (cons PASTA (cons SPRITE (cons COOKIE (cons APP empty)))))
(define ORDER4 (cons DUDE (cons MAN (cons COOKIE (cons SPRITE empty)))))
(define ORDER5 (cons GUY empty))
(define ORDER6 (cons APP (cons APP (cons APP (cons APP empty)))))
(define ORDER7 (cons APP (cons APP (cons APP (cons APP (cons APP (cons SPRITE empty)))))))
(define ORDER8 (cons APP (cons APP (cons APP (cons APP (cons APP (cons SPRITE empty)))))))


;; Signature: dollar-menu-items:  ListOfMenu-item -> ListOfString
;; Purpose: consumes a list of menu items and produces a list of the names of all the items with prices of $1 or less


(define (dollar-menu-items lomi)
  (local
    [(define (cheap? menu-item)
           (<= (menu-item-price menu-item) 1))]
   (map menu-item-name (filter cheap? lomi))))


(check-expect (dollar-menu-items ORDER2) empty) 
(check-expect (dollar-menu-items ORDER4) (list "Dude" "Man"))
(check-expect (dollar-menu-items ORDER5) (list "Guy")) 

  
;; 9.

; all-same-kind?:  ListOfMenu-item String -> Boolean
; consumes a ListOfMenu-items and a kind of food and produces true if every item is of that kind

(define (all-same-kind? lomi user-kind)
  (local
    [(define (same? menu-item)
           (equal? (menu-item-kind menu-item) user-kind))]
   (andmap same? lomi)))



(check-expect (all-same-kind? ORDER5 "appetizer") #false)
(check-expect (all-same-kind? ORDER2 "dessert") #false)
(check-expect (all-same-kind? ORDER3 "dessert") #false)
(check-expect (all-same-kind? ORDER6 "appetizer") #true)
(check-expect (all-same-kind? ORDER7 "appetizer") #false)

      

;; 10.

;; list-expensive-vegetarian:  ListOfMenu-item Number -> ListOfMenu-item
;; Consumes a list of menu items and a given amount and returns a list of those vegetarian items that exceed the given amount


(define (list-expensive-vegetarian lomi user-cost)
  (local
    [(define (valid? menu-item)
           (and (equal? (menu-item-vegetarian? menu-item) #true)
       (> (menu-item-price menu-item) user-cost)))]
   (filter valid? lomi))) 


(check-expect (list-expensive-vegetarian ORDER 2) (list PASTA COOKIE SPRITE))
(check-expect (list-expensive-vegetarian ORDER4 1) (list COOKIE SPRITE))
(check-expect (list-expensive-vegetarian ORDER4 0.2) (list COOKIE SPRITE))
(check-expect (list-expensive-vegetarian ORDER7 42) empty)


