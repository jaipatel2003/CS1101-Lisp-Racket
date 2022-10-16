;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Patel-J-Jain-A-hw3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Jai Patel
;; Adish Jain
;; jcpatel@wpi.edu
;; ajain5@wpi.edu


;; 1.

(define-struct Menu-item (name kind vegetarian? quantity cost))
;; a Menu-item is a (make-Menu-item String String Boolean Natural Number)
;;  interp:  represents an item of a menu
;;    name is the name of the item: String
;;    kind is the kind of item: String
;;    vegetarian? is whether or not the item is vegetarian: Boolean
;;    quantity is the number of items ordered: Natural
;;    cost is the cost of a single item: Number


(define PASTA (make-Menu-item "Pasta" "entree" #true 20 10))
(define SPRITE (make-Menu-item "Sprite" "beverage" #true 2 4))
(define COOKIE (make-Menu-item "Cookie" "dessert" #true 3 4))
(define APP (make-Menu-item "App" "appetizer" #false 4 20))


;; 2.

;; make-Menu-item: String String Boolean Natural Natural -> Menu-item

; ;; Menu-item-fcn:  Menu-item -> ...
; ;; ...
; (define (Menu-item-fcn a-Menu-item)
;   (...  (Menu-item-name a-Menu-item) ;; Natural
;         (Menu-item-kind a-Menu-item) ;; String
;         (Menu-item-vegetarian? a-Menu-item) ;; Boolean
;         (Menu-item-quantity a-Menu-item) ;; Natural
;         (Menu-item-cost a-Menu-item)))   ;; Number


;; 3.

;; an Order is one of
;;    empty
;;    (cons Menu-item ListOfMenu-item)

(define ORDER (cons PASTA (cons COOKIE (cons SPRITE empty))))
(define ORDER2 (cons SPRITE (cons COOKIE empty)))
(define ORDER3 (cons PASTA (cons SPRITE (cons COOKIE (cons APP empty)))))


;; 4.

; (define (order-fcn alomi)
;   (cond [(empty? alomi) (OBVIOUS ANSWER)
;         [(cons? alomi)  (... (first alomi)
;                             (order-fcn (rest alomi)))]))



;; 5.

;;Signature: count-appetizers: ListOfMenu-item -> Natural
;;Purpose: Consumes an order and returns the number of items in the order that are appetizers.

(define (count-appetizers alomi)
  (cond [(empty? alomi) 0]
        [(cons? alomi) (if (appetizer? (first alomi)) (+ 1 (count-appetizers (rest alomi)))
                            (count-appetizers (rest alomi)))]))

(check-expect (count-appetizers empty) 0)
(check-expect (count-appetizers (cons PASTA empty)) 0)
(check-expect (count-appetizers (cons SPRITE empty)) 0)
(check-expect (count-appetizers (cons APP empty)) 1)
(check-expect (count-appetizers (cons (make-Menu-item "Cheese" "appetizer" #true 20 3) (cons APP empty))) 2)
(check-expect (count-appetizers (cons COOKIE (cons PASTA (cons APP empty)))) 1)
(check-expect (count-appetizers (cons APP (cons APP (cons APP (cons APP (cons APP (cons APP (cons APP empty)))))))) 7)
(check-expect (count-appetizers ORDER3) 1)



;; Signature: appetizer?: Menu-item -> Boolean
;; Purpose: Consumes a menu item and produces a boolean true or false if the menu item
;; is an appetizer or not.

(define (appetizer? Menu-item)
  (if (equal? (Menu-item-kind Menu-item) "appetizer")
      #true
      #false))

(check-expect (appetizer? APP) #true)
(check-expect (appetizer? (make-Menu-item "Coke" "beverage" #true 3 4)) #false)


                        
;; 6.

;; Signature: list-expensive-vegetarian: ListOfMenu-item Number -> ListOfMenu-item
;; Purpose:  Consumes an order and a number and produces an order that contains only those items from
;; the original order that are vegetarian and cost more than an amount given.

 (define (list-expensive-vegetarian user-cost alomi)
   (cond [(empty? alomi) empty]
         [(cons? alomi)  (if (valid (first alomi) user-cost)
                         (cons (first alomi) (list-expensive-vegetarian user-cost (rest alomi)))
                                     (list-expensive-vegetarian user-cost (rest alomi)))]))

(check-expect (list-expensive-vegetarian 2 (cons APP (cons COOKIE (cons PASTA empty)))) (cons COOKIE (cons PASTA empty)))
(check-expect (list-expensive-vegetarian 20 (cons APP (cons COOKIE (cons PASTA empty)))) empty)
(check-expect (list-expensive-vegetarian 5 (cons APP (cons PASTA (cons COOKIE empty)))) (cons PASTA empty))
(check-expect (list-expensive-vegetarian 1 (cons COOKIE (cons PASTA (cons SPRITE empty)))) (cons COOKIE (cons PASTA (cons SPRITE empty))))
(check-expect (list-expensive-vegetarian 5 ORDER3) (cons PASTA empty))
(check-expect (list-expensive-vegetarian 10 empty ) empty)
(check-expect (list-expensive-vegetarian 2 (cons COOKIE empty)) (cons COOKIE empty))



;; Signature: valid: Menu-item Number -> Boolean
;; Purpose: Consumes a menu item and a number and produces a boolean true if
;; the menu item is both vegetarian and costs more than the amount given, else false.

(define (valid Menu-item user-cost)
  (and (equal? (Menu-item-vegetarian? Menu-item) #true)
       (> (Menu-item-cost Menu-item) user-cost)))
  
(check-expect (valid SPRITE 10) #false)
(check-expect (valid SPRITE 0) #true)
(check-expect (valid APP 0) #false)
(check-expect (valid APP 25) #false)



;; 7.

;; Signature: order-total: ListOfMenu-item -> Number
;; Purpose: Consumes an order and produces the total cost of the order.

(define (order-total alomi)
  (cond [(empty? alomi) 0]
        [(cons? alomi) (+ (cost (first alomi)) (order-total (rest alomi)))]))

(check-expect (order-total (cons APP (cons COOKIE (cons PASTA empty)))) 292)
(check-expect (order-total ORDER) 220)
(check-expect (order-total ORDER2) 20)
(check-expect (order-total ORDER3) 300)
(check-expect (order-total empty) 0)



;; Signature: cost: Menu-item -> Number
;; Purpose: Consumes an item and produces the product of the item's quantity and cost.

(define (cost Menu-item)
  (* (Menu-item-quantity Menu-item) (Menu-item-cost Menu-item)))

(check-expect (cost PASTA) 200)
(check-expect (cost SPRITE) 8)
(check-expect (cost COOKIE) 12)



;8

;; Signature: beverage-total: ListOfMenu-item -> Number
;; Purpose: Consumes an order and produces a number that is the total cost of all
;; beverages in the order.

(define (beverage-total alomi)
  (cond [(empty? alomi) 0]
        [(cons? alomi) (if (beverage? (first alomi))
                           (+ (bevcost (first alomi)) (beverage-total (rest alomi)))
                           (beverage-total (rest alomi)))]))

(check-expect (beverage-total empty) 0)
(check-expect (beverage-total (cons SPRITE empty)) 8)
(check-expect (beverage-total (cons SPRITE (cons (make-Menu-item "Coke" "beverage" #true 20 400) (cons APP (cons COOKIE empty))))) 8008)
(check-expect (beverage-total (cons SPRITE (cons SPRITE (cons SPRITE (cons SPRITE empty))))) 32)
(check-expect (beverage-total (cons PASTA empty)) 0)
(check-expect (beverage-total (cons PASTA (cons COOKIE (cons SPRITE empty)))) 8)
(check-expect (beverage-total ORDER) 8)
(check-expect (beverage-total ORDER2) 8)
(check-expect (beverage-total ORDER3) 8)



;; Signature: beverage?: Menu-item -> Boolean
;; Purpose: Consumes a menu item and produces a boolean true or false
;; dependent on whether or not the item is a beverage.

(define (beverage? Menu-item)
  (if (equal? (Menu-item-kind Menu-item) "beverage")
      #true
      #false))

(check-expect (beverage? SPRITE) #true)
(check-expect (beverage? PASTA) #false)
(check-expect (beverage? (make-Menu-item "Coke" "beverage" #true 20 3)) #true)
(check-expect (beverage? (make-Menu-item "Squash" "entree" #true 20 3)) #false)



;; Signature: bevcost: Menu-item -> Number
;; Purpose: Consumes a beverage and produces the cost of the beverage
;; by taking the product of its quantity and price.

(define (bevcost Menu-item)
  (* (Menu-item-quantity Menu-item) (Menu-item-cost Menu-item)))

(check-expect (bevcost SPRITE) 8)
(check-expect (bevcost (make-Menu-item "Coke" "beverage" #true 20 3)) 60)



;; 9.

;; Signature: cost-with-tip: ListOfMenu-Item Number -> Number
;; Purpose: Consumes an order and a number representing the percent of the tip
;; and produces the total cost of the order with the tip.

(define (cost-with-tip alomi num)
  (cond [(empty? alomi)0]
        [(cons? alomi) (cond [(notbev? (first alomi))
                           (+ (costtip (first alomi) num) (cost-with-tip (rest alomi) num)
                           )]
                       [(beverage? (first alomi))
                           (+ (beverage-cost (first alomi))(cost-with-tip (rest alomi) num))
                           ])]))

(check-expect (cost-with-tip ORDER 0.2) 262.4)
(check-expect (cost-with-tip ORDER2 0.2) 22.4)
(check-expect (cost-with-tip ORDER3 0.2) 358.4)
(check-expect (cost-with-tip empty 0.5) 0)
(check-expect (cost-with-tip (cons PASTA empty) 0.2) 240)
(check-expect (cost-with-tip (cons PASTA (cons SPRITE empty)) 0.2) 248)
(check-expect (cost-with-tip (cons PASTA(cons PASTA (cons PASTA empty))) 0.2) 720)
(check-expect (cost-with-tip (cons SPRITE empty) 0.3) 8)



;; Signature: notbev?: Menu-Item -> Boolean
;; Purpose: Consumes a menu item and produces a boolean true if the item is not a beverage
;; and false if the item is a beverage.

(define (notbev? Menu-item)
  (if (or (equal? (Menu-item-kind Menu-item) "appetizer") (equal? (Menu-item-kind Menu-item) "dessert") (equal? (Menu-item-kind Menu-item) "entree"))
      #true
      #false))

(check-expect (notbev? PASTA) #true)
(check-expect (notbev? SPRITE) #false)
(check-expect (notbev? (make-Menu-item "Fanta" "beverage" #false 304 20)) #false)
(check-expect (notbev? (make-Menu-item "yum" "appetizer" #false 304 20)) #true)



;; Signature: costtip: Menu-item Number -> Number
;; Purpose: Consumes a menu item and a number representing the tip and produces
;; the total cost including the tip.

(define (costtip Menu-item num)
  (+ (* (Menu-item-quantity Menu-item) num (Menu-item-cost Menu-item)) (* (Menu-item-quantity Menu-item) (Menu-item-cost Menu-item))))

(check-expect (costtip COOKIE 0.2) 14.4)
(check-expect (costtip APP 0.2) 96)
(check-expect (costtip APP 0) 80)



;; Signature: beverage-cost: Menu-item -> Number
;; Purpose: Consumes a beverage and produces the total cost of the beverage
;; by taking the product of its quantity and cost.

(define (beverage-cost Menu-item)
  (* (Menu-item-cost Menu-item ) (Menu-item-quantity Menu-item)))

(check-expect (beverage-cost SPRITE) 8)
(check-expect (beverage-cost (make-Menu-item "Coke" "beverage" #true 111 111)) 12321)


