;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Patel-J-Jain-A-hw6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #t)))
;; Jai Patel
;; Adish Jain
;; jcpatel@wpi.edu
;; ajain5@wpi.edu

;; 1.

(define-struct message (username text read?))
;; a message is a (make-message String String Boolean)
;; interp: represents a message where
;; username is the username of the sender -> String
;; text is the message -> String
;; read? is whether or not the recipient has read the text -> Boolean


(define MESSAGE1 (make-message "jcp" "hey" #false))
(define MESSAGE2 (make-message "cdp" "hi" #true))
(define MESSAGE3 (make-message "tcp" "hello" #false))


(define-struct user (username mailbox))
;; a user is a (make-user String ListOfMessage)
;; interp: represents a user where
;; username is the username of the sender -> String
;; mailbox is a ListOfMessage of the texts in a users mailbox -> ListOfMessage


(define USER1 (make-user "jcp" (list MESSAGE1 MESSAGE2)))
(define USER2 (make-user "cdp" (list MESSAGE2 MESSAGE3)))
(define MAILSYSTEM (make-user "sks" (list MESSAGE1 MESSAGE3)))


;; An Email System is one of
;; empty
;; (cons User ListofUsers)


;; 2.

;; mailsys: an empty email system (with no users)
(define mailsys empty)

;; newuser: a user with name "Newuser" and an empty list of messages
(define newuser (make-user "Newuser" empty))


;; 3.

;; Signature: add-user: String -> Void
;; Purpose: Consumes a username and produces void
;; Effect: Add a new user with the given username to the mail system

(define (add-user username)
  (set! mailsys (cons (make-user username empty) mailsys)))

(add-user "Jai")
(add-user "Day")
(add-user "Steve")
(add-user "Pol")
(add-user "Guy")


;; 4.

;; Signature: find-user: String ListOfUser -> User
;; Purpose: Consumes a username and a ListOfUser and produces the user
;; with the given username.

(define (find-user name lou)
  (cond [(empty? lou) false] 
        [(cons? lou) (if (equal? name (user-username (first lou)))
                         (first lou)
                         (find-user name (rest lou)))]))



(check-expect (find-user "Adi" empty) false)
(check-expect (find-user "Jai" mailsys) (make-user "Jai" empty))
(check-expect (find-user "Day" mailsys) (make-user "Day" empty))


;; Signature: send-email: String String String -> Void
;; Purpose: Consumes the name of the sender of an email, the name of the
;; recipient of the email, and the text of an email message, and produces
;; void.
;; Effect: Store a new unread message in the recipient's mailbox


(define (send-email name name-recipient text)
  (local
    [(define (add-message lou)
      (set-user-mailbox! (find-user name-recipient lou)
                         (cons (make-message name text false)
                               (user-mailbox (find-user name-recipient lou)))))]
    (add-message mailsys)))


(send-email "s" "Steve" "sss")
(send-email "s" "Pol" "sss")
(send-email "s" "Guy" "ssssss")


;; 5.


;; Signature: get-unread-messages: String -> ListOfMessages
;; Purpose: Consumes a username and produces a list of messages that contains
;; the unread messages in the mailbox of the user with the given name.
;; Effect: All unread messages in the named user's mailbox have been set to read.

(define (get-unread-messages username)
  (find-user-mailbox (user-mailbox (find-user username mailsys))))

;; Signature: find-user-mailbox: ListOfMessage -> ListOfMessage
;; Purpose: Consumes a ListOfMessage and produces a ListOfMessage equal to the input
;; however all messages have been set to read.

(define (find-user-mailbox lom)
  (cond
    [(empty? lom) lom]
    [(cons? lom)
     (if (message-read? (first lom))
         (cons (first lom) (find-user-mailbox (rest lom)))
         (begin
           (set-message-read?! (first lom) #true)
           (cons (first lom) (find-user-mailbox (rest lom)))))]))


(check-expect (find-user-mailbox empty) empty)
(check-expect (find-user-mailbox (user-mailbox (find-user "Jai" mailsys))) empty)
(check-expect (find-user-mailbox (user-mailbox (find-user "Steve" mailsys))) (list (make-message "s" "sss" true)))

(get-unread-messages "Guy")

 
; 6.

;; Signature: most-messages: Nothing -> User
;; Purpose: Consumes nothing and produces the user in the mailsystem with the
;; largest number of messages in his/her mailbox.


(define (most-messages)
  (cond
    [(empty? mailsys)"No users in the system"]
    [(cons? mailsys) (acccccc mailsys (first mailsys))]))


;; Signature: acccccc: ListOfUser User -> User
;; Purpose: Consumes a ListOfUser and an accumulator taking into account User
;; which produces the user with the largest number of messages in the mailbox.


(define (acccccc lou acc-acc)
  (cond
    [(empty? lou) acc-acc]
    [(cons? lou) (if (> (length (user-mailbox (first lou))) (length (user-mailbox acc-acc)))
                     (acccccc (rest lou) (first lou))
                     (acccccc (rest lou) acc-acc))]))

 

;; 7.

;(add-user "Jai")
;(add-user "Day")
;(send-email "s" "Jai" "sss")
;(send-email "s" "Day" "sss")
;(get-unread-messages "Jai")


;; 8.

;; Signature: total-string-length: ListOfString -> Natural
;; Purpose: Consumes a ListOfString and produces the sum of the lengths
;; of the strings in the list.

(define (total-string-length los)
  (local
    [(define (sum-up los accumulator)
       (if (empty? los)
           accumulator
           (sum-up (rest los) (+ accumulator (string-length (first los))))))]
    (sum-up los 0)))  

(check-expect (total-string-length (list "djdjs" "sjsjj" "a")) 11)
(check-expect (total-string-length (list "dd" "ddd" "dddd")) 9)
(check-expect (total-string-length (list "a" "aa" "aaa")) 6)
(check-expect (total-string-length (list "sssss" "ssss" "sss")) 12) 

;; 9.

;; Signature: one-giant-string: ListOfString -> String
;; Purpose: Consumes a ListOfString and produces the concatenation of
;; strings in the list in the order they appear in the list.


(define (one-giant-string alos)
  (append-words alos ""))


(define (append-words alos one-giant-string)
  (if (empty? alos)
            one-giant-string
            (append-words (rest alos) (string-append one-giant-string (first alos)))))


(check-expect (one-giant-string (list "Alice" "Bob")) "AliceBob")
(check-expect (one-giant-string (list "Hi" "Alice" "Bob")) "HiAliceBob")
(check-expect (one-giant-string empty) "")  