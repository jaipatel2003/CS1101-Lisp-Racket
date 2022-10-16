;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Patel-J-Jain-A-hw4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; Jai Patel
;; Adish Jain
;; jcpatel@wpi.edu
;; ajain5@wpi.edu


;; 1.

(define-struct student (name email))
;; a Student is a (make-student String String)
;;  interp:  represents a Student along with his/her descendants
;;    name -> String
;;    email -> String

(define DAVE (make-student "Dave" "dave@wpi.edu"))
(define JAI (make-student "Jai" "jcpatel@wpi.edu"))

;; A ListOfStudent is one of
;; empty
;; (cons Student ListOfStudent) 

(define LIST1 (list (make-student "Dave" "dave@wpi.edu")
                    (make-student "Jai" "jcpatel@wpi.edu" )))

(define LIST2 (list (make-student "Man" "man@wpi.edu")
                    (make-student "Woman" "woman@wpi.edu")))

(define LIST3 (list (make-student "f" "dfv@wpi.edu")
                    (make-student "d" "sdc@wpi.edu")))

(define LIST4 (list (make-student "w" "swq@wpi.edu")
                    (make-student "q" "pol@wpi.edu")))

(define LIST5 (list (make-student "h" "the@wpi.edu")
                    (make-student "r" "bat@wpi.edu")))



(define-struct coursenode (course-id title instructor students left right))
;; a CourseNode is a
;; (make-coursenode Number String String ListOfStudent BST BST)
;;   interp:
;;   course-id is the chosen number
;;   title is the course title that represents the number
;;   instructor is the instructor of the chosen course number
;;   students is the list of students enrolled in the course
;;   left is the left subtree (a BST)
;;   right is the right subtree (a BST)


;; a BST is one of
;;   false
;;   CourseNode: (make-coursenode Number String String ListOfStudent BST BST)
;;   interp:
;;   course-id is the chosen number
;;   title is the course title that represents the number
;;   instructor is the instructor of the chosen course number
;;   students is the list of students enrolled in the course
;;   left is the left subtree (a BST)
;;   right is the right subtree (a BST)


;;INVARIANT:
;;   course-id > all course-ids in left subtree
;;   course-id < all course-ids in right subtree
;;All course-ids are unique, i.e. no course-id appears twice in a BST



;; 2.

(define Y (make-coursenode 92.202 "the" "apple" LIST1 #false #false))

(define X (make-coursenode 92.202 "the" "apple" LIST1
                           (make-coursenode 20.392 "the" "bryuh" LIST2 #false #false)
                           (make-coursenode 104.204 "dj" "sk" LIST3 #false
                                            (make-coursenode 110.111 "d" "ph" LIST4 #false
                                                             (make-coursenode 172.222 "s" "dp" LIST5 #false #false)))))
(define Z (make-coursenode 75.222 "Course" "teacher" LIST1
                           (make-coursenode 42.203 "ds" "dd" LIST2 #false #false)
                           (make-coursenode 999.999 "djss" "ddd" LIST3 #false #false)))



;; 3. 

;; student-fcn: Student ->
;;
;; (define (student-fcn a-student)
;;    (... (student-name a-student)
;;         (student-email a-student)))


;; los-fcn: ListOfStudent ->
;;
;; (define (los-fcn alos)
;    (cond [(empty? alos) (...)]
;          [(cons? alos) (... (student-fcn (first alos))
;                             (los-fcn (rest alos)))]))


;;(define (fcn-for-BinaryTree a-tree)
;    (cond [(boolean? a-tree) (...)];base case
;          [(coursenode?  a-tree) (... (coursenode-course-id a-tree);Number
;                                  (coursenode-title a-tree);String
;                                  (coursenode-instructor a-tree);String
;                                  (coursenode-students a-tree);ListOfStudent
;                                  (fcn-for BinaryTree (coursenode-left a-tree));BST
;                                  (fcn-for BinaryTree (coursenode-right a-tree)))]));BST


;; 4.

;; Signature: any-taught-by?: BST String -> Boolean
;; Purpose: Consumes a binary search tree and the name of an instructor,
;; and produces a boolean true if any of the courses in the course database are taught by the given instructor. 

;; *Check parameter orders and make sure function is included in signature

 (define (any-taught-by? a-tree name)
    (cond [(boolean? a-tree) #false]
          [(coursenode?  a-tree) (if (equal? (coursenode-instructor a-tree) name)
                                     #true
                                  (or (any-taught-by? (coursenode-left a-tree) name)(any-taught-by? (coursenode-right a-tree) name )))]))


(check-expect (any-taught-by? X "apple") #true)
(check-expect (any-taught-by? X "bryuh") #true)
(check-expect (any-taught-by? X "ff") #false)
(check-expect (any-taught-by? X "sk") #true)
(check-expect (any-taught-by? #false "Ss") #false)



;; 5.

;; Signature: drop-student: BST Number String -> BST
;; Purpose: Consumes a binary search tree, a course number, and the email address of a student, and produces a binary search tree that
;; removes the student with the given email address from the list of students enrolled in the given course.

;; *Check parameter orders and make sure function is included in signature

(define (drop-student a-tree coursenum student-addy)
           (cond
             [(= coursenum (coursenode-course-id a-tree))
                                    (make-coursenode
                                     (coursenode-course-id a-tree)
                                     (coursenode-title a-tree)
                                     (coursenode-instructor a-tree)
                                     (remove-student (coursenode-students a-tree) student-addy)
                                     (coursenode-left a-tree)
                                     (coursenode-right a-tree)
                                     )]
                                     [(< coursenum (coursenode-course-id a-tree))
                                    (make-coursenode
                                     (coursenode-course-id a-tree)
                                     (coursenode-title a-tree)
                                     (coursenode-instructor a-tree)
                                     (coursenode-students a-tree)
                                     (drop-student (coursenode-left a-tree) coursenum student-addy)
                                     (coursenode-right a-tree))]
                                     [(> coursenum (coursenode-course-id a-tree))
                                    (make-coursenode
                                     (coursenode-course-id a-tree)
                                     (coursenode-title a-tree)
                                     (coursenode-instructor a-tree)
                                     (coursenode-students a-tree)
                                     (coursenode-left a-tree)
                                     (drop-student (coursenode-right a-tree) coursenum student-addy))])) 
                                    


(define TEST (make-coursenode 92.202 "the" "apple" LIST1
                           (make-coursenode 20.392 "the" "bryuh" (list (make-student "Woman" "woman@wpi.edu")) #false #false)
                           (make-coursenode 104.204 "dj" "sk" LIST3 #false
                                             (make-coursenode 110.111 "d" "ph" LIST4 #false
                                                             (make-coursenode 172.222 "s" "dp" LIST5 #false #false))))) 


(define TEST2 (make-coursenode 92.202 "the" "apple" (list (make-student "Dave" "dave@wpi.edu"))
                           (make-coursenode 20.392 "the" "bryuh" LIST2 #false #false)
                           (make-coursenode 104.204 "dj" "sk" LIST3 #false
                                            (make-coursenode 110.111 "d" "ph" LIST4 #false
                                                             (make-coursenode 172.222 "s" "dp" LIST5 #false #false)))))

(define TEST3 (make-coursenode 92.202 "the" "apple" LIST1
                           (make-coursenode 20.392 "the" "bryuh" LIST2 #false #false)
                           (make-coursenode 104.204 "dj" "sk" LIST3 #false
                                            (make-coursenode 110.111 "d" "ph" LIST4 #false
                                                             (make-coursenode 172.222 "s" "dp" (list (make-student "r" "bat@wpi.edu")) #false #false)))))



(check-expect (drop-student X 20.392 "man@wpi.edu") TEST)
(check-expect (drop-student X 92.202 "jcpatel@wpi.edu") TEST2) 
(check-expect (drop-student X 172.222 "the@wpi.edu") TEST3)



;; Signature: remove-student: ListOfStudent String -> ListOfStudent
;; Purpose: Consumes a list of students and an email and produces a list of students
;; with the student whose email address was provided removed.

;; *Check parameter orders and make sure function is included in signature

(define (remove-student los email)
  (cond
    [(empty? los) empty]
    [(cons? los) (if (get-email (first los) email)
                     (remove-student (rest los) email)
                     (cons (first los) (remove-student (rest los) email)))]))

(check-expect (remove-student LIST1 "jcpatel@wpi.edu") (list (make-student "Dave" "dave@wpi.edu")))
(check-expect (remove-student LIST2 "man@wpi.edu") (list (make-student "Woman" "woman@wpi.edu")))



;; Signature: get-email: Student String -> Boolean
;; Purpose: Consumes a student and an email and produces a boolean true or false
;; if the given student has the same email as the email provided.

;; *Check parameter orders and make sure function is included in signature

 (define (get-email a-student email) 
   (if(equal? (student-email a-student) email)
       #true
       #false))
       
(check-expect (get-email JAI "jcpatel@wpi.edu") #true)
(check-expect (get-email JAI "ndnd") #false)


;; 6.

;; Signature: list-titles-in-order-by-coursenum: BST -> ListOfString
;; Purpose: Consumes a binary search tree and produces a list of the titles
;; of the courses, sorted in order by ascending course number.

;; *Check parameter orders and make sure function is included in signature

(define (list-titles-in-order-by-coursenum a-tree)
  (cond [(boolean? a-tree) empty]
        [(coursenode?  a-tree) (append (list-titles-in-order-by-coursenum (coursenode-left a-tree))
                                       (list (coursenode-title a-tree))
                                       (list-titles-in-order-by-coursenum (coursenode-right a-tree)))]))
                               


(check-expect (list-titles-in-order-by-coursenum X) (list "the" "the" "dj" "d" "s"))
(check-expect (list-titles-in-order-by-coursenum Y) (list "the"))
(check-expect (list-titles-in-order-by-coursenum Z) (list "ds" "Course" "djss"))



;; 7.

;; Signature: add-course: BST Number String String -> BST
;; Purpose: Consumes a binary search tree, a course number, a course title,
;; name of the instructor, and creates a new binary search tree that is the same
;; as the original except that a new course with the given information has been added to
;; the tree.

;; *Check parameter orders and make sure function is included in signature


(define (add-course a-tree coursenum titlet nombre)
             (cond
               [(boolean? a-tree) (make-coursenode coursenum titlet nombre empty #false #false)]
               [(coursenode? a-tree)
                (cond [(< coursenum (coursenode-course-id a-tree))
                                    (make-coursenode
                                     (coursenode-course-id a-tree)
                                     (coursenode-title a-tree)
                                     (coursenode-instructor a-tree)
                                     (coursenode-students a-tree)
                                     (add-course (coursenode-left a-tree) coursenum titlet nombre)
                                     (coursenode-right a-tree))]
                [(> coursenum (coursenode-course-id a-tree))
                                    (make-coursenode
                                     (coursenode-course-id a-tree)
                                     (coursenode-title a-tree)
                                     (coursenode-instructor a-tree)
                                     (coursenode-students a-tree)
                                     (coursenode-left a-tree)
                                     (add-course (coursenode-right a-tree) coursenum titlet nombre))])]))


              

(check-expect (add-course X 103.727 "Math" "Man") (make-coursenode 92.202 "the" "apple" LIST1
                                                                   (make-coursenode 20.392 "the" "bryuh" LIST2 #false #false)
                                                                   (make-coursenode 104.204 "dj" "sk" LIST3
                                                                                    (make-coursenode 103.727 "Math" "Man" empty #false #false)
                                                                                    (make-coursenode 110.111 "d" "ph" LIST4 #false
                                                                                                     (make-coursenode 172.222 "s" "dp" LIST5 #false #false)))))

(check-expect (add-course X 109.482 "Science" "Woman") (make-coursenode 92.202 "the" "apple" LIST1
                                                                        (make-coursenode 20.392 "the" "bryuh" LIST2 #false #false)
                                                                        (make-coursenode 104.204 "dj" "sk" LIST3 #false
                                                                                         (make-coursenode 110.111 "d" "ph" LIST4
                                                                                                          (make-coursenode 109.482 "Science" "Woman" empty #false #false)
                                                                                                          (make-coursenode 172.222 "s" "dp" LIST5 #false #false)))))

(check-expect (add-course X 189.229 "History" "Dude") (make-coursenode 92.202 "the" "apple" LIST1
                                                                       (make-coursenode 20.392 "the" "bryuh" LIST2 #false #false)
                                                                       (make-coursenode 104.204 "dj" "sk" LIST3 #false
                                                                                        (make-coursenode 110.111 "d" "ph" LIST4 #false
                                                                                                         (make-coursenode 172.222 "s" "dp" LIST5 #false (make-coursenode 189.229 "History" "Dude" empty #false #false))))))