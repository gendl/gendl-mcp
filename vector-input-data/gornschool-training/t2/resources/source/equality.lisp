   
(in-package :gdl-user)

(define-object equality (base-object)

  :input-slots
  (
   )
  
  :computed-slots
  ((eq-1 (eq 1 1))                                 ;;; T
   (eq-2 (eq \#c \#c))                             ;;; T
   (eq-3 (eq 1 1.0))                               ;;; NIL
   (eq-4 (eq 1.0 1.0))                             ;;; NIL
   (eq-4 (eq "a" "a"))                             ;;; NIL
   (eq-6 (eq "A" "a"))                             ;;; NIL
   (eq-7 (eq (list 1 "a") (list 1 "a")))           ;;; NIL
   (eq-8 (eq (list 1 "A") (list 1.0 "a")))         ;;; NIL

   (eql-1 (eql 1 1))                               ;;; T
   (eql-2 (eql \#c \#c))                           ;;; T
   (eql-3 (eql 1 1.0))                             ;;; NIL
   (eql-4 (eql 1.0 1.0))                           ;;; T
   (eql-4 (eql "a" "a"))                           ;;; NIL
   (eql-6 (eql "A" "a"))                           ;;; NIL
   (eql-7 (eql (list 1 "a") (list 1 "a")))         ;;; NIL
   (eql-8 (eql (list 1 "A") (list 1.0 "a")))       ;;; NIL

   (equal-1 (equal 1 1))                           ;;; T
   (equal-2 (equal \#c \#c))                       ;;; T
   (equal-3 (equal 1 1.0))                         ;;; T
   (equal-4 (equal 1.0 1.0))                       ;;; T
   (equal-4 (equal "a" "a"))                       ;;; T
   (equal-6 (equal "A" "a"))                       ;;; NIL
   (equal-7 (equal (list 1 "a") (list 1 "a")))     ;;; T
   (equal-8 (equal (list 1 "A") (list 1.0 "a")))   ;;;NIL

   (equalp-1 (equalp 1 1))                         ;;; T
   (equalp-2 (equalp \#c \#c))                     ;;; T
   (equalp-3 (equalp 1 1.0))                       ;;; T
   (equalp-4 (equalp 1.0 1.0))                     ;;; T
   (equalp-4 (equalp "a" "a"))                     ;;; T
   (equalp-6 (equalp "A" "a"))                     ;;; T
   (equalp-7 (equalp (list 1 "a") (list 1 "a")))   ;;; T
   (equalp-8 (equalp (list 1 "A") (list 1.0 "a"))) ;;; T
   
	 
   (odd? (oddp 7))	       ;;; will return T, requires an integer
   (even? (evenp 2))	       ;;; will return T
   (zero? (zerop 0.0))	       ;;; will return T
   (positive? (plusp 190.445)) ;;; will return T
   (negative? (minusp -0.223)) ;;; will return T	 



   ;; Mathematical expressions and boolean operators
   
   ;; Numeric comparison
   (less-than? (< 45 33))                    ;;; will return nil as 45 is not less than 33
   (less-than-or-equal? (<= 12 32))          ;;; will return T
   (equal-to (= 1 1.0))		             ;;; will return T
   (greater? (> 100 5))                      ;;; will return T as 100 is compared to 5
   (greater-or-equal? (>= 1.000 1.0))        ;;; will return T
   (not-equal-1 (/= 1.0 2.0))	             ;;; will return T
   ;; or
   (not-equal-2 (not (= 1.0 2.0)))           ;;; will return T

   (multi-greater-1? (> 4 3 2))              ;;; will return T
   ;; which is equivalent to
   (multi-greater-1? (and (> 4 3) (> 3 2)))  ;;; will return T

   ;; for floats, because of rounding better to use near-to? for rather than =
   (near-to-1 (near-to? 1 1.1)) ;;; will return nil as tolerance is defined parameter *ZERO-EPSILON* (0.001)
   (near-to-2 (near-to? 1 1.1 0.5)) ;;; will return T as last 0.5 is the tolerance

   ;; similarly, rather than zerop, use near-zero?
   (zero-1 (near-zero? 0.0001)) ;;; retuns T as the tolerance is *ZERO-EPSILON* (0.001)
   
   
   
   
   
   (string-equal-1? (string= "text" "text"))       ;;; will return T
   (string-equal-2? (string-equal "text" "text"))  ;;; will return T
  
   (string-equal-3? (string= "text" "TEXT"))       ;;; will return NIL
   (string-equal-4? (string-equal "text" "TEXT"))  ;;; will return T

   (string-equal-5? (string= "text" "test" :start1 0 :end1 2 :start2 0 :end2 2))       ;;; will return T
   (string-equal-5? (string-equal "Text" "test" :start1 0 :end1 2 :start2 0 :end2 2))  ;;; will return T
   )
  )
   

  
   
   
