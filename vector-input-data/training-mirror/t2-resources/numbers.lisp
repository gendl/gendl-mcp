(in-package :gdl-user)

(define-object numbers (base-object)

  :computed-slots
  ((my-number 10)                  ;;; 10
   (my-float-number 10.0)          ;;; 10.0
   (my-float-number-1 10.0001)     ;;; 10.0001
   (my-notation-number-1 5.32E+4)  ;;; 53200.0
   (my-notation-number-2 123d123)  ;;; 1.23E+125
   (my-binary #b1011)              ;;; 11
   (my-complex #c(2 1))            ;;; #C(2 1)
   ;; note that theres no practical limit on the size of a number in lisp
   
   
   ;; mathematical operations
   (add-1 (+ (the my-number) (the my-number)))                                   ;;; 20
   (add-2 (+ (the my-number) (the my-float-number)))                             ;;; 20.0
   (add-3 (+ (the my-number) (the my-float-number) (the my-notation-number-1)))  ;;; 53220.0

   (subtract-1 (- (the my-number) 5))                        ;;; 5
   (subtract-2 (- (the my-number) (the my-notation-number))) ;;; - 53190.0

   (multiply-1 (* (the my-number) 5))                                 ;;; 50
   (multiply-2 (* (my-notation-number-1) (the my-notation-number-2))) ;;; 6.543600000000001E+129

   (ratio-1 (/ (the my-number) 3)) ;;; 10/3
   (ratio-2 (/ (the my-number) 4)) ;;; 5/2
   (ratio-3 (/ (the my-number)))   ;;; 1/10
	       

   (division-1 (div (the my-number) 3)) ;;; 3.3333333333333335
   (division-2 (div (the my-number) 4)) ;;; 2.5
   (division-3 (div (the my-number)))   ;;; 0.1

   (half-1 (half (the my-notation-number))) ;;; 26600.0
   (half-2 (half (the my-number)))          ;;; 5

   (twice-1 (twice (the my-number))) ;;; 20
   (twice-2 (twice (the my-binary))) ;;; 22
   
   )
   )

   
