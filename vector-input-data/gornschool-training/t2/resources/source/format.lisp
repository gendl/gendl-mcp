(in-package :gdl-user)

(define-object top-10-formatting (base-object)

  :computed-slots
  (("The most basic text formatting using the ~a directive"
    basic (let ((name "mike"))
	    (format nil "hello ~a" name))) ;;"hello mike"

   ("~$ directive outputs to 2dp, giving it a prefix parameter of 5 will output to 5dp. some directives take more than 1 prefic parameter, in this case the second prefix parameter to ~f controls the number of decimal places. Note all of these perform numeric rounding as well"
    floating-point-numbers (format nil "~$ ~5$ ~,4f" pi pi pi)) ;;"3.14 3.14159 3.1416"

   ("~d emits numbers in decimal, but using different prefix parameters alters the way they are printed"
    numeric-output (format nil "~d ~:d ~@d" 10000 10000 10000)) ;; "10000 10,000 +10000"

   ("for some outputs we may need a fixed width, irrespective of the length of the data. Here we have a width of 12, first padded with white-space, second with 0. The second example uses the ~:* directive to move backwards in the argument list"
    fixed-width (let ((str1 (format nil "~12d ~12,'0d" 1234 1234))
		      (str2 (format nil "~12d ~:*~12,'0:d" 1234)))
		  (format nil "~a~%~a" str1 str2)))                    ;;; "        1234 000000001234"
                                                                       ;;;         1234 000000001234"

   ("english language output"
    english-language (format nil "~r" 12)) ;; "twelve"

   ("control of case using the ~( and the @ and : modifiers. Also using ~% to introduce line breaks"
    case-control (let* ((men "Men")
		       (str1 (format nil "~(~r ~a~)" 12 men))
		       (str2 (format nil "~@(~r ~a~)" 12 men))
		       (str3 (format nil "~:(~r ~a~)" 12 men))
			(str4 (format nil "~:@(~r ~a~)" 12 men)))
		   (format nil "~a~%~a~%~a~%~a" str1 str2 str3 str4))) ;;; "twelve men
                                                                         ;;; Twelve men
                                                                         ;;; Twelve Men
					                                 ;;; TWELVE MEN"

  ("using the ~p and ~@p to introduce plurals. the : modifier causes the previous argument to be reprocessed"
   plural (let ((str1 (format nil "~a computer~:p" 1))
		(str2 (format nil "~a computer~:p" 2))
		(str3 (format nil "~r fl~:@p" 1))
		(str4 (format nil "~r fl~:@p" 2)))
	    (format nil "~a~%~a~%~a~%~a" str1 str2 str3 str4)))  ;;;"1 computer
                                                                ;;; 2 computers
                                                                ;;; one fly
                                                                ;;; two flies"
  ("~{ causes the enclosed formatting directives to be applied to each element in the list. the ~^ directive causeswhatever is following to be processed every time apart from the last itteration"
   itteration (format nil "~{~a~^,~}" (list 1 2 3 4 5))) ;;; "1,2,3,4,5"

  ("using the @ modifier causes the individual arguments to be treated as a list"
   itteration-2 (format nil "~@{~a~^,~}" 1 2 3 4 5))      ;;; "1,2,3,4,5"

  (conditional (let ((str1 (format nil "~@[first name=~a ~]~@[second name = ~a~]" "Peter" "Paul"))
		     (str2 (format nil "~@[first name=~a ~]~@[second name = ~a~]" nil "Paul"))
		     (str3 (format nil "~@[first name=~a ~]~@[second name = ~a~]" "Peter"  nil))
		     (str4 (format nil "~@[first name=~a ~]~@[second name = ~a~]" nil nil)))
	(format nil "~@{~a~^~%~}"str1 str2 str3 str4))))                                        ;;; "first name=Peter second name = Paul
                                                                                                ;;; second name = Paul
                                                                                                ;;; first name=Peter 
                                                                                                ;;; "
  
  
  )
   
