(in-package :gdl-user)

(define-object strings (base-object)

  :computed-slots
  (
   ;;; string manipulation
   (string-build-1 (concatenate 'string "my" " " "string"))                 ;;; "my string"
   (string-build-2 (apply #'concatenate 'string (list "my" " " "string")))  ;;; "my string"
   (string-build-3 (let ((a "my")
			 (b "string"))
		     (format nil "~a ~a" a b)))                             ;;; "my string"

   (reverse-string (reverse "My String"))                                   ;;; "gnirtS yM"
   (sort-string (safe-sort (list "My" "Empty" "String")  #'string>))        ;;; ("String" "My" "Empty")
   (split-string-1 (glisp:split-regexp "\\s" "my Empty String"))            ;;; ("my" "Empty" "String")
   (split-string-2 (glisp:split-regexp "\\s" "my    Empty String"))         ;;; ("my" "" "" "" "Empty" "String")
   (split-string-3 (glisp:split-regexp "\\s+" "my    Empty String"))        ;;; ("my" "Empty" "String")
   (split-string-4 (glisp:split-regexp "," "my,comma,delimited,String"))    ;;; ("my" "comma" "delimited" "String")

   ;; string information
   (string-info-1 (length (the my-string)))                            ;;; 16
   (string-info-2 (position "s" (the my-string) :test 'string-equal))  ;;; 3  Note that position give the position of the first occurrence
   (string-info-3 (position "is" (the my-string) :test 'string-equal)) ;;; NIL
   (string-info-4 (glisp:match-regexp "is" (the my-string)))           ;;; 2 this is the value thats restrned and assigned to (the string-info-4
                                                                       ;;; 4 this is printed in the buffer but not returned, same for the 2 values below
                                                                       ;;; #()
                                                                       ;;; #()

   (example-1 (subseq (the my-string) (the string-info-4)))            ;;; "is is a string"
   
   ;;; glisp:match-regexp returns multiple values. To access any apart from the first value we need to use multiple-value-bind
   (string-info-5 (multiple-value-bind (a b)
		      (glisp:match-regexp "is" (the my-string))
		    (list :start a :end b)))                            ;;; (:START 2 :END 4)
   (example-2 (subseq (the my-string)
		    (getf (the string-info-5) :start)))                 ;;; "is is a string"
   (example-3 (subseq (the my-string)
		      (getf (the string-info-5) :end)))                 ;;; " is a string"
   (example-4 (subseq (the my-string)
		      (getf (the string-info-5) :start)
		      (getf (the string-info-5) :end)))                 ;;; "is"
   
		  
	   
   
	  


   )
   )

   
