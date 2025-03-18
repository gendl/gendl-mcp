(in-package :training-2)

(define-object strings (base-training-sheet)
  :computed-slots
  ((index-words (list "String" "Character" "string-equal" "string=" "safe-sort" "position" "string<" "concatenate" "position" "format" "escape"))

   (repl-1 (list (list :command "(setq str \"Introduction to GendL\")"
		       :output "\"Introduction to GendL\"")
		 (list :command "(reverse str)"
		       :output "\"LdneG ot noitcudortnI\"")
		 (list :command "(length str)"
		       :output 21)
		 (list :command "(setq escaped-str \"Introduction to \"GendL\"\")"
		       :output "\"Introduction to \"GendL\"\"")
		 (list :command "(length escaped-str)"
		       :output 23)))
   
   (repl-2 (list (list :command "(setq a #\c)"
		       :output "#\c")
		 (list :command "(setq b #\c)"
		       :output "#\c")
		 (list :command "(setq c #\C)"
		       :output "#\C")
		 (list :command "(and (eq a b) (eql a b) (equal a b) (equalp a b))"
		       :output "T")
		 (list :command "(char= a b)"
		       :output "T")
		 (list :command "(char= a c)"
		       :output "NIL")
		 (list :command "(char-equal a c)"
		       :output "T")))

   (repl-3 (list (list  :command "(setq str1 \"genworks\")"
		       :output "\"genworks\"")
		(list :command "(setq str2 \"Genworks\")"
		      :output "\"Genworks\"")
		(list :command "(string= str1 str2)"
		      :output "NIL")
		(list :command "(string= str1 str2 :start1 1 :start2 1)"
		      :output "T")		   
		(list :command "(string-equal str1 str2)"
		      :output "T")
		(list :command "(string-equal str1 str2 :start1 2)"
		      :output "NIL")))
   
   (repl-4 (list (list :command "(setq str \"GendL\")"
		       :output "NIL")
		 (list :command "(position \"l\" str)"
		       :output "NIL")
		 (list :command "(position \"l\" str :test 'string=))"
		       :output "NIL")
		 (list :command "(position \"l\" str :test 'string-equal))"
		       :output 4)
		 (list :command "(setq lis (list \"Peter\" \"Paul\" \"John\" \"Craig\"))"
		       :output "(\"Peter\" \"Paul\" \"John\" \"Craig\")")
		 (list :command "(safe-sort lis 'string<)"
		       :output "(\"Craig\"\"John\" \"Paul\" \"Peter\")")))

   (repl-5 (list (list :command "(setq str1 \"Introduction\")"
		       :output "\"Introduction\"")
		 (list :command "(setq str2 \"To\")"
		       :output "\"To\"")
		 (list :command "(setq str3 \"GendL\")"
		       :output"\"GendL\"")
		 (list :command "(format nil \"~a ~a ~a\" str1 str2 str3)"
		       :output "\"Introduction to GendL\"")
		 (list :command "(concatenate 'string str1 \" \" str2 \" \" str3)"
		       :output "\"Introduction to GendL\"")
		 (list :command "(setq lis (list \"My\" \"List\" \"of\" \"strings\"))"
		       :output "(\"My\" \"List\" \"of\" \"strings\")")
		 (list  :command "(apply #'concatenate 'string lis)"
			:output "\"MyListofstrings\"")))	
		   
   (repl-6 (list (list :command "(setq str1 \"Introduction\")"
		       :output "\"Introduction\"")
		 (list :command "(subseq str1 3)"
		       :output "\"roduction\"")))
  
   (body-content (with-cl-who-string()
		   ((:div :class "main-page-container")
		    ((:div :class "main-page-item")
		     (:div :class "grid-container-2-650px"
			   (:div :class "grid-item"
		      
			         (:h3 "Character and String definitions")
		                 (:p (:em (:b "Characters"))" are denoted with a #\ prefix, whilst "(:em (:b "Strings"))
                                     " are represented textually as a series of characters between double
quotes. Because Strings are sequences of Characters, any function
which acts upon a sequence can be used on a string. Because a string
is represented textually as characters enclosed in double quotes, if
you want to include literal double quotes in your string you need to
escape them with \\ (backslash) characters. If you want to include a
literal backslash (\\) it needs to be escaped with another backslash")
			         (str (repl-example (the repl-1)))
			         (:h3 "Character Comparison")
			         (:p "Recall from the Equality tutorial that, because a character is an object with the same symbol, any of the tests "
			             ((:span :class "function")"EQ")", "
			             ((:span :class "function")"EQL")", "
			             ((:span :class "function")"EQUAL")" or "
			             ((:span :class "function")"EQUALP")" will work with characters. Additionally the function "
			             ((:span :class "function")"char=")" may be used, which accepts any number of arguments and will return T if the arguments are all the same character. The case-insensitive version is "
			             ((:span :class "function")"char-equal"))
			         (str (repl-example (the repl-2)))
			         (:h3 "String Comparison")
			         (:p "Recall from the Equality tutorial that "
			             ((:span :class "function")"equal")" will return T for 2 identical strings and "
			             ((:span :class "function")"equalp")" will return T for 2 strings which are identical ignoring case. "
			             ((:span :class "function")"string=")" and "
			             ((:span :class "function")"string-equal")" will give the same result as "
			             ((:span :class "function")"equal")" and "
			             ((:span :class "function")"equalp")", but additionally provide keyword arguments :start1, :end1, start2, end2 which enable substrings to be tested")
			         (str (repl-example (the repl-3)))
			         (:h3 "String Searching/Ordering")
			         (:p "When searching or ordering strings, some extra arguments to the functions may be required, because the default test for equality is "
			             ((:span :class "function")"EQL")" which, you may recall, will not return T for two identical strings. The keyword input "
			             ((:span :class "object-keyword")":test")" generally has to be specified to be an equality test which works with strings, for example "
			             ((:span :class "function")"string-equal"))
			         (str (repl-example (the repl-4)))
			         (:h3 "String Building")
			         (:p "One of the easiest ways to build strings is with the CL "
			             ((:span :class "function")"format")" function. We will come onto it in the next tutorial but for now it can be though of as a function which defines a format template that you define and then assign specific strings to. At its most basic, the CL function "
			             ((:span :class "function")"concatenate")" can achieve the same output")
			         (:p "To assemble a string from a list of strings using concatenate the apply function must be used")
			         (str (repl-example (the repl-5)))
			         (:h3 "String Destructuring")
			         (:p "Strings may be taken apart using the subseq function.")
			         (str (repl-example (the repl-6))))))
		    
		    ((:div :class "main-page-item")
		     (:h2 "Resources")
		     (str (the resource-links))))))))
