(in-package :training-2)

(define-object equality (base-training-sheet)
  :computed-slots
  ((index-words (list "eq" "eql" "equal" "equalp" "numberp" "floatp" "characterp" "floatp" "stringp" "zerop" "plusp" "minusp" ">" ">=" "=" "\<=" "\<"
		"string\<" "string-lessp" "string=" "string-equal" "string>" "string-greaterp" "string/=" "string-not-equal"))

   (repl-1 (list (list :command "((eq 1 1)"
		       :output T)
		 (list :command "(eq #\\a #\\a)"
		       :output "T")))
		 

   (repl-2 (list (list :command "(eq 1.0 1.0)"
		       :output "NIL")
		 (list :command "(eq \"a\" \"a\")"
		       :output "NIL")
		 (list :command "(eq (list 1 2 3) (list 1 2 3))"
		       :output "NIL")
		 (list :command "(eq 1 1.0)"
		       :output "NIL")))
   
   (repl-3 (list (list :command "(eql 1.0 1.0)"
		       :output "T")		 
		 (list :command "(eql \"a\" \"a\")"
		       :output "NIL")
		 (list :command "(eql (list 1 2 3) (list 1 2 3))"
		       :output "NIL")
		 (list :command "(eql 1 1.0)"
		       :output "NIL")))

   (repl-4 (list (list :command "(equal \"a\" \"a\")"
		       :output "T")
		 (list :command "(equal (list 1 2 3) (list 1 2 3))"
		       :output "T")
		 (list :command "(equal 1 1.0)"
		       :output "NIL")
		 (list :command "(equal \"A\" \"a\")"
		       :output "NIL")
		 (list :command "(equal (list 1 2 3) (list 1 2 3.0))"
		       :output "NIL")))

   (repl-5 (list (list :command "(equalp 1 1.0)"
		       :output "T")
		 (list :command "(equalp \"A\" \"a\")"
		       :output "T")
		 (list :command "(equalp (list 1 2 3) (list 1 2 3.0))"
		       :output "T")))

   (repl-6 (list (list :command "(> 5 4 3)"
		       :output "T")
		 (list :command"(> 5 4 5)"
		       :output "NIL")
		 (list :command"(/= 1 2 3 4)"
		       :output "T")
		 (list :command"(/= 1 1 4 1)"
		       :output "NIL")))
		 
   (body-content (with-cl-who-string()
		   (:p "Lisp defines 4 generic equality predicates. In order of \"strictness\" they are:"
		       (:ul (:li (:span :class "macro" "eq"))
			    (:li (:span :class "macro" "eql"))
			    (:li (:span :class "macro" "equal"))
			    (:li (:span :class "macro" "equalp")))

                       "When we say "
                       (:span :class "macro" "eq")
                       " is the strictest test, we mean that it is most likely to return "
                       (:span :class "value" "NIL")
                       ".  So any two items which compare as "
                       (:span :class "value" "T")
                       " with "
                       (:span :class "macro" "eq")
                       " will certainly compare as "
                       (:span :class "value" "T")
                       " with "
                       (:span :class "macro" "eql")
                       " and with all the other equality predicates below "
                       (:span :class "macro" "eql")
                       " in the stack. A similar relationship may be drawn between "
                       (:span :class "macro" "eql") " and "
                       (:span :class "macro" "equal") ", and likewise between "
                       (:span :class "macro" "equal") " and "
                       (:span :class "macro" "equalp") ". Now we will show some examples and describe
the characteristics of each of these equality predicates.")

		   (:h3 (:span :class "macro" "eq"))
		   (:p (:em (:b "eq")) " takes 2 objects as arguments and returns t if they are identical" (:br)
		       "Integers and Characters have the same symbol-name and value, so when comparing Integers and Characters, whilst it may appear "
		       (:span :class "macro" "eq") " is testing for value equality, it is in fact testing for object equality." (:br)
		       (:span :class "macro" "eq") " is fast but is only safe to use when the types (i.e. classes) of the objects being tested are known in advance")
		   (str (repl-example (the repl-1)))
				 
		   (:p "Floats, lists, and strings however are just pointers to the actual objects containing the value in memory, so "
		       (:span :class "macro" "eq")
                       " will generally return NIL if either of the objects being tested is a float, list or string. This is because
two floats, lists, or strings, even though they may appear to be identical, typically are existing in two different places in memory.")
		   (str (repl-example (the repl-2)))
			
		   (:h3 (:span :class "macro" "eql"))
		   (:p (:em (:b "eql"))" behaves as "
		       (:em (:b "eq"))" except it guarantees to consider 2 objects of the same class representing the same "
		       (:em "numeric or character")" value to be equal")
		   (str (repl-example (the repl-3)))

		   (:h3 (:span :class "macro" "equal"))
		   (:p (:span :class "macro" "equal") " behaves as "
		       (:span :class "macro" "eql")  " except different objects may be considered equivalent if their values look the same.
This is particularly useful when testing for equality between two strings or two lists")
		   (str (repl-example (the repl-4)))

		   (:h3 (:span :class "macro" "equalp"))
		   (:p (:span :class "macro" "equalp") " behaves as "
		       (:span :class "macro" "equal") ", except numbers are considered equal if they are mathematically equal,
 and string equality is not case-sensitive")
                        
		   (str (repl-example (the repl-5)))

			
                        
		   (:p "In addition to the above, Lisp defines specific equality tests (predicates) for numbers and strings")

		   (:h3 "Number Comparison")
		   (:ul (:li (:span :class "macro" "&lt;") " - less than")
			(:li (:span :class "macro" "&lt;=") " - less than or equal")
			(:li (:span :class "macro" "=") " - equal")
			(:li (:span :class "macro" "&gt;=") " - greater than or equal")
			(:li (:span :class "macro" "&gt;") " - greater than")
			(:li (:span :class "macro" "/=") " - not equal"))
		   (:p "All of these functions take one or more arguments, and with the exception of "
                       (:span :class "macro" "/=")
                       ", all make their comparisons between consecutive pairs. With "
                       (:span :class "macro" "/=")
                       ", it compares all posible combinations of the arguments")
		   (str (repl-example (the repl-6)))
		   (:p "Specific tests for number values are also avaliable"
		       (:ul (:li (:span :class "function" "zerop") " - the number is zero")
			    (:li (:span :class "function" "plusp") " - the number is greater than zero")
			    (:li (:span :class "function" "minusp") " - the number is less than zero")))
		   (:p "When comparing Float numbers, it is better to use the GendL functions "
		       ((:span :class "function") "near-to?") " and "
		       ((:span :class "function") "near-zero?") ". These return " (:span :class "value" "T") " if the two arguments are within "
                       (:span :class "variable-name" "*ZERO-EPSILON*") " of each other")
		   (:h3 "String Comparison")
		   (:p "string comparison is similar number comparison, except there is one variant (the first shown below)
which is case-sensitive and one (the second) which is case-insentitive. These functions accept two string arguments.")
		   (:ul (:li (:span :class "macro" "string<") ", "
                             (:span :class "macro" "string-lessp")
                             " - one string less than another")
			(:li (:span :class "macro" "string=") ", "
                             (:span :class "macro" "string-equal")
                             " - one string equal to another")
			(:li (:span :class "macro" "string>") ", "
                             (:span :class "macro" "string-greaterp")
                             " - one string greater than another")
			(:li (:span :class "macro" "string/=") ", "
                             (:span :class "macro" "string-not-equal")
                             " - strings are not equal"))
		   (:p " These functions  also have keyword inputs "
                       (:span :class "general-keyword" "(:start1 :end1 :start2 and :end2)")
                       " to enable substrings of the supplied strings to be compared")

                   (:h3 "Object Type")
		   (:p "Finally, Lisp defines predicates to test for object type"
		       (:ul (:li (:span :class "macro" "numberp") " - returns " (:span  :class "value" "T") " if the object is a number.")
			    (:li (:span :class "macro" "floatp") " - returns " (:span :class "value" "T") " if the object is a float.")
			    (:li (:span :class "macro" "stringp") " - returns " (:span :class "value" "T") " if the object is a string.")
			    (:li (:span :class "macro" "characterp") " - returns " (:span :class "value" "T") " if the object is a character.")
			    (:li (:span :class "macro" "listp")  " - returns " (:span :class "value" "T") " if the object is a list.")))

		    (:h2 "Resources")
		    (str (the resource-links))))))
