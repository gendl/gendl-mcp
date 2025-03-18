(in-package :training-2)

(define-object numbers-and-arithmetic (base-training-sheet)
  
  :computed-slots
  ((index-words (list "+" "-" "/" "*" "div" "half" "twice" "Arithmetic Precedence" "Integer" "Floating Point" "Ratio" "Complex Number" "Prefix Notation" ":computed-slots"))
   (repl-1 (list (list :command "(+)"
		       :output 0)
		 (list :command "(+ 5)"
		       :output 5)
		 (list :command "(+ 5 1 9 2)"
		       :output 17)))
   
   (repl-2 (list (list :command "(- 5)"
		       :output -5)
		 (list :command "(- 5 1 9 2)"
		       :output -7)
		 (list :command "(- 1 -5)"
		       :output 6)))

   (repl-3 (list (list :command "(*)"
		       :output 1)
		 (list :command "(* 2)"
		       :output 2)
		 (list :command "(* 2 3 4)"
		       :output 24)
		 (list :command "(* 2 -3 4)"
		       :output -24)))

   (repl-4 (list (list :command "(/ 3)"
		       :output "1/3")
		 (list :command "(/ 10 2)"
		       :output "5")
		 (list :command "(/ 10 3)"
		       :output "10/3")
		 (list :command "(/ 10 2 5)"
		       :output "1")
		 (list :command "(/ 10 1 6)"
		       :output "5/3")))

   (repl-5 (list (list :command "(div 3)"
		       :output "0.3333333333333333")
		 (list :command "(div 10 2)"
		       :output "5.0")
		 (list :command "(div 10 3)"
		       :output "3.3333333333333333")
		 (list :command "(div 10 2 5)"
		       :output "1.0")
		 (list :command "(div 10 1 6)"
		       :output "1.6666666666666667")))

   (repl-6 (list (list :command "(half 5)"
		       :output "5/2")
		 (list :command "(half 5.0)"
		       :output "2.5")
		 (list :command "(half 5/3)"
		       :output "5/6")))
   (repl-7 (list (list :command "(twice 5)"
		       :output "10")
		 (list :command "(twice 5.0)"
		       :output "10.0")
		 (list :command "(twice 5/3)"
		       :output "10/3")))
   (repl-8 (list (list :command "(/ (* (+ 2 3) (- 3 4)) (* (- 2 6) (+ 2 (* 2 3))))"
		       :output "5/32")
		 (list :command "(/ (* (+ 2 3) (- 3 4)) (* (- 2 6) (+ 2 6)))"
		       :output "5/32")
		 (list :command "(/ (* 5 -1) (* -4 8))"
		       :output "5/32")
		 (list :command "(/ -5 -32)"
		       :output "5/32")))

   
	      
   (code-1 (list"(define-object my-box-4 (box)"
		" :input-slots"
		" (length"
		"  (width 4)"
		"  (height 4)))"
		" :computed-slots"
		" ((density 7800)"
		"  (mass (* (div (the volume) 1000000000) (the density)))))"))

   (body-content (with-cl-who-string()
		
		   (:p "In the " (:em (:b (:a :href "../object-definition/index.html" "Defining Objects")))
                       " section, we briefly touched on numbers and arithmetic")
		   (str (code-example (the code-1)))
		   (:p "In this section we will take a bit more detailed look at how GendL handles numbers and arithmetic.")
		   (:p "The majority of this is pure Common Lisp (CL), although GendL does define a few "
		       (:em "Convenience Functions") " which may be classed as arithmetic. ")
		   (:p "CL has four number types:"
		       (:ul (:li "Integers (e.g. "
                                 (:span :class "value" "1") ", "
                                 (:span :class "value" "38") ", "
                                 (:span :class "value" "183749372628") ")")
			    (:li "Floating Point (e.g. "
                                 (:span :class "value" "253.67") ", "
                                 (:span :class "value" "2.52E+26") ")")
			    (:li "Ratios (e.g. "
                                 (:span :class "value" "6/7") ", "
                                 (:span :class "value" "347/395") ")"))
		       (:li "Complex (e.g. " (:span :class "value" "(#C(0 5)") ", representing the square root of -25)"))
		   (:p "In contrast with other languages, you don't need to worry too much
about the type of number you are using, as the CL runtime system will
sort that out for us (unless you need to intervene for specific
performance reasons by bypassing the dynamic typing feature).")
		   (:p "CL can handle rational numbers with \"infinite\" precision. For
example, the number "
                       (:span :class "value" "1/3")
                       " represents one-third with no loss of precision as happens with the decimal representation of "
                       (:span :class "value" "0.33333...")
                       " truncated to some arbitray number of decimal places.")

		   (:p "And there is no realistic limit on the maximum size of a number that
CL can represent (although Integers up to a certain size may be
processed more efficiently than arbitrarily huge Integers).")
		   (:h3 "Arithmetic functions")
		   (:p "There are four basic CL arithmetic functions"
		       (:ul (:li (:span :class "function" "+"))
			    (:li (:span :class "function" "-"))
			    (:li (:span :class "function" "*"))
			    (:li (:span :class "function" "/")))
		       "Additionally GendL defines a fifth:"
		       (:ul (:li (:span :class "function" "div")))
		       "and two "(:em "shortcut") " functions"
		       (:ul (:li (:span :class "function" "half"))
			    (:li (:span :class "function" "twice"))))
			
		   (:p "As with all Lisp dialects, CL  uses prefix notation for
functions. All five basic arithmetic functions ("
                       (:span :class "function" "+") ", "
                       (:span :class "function" "-") ", "
                       (:span :class "function" "*") ", "
                       (:span :class "function" "/") ", and "
                       (:span :class "function" "div") ") can take one or more arguments, while "
		       (:span :class "function" "+") " and "
		       (:span :class "function" "*") " can also take zero arguments.")
		   (:h4 "CL function " (:span :class "function" "+"))
		   (:p "The return value from "
		       (:span :class "function" "+") " depends on the number of arguments it is given."
		       (:ul (:li "Zero Arguments - returns " (:span :class "value" "0"))
			    (:li "One Argument - returns the argument")
			    (:li "Two or more arguments - returns the sum of all the arguments")))
		   (str (repl-example (the repl-1)))
		   (:p "Also note that because Lisp evaluates functions inside-out, (+ 1 2 3) is equivalent to (+ (+ 1 2) 3)")
		   (:p "More about functions later.")
		   (:h4 "CL function " (:span :class "function" "-"))
		   (:p "The " (:span :class "function"  "-") " function requires at least one argument. If no arguments are supplied, then an error will result.")
		   (:p "The return value from " (:span :class "function"  "-") " depends on the number of arguments it is given."
		       (:ul (:li "One Argument - returns the negative value of the supplied argument")
			    (:li "Two or more arguments - accumulates the value of each argument subrtacted from the previous argument and returns the final result.")))
		   (str (repl-example (the repl-2)))
		   (:h4 "CL function " (:span :class "function" "*"))
		   (:p "The return value from "
		       (:span :class "function" "*") " depends on the number of arguments it is given."
		       (:ul (:li "Zero Arguments - returns " (:span :class "value" "1") ".")
			    (:li "One Argument - returns the argument.")
			    (:li "Two or more arguments - returns the product of all the arguments.")))
		   (str (repl-example (the repl-3)))
		   (:h4 "CL function " (:span :class "function" "/"))
		   (:p "The " (:span :class "function" "/") " requires at least one argument. If no arguments are supplied, then an error will result.")
		   (:p "The return value from "
		       (:span :class "function" "/") " depends on the number of arguments it is given and the value of the result"
		       (:ul (:li "One Argument - returns the reciprocal of the argument as a ratio (1/argument).")
			    (:li "Two or more arguments - returns the value of the first argument
divided by the product of the remaining arguments. If the resulting
answer is a whole number, then the return value will be a whole number.
Otherwise it will be a ratio with the least possible whole denominator.")))
		   (str (repl-example (the repl-4)))
		   (:h4 "GendL function " (:span :class "function" "div"))
		   (:p "The GendL function "(:span :class "function" "div") " is identical in operation to the Lisp function "
		       (:span :class "function" "/") " except it uses rational division and converts the return value to a floating point number")
		   (str (repl-example (the repl-5)))
		   (:h4 "GendL function " (:span :class "function" "half"))
		   (:p "The GendL function "
                       (:span :class "function" "half")
                       " takes a single argument and returns the result of dividing that
argument by the integer "
                       (:span :class "value" "2")
                       ". The type of the returned number will depend on the type of the argument" )
		   (str (repl-example (the repl-6)))
		   (:h4 "GendL function " (:span :class "function" "twice"))
		   (:p "The GendL function "
                       (:span :class "function" "twice")
                       " takes a single argument and returns the result of multiplying The
argument by the integer "
                       (:span :class "value" "2") ". The type of the returned number will depend
on the type of the argument.")
		   (str (repl-example (the repl-7)))
			
		   (:h3 "Arithmetic Precedence")
		   (:p "Due to Lisp's generalized prefix notation for functions, and the
basic rule that functions are evaluated inside-out (in other words,
the innermost functions are evaluated first), precedence of arithmetic
operators is completely explicit in Lisp. The example below shows
conceptually how a long arithmetic expression is broken down during
evaluation")
		   (str (repl-example (the repl-8)))
		   (:p "So back to our code, " (:span :class "object" "my-box-4"))
		   (str (code-example (the code-1)))
		   (:p "In the "
		       (:span :class "object-keyword" ":computed-slots")", "
		       (:span :class "slot" "density") " has been assigned the value of "
                       (:span :class "value" "7800") ".")
		   (:p "Because the box dimensions are in mm, and the "
		       (:span :class "slot" "density") " is in kg/m" (:sup "3") ", we first divide the "
		       (:span :class "slot" "volume") " by " (:span :class "value" "1000000000")
                       " to convert into cubic metres. We then multiply this result by the value of "
		       (:span :class "slot" "density")
                       " to get the mass in kg. The result will always be a floating point number, since "
                       (:span :class "function" "div") " indeed always returns a floating point number.")
		   (:div :class "main-page-item" (:h2 "Resources") (str (the resource-links)))))))
