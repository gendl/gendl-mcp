# Gendl Documentation - tutorial_2_object_definition

## iteration-and-mapping.lisp - header
Source: gornschool-training/t2/source/iteration-and-mapping.lisp
Type: tutorial

```
(in-package :training-2)


```

---

## iteration-and-mapping.lisp - iteration-and-mapping
Source: gornschool-training/t2/source/iteration-and-mapping.lisp
Type: tutorial

```
(define-object iteration-and-mapping (base-training-sheet)

  :computed-slots
  ((index-words (list "dolist" "dotimes" "mapcar" "mapcan" "mapc" "lambda function" "anonymous function"))
   (repl-1 (list (list :command "(setq a (list 1 2 3 4 5 6))"
		       :output "1 2 3 4 5 6)")
		 (list :command (list "(setq b (let ((res nil))"
				      "                 (dolist (var a res)"
				      "                    (push (* var 5) res))))")
		       :output "(30 25 20 15 10 5)")
		 (list :command (list "(setq b (let ((res nil))"
				      "                 (dolist (var a (nreverse res))"
				      "                    (push (* var 5) res))))")
		       :output "(5 10 15 20 25 30)")))

   (repl-2 (list (list :command (list "(setq c (let ((len 0))"
				      "                  (dolist (var a)"
				      "                     (setf len (+ len 1)))"
				      "                   len))")
		       :output 6)
		 (list :command (list "(setq c (let ((len 0))"
				      "                  (dolist (var a)"
				      "                     (incf len)"
				      "                  len))")
		       :output 6)))

   (repl-3 (list (list :command "(setq a (list 1 2 3 4 5))"
		       :output "(1 2 3 4 5)")
		 (list :command "(setq b (list \"a\" \"b\" \"c\" \"d\" \"e\"))"
		       :output "(\"a\" \"b\" \"c\" \"d\" \"e\")")
		 (list :command (list "(let ((res nil))"
				      "       (dotimes (n (length a) (nreverse res))"
				      "         (push (nth n a) res)"
				      "         (push (nth n b) res)))")
		       :output "(1 \"a\" 2 \"b\" 3 \"c\" 4 \"d\" 5 \"e\")")

		 (list :command (list "(setq b (let ((res nil))"
				      "               (dotimes (n (length a) (nreverse res))"
				      "                 (push (* (nth n a) 5) res))))))")
		       :output "(5 10 15 20 25)")))
   
   (body-content (with-cl-who-string()
		   (:p "Common Lisp provides a number of iteration and mapping macros and functions which enable us to process the
contents of lists, the most commonly used being"
		       (:ul (:li "Iteration - the macros "
				 ((:span :class "macro")"dolist")" and "
				 ((:span :class "macro")"dotimes"))
			    (:li "Mapping - the functions "
				 ((:span :class "function")"mapcar")", "
				 ((:span :class "function")"mapcan")" and "
				 ((:span :class "function")"mapc")))
		       "Common Lisp also defines 2 powerful and more general iteration macros, "
		       ((:span :class "macro")"do/do*")" and "
		       ((:span :class "macro")"loop")", but they are outside the scope of this tutorial")
		   (:h3 "Iteration Macros")
		   (:ul ((:div :class "grid-container-2")
			 ((:div :class "main-page-item")
			  (:li ((:span :class "macro")"dolist")" - takes a list of a "
			       (:em "variable")" and an "
			       (:em "expression")" which returns a "
			       (:em "list")", followed by a "
			       (:em "body of expressions")". Optionally "
			       ((:span :class "macro")"dolist")
                               " can take a third element for its list, which is an expression which
will be evaluated and its value returned from the "
                               (:span :class "macro" "dolist")
                               " expression once the "
			       ((:span :class "macro") "dolist") " iteration has completed. The "
			       (:em "body of expressions") " is evaluated with the "
			       (:em "variable")" bound to successive elements in the list."
			       (:br) "The example on the right first sets a return variable  "
			       (:em "res") " to "
                               (:span :class "value" "nil") ". It then iterates through the list "
			       (:em "a")", multiplying each value by "
                               (:span :class "value" "5")
                               " and pushing the result onto "
			       (:em "res") ". Finally "
			       (:em "res") " is returned from the "
			       (:span :class "macro" "dolist") " and set to "
			       (:em "b")"."
			       (:br) "If you wanted to return the resultant list values in the same order as they
occur in the input list, you would need to wrap the dolist return value "
			       (:em "res")" in a call to the function "
			       ((:span :class "function")"reverse") ". But because "
			       (:em "res")" is locally bound and will never be used after "
			       (:em "b") " has been evaluated, you could use the destructive function "
			       ((:span :class "function") "nreverse") ", which is slightly more efficient than "
			       ((:span :class "function")"reverse" )))
			 ((:div :class "main-page-item")
			  (str (repl-example (the repl-1))))
			 ((:div :class "main-page-item")
			  (:p "It is also possible to use "
			      ((:span :class "macro") "dolist")" without any return value argument, in which case you'd be relying on "
			      (:em (:b "side-effecting") " within the body. The two examples shown are different ways of returning the length of a list")))
			 ((:div :class "main-page-item")
			  (str (repl-example (the repl-2))))
			 ((:div :class "main-page-item")
			  (:li (:span :class "macro" "dotimes") " - takes two required inputs, a "
			       (:em "variable")" and an "
			       (:em "integer")", followed by a "
			       (:em "body of expressions")". As with "
                               (:span :class "macro" "dolist") ", the list at the start of "
                               (:span :class "macro" "dotimes") "'s argument list can optionally "
			       ((:span :class "macro")"dotimes")" have a third element, an expression which will "
			       " be evaluated and the resulting value returned once the "
			       ((:span :class "macro")"dotimes")" has completed. The "
			       (:em "body of expressions")" are evaluated with the "
			       (:em "variable")" bound to successive integers between 0 and "(:em "integer") " minus 1"
			       (:p "In contrast to "
				   ((:span :class "macro")"dolist")" which is generally used to iterate through a single list, using "
				   ((:span :class "macro")"dotimes")" with the function "
				   ((:span :class "function") "nth")" permits iteration on multiple lists")
			       (:p "The first example shows "((:span :class "macro")"dotimes")" being used to splice together 2 lists. The second example is the first "
				   ((:span :class "macro")"dolist")" example converted to a "
				   ((:span :class "macro")"dotimes")" implementation")))
			 ((:div :class "main-page-item")
			  (str (repl-example (the repl-3))))))
		   (:h3 "Mapping Functions")
		   (:ul ((:div :class "grid-container-2")
			 ((:div :class "main-page-item")
			  (:li ((:span :class "function") "mapcar")" - one of the most heavily used mapping functions, it takes a "
			       (:em "function")" and one or more "
			       (:em "lists")" and calls the "
			       (:em "function")" on successive elements of the "
			       (:em "lists")))
			 ((:div :class "main-page-item")
			  (str (repl-example (the repl-4))))
			 ((:div :class "main-page-item")
			  (:p ((:span :class "function")"mapcar")" is very commonly used with a "
			      (:em (:b "lambda"))" or anonymous function. This effectively allows functions to be defined on the fly and they can be used
to map across multiple lists. The first example is an alternative to the "
			      ((:span :class "macro")"dotimes")" example for splicing 2 lists together. Successive elements from the 2 input lists are bound to the "
			      (:em (:b "lambda"))" vailables "
			      (:em "x")" and "
			      (:em "y")" and the return value from the "
			      (:em (:b "lambda"))" expression appended to the return list. The second example replaces the "
			      ((:span :class "function")"LIST-PRODUCT")" function with a "
			      (:em (:b "lambda")" function to achieve the same result")
			      (:p ((:span :class "function")"mapcar")" can often be used to accomplish the same thing as "
				  ((:span :class "macro")"dolist")" and may be less verbose. However, when the list processing becomes more complex "
				  ((:span :class "macro")"dolist")" may have some advantage in clarity and debugging")))
			 ((:div :class "main-page-item")
			  (str (repl-example (the repl-5))))
			 ((:div :class "main-page-item")
			  (:li ((:span :class "function")"mapcan")" - takes a "
			       (:em "function")" and one or more "
			       (:em"lists")" like "
			       ((:span :class "function")"mapcar")" but splices together the values returned by the " (:em"function")))
			 ((:div :class "main-page-item")
			  (str (repl-example (the repl-6))))
			 ((:div :class "main-page-item")
			  (:li ((:span :class "function")"mapc")" is like "
			       ((:span :class "function")"mapcar")" but it doesn't accumulate any data to return so the only reason to use it is for side-effecting. "
			       ((:span :class "function")"mapc")" always returns is second argument (the first list provided). When only side-effecting is required "
			       ((:span :class "function")"mapc")" may be a better option than "((:span :class "macro")"dolist")" because is can traverse multiple lists in parallel"))
			 ((:div :class "main-page-item")
			  (str (repl-example (the repl-7))))))))
			     

   (repl-4 (list (list :command (list "
```

---

## iteration-and-mapping.lisp - plus2
Source: gornschool-training/t2/source/iteration-and-mapping.lisp
Type: tutorial

```
(defun plus2(i)"
				      "        (+ i 2))")
		       :output "PLUS2")
		 (list :command "(mapcar #'plus2 (list 1 2 3))"
		       :output "(3 4 5)")
		 (list :command (list "
```

---

## iteration-and-mapping.lisp - list-product
Source: gornschool-training/t2/source/iteration-and-mapping.lisp
Type: tutorial

```
(defun list-product (a b)"
				      "        (* a b))")
		      :output "LIST-PRODUCT")
		 (list :command "(mapcar #'list-product (list 1 2 3) (list 4 5 6))"
		       :output "(4 10 18)")))
   
   (repl-5 (list (list :command (list "(flatten" 
				      "       (mapcar"
				      "         #'(lambda(x y) (list x y))"
				      "            (list 1 2 3 4 5)"
				      "            (list \"a\" \"b\" \"c\" \"d\" \"e\")))")
		       :output "(1 \"a\" 2 \"b\" 3 \"c\" 4 \"d\" 5 \"e\")")
		 (list :command "mapcar #'(lambda(x y) (* x y) (list 1 2 3) (list 4 5 6))"
		       :output "(4 10 18)")))

   (repl-6 (list (list :command "(mapcan #'list (list 1 2 3 4 5) (list \"a\" \"b\" \"c\" \"d\" \"e\"))"
		 :output "(1 \"a\" 2 \"b\" 3 \"c\" 4 \"d\" 5 \"e\")")))

   (repl-7 (list (list :command (list "(let ((x 0))"
				      "          (mapc #'(lambda(a) (setf x (+ x a))) (list 1 2 3))"
				      "       x)")
		       :output 6)
		 (list :command (list "(let ((x 0))"
				      "         (mapc #'(lambda(a b c) (setf x (+ x a b c)))" 
				      "             (list 1 2 3) "
				      "             (list 4 5 6)" 
				      "             (list 7 8 9))"
				      "       x)")
		       :output 45)))))
		      


```

---

## equality.lisp - header
Source: gornschool-training/t2/source/equality.lisp
Type: tutorial

```
(in-package :training-2)


```

---

## equality.lisp - equality
Source: gornschool-training/t2/source/equality.lisp
Type: tutorial

```
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

```

---

## tutorial-intro.lisp - header
Source: gornschool-training/t2/source/tutorial-intro.lisp
Type: tutorial

```
(in-package :training-2)


```

---

## tutorial-intro.lisp - tutorial-intro
Source: gornschool-training/t2/source/tutorial-intro.lisp
Type: tutorial

```
(define-object tutorial-intro (base-training-sheet)
  :input-slots
  (index-url)
  
  :computed-slots
  ((setq-repl (list (list :command "(setq a 1)"
			  :output 1)
		    (list :command "(setq b 2 c \"three\" d nil)"
			  :output "NIL")
		    (list :command "b"
			  :output 2)
		    (list :command "c"
			  :output "three")
		    (list :command "d"
			  :output "NIL")))

   (code-1 (list
	    "
```

---

## tutorial-intro.lisp - my-box-1
Source: gornschool-training/t2/source/tutorial-intro.lisp
Type: tutorial

```
(define-object my-box-1 (box)"
	    " :input-slots"
	    " ((length 2)"
	    "  (width 3)"
	    "  (height 4)))"))


   (body-content (with-cl-who-string ()
		
                   (wmd "This tutorial aims to get you started with GendL.
It assumes a standard GendL installation and setup which provides
Emacs and will start the GendL web server on localhost port 9000.

The tutorial makes use of GendL's built-in wireframe geometry (the
`geom-base` package) to demonstrate some of the basic concepts of
GendL. It also covers most of the Common Lisp (CL) basics that you
will need to master in order to work with GendL effectively. What you
will _not_ need is to become a CL expert or specialist in order to
excel as a GendL user, as GendL does much of the language \"heavy
lifting\" for you.

Most of the topics covered in this tutorial have either `.lisp` or PDF
files which you can download and/or use. These are presented in the
References section on the right-hand sidebar of each page. Any `.lisp`
files with valid ANSI Common Lisp (CL) code sould be compileable in GendL")


                   (:p "This tutorial uses the following color-coding and font convention for
identifying categories of Lisp symbols when they occur in the narrative portion of the text (don't worry if
you don't understand the meaning of all these terms yet):"
                       (:ul (:li (:span :class "function" "Function") " - a CL or GendL Function name")
			    (:li (:span :class "macro" "Macro") " - a CL or GendL Macro name")
                            (:li (:span :class "object" "Object Definition name")
                                 " - a symbol being used to name a Gendl object definition")
			    (:li (:span :class "object-keyword" "Object Definition Keyword")
                                 " - one of the valid Keywords which can be used in any GendL object definitions")
			    (:li (:span :class "slot" "Slot Name")
                                 " - a name of a slot specified in a particular GendL object definition")
			    (:li (:span :class "special-operator" "Special Operator") " - CL special operators")
                            (:li (:span :class "variable-name" "Variable Name") " - symbol being used as a variable name")
                            (:li (:span :class "package-name" "Package Name") " - symbol being used to name a CL Package")
                            (:li (:span :class "general-keyword" "General Keyword") " - keyword symbol used for its name only")
                            (:li (:span :class "value" "Value")
                                 " Any Lisp data value, e.g. a Number, String, List, or Gendl Object
instance. Such values are typically obtained by calling a function or
sending a message to an object.")))

                        
		   (:p "Examples in the CL Read-Eval-Print-Loop are shown in sections of the screen as shown below")
		      
		       
		   (str (repl-example (the setq-repl)))
		      
		   (:p "Code examples are shown in sections of the screen as shown below and will generally be downloadable from the References, as shown on this page")

		   (str (code-example (the code-1)))
		      
		   (:p "Each topic page has Previous and Next navigation links as well as a link back to the Home page which contains a list of all topics in this tutorial. Note that most pages topics build on what was covered in previous topics so its recomended to work through the pages sequentially")
		   (:p "Finally an index is provided with links to indexed words reference in each topic. The index may be accessed from the home page. To view the index now click "
		       ((:a :href (the index-url)) "here"))
                   
		 (:div :class "main-page-item"
		       (:h2 "Resources")
		       (str (the resource-links)))))))

  

```

---

## wall-example.lisp - header
Source: gornschool-training/t2/source/wall-example.lisp
Type: tutorial

```
(in-package :training-2)


```

---

## wall-example.lisp - wall-example
Source: gornschool-training/t2/source/wall-example.lisp
Type: tutorial

```
(define-object wall-example (base-training-sheet)
  :computed-slots
  ((index-words (list ":pass-down" "list-elements" "the-element" "the-object" "theo" "apply" "+" "apply '+" "zerop" "evenp" "last (sequence)"
		      "let" "local binding" "translate-along-vector" "face-center" "face-normal-vector" ":functions" "debugging" "code maintenance" "code readability"))

   (hint-1 nil :settable)
   (hint-2 nil :settable)
   (hint-3 nil :settable)
   (hint-4 nil :settable)
   (hint-5 nil :settable)
   (hint-6 nil :settable)
   (hint-7 nil :settable)

   
   
   (code-1 (list "
```

---

## wall-example.lisp - wall
Source: gornschool-training/t2/source/wall-example.lisp
Type: tutorial

```
(define-object wall()"
		 "  :objects"
		 "  ((row :type 'row))"
		 " )"
		 ""
		 "
```

---

## wall-example.lisp - row
Source: gornschool-training/t2/source/wall-example.lisp
Type: tutorial

```
(define-object row ()"
		 "  :objects"
		 "  ((bricks-and-mortar :type 'bricks-and-mortar)"
		 "   (mortar-bed :type 'box)))"
		 ""
		 "
```

---

## wall-example.lisp - bricks-and-mortar
Source: gornschool-training/t2/source/wall-example.lisp
Type: tutorial

```
(define-object bricks-and-mortar ()"
		 "  :objects"
		 "  ((full-brick :type 'box)"
		 "   (half-brick :type 'box)"
		 "   (mortar-joint :type 'box)))"))

   (code-2 (list "
```

---

## wall-example.lisp - bricks-and-mortar
Source: gornschool-training/t2/source/wall-example.lisp
Type: tutorial

```
(define-object bricks-and-mortar ()"
		 "  :input-slots"
		 "  (brick-height"
		 "   brick-length"
		 "   brick-width"
		 "   mortar-joint-width)"
		 ""
		 "  :objects"
		 "  ((full-brick :type 'box"
		 "	           :length (the brick-length)"
		 "	           :height (the brick-height)"
		 "	           :width (the brick-width))"
		 "   (half-brick :type 'box"
		 "	           :length (half (the brick-length))"
		 "	           :height (the brick-height)"
		 "	           :width (the brick-width))"
		 "   (mortar-joint :type 'box"
		 "		     :height (the brick-height)"
		 "		     :width (the brick-width)"
		 "		     :length (the mortar-joint-width))))"))

   (code-3 (list "
```

---

## wall-example.lisp - row
Source: gornschool-training/t2/source/wall-example.lisp
Type: tutorial

```
(define-object row ()"
		 "  :input-slots"
		 "  (brick-height"
		 "   brick-length"
		 "   brick-width"
		 "   mortar-joint-width)"
		 ""
		 "  :objects"
		 "  ((bricks-and-mortar :type 'bricks-and-mortar"
		 "  		        :brick-height (the brick-height)"
		 "  		        :brick-length (the brick-length)"
		 "  		        :brick-width (the brick-width)"
		 "  		        :mortar-joint-width (the mortar-joint-width))"
		 "   (mortar-bed :type 'box"
		 "	         :height (the mortar-joint-width))))"))

   (code-4 (list "
```

---

## wall-example.lisp - wall
Source: gornschool-training/t2/source/wall-example.lisp
Type: tutorial

```
(define-object wall()"
		 "  :input-slots"
		 "  ((brick-height 45)"
		 "   (brick-length 180)"
		 "   (brick-width 90)"
		 "   (mortar-joint-width 10)"
		 "   (wall-length 3700)"
		 "   (wall-height 900))"
		 "   "
		 "  :objects"
		 "  ((row :type 'row"
		 "        :brick-height(the brick-height)"
		 "        :brick-length (the brick-length)"
		 "        :brick-width (the brick-width)"
		 "        :mortar-joint-width (the mortar-joint-width))))"))

   (code-5 (list "
```

---

## wall-example.lisp - wall
Source: gornschool-training/t2/source/wall-example.lisp
Type: tutorial

```
(define-object wall(box)"
		 ":input-slots"
		 "((brick-height 45)"
		 " (brick-length 180)"
		 " (brick-width 90)"
		 " (mortar-joint-width 10)"
		 " (wall-length 3700)"
		 " (wall-height 900))"
		 ""
		 ":computed-slots"
		 "((row-height (+ (the brick-height) (the mortar-joint-width)))"
		 " (number-of-rows (round-to-nearest (div (the wall-height) (the row-height)) 1))"
		 " (actual-wall-height (* (the row-height) (the number-of-rows)))"
		 ""
		 " ;; for the wall-length we need the number of full bricks"
		 " ;; if there are n full bricks then there will be (n-1) mortar joints"
		 " ;; so n*brick-length + n-1*mortar-joint-width = overall-length"
		 " ;; or n(brick-length + mortar-join-width) - mortar-joint-width = overall-length"
		 " ;; or n = (overall-length - mortar-joint-width)/(brick-length + mortar-joint-width)"
		 " (number-of-bricks (round-to-nearest "
		 "                       (div (- (the wall-length) (the mortar-joint-width))"
		 "			      (+ (the brick-length) (the mortar-joint-width)))"
		 "			 1))"
		 " (actual-wall-length (+ (* (the number-of-bricks) (the brick-length))"
		 "			  (* (- (the number-of-bricks) 1) (the mortar-joint-width))))"
		 ""
		 " ;; box inputs - gives the wall bounding box"
		 " (height (the actual-wall-height))"
		 " (width (the brick-width))"
		 " (length (the actual-wall-length)))"
		 " "
		 ":objects"
		 "((row :type 'row"
		 "      :sequence (:size (the number-of-rows))"
		 "      :bricks-per-row (the number-of-bricks)"
		 "      :length (the length)"
		 "      :width (the width)"
		 "      :brick-height(the brick-height)"
		 "      :brick-length (the brick-length)"
		 "      :brick-width (the brick-width)"
		 "      :mortar-joint (the mortar-joint-width))))"))

   (code-6 (list "
```

---

## wall-example.lisp - wall
Source: gornschool-training/t2/source/wall-example.lisp
Type: tutorial

```
(define-object wall(box)"
		 "..."
		 "..."
		 "..."
		 ":objects"
		 "((row :type 'row"
		 "      :sequence (:size (the number-of-rows))"
		 "      :center (translate-along-vector (the (face-center :bottom))"
		 "				       (the (face-normal-vector :top))"
		 "				       (+ (half (the-child height))"
		 "					  (* (the-child index) (the-child height))))"
		 "      :length (the length)"
		 "      :width (the width)"
		 "      :height (+ (the brick-height) (the mortar-joint-width)))"
		 "      :bricks-per-row (the number-of-bricks)"
		 "      :brick-height(the brick-height)"
		 "      :brick-length (the brick-length)"
		 "      :brick-width (the brick-width)"
		 "      :mortar-joint-width (the mortar-joint-width))"))

   (code-7 (list "
```

---

## wall-example.lisp - row
Source: gornschool-training/t2/source/wall-example.lisp
Type: tutorial

```
(define-object row (box)"
		 ":input-slots"
		 "(brick-height"
		 " brick-length"
		 " brick-width"
		 " mortar-joint-width)"
		 ""
		 ":objects"
		 "((bricks-and-mortar :type 'bricks-and-mortar"
		 "		      :width (the width)"
		 "		      :length (the length)"
		 "		      :height (the brick-height)"
		 "		      :center (translate-along-vector "
		 "                               (the mortar-bed (face-center :top))"
		 "				 (the mortar-bed (face-normal-vector :top))"
		 "				 (half (the-child height)))"
		 "		      :brick-height (the brick-height)"
		 "		      :brick-length (the brick-length)"
		 "		      :brick-width (the brick-width)"
		 "		      :mortar-joint-width (the mortar-joint-width))"
		 " (mortar-bed :type 'box"
		 "	       :height (the mortar-joint-width)"
		 "	       :width (the width)"
		 "	       :length (the length)"
		 "	       :center (translate-along-vector (the (face-center :bottom))"
		 "					       (the (face-normal-vector :top))"
		 "					       (half (the-child height))))))"))


   (code-8 (list "
```

---

## wall-example.lisp - row
Source: gornschool-training/t2/source/wall-example.lisp
Type: tutorial

```
(define-object row (box)"
		 ":input-slots"
		 "(full-bricks-per-row"
		 " brick-height"
		 " brick-length"
		 " brick-width"
		 " mortar-joint-width)"
		 ""
		 ":computed-slots"
		 " ((full-brick-row? (or (zerop (the index)) (evenp (the index)))))"
		 ""
		 ":objects"
		 "((bricks-and-mortar :type 'bricks-and-mortar"
		 "		      :height (the brick-height)"
		 "		      :center (translate-along-vector "
		 "                                     (the mortar-bed (face-center :top))"
		 "				       (the mortar-bed (face-normal-vector :top))"
		 "				       (half (the-child height)))"
		 "		      :pass-down (width"
		 "				  length"
		 "				  full-brick-row?"
		 "				  brick-height"
		 "				  brick-length"
		 "				  brick-width"
		 "				  mortar-joint-width"
		 "				  full-bricks-per-row))"
		 "  " 
		 " (mortar-bed :type 'box"
		 "	       :height (the mortar-joint-width)"
		 "	       :center (translate-along-vector (the (face-center :bottom))"
		 "					       (the (face-normal-vector :top))"
		 "					       (half (the-child height)))"
		 "	       :pass-down (width"
		 "			   length))))"))

   (code-9 (list "
```

---

## wall-example.lisp - bricks-and-mortar
Source: gornschool-training/t2/source/wall-example.lisp
Type: tutorial

```
(define-object bricks-and-mortar (box)"
		 "..."
		 "..."
		 "..."
		 ":computed-slots"
		 "((first-full-brick-start-point (if (the full-brick-row?)"
		 "				     (the (face-center :front))"
		 "				     (the (mortar-joint 0) (face-center :rear))))"
		 ""
		 " (first-mortar-joint-start-point (if (the full-brick-row?)"
		 "				       (the (full-brick 0) (face-center :rear))"
		 "				       (the (half-brick 0) (face-center :rear))))"
		 ""
		 " (number-of-full-bricks (if (the full-brick-row?)"
		 "			      (the full-bricks-per-row)"
		 "			      (- (the full-bricks-per-row) 1)))"
		 ""
		 " (number-of-mortar-joints (if (the full-brick-row?)"
		 "				(- (the number-of-full-bricks) 1)"
		 "				(+ (the number-of-full-bricks) 1)))"
		 ""
		 "   ;; if it isn't a full brick row then there will be an extra joint because one "
		 "   ;; full brick is replaced with 2 half bricks so without correcting the "
		 "   ;; mortar-joint-width the ends of a full brick rowand one startng and"
		 "   ;;  finishing with half bricks won't align. So we need to correct "
		 "   ;; the mortar-joint-width"
		 " (corrected-joint-width (if (the full-brick-row?)"
		 "		              (the mortar-joint-width)"
		 "		              (let ((total-gap (* (- (the number-of-mortar-joints) 1)"
		 "					          (the mortar-joint-width))))"
		 "                                 (div total-gap (the number-of-mortar-joints))))))"
		 "..."
		 "..."
		 ")"))

   (code-10 (list "
```

---

## wall-example.lisp - bricks-and-mortar
Source: gornschool-training/t2/source/wall-example.lisp
Type: tutorial

```
(define-object bricks-and-mortar (box)"
		  "..."
		  "..."
		  "..."
		  ":functions"
		  "((first-full-brick-center!"
		  "   ()"
		  "   (translate-along-vector (the first-full-brick-start-point)"
		  "			   (the (face-normal-vector :rear))"
		  "			   (half (the brick-length))))"
		  ""
		  " (other-full-brick-center!"
		  "   (index)"
		  "   ;; if its a full brick row, (full-brick 1) is positioned relative to (joint 0)"
		  "   ;; if its a half brick row, (full-brick 1) is positioned relative to (joint 1)"
		  "   (let ((ind (if (the full-brick-row?) (- index 1) index)))"
		  "      (translate-along-vector (the (mortar-joint ind) (face-center :rear))"
		  "			      (the (face-normal-vector :rear))"
		  "			      (half (the brick-length)))))"
		  ""
		  " (first-joint-center!"
		  "   ()"
		  "   (translate-along-vector (the first-mortar-joint-start-point)"
		  "			    (the (face-normal-vector :rear))"
		  "			    (half (the corrected-joint-width))))"
		  ""
		  " (other-joint-center!"
		  "   (index)"
		  "   ;; if its a full brick row, (joint 1) is positioned relative to (full-brick 1)"
		  "   ;; if its a half brick row, (joint 1) is positioned relative to (full-brick 0)"
		  "   (let ((ind (if (the full-brick-row?) index (- index 1))))"
		  "      (translate-along-vector (the (full-brick ind) (face-center :rear))"
		  "			      (the (face-normal-vector :rear))"
		  "			      (half (the corrected-joint-width)))))"
		  ""
		  " (first-half-brick-center!"
		  "   ()"
		  "   (translate-along-vector (the (face-center :front))"
		  "			   (the (face-normal-vector :rear))"
		  "			   (half (half (the brick-length)))))"
		  ""
		  "  (last-half-brick-center!"
		  "    ()"
		  "    (translate-along-vector (theo (the mortar-joint last) (face-center :rear))"
		  "			    (the (face-normal-vector :rear))"
		  "			    (half (half (the brick-length))))))"
		  "..."
		  "..."
		  ")" ))
   
   (code-11 (list "
```

---

## wall-example.lisp - bricks-and-mortar
Source: gornschool-training/t2/source/wall-example.lisp
Type: tutorial

```
(define-object bricks-and-mortar (box)"
		  "..."
		  "..."
		  "..."
		  " :objects"
		  " ((full-brick :type 'box"
		  "	         :sequence (:size (the number-of-full-bricks))"
		  "	         :center (if (= (the-child index) 0)"
		  "			   (the first-full-brick-center!)"
		  "			   (the (other-full-brick-center! (the-child index))))"
		  "              :length (the brick-length)"
		  "              :height (the brick-height)"
		  "	         :width (the brick-width))"
		  ""
		  "  (half-brick :type 'box"
		  "	         :sequence (:size (if (the full-brick-row?) 0 2))"
		  "	         :center (if (= (the-child index) 0)"
		  "		   	     (the first-half-brick-center!)"
		  "			     (the last-half-brick-center!))"
		  "	         :length (half (the brick-length))"
		  "	         :height (the brick-height)"
		  "	         :width (the brick-width))"
		  ""
		  "  (mortar-joint :type 'box"
		  "		   :sequence (:size (the number-of-mortar-joints))"
		  "		   :center (if (= (the-child index) 0)"
		  "			       (the first-joint-center!)"
		  "			       (the (other-joint-center! (the-child index))))  " 
		  "		   :height (the brick-height)"
		  "		   :width (the brick-width)"
		  "		   :length (the corrected-joint-width))))"))

   (code-12 (list "
```

---

## wall-example.lisp - wall
Source: gornschool-training/t2/source/wall-example.lisp
Type: tutorial

```
(define-object wall(box)"
		  "  :computed-slots"
		  "  ((full-bricks (apply '+ (list-elements (the row) (the-element full-bricks))))"
		  "   (half-bricks (apply '+ (list-elements (the row) (the-element half-bricks))))"
		  "   (mortar-volume (apply '+ (list-elements (the row) (the-element mortar-volume))))"
		  "   (mortar-density 2162)"
		  "   (mortar-mass (* (the mortar-density) (div (the mortar-volume) 1000000000)))))"
		  ""
		  "
```

---

## wall-example.lisp - row
Source: gornschool-training/t2/source/wall-example.lisp
Type: tutorial

```
(define-object row (box)"
		  "  :computed-slots"
		  "  ((full-bricks (the bricks-and-mortar full-bricks))"
		  "   (half-bricks (the bricks-and-mortar half-bricks))"
		  "   (mortar-volume (+ (the bricks-and-mortar mortar-joint-volume)"
		  "		     (the mortar-bed volume)))))"
		  ""
		  "
```

---

## wall-example.lisp - bricks-and-mortar
Source: gornschool-training/t2/source/wall-example.lisp
Type: tutorial

```
(define-object bricks-and-mortar (box)"
		  "  :computed-slots"
		  "  ("
		  "   ;; collating the output. We could do this analytically,"
		  "   ;; but for this example we'll use the geometry"
		  "   (full-bricks (length (list-elements (the full-brick))))"
		  "   (half-bricks (length (list-elements (the half-brick))))"
		  "   (mortar-joint-volume (apply '+ (list-elements (the mortar-joint)"
		  "                                              (the-element volume))))))"))

   (repl-1 (list (list :command "(setq self (make-object 'wall))"
		       :output "#<GDL-USER::WALL #x210582B91D>")
		 
		 (list :command "(the full-bricks)"
		       :output 296)
		 (list :command "(the half-bricks)"
		       :output 16)
		 (list :command "(the mortar-volume)"
		       :output 6.3504E+7)
		 (list :command "(the mortar-mass)"
		       :output 137.295648)))


   (body-content (with-cl-who-string()
		      
		   ((:div :class "main-page-container")
		    ((:div :class "main-page-item")
		     (str (the start-section main-div))
		     (str (the hint-1-section main-div))
		     (str (the hint-2-section main-div))
		     (str (the hint-3-section main-div))
		     (str (the hint-4-section main-div))		
		     (str (the hint-5-section main-div))
		     (str (the hint-6-section main-div))
		     (str (the hint-7-section main-div)))
		    
		    ((:div :class "main-page-item")
		     (:h2 "Resources")
		     (str (the resource-links-section main-div)))))))

  :functions
  (
   ;;
   ;; FLAG - reduce these repetitive slots somehow.
   ;;
   (hint-1! () (the (set-slot! :hint-1 (not (the hint-1)))))
   (hint-2! () (the (set-slot! :hint-2 (not (the hint-2)))))
   (hint-3! () (the (set-slot! :hint-3 (not (the hint-3)))))
   (hint-4! () (the (set-slot! :hint-4 (not (the hint-4)))))
   (hint-5! () (the (set-slot! :hint-5 (not (the hint-5)))))
   (hint-6! () (the (set-slot! :hint-6 (not (the hint-6)))))
   (hint-7! () (the (set-slot! :hint-7 (not (the hint-7)))))


   )

  :objects
  ((start-section
    :type 'sheet-section
    :inner-html (with-cl-who-string ()
		  (:div :class "grid-container-2-650px"
			(:div :class "grid-item"
			      (:p "Up to now we have covered the basics of "
			          (:ul
                                   (:li "Defining & Instantiating GendL objects")
				   (:li "Positioning and orienting geometry")
				   (:li "Defining child objects and sequences of child objects")
				   (:li "Dealing with numbers and lists")
				   (:li "Defining standard CL functions and GendL object functions"))
			          "This tutorial aims to bring the above concepts together with a
tangible example. Starting with the example briefing below, you can
either try to develop your own solution or build up a solution in a
guided step-by-step manner by clicking the Hint button")
			      (:h3 "Example Briefing")
			      (:p "Imagine a wall is built from bricks which
are 180mm long, 45mm high and 90mm wide. Each course (i.e. row) of
bricks sits on a bed of mortar which is 10mm thick, and each brick is
joined to its lateral neighbor(s) with mortar having a nominal joint
thickness of 10mm. While a wall instance can be specified using
nominal dimensions for length and height, the actual dimensions may
vary slightly in order to use a whole number of bricks per
row. Vertically adjacent rows of bricks are offset by half a
brick with respect to the row below and so will consist of a
half-brick at the beginning and a half-brick at the end of the
row (i.e. row).")

                              (:p "Define and instantiate a GendL model of a wall which has a default
nominal height of 900mm and a nominal length of 3700mm, and report
the number of full bricks and number of half bricks used. Assuming the
density of mortar is 2182kg/m"(:sup "3") " determine also the mass of
mortar used")))
                  
		  (:p (str (the (hint-button :function-key :hint-1!))))))
                  
   (hint-1-section
    :type 'sheet-section
    :inner-html (with-cl-who-string()
		  (when (the hint-1)
		    (htm
		     (:div :class "grid-container-2-650px"
			   (:div :class "grid-item"
				 (:p "Start by thinking of the structure"
				     (:ul (:li "A wall has rows of bricks and mortar.")
					  (:li "A row of bricks and morter has a row of bricks seperated by mortar joints, sat on a bed of mortar")
					  (:li "A row of bricks seperated by mortar joints is made of full bricks and half bricks and mortar")
					  (:li "A bed of mortar sits below every row of bricks separated by mortar joints"))
				     "And from that conceptual description, lay down a definition for an
outline object structure. Note that while as yet it contains neither dimensions
nor mixins, at its leaf level it is already using GendL wireframe geometry primitives")))
                     
		     (str (code-example (the code-1)))
                     
		     (str (the (hint-button :function-key :hint-2!)))))))
                                            
   (hint-2-section
    :type 'sheet-section
    :inner-html (with-cl-who-string()
		  (when (the hint-2)
		    (htm
		     (:div :class "grid-container-2-650px"
			   (:div :class "grid-item"
				 (:p "Working from the bottom with the GendL leaf instances of "
				     ((:span :class "object")"brick")", "
				     ((:span :class "object")"half-brick")" and "
				     ((:span :class "object")"mortar-joint")" (all standard "
                                     (:span :class "object" "box") "es), determine appropriate referencing expressions for their
dimensional inputs. This will also inform as to what "
                                     (:span :class "object-keyword" ":input-slots")
                                     " you will need coming into the "
                                     ((:span :class "object") "bricks-and-mortar")" parent object.")
				 (str (code-example (the code-2)))
				 (:p "Then move up to the "((:span :class "object") "row")" object and repeat")
				 (str (code-example (the code-3)))
				 (:p "Then move up to the "
				     ((:span :class "object")"wall")" object and repeat, but with this object add the default values for "
				     ((:span :class "object-keyword")":input-slots")", and the other 2 known inputs "
				     ((:span :class "slot")"wall-height")" and "
				     ((:span :class "slot")"wall-length"))
				 (str (code-example (the code-4)))))
                     
		     (:p (str (the (hint-button :function-key :hint-3!))))))))

   (hint-3-section :type 'sheet-section
		   :inner-html (with-cl-who-string()
				 (when (the hint-3)
				   (htm
				    (:div :class "grid-container-2-650px"
					  (:div :class "grid-item"
						(:p "Now working from the top down you need to determine the actual overall dimensions of the "
					    ((:span :class "object")"wall")" and from the the number of "
					    ((:span :class "object")"rows")". If you use a "
					    ((:span :class "object")"box")" mixin for the "
					    ((:span :class "object")"wall")
                                            " object and specify its "
                                            (:span :class "slot" "width") ", "
                                            (:span :class "slot" "length") ", and "
                                            (:span :class "slot" "height")                                             
                                            " appropriately (and leaving "
                                            (:span :class "slot" "center") " and "
                                            (:span :class "slot" "orientation")
                                            " to their default values for now), then you can probe and display its reference box i.e. bounding-box. Having calculated the "
					    ((:span :class "slot")"number-of-rows")", you can also set the "
					    ((:span :class "object-keyword")":sequence (:size )")" for the "
					    ((:span :class "object")"row")" object and additionally pass in the "
					    ((:span :class "slot")"bricks-per-row")", "
					    ((:span :class "slot")"width")" and "
					    ((:span :class "slot")"length")". Compiling "
					    ((:span :class "object")"wall")" and displaying in Geysr, you can show the overall bounding box and see the sequence of "
					    ((:span :class "object")"row")" objects"))
					 (:div :class "grid-item") 
					(:div :class "grid-item"
					  (str (code-example (the code-5))))
					(:div :class "grid-item"
					  (:img :src (format nil "/~a-images/geysr-wall-1.png" (the publish-prefix)) :style "width: auto; height: 300px; margin: 1em 0 1em 3% ;" )))
				    (:p (str (the (hint-button :function-key :hint-4!))))))))
                                                               
   (hint-4-section :type 'sheet-section
		   :inner-html (with-cl-who-string()
				 (when (the hint-4)
				   (htm
				    (:div :class "grid-container-2-650px"
					  (:div :class "grid-item"
						(:p "The next task is to position each row of bricks, first by positioning the bounding box for each "
					    ((:span :class "object")"row")". Each "
					    ((:span :class "object")"row")" is "
					    ((:span :class "slot")"(the brick-height)")" + "
					    ((:span :class "slot")"(the mortar-jpint-width)")" high. The center of the first "
					    ((:span :class "object")"row")" is half of this dimension above the bottom of the "
					    ((:span :class "object")"wall")", and the center of each subsequent "
					    ((:span :class "object")"row")" is the "
					    ((:span :class "object")"row")" height above the center of the previous "
					    ((:span :class "object")"row")". Updating the definition of "
					    ((:span :class "object")"row")" to use "
					    ((:span :class "object")"box")" as its mixin and then specifying "
					    ((:span :class "object-keyword")":center")" and "
					    ((:span :class "object-keyword")":height")" is sufficient to define and position each "
					    ((:span :class "object")"row")" of the "
					    ((:span :class "object")"wall")". The code below shows just the update to the definition of the "
					    ((:span :class "object")"row")" child object of "
					    ((:span :class "object")"wall")))
					 (:div :class "grid-item") 
					(:div :class "grid-item"
					  (str (code-example (the code-6))))
					 (:div :class "grid-item"
					  (:img :src (format nil "/~a-images/geysr-wall-2.png"  (the publish-prefix)):style "width: auto; height: 300px; margin: 1em 0 1em 3% ;" )))
					(:p (str (the (hint-button :function-key :hint-5!))))))))

   (hint-5-section :type 'sheet-section
		   :inner-html (with-cl-who-string()
				 (when (the hint-5)
				   (htm
				    (:div :class "grid-container-2-650px"
					  (:div :class "grid-item"
						(:p "Now you can move down to the "
					    ((:span :class "object")"row")" level and position the "
					    ((:span :class "object")"bricks-and-mortar")" object plus the "
					    ((:span :class "object")"mortar-bed")" object. The "
					    ((:span :class "object")"mortar-bed")" is positioned relative to the "
					    ((:span :class "object-keyword")":bottom")" face of the "
					    ((:span :class "object")"row")" and the "
					    ((:span :class "object")"bricks-and-mortar")" is positioned relative to the "
					    ((:span :class "object-keyword")"top")" face of the "
					    ((:span :class "object")"mortar-bed")))
					  (:div :class "grid-item")
					(:div :class "grid-item"
					  (str (code-example (the code-7))))
					 (:div :class "grid-item"
					  (:img :src (format nil "/~a-images/geysr-wall-3.png" (the publish-prefix)) :style "width: auto; height: 200px; margin: 1em 0 1em 3% ;" )))
				    (:p (str (the (hint-button :function-key :hint-6!))))))))
   
   (hint-6-section :type 'sheet-section
		   :inner-html (with-cl-who-string()
				 (when (the hint-6)
				   (htm
				    (:div :class "grid-container-2-650px"
					  (:div :class "grid-item"
						(:p "Moving on to the actual "
					    ((:span :class "object") "bricks-and-mortar")
                                            ", you first need to determine if the row of bricks is made of full bricks only or starts and ends
with half bricks. You can do this using the Lisp predicates "
					    ((:span :class "function")"zerop")" and "
					    ((:span :class "function")"evenp")" to test the "
					    ((:span :class "object")"row")" object index number. If it is zero or an even number then the "
					    ((:span :class "object")"row")" will be made of full bricks only. One other change made to the "
					    ((:span :class "object")"row")" definition is to use "
					    ((:span :class "object-keyword")":pass-down")" in the object definitions for "
					    ((:span :class "object")"bricks-and-morter")" and "
					    ((:span :class "object")"morter-bed")". When a parent slot has the same name as a child objects "
					    ((:span :class "object-keyword")":input-slot")" you can use "
					    ((:span :class "object-keyword")":pass-down")" and just provide a list of the slot names avoiding having to map the "
					    ((:span :class "object-keyword")":input-slot")" to the parents slot with the "
					    ((:span :class "macro")"the")" macro")
					(str (code-example (the code-8)))
					(:p "you need to add the input-slot full-brick-row? to the bricks-and-morter object and start to thnk about positioning the full-brick, half-brick and mortar-joint objects. If you consider a row of full bricks"))
					  (:div :class "grid-item")
					  (:div :class "grid-item"
					  (str (code-example (the code-9))))
					 (:div :class "grid-item"
					  (:p "This section of code for "
					      ((:span :class "object")"bricks-and-mortar")" covers just the "
					      ((:span :class "object-keyword")":computed-slots")". They could quite easily be defined as part of the child object definitions but defining them as "
					      ((:span :class "object-keyword")":computed-slots")" helps with debugging and makes the code more maintainable. There may be a slight performance hit in doing this but that can be dealt with later if required. Its one of the great things about GendL and Lisp, start of by writing code fast, then if theres and issue transform it to fast code. A detail to consider is the width of the mortar joint. In a full brick row, with N full bricks there are N-1 joints, but if the row starts and ends with half bricks an extra joint will be included. If you maintain the joint width, the end of a row starting and ending with half bricks will not be alligned with the end of a full brick row, so you have to reduce the mortar joint for rows starting and ending with half bricks. In the "
					      ((:span :class "object-keyword")":computed-slot")" "
					      ((:span :class "slot")"corrected-joint-width")" the Lisp special operator "
					      ((:span :class "special-operator")"let")" has been used to enable a local binding to be created. We'll cover local bindings in a later tutorial, but for now its just like a temporary variable only available to the slot its defined in"))
					(:div :class "grid-item"
					  (str (code-example (the code-10))))
					(:div :class "grid-item"
					  (:p "This section of code for "
					      ((:span :class "object")"bricks-and-mortar")" covers just the "
					      ((:span :class "object-keyword")":functions")". These "
					      ((:span :class "object-keyword")":functions")" are used in the child objects to define the "
					      ((:span :class "object-keyword")":center")" of "
					      ((:span :class "object")"full-brick")", "
					      ((:span :class "object")"half-brick")" and "
					      ((:span :class "object")"mortar-joint")". As above, this is implemented more for convenience and code managability than technical requirement"
					      (:ul (:li "The first "
							((:span :class "object")"full-brick")" (index = 0) will be positioned relative to the front face of the parents ("
							((:span :class "object")"bricks-and-mortar")") bounding box.")
						   (:li "The first "
							((:span :class "object")"mortar-joint")" (index = 0) will be positioned relative to the rear face of the first (nth = 0) "
							((:span :class "object")"full-brick")".")
						   (:li "The second "
							((:span :class "object")"full-brick")" (index = 1) will be positioned relative to the rear face of the first "
							((:span :class "object")"mortar-joint")" (index=0).")
						   (:li "The second "
							((:span :class "object")"mortar-joint")" (index = 1) will be positioned relative to the rear face of the second "
							((:span :class "object")"full-brick")" (nth = 1).")
						   (:li "The third "
							((:span :class "object")"full-brick")" (index = 2) will be positioned relative to the rear face of the second "
							((:span :class "object")"mortar-joint")" (index = 1).")
						   (:li "And so on... You can see a pattern emerging that you can use to code the positioning"))
					      "When the row starts with a "
					      ((:span :class "object")"half-brick")" its slightly different"
					      (:ul (:li "The first "
						        ((:span :class "object")"half-brick")" (index = 0) will be positioned relative to the front face of the parents ("
						        ((:span :class "object")"bricks-and-mortar")") bounding box.")
						   (:li "The first "
						        ((:span :class "object")"mortar-joint")" (index = 0) will be positioned relative to the rear face of the first (nth = 0) "
						        ((:span :class "object")"half-brick")".")
						   (:li "The first "
						        ((:span :class "object")"full-brick")" (index = 0) will be positioned relative to the rear face of the first  "
						        ((:span :class "object")"mortar-joint")" (index = 0).")
						   (:li "The second "
						        ((:span :class "object")"mortar-joint")" (index = 1) will be positioned relative to the rear face of the first "
						        ((:span :class "object")"full-brick")" (nth = 0).")
						   (:li "The second "
						        ((:span :class "object")"full-brick")" (index = 1) will be positioned relative to the rear face of the second "
						        ((:span :class "object")"mortar-joint")" (index = 1).")
						   (:li "And so on.. Again you can see a pattern emerging that you can use to code the positioning")))
					  (:p "Some points to note"
					      (:ul (:li "The "
							((:span :class "object-keyword")":functions")" "
							((:span :class "function")"other-brick-center!")" and "
							((:span :class "function")"other-joint-center!")" use the special operator "
							((:span :class "special-operator")"let")" to create a local binding again, just to make the calculation easier to understand")
						   (:li "When calculating the reference point for the second "
							((:span :class "object")"half-brick")" ("
							((:span :class "function")"last-half-brick-center!")"), you send the message "
							((:span :class "slot")"last")" to the "
							((:span :class "object")"mortar-joint")" sequence to return the last "
							((:span :class "object")"mortar-joint")" in the sequence. As this returns an object, to get the "
							((:span :class "function")"(face-center :rear)")" value you need to use the "
							((:span :class "macro")"the-object")" or "
							((:span :class "macro")"theo")" macro instead of "
							((:span :class "macro")"the")
							(:li "As discussed in the Functions and :functions tutorial, the "
							     ((:span :class "object-keyword")":function")" names all end with the ! character. This is out of convention for readability rather than and technical need")))))
					(:div :class "grid-item"
					  (str (code-example (the code-11))))
					(:div :class "grid-item"
					  (:p "Finally, if you look at the "((:span :class "object")":objects")" definition, you can see that the use of the "
					      ((:span :class "function")":functions")" has enabled the readability of this section to be maintained. An interesting point to note is the definition of "
					      ((:span :class "object")"half-brick")", particularly the "
					      ((:span :class "general-keyword")":sequence (:size )")" value. When the row is a full brick row (ie no half bricks), the size is zero which means that this sequence contains no objects. This will need to be borne in mind when you come to collecting information about the number of "
					      ((:span :class "object")"full-bricks")" and "
					      ((:span :class "object")"half-bricks")" in the "
					      ((:span :class "object")"wall")))
					(:div :class "grid-item"
					(:p "You can now inspect the model in Geysr, first by drawing the first 2 "
					    ((:span :class "object")"full-bricks")" and "
					    ((:span :class "object")"mortar-joints")" from the bottom "
					    ((:span :class "object")"row")", then the whole bottom "
					    ((:span :class "object")"row")" of bricks, then the bottom "
					    ((:span :class "object")"row")" with the "
					    ((:span :class "object")"morter-bed")", then add the second "
					    ((:span :class "object")"row")" (showing the "
					    ((:span :class "object")"half-bricks")" to get the brick offset) and then finally the full "
					    ((:span :class "object")"wall"))
					(:img :src (format nil "/~a-images/geysr-wall-4.png" (the publish-prefix)) :style "width: auto; height: 200px; margin: 1em 0 1em 3% ;" ) (:br)
					(:img :src (format nil "/~a-images/geysr-wall-5.png" (the publish-prefix)) :style "width: auto; height: 200px; margin: 1em 0 1em 3% ;" )(:br)
					(:img :src (format nil "/~a-images/geysr-wall-6.png" (the publish-prefix)) :style "width: auto; height: 200px; margin: 1em 0 1em 3% ;" )(:br)
					(:img :src (format nil "/~a-images/geysr-wall-7.png" (the publish-prefix)) :style "width: auto; height: 200px; margin: 1em 0 1em 3% ;" )
					(:p "The final task is to use the geometry to determine how many full bricks and half bricks and what volume of morter are required ")))
					
					(:p (str (the (hint-button :function-key :hint-7!))))))))

   (hint-7-section :type 'sheet-section
		   :inner-html (with-cl-who-string()
				 (when (the hint-7)
				   (htm
				    (:div :class "grid-container-2-650px"
					  (:div :class "grid-item"
						(:p "You now need to gather the outputs. You can do this at the lowest ("
					    ((:span :class "object")"bricks-and-morter")") level and then collate them from the object sequences. Whilst
you could re-do the calculation explicitly, the model is already calculating these values implicitly anyway and you can use this fact to your advantage
by probing the model directly for the number of objects (for "
					    ((:span :class "object")"full-bricks")" and "
					    ((:span :class "object")"half-bricks")") in each "
					    ((:span :class "object")"row")" and getting a total "
					    ((:span :class "slot")"volume")" for the "
					    ((:span :class "object")"mortar-joint")" and "
					    ((:span :class "object")"mortar-bed")". The code extract below just shows the additional "
					    ((:span :class "object-keywords")":computed-slots")" required in each object to achieve this.")
					(:p "Note the use of the "
					    ((:span :class "function")"apply")" function to sum the "
					    ((:span :class "slot")"mortar-joint-volume")" in "
					    ((:span :class "object")"bricks-and-mortar")" and to sum "
					    ((:span :class "slot")"full-bricks")", "
					    ((:span :class "slot")"half-bricks")" and "
					    ((:span :class "slot")"mortar-volume")" in "
					    ((:span :class "object")"wall")". We use "
					    ((:span :class "function")"apply")" in conjunction with the "
					    ((:span :class "function")"+")" function to sum the elements of the list returned by ("
					    ((:span :class "macro") "list-elements")" [object] ("((:span :class "macro")"the-element")
                                            " [slot])). There are more efficient ways of summing and otherwise summarizing values from sequences and trees of GendL
objects, but the "
                                            (:span :class "function" "apply")
                                            " technique shown here has the advantage of being simple and generally applicable."))
					  (:div :class "grid-item")
					  (:div :class "grid-item"
				          (str (code-example (the code-12))))
				        (:div :class "grid-item"
				          (str (repl-example (the repl-1)))))))))

   (resource-links-section :type 'sheet-section
			   :inner-html (with-cl-who-string()
					 (:table
					     (let ((icon "/common-images/lisp-file.png"))
				
					       (htm (:tr (when (the hint-1)
							   (htm (:td (:a :href (format nil "/~a-resources/wall-hint-1.lisp" *publish-prefix*)
									 (:img :src icon :style "width: 40px; height: auto;")))
								(:td (:a :href (format nil "/~a-resources/wall-hint-1.lisp" *publish-prefix*)
									 "wall-hint-1.lisp")))))
						    (:tr (when (the hint-2)
							   (htm (:td (:a :href (format nil "/~a-resources/wall-hint-2.lisp" *publish-prefix*) 
									 (:img :src icon :style "width: 40px; height: auto;")))
								(:td (:a :href (format nil "/~a-resources/wall-hint-2.lisp" *publish-prefix*)
									 "wall-hint-2.lisp")))))
						    (:tr (when (the hint-3)
							   (htm (:td (:a :href (format nil "/~a-resources/wall-hint-3.lisp" *publish-prefix*)
									 (:img :src icon :style "width: 40px; height: auto;")))
								(:td (:a :href (format nil "/~a-resources/wall-hint-3.lisp" *publish-prefix*)
									 "wall-hint-3.lisp")))))
						    (:tr (when (the hint-4)
							   (htm (:td (:a :href (format nil "/~a-resources/wall-hint-4.lisp" *publish-prefix*)
									 (:img :src icon :style "width: 40px; height: auto;")))
								(:td (:a :href (format nil "/~a-resources/wall-hint-4.lisp" *publish-prefix*)
									 "wall-hint-4.lisp")))))
						    (:tr (when (the hint-5)
							   (htm (:td (:a :href (format nil "/~a-resources/wall-hint-5.lisp" *publish-prefix*)
									 (:img :src icon :style "width: 40px; height: auto;")))
								(:td (:a :href (format nil "/~a-resources/wall-hint-5.lisp" *publish-prefix*)
									 "wall-hint-5.lisp")))))
						    (:tr (when (the hint-6)
							   (htm (:td (:a :href (format nil "/~a-resources/wall-hint-6.lisp" *publish-prefix*)
									 (:img :src icon :style "width: 40px; height: auto;")))
								(:td (:a :href (format nil "/~a-resources/wall-hint-6.lisp" *publish-prefix*)
									 "wall-hint-6.lisp")))))
						    (:tr (when (the hint-7)
							   (htm (:td (:a :href (format nil "/~a-resources/wall-hint-7.lisp" *publish-prefix*)
									 (:img :src icon :style "width: 40px; height: auto;")))
								(:td (:a :href (format nil "/~a-resources/wall-hint-7.lisp" *publish-prefix*)
									 "wall-hint-7.lisp"))))))))))

   )
				   
				       
  )

```

---

## assembly.lisp - header
Source: gornschool-training/t2/source/assembly.lisp
Type: tutorial

```
(in-package :training-2)

(defparameter *publish-prefix* "t2")


```

---

## assembly.lisp - assembly
Source: gornschool-training/t2/source/assembly.lisp
Type: tutorial

```
(define-object assembly (base-tutorial-sheet)
  :input-slots
  (
  (tutorial-name "Getting started with GendL"))
  
  :computed-slots
  ((introduction (with-cl-who-string ()
		   (:p "This tutorial should equip you with the basics to get you started using GendL including"
		 (:ul (:li "Development tools")
		      (:li "An introduction to some of the commonly used Common Lisp functionality")
		      (:li "Using GendL's built in primitives, and extending them for your own specific needs")
		      (:li "Examples on building applications")
		      (:li "Practical guidance on designing code structures"))))))
  :objects
  ((intro :type 'tutorial-intro
		 :pass-down (page-objects)
		 :page 1
		 :page-title "Getting started with GendL"
		 :resources (list "my-box-1.lisp")
		 :publish-prefix *publish-prefix*
		 :index-url (the index-page url))
   (gendl-intro :type 'gendl-intro
		:pass-down (page-objects)
		:page 2
		:publish-prefix *publish-prefix*
		:page-title "GendL Introduction"
		:resources (list "Useful-Emacs-Key-Combinations.pdf"))
   (instantiate-repl :type 'instantiate-repl
		     :pass-down (page-objects)
		     :page 3
		     :publish-prefix *publish-prefix*
		     :page-title "Instantiating objects in the REPL"
		     :resources (list "my-box-1.lisp"))
   (instantiate-geysr :type 'instantiate-geysr
		      :pass-down (page-objects)
		      :page 4
		      :publish-prefix *publish-prefix*
		      :page-title "Instantiating objects in Geysr"
		      :resources (list "my-box-1.lisp"))

   (object-definition :type 'object-definition
		      :pass-down (page-objects)
		      :page 5
		      :publish-prefix *publish-prefix*
		      :page-title "Defining Objects"
		      :resources (list "define-object.lisp"))

   (numbers-and-arithmetic :type 'numbers-and-arithmetic
			   :pass-down (page-objects)
			   :page 6
			   :publish-prefix *publish-prefix*
			   :page-title "Numbers and Arithmetic"
			   :resources (list "numbers.lisp"))

   (positioning-and-orientation :type 'positioning-and-orientation
				:pass-down (page-objects)
				:page 7
				:publish-prefix *publish-prefix*
				:page-title "Positioning and Orientation"
				:resources (list "axis-system.pdf" "positioning-and-orientation.lisp" "point-and-vector-examples.lisp"))

   (object-sequences :type 'object-sequences
		     :pass-down (page-objects)
		     :page 8
		     :publish-prefix *publish-prefix*
		     :page-title "Sequences of Objects"
		     :resources (list "sequences.lisp"))
   
   (lists :type 'lists
	  :pass-down (page-objects)
	  :page 9
	  :publish-prefix *publish-prefix*
	  :page-title "Lists"
	  :resources (list "lists.lisp" "using-lists.lisp"))

   (functions-and-functions :type 'functions-and-functions
			    :pass-down (page-objects)
			    :page 10
			    :publish-prefix *publish-prefix*
			    :page-title "Functions and :functions"
			    :resources (list "functions.lisp"))

   (equality :type 'equality
			      :pass-down (page-objects)
	     :page 11
	     :publish-prefix *publish-prefix*
	     :page-title "Equality"
	     :resources (list "equality.lisp"))
   
   (conditionals :type 'conditionals
			      :pass-down (page-objects)
		 :page 12
		 :publish-prefix *publish-prefix*
		 :page-title "Conditionals"
		 :resources (list "using-conditionals.lisp"))
   (wall-example :type 'wall-example
		 :pass-down (page-objects)
		 :page 13
		 :publish-prefix *publish-prefix*
		 :page-title "Wall example - sequences, positioning and collating outputs")
   (more-on-lists :type 'more-on-lists
		  :pass-down (page-objects)
		  :prior-tutorial-url (the lists url)
		  :page 14
		  :publish-prefix *publish-prefix*
		  :page-title "More on Lists"
		  :resources (list "more-on-lists.lisp"))

   (iteration-and-mapping :type 'iteration-and-mapping
			  :pass-down (page-objects)
			  :page 15
			  :publish-prefix *publish-prefix*
			  :page-title "Iteration and Mapping"
			  :resources (list "itteration-and-mapping.lisp"))

   (truss-example :type 'truss-example
		  :pass-down (page-objects)
		  :page 16
		  :publish-prefix *publish-prefix*
		  :page-title "Truss example - positioning and orientation")

   (building-example :type 'building-example
		     :pass-down (page-objects)
		     :page 17
		     :publish-prefix *publish-prefix*
		     :page-title "Assembling objects")
   (strings :type 'strings
	    :pass-down (page-objects)
	    :page 18
	    :publish-prefix *publish-prefix*
	    :page-title "Characters and strings"
	    :resources (list "strings.lisp"))

   (formatted-output :type 'formatted-output
		     :pass-down (page-objects)
		     :page 19
		     :publish-prefix *publish-prefix*
		       :page-title "Formatted output"
		     :resources (list (list :title "Format Recipes" :url "https://gigamonkeys.com/book/a-few-format-recipes.html")
				      "format.lisp"))
   
   (building-example-2 :type 'building-example-2
		       :pass-down (page-objects)
		       :page 20
		       :publish-prefix *publish-prefix*
		       :page-title "Generating a formatted BoM"
		       :resources (list "building-hint-6"))

   )
  )





```

---

## object-sequences.lisp - header
Source: gornschool-training/t2/source/object-sequences.lisp
Type: tutorial

```
(in-package :training-2)


```

---

## object-sequences.lisp - object-sequences
Source: gornschool-training/t2/source/object-sequences.lisp
Type: tutorial

```
(define-object object-sequences (base-training-sheet)

  :computed-slots
  ((index-words (list ":sequence" "standard sequence" "radial sequence"
                      "variable sequence" "matrix sequence" ":lateral"
                      ":longitudinal" ":vertical" "index" "the-child" ":size" ":radial"))

   (code-1 (list "
```

---

## object-sequences.lisp - assembly-2
Source: gornschool-training/t2/source/object-sequences.lisp
Type: tutorial

```
(define-object assembly-2 (base-object)"
		 " :objects"
		 " ((box-1 :type 'box"
		 "         :length 5"
		 "         :width 1"
		 "         :height 1)"
		 "  (box-2 :type 'box"
		 "         :length 10"
		 "         :height 5"
		 "         :width 3"
		 "         :center (make-point 2 2 2))"
		 "  (box-3 :type 'box"
		 "         :length 5"
		 "         :height 5"
		 "         :width 5"
		 "         :center (translate-along-vector (the box-2 center)"
		 "                                         (make-vector 1 1 0)"
		 "                                          5)))"
		 " )"))
   
   (code-2 (list "
```

---

## object-sequences.lisp - assembly-6
Source: gornschool-training/t2/source/object-sequences.lisp
Type: tutorial

```
(define-object assembly-6 (base-object)"
		 " "
		 " :input-slots"
		 " ((number-of-boxes 3))"
		 " "
		 " :computed-slots"
		 " ((first-box-volume-1 (the (boxes 0) volume))"
		 "  (first-box-volume-2 (the-object (the boxes last) volume))"
		 "  (second-box-volume (the (boxes 1) volume))"
		 "  (last-box-volume-1 (the (boxes (- (the number-of-boxes)) volume)))"
		 "  (last-box-volume-1 (the-object (the boxes last) volume)))"
		 " "
		 " :objects"
		 " ((boxes :type 'box"
		 "	    :sequence (:size (the number-of-boxes))"
		 "	    :length (+ 2 (* (the-child index) 3))"
		 "	    :width 2"
		 "	    :height 1"
		 "	    :center (make-point (* (the-child index) 6) 0 0))))"))

   
   (code-3 (list "
```

---

## object-sequences.lisp - assembly-7
Source: gornschool-training/t2/source/object-sequences.lisp
Type: tutorial

```
(define-object assembly-7 (base-object)"
		 " "
		 " :input-slots"
		 " ((number-of-boxes 3))"
		 " "
		 " :objects"
		 " ((boxes :type 'box"
		 "	    :sequence (:radial (the number-of-boxes))"
		 "	    :length (+ 2 (* (the-child index) 3))"
		 "	    :width 2"
		 "	    :height 1"))
   
   (repl-1 (list (list :command "(setq self (make-object 'assembly-6))"
		       :output "#<ASSEMBLY-6 #x21045E42CD>")
		 (list :command "(the boxes)"
		       :output "#<GENDL::STANDARD-SEQUENCE #x21045E3E9D>")
		 (list :command "(the (boxes 0))"
		       :output "#<BOX #x21046CBA5D>")
                 (list :command "(the boxes first)"
                       :output "#<BOX #x21046CBA5D>")
                 (list :command "(the boxes last)"
                       :output "#<BOX #x21082334D8>")
                 (list :command "(the (boxes 1) previous)"
                       :output "#<BOX #x21046CBA5D>")))



   (body-content (with-cl-who-string ()
                    (:p "In many cases you may have child objects which are of the same type
and only differ in the values of inputs they are given. If you know how
many objects there are (and assuming not too many), you could define
them independently and give each its own name, as we did for example
in assembly-2")
                    
                    (str (code-example (the code-1)))
                    
		    (:p "However, there may be times when you do not know in advance how many
objects there will be, or you may want to compute the properties of each object
based on its identity (e.g. based on its numerical "
                        (:span :class "slot" "index") "). In either of these cases, a "
			(:span :class "general-keyword" ":sequence") " of objects may be appropriate.")
		    (:p "A "
			(:span :class "general-keyword" ":sequence")" defines an ordered group of objects.
There are four types of "
			(:span :class "general-keyword" ":sequence") "s:"
			(:ul (:li "A standard sequence - the object specification is defined with the input "
				  (:span :class "general-keyword"
                                         (esc ":sequence (:size <number-expression>)"))
                                  " and a number of objects equal to the value of "
				  (:em (esc "<number-expression>"))" will be created")
			     (:li "A radial sequence -  the object specification is defined with the input "
				  (:span :class "general-keyword"
                                         (esc ":sequence (:radial <number-expression>)"))
                                  " and a number of objects equal to the value of "
				  (:em (esc "<number-expression>"))" will be created")
			     (:li "A variable sequence - Allows objects in the groups to be
programatically added and deleted. The object specification is defined with the input "
				  (:span :class "general-keyword"
                                         (esc ":sequence (:indices <list-of-indices>)")))
			     (:li "A matrix sequence - quantification is generated
as a result of specifying "
				  ((:span :class "general-keyword")
                                   (esc ":sequence (:matrix <direction-keyword-1> <number-1>
<direction-keyword-2><number-2>)"))
                                  " in an "
                                  (:span :class "object-keyword" ":objects")
                                  " specification. The direction-keywords can be one of: "
			          (:span :class "general-keyword" ":lateral")", "
				  (:span :class "general-keyword" ":longitudinal")", or "
				  (:span :class "general-keyword" ":vertical")".")))
                    
		    (:p "In this tutorial we will cover only standard and radial sequences.")
		    (:p "When a sequence is generated, each object is assigned a zero-based "
			(:span :class "slot" "index")
                        " number. When referencing the object from " (:em "outside") " the "
                        (:span :class "object-keyword" ":objects")
                        " specification, its "
                        (:span :class "slot" "index")
                        " number must be included with the object name between parentheses.
Likewise, from "
                        (:em "inside")
                        " the "
                        (:span :class "object-keyword" ":objects")
                        " specification, each child can reference its own "
                        (:span :class "slot" "index") " slot (or any other of its slots) by
using the reference macro "
                        (:span :class "macro" "the-child") ". ")
                    
                    (:h3 "Standard Sequence")
                    
                    (:p "So consider the following code which defines a sequence of three boxes,
each of which has its "
			(:span :class "slot" "length") " and "
			(:span :class "slot" "center") " defined based on its "
                        (:span :class "slot" "index") " number.")
                    
                    (str (code-example (the code-2)))
                  
		    (:img :src (format nil "/~a-images/sequence-1.png"
                                       (the publish-prefix))
                          :style "width: auto; height: 300px; margin: 1em 0 1em 3% ;" )
   

                    (:p "Note in the Geysr tree, the the names of the child instances include
the index number of each.")
                    (:p "Also note in the code example the diferent ways to access
the object instances and their messages:"
		        (:ul (:li "Using an index - "
                                  (:code (esc "(the (boxes <index-number>) volume)"))
                                  ", where "
                                  (:code (esc "(boxes <index-number>)"))
                                  ", i.e. the combination of object name and "
				  (:span :class "slot" "index") " enclosed in parentheses,
evaluates to the object instance which is the desired element of the sequence.")
			     (:li "Using "
				  (:span :class "slot" "first") " and "
				  (:span :class "slot" "last") " - these are "
				  (:span :class "object-keyword" ":computed-slots")" in the sequence definition which return the first and last objects in the sequence.")))
		    (:p "To review:  "
		        (:ul (:li (:code (:b "(the boxes)"))" refers to the whole "
				  (:em (:b "sequence")) ".")
			     (:li (:code (:b (esc "(the (boxes <index>))")))" refers to a "
				  (:em (:b "specific element"))" within that "(:em (:b "sequence")))))
	            (str (repl-example (the repl-1)))
                    (:h3 "Radial Sequence")
		    (:p "Consider the following code, identical to the standard sequence
example except the "
		        (:span :class "general-keyword" ":sequence") " definition is changed to "
		        (:span :class "general-keyword" ":radial") " and the "
		        (:span :class "slot" ":center") " definition is removed.")
                    
		    (str (code-example (the code-3)))
                    
                    (:img :src (format nil "/~a-images/radial-sequence.png"  (the publish-prefix))
                          :style "width: auto; height: 300px; margin: 1em 0 1em 3% ;" )
                    (:div :class "main-page-item"
			  (:p "The Geysr image is with "
			      (:em (:b "View..Perspective..Top")) " enabled to show the effect of the "
			      (:span :class "general-keyword" ":radial") " definition. Essentially a "
			      (:span :class "general-keyword" ":radial")
                              " sequence orients each obect so it is rotated "
			      (:em (:b "360/[number-expression]"))
                              " degrees compared to its predecessor. You can of course override this
default behavior by feeding an explicit "
                              (:span :class "slot" "center") " and/or "
                              (:span :class "slot" "orientation") " into each element."))
                    
                    (:h2 "Resources") (str (the resource-links))))))
  
  

```

---

## formatted-output.lisp - header
Source: gornschool-training/t2/source/formatted-output.lisp
Type: tutorial

```
(in-package :training-2)


```

---

## formatted-output.lisp - formatted-output
Source: gornschool-training/t2/source/formatted-output.lisp
Type: tutorial

```
(define-object formatted-output (base-training-sheet)
  :computed-slots
  ((index-words (list "~a" "~d" "~f" "~$" "~p" "~r" "~( ~)" "~{ ~}" "~[ ~]" "format" "format directive" "format directive modifier"
		      "format prefix parameter" "format conditional" "format iteration" "integer directive (format)"
		      "floating point directive (format)" "english language directive (format)" "case control (format)"
		      "iteration (format)" "conditional control (format)"))
   
   (repl-1 (list (list :command "(setq text \"Hello\")"
		       :output "\"Hello\"")
		 (list :command "(setq number 123.456)"
		       :output 123.456)
		 (list :command "(setq my-list (list 1 2 3))"
		       :output "(1 2 3)")
		 (list :command "(format nil \"~a\" text)"
		       :output "\"Hello\"")
		 (list :command "(format nil \"~a\" number)"
		        :output "\"123.456\"")
		 (list :command "(format nil \"~a\" my-list)"
		       :output "\"(1 2 3)\"")
		 (list :command "(format nil \"Line 1~%Line2\")"
		       :output (list "\"Line 1" "Line 2\""))
		 (list :command "(format nil \"Line 1~%Line2\")"
		       :output (list "\"Line 1" "" "Line 2\""))))
   
   (repl-2 (list (list :command "(format nil \"~d\" 1234567)"
		       :output "\"1234567\"")
		 (list :command "(format nil \"~:d\" 1234567)"
		       :output "\"1,234,567\"")
		 (list :command "(format nil \"~@d\" 1234567)"
		       :output "\"+1234567\"")
		 (list :command "(format nil \"~:@d\" 1234567)"
		       :output "\"+1,234,567\"")
		 (list :command "(format nil \"~12d\" 1234567)"
		       :output "\"     1234567\"")
		 (list :command "(format nil \"~12,'0d\" 1234567)"
		       :output "\"000001234567\"")
		 (list :command "(format nil \"~2,'0d-~2,'0d-~d\" 31 7 2022)"
		       :output "\"31-07-2022\"")))

   (repl-3 (list (list :command "(format nil \"~f\" pi)"
		       :output "\"3.141592653589793\"")
		 (list :command "(format nil \"~,3f\" pi)"
		       :output "3.142")
		 (list :command "(format nil \"~e\" pi)"
		       :output "\"3.141592653589793E+0\"")
		 (list :command "(format nil \"~$\" pi)"
		       :output "3.14")))

   (repl-4 (list (list :command "(format nil \"computer~p\" 1)"
		       :output "\"computer\"")
		 (list :command "(format nil \"~r computer~:p\" 1)"
		       :output "\"one computer\"")
		 (list :command "(format nil \"~r computer~:p\" 2)"
		       :output "\"two computers\"")
		 (list :command "(format nil \"~r fl~:@p\" 1)"
		       :output "\"one fly\"")
		 (list :command "(format nil \"~r fl~:@p\" 2)"
		       :output "\"two flies\"")))
   
   (repl-5 (list (list :command "(setq txt \"sOme RANdom TexT\")"
		       :output "\"sOme RANdom TexT\"")
		 (list :command "(format nil \"~(~a~)\" txt)"
		       :output "\"some random text\"")
		 (list :command "(format nil \"~@(~a~)\" txt)"
		       :output "\"Some random text\"")
		 (list :command "(format nil \"~:(~a~)\" txt)"
		       :output "\"Some Random Text\"")
		 (list :command "(format nil \"~:@(~a~)\" txt)"
		       :output "\"SOME RANDOM TEXT\"")))

   (repl-6 (list (list :command "(format nil \"~{~a, ~}\" (list 1 2 3))"
		       :output "\"1 2 3 \"")
		 (list :command "(format nil \"~{~a ~}\" (list 1 2 3))"
		       :output "\"1, 2, 3, \"")
		 (list :command "(format nil \"~{~a~^,~}\" (list 1 2 3))"
		       :output "\"1, 2, 3\"")
		 (list :command "(format nil \"~@{~a~^,~}\" 1 2 3)"
		       :output "\"1, 2, 3\"")))
   
   (repl-7 (list (list :command "(format nil \"~[Peter~;Paul~;John~]\" 0)"
		       :output "\"Peter\"")
		 (list :command "(format nil \"~[Peter~;Paul~;John~]\" 4)"
		       :output "\"\"")
		 (list :command "(format nil \"~[Peter~;Paul~;;John~]\" 4)"
		       :output "\"John\"")
		 (list :command "(format nil \"~@[~r cat~:p ~]~@[~r dog~:p~]\" nil 2)"
		       :output "\"two dogs\"")
		 (list :command "(format nil \"~@[~r cat~:p ~]~@[~r dog~:p~]\" 3 nil)"
		       :output "\"three cats \"")
		 (list :command "(format nil \"~@[~r cat~:p ~]~@[~r dog~:p~]\" 3 1)"
		       :output "\"three cats one dog\"")))
   
   (body-content (with-cl-who-string()
		   ((:div :class "main-page-container")
		    ((:div :class "main-page-item")
		     (:div :class "grid-container-2-650px"
			   (:div :class "grid-item"
		      
			         (:p "When we want more control, flexibility and sophistication with text output we use the "
			             ((:span :class "function")"format")" function")
			         (:p "below are some of the most commonly used and useful features of "((:span :class "function")"format")
                                     ", but it can sometimes be difficult to understand or read, due to its compactness and its 
non-Lispy syntax. A great resource on the Internet is an article by Peter Seibel, called a few FORMAT recipes
which provides a detailed discussion on the features and uses of "
                                     ((:span :class "function") "format")
                                     " and some very useful examples. See the link in the resources section")
			         (:p ((:span :class "function")"Format")" takes 2 required arguments"
			             (:ul (:li "A "(:em "destination")" for its output")
				          (:li "A "(:em "control string")" that will generally contain literal text, but will always contain format"
				               (:em (:b "directives")))) ". "
			             ((:span :class "function")"Format")" will also generally be provided with extra arguments which are used by the "
			             (:em (:i "directives")) " in the control string.")
		                 (:h3 "Output Destination")
		                 (:p "The "(:em (:b "output destination"))" is the first argument to format. There are 4 types of output destination, but we'll discuss just the first 3"
			             (:ul (:li "If the value is "
				               (:em (:span :class "general-keyword"  "T"))", the output destination is the "
				               (:span :class "variable-name"  "*standard-output*")" stream")
				          (:li "If the value is "
				               (:em (:span :class "value"  "NIL"))", the output is generated as a string and returned by "
				               ((:span :class "function") "format"))
				          (:li "If the destination is a stream, the output will be written to that stream")))
			         (:h3 "Control String")
			         (:p "The "(:em "control string")" may look complex because it is based on characters, not s-expressions,
and is optimised for compactness. In many ways it is another 
mini-programming language within Lisp. As well as containing literal text,
the control string contains format "
			             (:em (:b"directives")) ". "
			             (:ul (:li "All "
				               (:em (:b"directives"))" start with a tilde (~) character, and end with a single character.
This character can be either upper of lower case")
				          (:li "Some "
				               (:em (:b"directives"))" take "
				               (:em "prefix parameters")", between the tilde and the character. If more than one "
				               (:em "prefix parameter")" is used they are separated by commas (,). "
				               (:em "Prefix parameters")" give additional output control")
				          (:li "Some "
				               (:em (:b "directives") " use either the colon (:) or at-sign (@) "
					            (:em (:b "modifiers")) " which change the behavior of the "
					            (:em (:b"directive"))" in small ways"))
				          (:li "There are also some special "
				               (:em (:b"directives"))" which are used in pairs and can be wrapped around other "
				               (:em (:b"directives"))", e.g. to control capitalisation ( "
				               (:span :class "format-directive"  "~( ~)")" ), conditional formatting ("
				               (:span :class "format-directive"  "~[ ~]")") or iteration ("
				               (:span :class "format-directive"  "~{ ~}")")"))
			             "This list isn't exhaustive, but contains the most frequently used principles.")
			         (:h3 "Basic Formatting")
			
			         (:p "The most basic "
			             (:em (:b "directive"))" is "
			             ((:span :class "format-directive") "~a") ". It consumes one of the "
                                     ((:span :class "function") "format")
                                     " arguments and outputs it in human-readable form. Note that because we are using "
                                     (:span :class "value" "NIL") " as the "
			             (:em (:b "output destination"))", "
			             ((:span :class "function")"format") " is returning a string as return-value and not outputting anywhere.")
			         "Another basic "(:em (:b "directive"))" is "
			         (:span :class "format-directive" "~%")", which causes a newline to be emitted. It optionally takes a single "

			         (:em "prefix parameter")" that defines how many newlines to be emitted"
                         
			         (str (repl-example (the repl-1)))
			
			         (:h3 "Integer Directives")
			
			         (:p "Whist the "
			             (:em (:b"directive"))" "
			             ((:span :class "format-directive")"~a")" can be used to output numbers, the "
			             (:em (:b"directive"))" "
			             ((:span :class "format-directive")"~d")
			             " offers more control for outputting integers (d standing for decimal, or base 10). There are 2 "
			             (:em "modifiers")":"
			             (:ul (:li "A colon ("
                                               (:span :class "format-directive" ":")
                                               ") adds commas separating the number into groups of 3 integers")
				          (:li "An at-sign ("
                                               (:span :class "format-directive" "@")
                                               ") always prints a sign before the number")
				          (:li "These 2 modifiers may be combined"))
				     "It can also take 2 "
				     (:em "prefix parameters")":"
				     (:ul (:li "The first "
					       (:em "prefix parameter")" specifies the minimum width for the output")
					  (:li "The second "
					       (:em "prefix parameter")" specifies the padding character. By default this is a space. Padding characters must be quoted are always inserted before the number")))
			         (str (repl-example (the repl-2)))
			         (:h3 "Floating Point Directives")
			
			         (:p "The 2 principle "
			             (:em (:b"directives"))" handling floating point numbers are "
			             ((:span :class "format-directive")"~f")" and "
			             ((:span :class "format-directive")"~e")". The difference is "
			             ((:span :class "format-directive")"~f")" is alowed to use scientific notation if the number is large enough or small enough, whilst "
			             ((:span :class "format-directive")"~e")" will always emit the number argument in scientific notation. ")
			         (:p" There are a number of "
			            (:em "prefix parameters")", but the only one of real significance is the second, which specifies the  number of digits to output after the decimal point. Note that, if this "
			            (:em "prefix parameter")" is less than the number of decimal digits in the argument, the argument will be mathematically rounded.")
			         (:p "A third floating point number "
				     (:em (:b"directive"))" is "
				     ((:span :class "format-directive")"~$")" which is a monetary directive. It is basically equivalent to "
				     ((:span :class "format-directive")"~f")" with the second prefix parameter dafaulting to 2")
			         (str (repl-example (the repl-3)))

			         (:h3 "English Language Directives")
				
				 (:p "These directives are useful for converting number to english language, outputting plurals and performing case conversions"
				     (:ul (:li ((:span :class "format-directive")"~r") " prints numbers as english words. With the "
					       (:span :class "format-directive"  ":")" "
					       (:em "modifier")" it prints the number as an ordinal")
					  (:li ((:span :class "format-directive")"~p") " pluralises a word, emitting an "
					       (:em (:span :class "general-keyword"  "s"))" character when the argument is anything but 1. It is often used with the "
					       (:em (:span :class "format-directive"  ":"))" "
					       (:em "modifier")" which makes it reprocess the previous format argument. Using the "
					       (:em (:span :class "format-directive"  "@"))" "
					       (:em "modifier")" causes a "
					       (:em (:span :class "general-keyword"  "y"))" character to be emitted when the format argument is 1, or "

					       (:em (:span :class "general-keyword"  "ies"))" to be emitted for other values")))
				 (str (repl-example (the repl-4)))
				 

				 (:p "To control case we use the "
				     ((:span :class "format-directive")"~(")" directive paired with a "
				     ((:span :class "format-directive")"~)")" in conjunction with the 2 "
				     (:em "modifiers")" "
				     (:em (:span :class "format-directive"  ":"))" and "
				     (:em (:span :class "format-directive"  "@"))
				     (:ul (:li "Without either modifier the output is all lower case")
					  (:li "With the "
					       (:em (:span :class "format-directive"  "@"))" "
					       (:em "modifier")" the first word is the string between the "
					       ((:span :class "format-directive")"~(")" and "
					       ((:span :class "format-directive")"~)")" directives is capitalised")
					  (:li "With the "
					       (:em (:span :class "format-directive"  ":"))" "
					       (:em "modifier")" all words is the string between the "
					       ((:span :class "format-directive")"~(")" and "
					       ((:span :class "format-directive")"~)")" directives are capitalised")
					  (:li "With both "

					       (:em "modifiers")" the output is all upper case")))
				 (str (repl-example (the repl-5)))
			         (:h3 "Iteration")
			         (:p "The "
			             ((:span :class "format-directive")"~{")" directive paired with"
			             ((:span :class "format-directive")"~}")" makes "
			             ((:span :class "function")"format")" itterate over the elements of a list. The control string between these 2 "
			             (:em (:b "directives"))" will be repeatedly processed as long as there are elements left in the list.")
			         (:p "In many cases we may wish to add some text seperators, for example a comma between the elements in the list, but avoid a final seperator. To do this we use the "
			             ((:span :class "format-directive")"~^")" directive before the seperator. By using the "
			             (:em (:span :class "format-directive"  "~@"))" modifier, any remaining "

			             ((:span :class "function")"format")" arguments are treated as a list")
			         (str (repl-example (the repl-6)))
			
		                 (:h3 "Conditionals")
		                 (:p "The "
			             ((:span :class "format-directive")"~[")" directive paired with"
			             ((:span :class "format-directive")"~]")" provides a simple control construct. Inbetween these directive are a number of clauses seperated by "
			             (:em (:span :class "format-directive"  "~;"))" and the argument supplied represents the index number (0 based) of the clasue to be used. If the index number is bigger then the number of clauses nothing is printed, unless the last clause is seperated by "(:em (:span :class "format-directive"  "~:;"))" in which case this clause is used as the default if no match is found"
			             (:p "With the "
			                 (:em (:span :class "format-directive" "~@"))" modifier the control string between "
			                 ((:span :class "format-directive")"~[")" and "
			                 ((:span :class "format-directive")"~]")" is only emitted if the "
			                 ((:span :class "function")"format")" argument is non-NIL"))
			         (str (repl-example (the repl-7))))))
				 
		    ((:div :class "main-page-item")
		     (:h2 "Resources")
		     (str (the resource-links))))))))
  


```

---

## building-example.lisp - header
Source: gornschool-training/t2/source/building-example.lisp
Type: tutorial

```
(in-package :training-2)


```

---

## building-example.lisp - building-example
Source: gornschool-training/t2/source/building-example.lisp
Type: tutorial

```
(define-object building-example (base-training-sheet)

  :functions
  ((hint-1! () (the (set-slot! :hint-1 (not (the hint-1)))))
   (hint-2! () (the (set-slot! :hint-2 (not (the hint-2)))))
   (hint-3! () (the (set-slot! :hint-3 (not (the hint-3)))))
   (hint-4! () (the (set-slot! :hint-4 (not (the hint-4)))))
   (hint-5! () (the (set-slot! :hint-5 (not (the hint-5))))))

  :computed-slots
  ((hint-1 nil :settable)
   (hint-2 nil :settable)
   (hint-3 nil :settable)
   (hint-4 nil :settable)
   (hint-5 nil :settable)
   
   
   (code-1 (list "
```

---

## building-example.lisp - full-start-wall
Source: gornschool-training/t2/source/building-example.lisp
Type: tutorial

```
(define-object full-start-wall (wall)"
		 "  :input-slots"
		 "  ((first-row :start-full)))"
		 ""
		 "
```

---

## building-example.lisp - half-start-wall
Source: gornschool-training/t2/source/building-example.lisp
Type: tutorial

```
(define-object half-start-wall (wall)"
		 "  :input-slots"
		 "  ((first-row :start-half)))"
		 ""
		 "
```

---

## building-example.lisp - wall
Source: gornschool-training/t2/source/building-example.lisp
Type: tutorial

```
(define-object wall(box)"
		 "  :input-slots"
		 "  (..."
		 "   ..."
		 "   (first-row :start-full))"
		 "  ..."
		 "  ..."
		 "  :objects"
		 "  ((row :type 'row"
		 "    ..."
		 "    ..."
		 "    :pass-down (..."
		 "                ..."
		 "                first-row))))"
		 ""
		 "
```

---

## building-example.lisp - row
Source: gornschool-training/t2/source/building-example.lisp
Type: tutorial

```
(define-object row (box)"
		 "  :input-slots"
		 "  (..."
		 "   ..."
		 "   first-row)"
		 ""
		 "  :computed-slots"
		 "  ((full-brick-row? (cond ((eq (the first-row) :start-full)"
		 "                           (or (zerop (the index)) (evenp (the index))))"
		 "                          ((eq (the first-row) :start-half)"
		 "                           (not (or (zerop (the index)) (evenp (the index)))))))"
		 "    ..."		 
		 "    ..."
		 "  )"
		 ")"))

   (code-2 (list "
```

---

## building-example.lisp - half-start-wall-front-key
Source: gornschool-training/t2/source/building-example.lisp
Type: tutorial

```
(define-object half-start-wall-front-key (wall)"
		 "  :input-slots"
		 "  ((first-row :start-half)"
		 "   (front-edge :keyed)))"
		 ""
		 "
```

---

## building-example.lisp - half-start-wall-rear-key
Source: gornschool-training/t2/source/building-example.lisp
Type: tutorial

```
(define-object half-start-wall-rear-key (wall)"
		 "  :input-slots"
		 "  ((first-row :start-half)"
		 "   (rear-edge :keyed)))"
		 ""
		 "
```

---

## building-example.lisp - half-start-wall-both-key
Source: gornschool-training/t2/source/building-example.lisp
Type: tutorial

```
(define-object half-start-wall-both-key (wall)"
		 "  :input-slots"
		 "  ((first-row :start-half)"
		 "   (front-edge :keyed)"
		 "   (rear-edge :keyed)))"
		 ""
		 "
```

---

## building-example.lisp - wall
Source: gornschool-training/t2/source/building-example.lisp
Type: tutorial

```
(define-object wall(box)"
		 "  :input-slots"
		 "  (..."
		 "   ..."
		 "   (front-edge :full)"
		 "   (rear-edge :full))"
		 "  :objects"
		 "  ((row :type 'row"
		 "        ..."
		 "        :pass-down (..."
		 "                    ..."
		 "                    front-edge"
		 "                    rear-edge))))"
		 ""
		 "
```

---

## building-example.lisp - row
Source: gornschool-training/t2/source/building-example.lisp
Type: tutorial

```
(define-object row (box)"
		 "  :input-slots"
		 "  (..."
		 "   ..."		 
		 "   front-edge"
		 "   rear-edge )"
		 "  :objects"
		 "  ((bricks-and-mortar :type 'bricks-and-mortar"
		 "                     ..."
		 "                     :pass-down (..."
		 "                                ..."
		 "                                front-edge"
		 "                                rear-edge)))"
		 ""
		 "
```

---

## building-example.lisp - bricks-and-mortar
Source: gornschool-training/t2/source/building-example.lisp
Type: tutorial

```
(define-object bricks-and-mortar (box)"
		 "  :input-slots"
		 "  (..."
		 "   ..."
		 "   front-edge"
		 "   rear-edge)"
		 ""
		 "  :computed-slots"
		 "  (..."
		 "   ..."	   
		 "   (first-mortar-joint-start-point (cond ((the full-brick-row?)"
		 "                                        (the (full-brick 0) (face-center :rear)))"
		 "                                       ((eq (the front-edge) :full)"
		 "                                        (the (half-brick 0) (face-center :rear)))"
		 "                                       ((eq (the front-edge) :keyed)"
		 "                                        (translate-along-vector "
		 "                                           (the (face-center :front))"
		 "	                                      (the (face-normal-vector :rear))"
		 "	                                      (half (the brick-length))))))"
		 "   (number-of-half-bricks (cond ((the full-brick-row?) 0)"
		 "                              ((and (eq (the front-edge) :full)"
		 "                                    (eq (the rear-edge) :full)) 2)"
		 "                              ((and (eq (the front-edge) :keyed)"
		 "                                    (eq (the rear-edge) :full)) 1)"
		 "                              ((and (eq (the front-edge) :full)"
		 "                                    (eq (the rear-edge) :keyed)) 1)"
		 "                              ((and (eq (the front-edge) :keyed)"
		 "                                    (eq (the rear-edge) :keyed)) 0)))"
		 " :objects"
		 " (..."
		 "  ..."
		 "  (half-brick :type 'box"
		 "              :sequence (:size (the number-of-half-bricks))"
		 "              :center (cond ((and (= (the-child index) 0)"
		 "			           (eq (the front-edge) :full)) "
		 "                            (the first-half-brick-center!))"
		 "	                     ((and (= (the-child index) 0)"
		 "	                           (eq (the front-edge) :keyed)"
		 "	                           (eq (the rear-edge) :full)) "
		 "                            (the last-half-brick-center!))"
		 "                           ((eq (the rear-edge) :full) "
		 "                            (the last-half-brick-center!)))"
	       	 "               ...)))"))

   (code-3 (list "
```

---

## building-example.lisp - building
Source: gornschool-training/t2/source/building-example.lisp
Type: tutorial

```
(define-object building (box)"
		 "  :input-slots"
		 "  (..."
		 "   ..."
		 "   (max-beam-spacing 1500))"
		 "  :computed-slots"
		 "  (..."
		 "   ..."
		 "   (number-of-roof-trusses (let ((trusses (ceiling (the left-wall-length) 1500)))"
		 "			      (max trusses 2)))"
		 "  ))"))

   (code-4 (list "
```

---

## building-example.lisp - truss
Source: gornschool-training/t2/source/building-example.lisp
Type: tutorial

```
(define-object truss (box)"
		 " ..."
		 " ..."
		 " :computed-slots"
		 "  (..."
		 "   ..."
		 "   ;;messages to support roof cladding sizing and positioning "
		 "   (apex-point (inter-line-plane (the rear-slope-construction-line end)"
		 "                                 (the truss-rear-slope-vector)"
		 "                                 (the lower-beam center)"
		 "                                 (the (face-normal-vector :rear))))"
		 "   (front-gutter-point (the front-slope-construction-line start))"
		 "   (rear-gutter-point (the rear-slope-construction-line start))"
		 "   (front-slope-normal (the front-slope-beam (face-normal-vector :top)))"
		 "   (rear-slope-normal (the rear-slope-beam (face-normal-vector :top))))"
		 " )"))
   (code-5 (list "
```

---

## building-example.lisp - building
Source: gornschool-training/t2/source/building-example.lisp
Type: tutorial

```
(define-object building (box)"
		 "  :input-slots"
		 "   (..."
		 "    ..."
		 "    (roof-overhang 50)"
		 "    (cladding-thickness 10))"
		 ""
		 "  :computed-slots"
		 "   (..."
		 "    ..."
		 "    (roof-length (+ (the left-wall length) (twice (the roof-overhang))))"
		 "    (roof-width (the cladding-thickness))"
		 "    (roof-height (let ((apex (the (roof-truss 0) apex-point))"
		 "                       (gutter (the (roof-truss 0) front-gutter-point)))"
		 "                  (+ (3d-distance apex gutter) (the roof-overhang))))"))

   (code-6 (list "
```

---

## building-example.lisp - building
Source: gornschool-training/t2/source/building-example.lisp
Type: tutorial

```
(define-object building (box)"
		 " ..."
		 " ..."
		 " :functions"
		 "  ((get-roof-mid-point! (first-gutter last-gutter last-index)"
		 "                          (let*((mid-gutter (midpoint first-gutter last-gutter))"
		 "                                (first-apex (the (roof-truss 0) apex-point))"
		 "                                (last-apex (the (roof-truss last-index) apex-point))"
		 "                                (mid-apex (midpoint first-apex last-apex))"
		 "                                (vec (subtract-vectors mid-gutter mid-apex))"
		 "                                (mid-edge (translate-along-vector "
		 "                                                   mid-gutter "
		 "                                                   vec "
		 "                                                   (the roof-overhang))))"
		 "                         (midpoint mid-apex mid-edge))))"
		 " :objects"
		 "  (..."
		 "   ..."
		 "   (roof-cladding-left "
		 "     :type 'box"
		 "     :length (the roof-length)"
		 "     :height (the roof-height)"
		 "     :width (the cladding-thickness)"
		 "     :orientation (alignment :left (the (roof-truss 0) front-slope-normal))"
		 "     :center (let* ((last-index (- (the number-of-roof-trusses) 1))"
		 "		      (first-gutter (the (roof-truss 0) front-gutter-point))"
		 "		      (last-gutter (the (roof-truss last-index) front-gutter-point))"
		 "		      (mid-ctr (the (get-roof-mid-point! "
		 "                                            first-gutter "
		 "                                            last-gutter "
		 "                                            last-index))))"
		 "		  (translate-along-vector "
		 "                             mid-ctr"
		 "			       (the (roof-truss 0) front-slope-normal)"
		 "			       (half (the cladding-thickness)))))"))

   (code-7 (list "
```

---

## building-example.lisp - building
Source: gornschool-training/t2/source/building-example.lisp
Type: tutorial

```
(define-object building (box)"
		 " ..."
		 
		 ":computed-slots"
		 "(..."
		 " ..."
		 " ;; building properties"
		 " (walls (remove nil (mapcar #'(lambda(a) (when (typep a 'wall) a)) (the children))))"
		 " (full-bricks (apply '+ (mapsend (the walls) :full-bricks)))"
		 " (half-bricks (apply '+ (mapsend (the walls) :half-bricks)))"
		 " (mortar-volume (apply '+ (mapsend (the walls) :mortar-volume)))"
		 " (cladding-dimensions (list :length (the roof-length)"
		 "			    :width (the roof-height)))"
		 " (beam-properties (the (roof-truss 0) beam-properties))"
		 " (beam-qty-by-size "
		 "    (let ((res nil))"
		 "	(dolist (plis (the beam-properties) )"
		 "	  (let* ((trusses (the number-of-roof-trusses))"
		 "		 (l (getf plis :length-mm))"
		 "                (p (position l res :key #'(lambda(a) (getf a :length-mm))))"
		 "                (qty (when p (getf (nth p res) :qty))))"
		 "          (if p (setf (getf (nth p res) :qty) (+ qty trusses))"
		 "              (setq res (append (list (list :length-mm l :qty trusses)) res)))))"
		 "      (safe-sort '< res :key #'(lambda(a) (getf a :length-mm)))))"
		 ""
		 " (roof-truss-mass (* (apply '+ (mapcar #'(lambda(a) (getf a :mass-kg)) "
		 "                                       (the beam-properties)))"
		 "		       (the number-of-roof-trusses)))"
		 ""
		 " (building-materials "
		 "   (list :full-bricks (the full-bricks)"
		 "	 :half-bricks (the half-bricks)"
		 "	 :mortar-volume-m3 (div (the mortar-volume) 1000000000)"
		 "	 :beams (the beam-qty-by-size)"
		 "	 :roof-cladding (append (the cladding-dimensions) (list :qty 2))))"))
   (repl-1 (list (list :command "(setq self (make-object 'building)"
		       :output "#<BUILDING #x21045DF6AD>")
		  (list :command "(the building-materials)"
			:output (list "(:FULL-BRICKS 3109"
				      " :HALF-BRICKS 162"
				      " :MORTAR-VOLUME-M3 0.6665175"
				      " :BEAMS ((:LENGTH-MM 874.685657822283"
				      "          :QTY 3)"
				      "         (:LENGTH-MM 1727.7658984943535 "
				      "          :QTY 6)"
				      "         (:LENGTH-MM 3030 "
				      "          :QTY 3))"
				      " :ROOF-CLADDING (:LENGTH 4080 "
				      "                 :WIDTH 1806.755259207346 "
				      "                 :QTY 2))"))))
   
   (body-content (with-cl-who-string()
		   ((:div :class "main-page-container")
		    ((:div :class "main-page-item")
		     (str (the start-section main-div))
		     (str (the hint-1-section main-div))
		     (str (the hint-2-section main-div))
		     (str (the hint-3-section main-div))
		     (str (the hint-4-section main-div))
		     (str (the hint-5-section main-div)))
		    ((:div :class "main-page-item")
		     (:h2 "Resources")
		     (str (the resource-links-section main-div)))))))

  :objects
  ((resource-links-section :type 'sheet-section
			   :inner-html (with-cl-who-string()
					 (:table
					     (let ((icon "/common-images/lisp-file.png")
						   (lis (list (list :available (the hint-1) :file "building-hint-1.lisp")
							      (list :available (the hint-2) :file "building-hint-2.lisp")
							      (list :available (the hint-3) :file "building-hint-3.lisp")
							      (list :available (the hint-4) :file "building-hint-4.lisp")
							      (list :available (the hint-5) :file "building-hint-5.lisp"))))
					       (dolist (l lis)
						 (let* ((f (getf l :file))
						       (link (format nil "/t2-resources/~a" f)))
						   (htm (:tr (when (getf l :available)
							     (htm (:td ((:a :href link) ((:img :src icon :style "width: 40px; height: auto;"))))
								  (:td ((:a :href link) (str f)))))))))))))

   
   (start-section :type 'sheet-section
		  :inner-html (with-cl-who-string()

				(:div :class "grid-container-2-650px"
				      (:div :class "grid-item"
					    (:p "In our two worked examples we have made a wall and a roof truss.

In this worked example, using these objects we now want to create a building")
				(:h2 "Brief")
				(:p "Using the wall and truss objects, and modifying where necessary,
generate a model of a three-sided structure (4th side is open) with a
pitched roof. The 3 sides of the structure are brick walls and should
be keyed together where they join and have a flush face at the open
edge. The brick structure should be 3m wide, 4m long and 3m
high (nominal dimension), the roof slope should be 30 degrees. The
roof truss beams are 40mm wide and 50mm tall, with a wall thickness of
3mm and a density of 7800 kg/m" (:sup "3")". They should be equally
spaced, no more than 1.5m apart and the minimum number of roof trusses
should be used subject to there being at least 2. Cladding should be
added to the roof, such that it touches on the apex and overhangs at
the front, rear and sides by 50mm. The cladding is 10mm
thick. Determine the size of the cladding panels, the mass of the roof
trusses and the lengths of the individual beams, the number of bricks
and half-bricks required and the volume of morter. Bricks are the
standard size - 180mm long, 45mm high and 90mm wide") (str (the (hint-button :function-key :hint-1!)))))))


   (hint-1-section :type 'sheet-section
		   :inner-html (with-cl-who-string()
				 (when (the hint-1)
				   (htm
				    (:div :class "grid-container-2-650px"
					(:div :class "grid-item"
				    
					  (:p "The first stage in the process is to define the object structure of the model. We will have"
					      (:ul (:li "A left wall")
						   (:li "A right wall")
						   (:li "A rear wall")
						   (:li "A number of roof trusses")
						   (:li "A left side roof cladding")
						   (:li "A right side roof cladding")))
					  (:p "Then we pass the nominal structure dimensions into the wall objects"
					      (:ul (:li "Right and Left wall "
							((:span :class "slot")":wall-length")" and "
							((:span :class "slot")":wall-height")" are nominal height and length respectively")
						   (:li "Rear wall "
							((:span :class "slot")":wall-length")" is the nominal width and "
						((:span :class "slot")":wall-height")" is the nominal height"))
					      "The wall objects provide "
					      ((:span :class "slot")":length")" and "
					      ((:span :class "slot")":height")" messages which are the actual dimensions, based on the supplied nominal dimensions. We use these values to specify"
					      (:ul (:li "The Truss "
							((:span :class "slot")":truss-length")" (equal to the rear wall "
							((:span :class "slot")":length")")")
						   (:li "The building length (equal to the left/right wall "
							((:span :class "slot")":length")"")
						   (:li "The building width (equal to the rear wall "
							((:span :class "slot")":length")")")
						   (:li "The building height (equal to the Truss "
							((:span :class "slot")":height")" plus any of the walls "
							((:span :class "slot")":height")""))
					      "We have now defined the bounding boxes of the 3 walls, the trusses and the overall building")
					  (:p "Next we need to position the objects. First the positioning of the walls:"
					      (:ul (:li "The left and right walls are positioned relative to the "
							((:span :class "function")"(edge-center :left :bottom)")" and the "
							((:span :class "function")"(edge-center :right :bottom)")" points of the main bounding box")
						   (:li "The rear wall is positioned relative to the "
							((:span :class "function")"(edge-center :rear :bottom)")" of the main bounding box"))
					      "Positioning of the trusses will be relative to the "
					      ((:span :class "function")"(edge-center :front :top)")" of the main bounding box, but we need to calculate the
offsets of the trusses from this point based on the overall length and the number of trusses. In the first instance we will hard-code the number of trusses,
calculate these points and then apply them t the sequence of trusses based on the index number of the truss")
					  (:p "Orientation is relatively straight forward"
					      (:ul (:li "The right and left walls do not need any change to orientation")
						   (:li "The rear wall has its "((:span :class "object-keyword")":rear")" axis aligned with the main bounding box "
							((:span :class "function")"(face-normal-vector :right)")"")
						   (:li "Each of the trusses has their "((:span :class "object-keyword")":rear")" axis aligned with the main bounding box "
							((:span :class "function")"(face-normal-vector :right)")"")))
					  (:p "With all of these calculations implemented, the bounding boxes of each object may now be viewed in Geysr to ensure everything aligns correctly")
					  (:p "The next step is to modify the wall object to enable the left to rear and right to rear joints to key together")
					  (str (the (hint-button :function-key :hint-2!)))))
				    (:div :class "grid-item"
					  (:img :src (format nil "/~a-images/geysr-building-1.png" (the publish-prefix))
                                                :style "width: auto; height: 300px; margin: 1em 0 1em 3% ;" ))))))

   (hint-2-section :type 'sheet-section
		   :inner-html (with-cl-who-string()
				 (when (the hint-2)
				   (htm ((:div :class "main-page-container")
					 ((:div :class "main-page-item")
					  (:p "To key 2 walls together we need 2 conditions"
					      (:ul (:li "For 2 adjoining walls, the first row in one must start with full bricks and the first row in the other must start with half bricks")
						   (:li "On the joining edge, the half bricks must be left out, so the gap can be filled by the full brick of the adjoining wall"))
					      "We will deal with these seperately")
					  (:h2 "Full or Half brick starting row")
					  (:ul (:li "Add an "
						    ((:span :class "object-keyword")":input-slot")" "
						    ((:span :class "slot")"first-row")" to the "
						    ((:span :class "object")"wall")" object")
					       (:li "Its values are "
						    ((:span :class "object-keyword")":full-start")" for the first row to be started with full bricks, and "
						    ((:span :class "object-keyword")":half-start")" for the first row to be started with half bricks")
					       (:li "Pass this variable into the "
						    ((:span :class "object")"row")" object")
					  (:li "Modify the "
					       ((:span :class "object-keyword")":computed-slot")" "
					       ((:span :class "slot")"full-brick-row?")" in the "
					       ((:span :class "object")"row")" object to include the value of this slot in its logic"))
					 (:p "A simple way to test this is to create 2 test objects, say "
					     ((:span :class "object")"full-start-wall")" and "
					     ((:span :class "object")"half-start-wall")". Both mixin the "
					     ((:span :class "object")"wall")" object and whilst "
					     ((:span :class "object")"full-start-wall")" sets the "
					     ((:span :class "object-keyword")":input-slot")" "
					     ((:span :class "slot")"first-row")" to "
					     ((:span :class "object-keyword")":full-start")", "
					     ((:span :class "object")"half-start-wall")" sets the "
					     ((:span :class "object-keyword")":input-slot")" "
					     ((:span :class "slot")"first-row")" to "
					     ((:span :class "object-keyword")":half-start")". (Note that strictly, because wall defaults "
					     ((:span :class "slot")"first-row")" to "
					     ((:span :class "object-keyword")":full-start")" we don't need to provide any "
					     ((:span :class "object-keyword")":input-slot")" values for "
					     ((:span :class "object")"full-start-wall")", its just simpler to see what the 2 different values are) We can then instantiate both objects in Geysr and test that one starts with full bricks on the first row whilst the other starts with half bricks on the first row"))
					 ((:div :class "main-page-item"))
					 ((:div :class "main-page-item")
					  (str (code-example (the code-1))))
					 ((:div :class "main-page-item")
					  (:img :src (format nil "/~a-images/geysr-building-2.png" (the publish-prefix)) :style "width: auto; height: 300px; margin: 1em 0 1em 3% ;" )
					  (:img :src (format nil "/~a-images/geysr-building-3.png" (the publish-prefix)) :style "width: auto; height: 290px; margin: 1em 0 1em 3% ;" ))
					 ((:div :class "main-page-item")
					  (:h2 "Full or Keyed edges")
					  (:ul (:li "Add 2 "
						    ((:span :class "object-keyword")":input-slots")", "
						    ((:span :class "slot")"front-edge")" and "
						    ((:span :class "slot")"rear-edge")" to the wall object. Keyword values will be "
						    ((:span :class "object-keyword")":full")" for a flush edge, "
						    ((:span :class "object-keyword")":keyed")" for a keyed edge")
					       (:li "Pass these values down through the "
						    ((:span :class "object")"row")" object into the "
						    ((:span :class "object")"bricks-and-mortar")" object")
					       (:li "Update the "
						    ((:span :class "object-keyword")":computed-slot")" "
						    ((:span :class "slot")"first-mortar-joint-start-point")". The original definition located the first mortar joint off the back face of the firt half brick when it was a half-brick row. An extra condition is added to calculate the location point when the front edge of the row is "
						    ((:span :class "object-keyword")":keyed")"")
					       (:li "Add a new "
						    ((:span :class "object-keyword")":computed-slot")" "
						    ((:span :class "slot")"number-of-half-bricks")" which defines how many half bricks are present depending on whether the row is a full brick row or whether the front, rear or both edges are "
						    ((:span :class "object-keyword")":keyed"))
					       (:li "Update the definition of the "((:span :class "object")"half-brick")" child object, such that the "
						    ((:span :class "object-keyword")":sequence (:size)")" refers to "
						    ((:span :class "slot")" (the number-of-half-bricks)")" and the "
						    ((:span :class "object-keyword")":center")" position depends on whether it is the front or rear half brick and whether or not this is "
						    ((:span :class "object-keyword")":full")" or "
						    ((:span :class "object-keyword")":keyed")))
					  (:p "Again, we can create some simple test objects to test the different "
					      ((:span :class "object-keyword")":keyed")" or "
					      ((:span :class "object-keyword")":full")" combinations"))
					 ((:div :class "main-page-item"))
					 ((:div :class "main-page-item")
					  (str (code-example (the code-2))))
					 ((:div :class "main-page-item")
					  (:img :src (format nil "/~a-images/geysr-building-4.png" (the publish-prefix)) :style "width: auto; height: 300px; margin: 1em 0 1em 3% ;" )
					  (:img :src (format nil "/~a-images/geysr-building-5.png" (the publish-prefix)) :style "width: auto; height: 270px; margin: 1em 0 1em 3% ;" )
					  (:img :src (format nil "/~a-images/geysr-building-6.png" (the publish-prefix)) :style "width: auto; height: 285px; margin: 1em 0 1em 3% ;" )))
					(:p "Up to now, the number of trusses has been hard coded, we now need to make that dependant on the structure length")
					(str (the (hint-button :function-key :hint-3!))) ))))

   (hint-3-section :type 'sheet-section
		   :inner-html (with-cl-who-string()
				 (when (the hint-3)
				   (htm (:div :class "grid-container-2-650px"
					(:div :class "grid-item"
					  (:p "The requirements says there should be a minimum of 2 roof trusses, they should be no more than 1500mm apart and the minimum number of trusses should be used. The trusses are spaced along the side wall length side, so if we divide that length "
					      ((:span :class "slot")"(the left-wall length)")" by 1500 we get the exact pitch. We need an integer and we need to round up so the "
					      ((:span :class "function")"ceiling")" function is used which round up to the nearest whole number. Finally should return this number or 2, whichever is greatest")
					(str (code-example (the code-3)))
					(:p "Our final task in the design is to calculate the size of the roof panels and position and orient them")

                                        (str (the (hint-button :function-key :hint-4!)))                                        
					))))))

   
   (hint-4-section :type 'sheet-section
		   :inner-html (with-cl-who-string()
				 (when (the hint-4)
				   (htm
				    (:div :class "grid-container-2-650px"
					  (:div :class "grid-item"
						(:p "First we need to calculate the apex point for the roof. There are a number of approaches for this; here we choose to augment the "
					    ((:span :class "object")"truss")" object to add in an "
					    ((:span :class "slot")"apex-point")" message, using the GendL "
					    ((:span :class "function")"inter-line-plane")" function. The line is defined as a point and vector (in our case the start of the foor slope construction line and the roof slope vector) and the plane is defines as a point and a vector which is normal to the plane (in out case the center point of the lower beam and the "
					    ((:span :class "function")"(face-normal-vector :rear)")" of the "
					    ((:span :class "object")"truss")" bounding box). We also add some further messages which we need to compute the roof size and orientation"
					    (:ul (:li ((:span :class "slot")"front-gutter-point")" - the bottom edge of the front-slope-beam")
						 (:li ((:span :class "slot")"rear-gutter-point")" - the bottom edge of the rear-slope-beam")
						 (:li ((:span :class "slot")"front-slope-normal")" - the (face-normal-vector :top) of the front-slope-beam")
						 (:li ((:span :class "slot")"rear-slope-normal")" - the (face-normal-vector :top) of the rear-slope-beam")))
					(str (code-example (the code-4)))
					(:p "Next we need to add the remaining "
					    ((:span :class "object-keyword")":input-slots")" which enable the dimensions of the roof cladding to be calculated and add the "
					    ((:span :class "object-keyword")":computed-slots")" which will evaluate to the roof cladding dimensions"
					    (str (code-example (the code-5)))
					    (:p "And finally we set the "
						((:span :class "slot")":length")", "
						((:span :class "slot")":width")" and "
						((:span :class "slot")":height")" of the "
						((:span :class "object")"roof-cladding-right")" and "
						((:span :class "object")"roof-cladding-left")" objects, align their orientation such that their "
						((:span :class "object-keyword")":left")" axis is aligned twith the"
						((:span :class "function")"(face-normal-vector :top)")" of the sloping beams, and define their center points. The "
						((:span :class "slot")":center")" calculation is done using a "
						((:span :class "special-operator")"let")" binding, but since some of the local bindings need previously defined symbols, we use the "
						((:span :class "special-operator")"let*")" special operator rather than "
						((:span :class "special-operator")"let")". Also note the use of a "
						((:span :class "object-keyword")":function")" "
						((:span :class "function")":get-roof-mid-point!")" as the processing of this is common to both the left and right cladding")))
					(:div :class "grid-item")
					(:div :class "grid-item"
					      (str (code-example (the code-6))))
					(:div :class "grid-item"
					      (:img :src (format nil "/~a-images/geysr-building-7.png" (the publish-prefix)) :style "width: auto; height: 400px; margin: 1em 0 1em 3% ;" ))
						(:div :class "grid-item"
					              (:p "To complete the brief we now need to assemble the building properties")

                                                      (str (the (hint-button :function-key :hint-5!)))))))))


   (hint-5-section :type 'sheet-section
		   :inner-html (with-cl-who-string()
				 (when (the hint-5)
				   (htm
				    (:div :class "grid-container-2-650px"
					(:div :class "grid-item"
				    (:p "Each of the "
					    ((:span :class "object")"wall")" objects supports the messages "
					    ((:span :class "slot")"full-bricks")", "
					    ((:span :class "slot")"half-bricks")" and "
					    ((:span :class "slot")"mortar-volume")". We need to retrieve and sum these")
					(:p "The roof cladding dimensions can be taken from the inputs to the "
					    ((:span :class "object")"roof-cladding-right")" and "
					    ((:span :class "object")"roof-cladding-left")" objects")
					(:p "Each "
					    ((:span :class "object")"roof-truss")" supports the message "
					    ((:span :class "slot")"beam-properties")". As all the "
					    ((:span :class "object")"roof-truss")" objects are identical, we can just work with "
					    ((:span :class "slot")"beam-properties")" from one "
					    ((:span :class "object")"roof-truss")" and multiply by "
					    ((:span :class "slot")"(the number-of-roof-trusses)")". We need to process this list to determine quantities of beams by size and the total truss mass. Note that when processing the "
					    ((:span :class "slot")"beam-properties")" to obtain "
					    ((:span :class "slot")"(the beam-qty-by-size)")" the "
					    ((:span :class "macro")"dolist")" iteration doesn't return anything , its just side-effecting on the locally bound variable "
					    ((:span :class "slot")"res")" and then returns the value of "
					    ((:span :class "slot")"res")" sorted by "
					    ((:span :class "object-keyword")":length-mm")". The final return value is a plist,  "
					    ((:span :class "slot")"building-materials")))
					(:div :class "grid-item")
					(:div :class "grid-item"
					  (str (code-example (the code-7))))
					 (:div :class "grid-item"
					 (str (repl-example (the repl-1)))) )))))))


```

---

## conditionals.lisp - header
Source: gornschool-training/t2/source/conditionals.lisp
Type: tutorial

```
(in-package :training-2)


```

---

## conditionals.lisp - conditionals
Source: gornschool-training/t2/source/conditionals.lisp
Type: tutorial

```
(define-object conditionals (base-training-sheet)
  :computed-slots
  ((index-words (list "if" "when" "case" "cond" "unless"))

   (repl-1 (list (list :command "(if (> 1 0) (+ 3 4))"
		       :output 7)
		 (list :command "(if (< 1 0) (+ 3 4))"
		       :output "NIL")
		 (list :command "(if (< 1 0) (+ 3 4)(+ 5 6))"
		       :output 7)))
   (repl-2 (list (list :command "(defparameter *evaluated* NIL)"
		       :output "*EVALUATED*")
		 (list :command (list "(if (> 1 0)"
				      "    (progn (+ 1 2)"
				      "           (setf *evaluated* (not *evaluated*))"
				      "           (+ 4 5))"
				      "    (+ 3 2))")
		       :output 9)
		 (list :command "*evaluated*"
		       :output "T")
		(list :command (list "(if (> 1 0)"
				      "    (prog1 (+ 1 2)"
				      "           (setf *evaluated*  (not *evaluated*))"
				      "           (+ 4 5))"
				      "    (+ 3 2))")
		      :output 3)
		 (list :command "*evaluated*"
		       :output "NIL")))
   
   (repl-3 (list (list :command "(defparameter *evaluated* NIL)"
		       :output "*EVALUATED*")
		 (list :command (list "(when (> 1 0)"
				      "  (+ 1 2)"
				      "  (setf *evaluated* (not *evaluated*))"
				      "  (+ 4 5))")
		       :output 9)
		 (list :command "*evaluated*"
		       :output "T")
		 (list :command (list "(when (< 1 0)"
				      "  (+ 1 2)"
				      "  (setf *evaluated* (not *evaluated*))"
				      "  (+ 4 5))")
		       :output nil)))
   (repl-4 (list (list :command (list "(cond ((> 1 2) (+ 3 4))"
				      "((string= \"peter\" \"paul\") (+ 5 6))"
				      "((equalp (list 1 2) (list 1 2)) (setq a (+ 7 8))"
				      "                                (oddp a))"
				      "(T 0))")
		       :output T)))

   (repl-5 (list (list :command (list "
```

---

## conditionals.lisp - month-days
Source: gornschool-training/t2/source/conditionals.lisp
Type: tutorial

```
(defun month-days (month &key (leap-year nil))"
				      "(case month"
				      " ((1 3 5 7 8 10 12) 31)"
				      " ((4 6 9 11) 30)"
				      " (2 (if leap-year 29 28))"
				      " (otherwise \"month must be between 1 and 12\"))) ")
		       :output "MONTH-DAYS")
		 (list :command	"(month-days 3)"
		       :output 31)
		 (list :command "(month-days 2 :leap-year t)"
		       :output 29)
		 (list :command "(month-days 13)"
		       :output "month must be between 1 and 12")))

   (repl-6 (list (list :command "(setq self (make-object 'assembly-8))"
		 :output "#<ASSEMBLY-8 #x2103F266ED>")
	   (list :command "(the box-centers)"
		 :output "(#(0.0 0.0 0.0) #(6.0 0.0 0.0) #(12.0 0.0 0.0))")
	   (list :command "(setq self (make-object 'assembly-8 :box-lengths (list 3 8)))"
		 :output "#<ASSEMBLY-8 #x2103F25B2D>")
	   (list :command "(the box-centers)"
		 :output "(#(0.0 0.0 0.0) #(6.0 0.0 0.0))")))

   (code-1 (list "
```

---

## conditionals.lisp - assembly-8
Source: gornschool-training/t2/source/conditionals.lisp
Type: tutorial

```
(define-object assembly-8 (base-object)"
		 ":input-slots"
		 "((box-lengths (list 2 5 8 12)))"
		 ":computed-slots"
		 "((number-of-boxes (if (> (length (the box-lengths)) 3)"
		 "			3"
		 "			(length (the box-lengths))))"
		 ""
		 " (box-centers (case (the number-of-boxes)"
		 "		  (1 (list (make-point 0 0 0)))"
		 "		  (2 (list (make-point 0 0 0)"
		 "			   (make-point 6 0 0)))"
		 "		  (3 (list (make-point 0 0 0)"
		 "			    (make-point 6 0 0)"
		 "			    (make-point 12 0 0)))))"
		 "   "
		 " (box-volumes (list-elements (the my-box) (the-element volume)))"
		 " (box-1-volume (nth 0 (the box-volumes))))"
		 ""
		 ":objects"
		 "((my-box :type 'box"
		 "	   :sequence (:size (the number-of-boxes))"
		 "	   :length (nth (the-child index) (the box-lengths))"
		 "	   :width 2"
		 "	   :height 1"
		 "	   :center (nth (the-child index) (the box-centers)))))"))

   (body-content
    (with-cl-who-string()
      (:p "We use conditionals mainly to implement logic in our computer programs. Common Lisp provides the following conditionals"
	  (:ul (:li (:span :class "special-operator" "if"))
	       (:li (:span :class "macro" "when"))
	       (:li (:span :class "macro" "unless"))
	       (:li (:span :class "macro" "cond"))
	       (:li (:span :class "macro" "case"))))
      (:h3 (:span :class "special-operator" "if"))
      (:p (:span :class "special-operator" "if")" is a special operator.")
      (:p "The syntax is " (:span :class "general-keyword"  "(if test then [else])")
	  ", where"
	  (:ul (:li (:span :class "general-keyword" "test")
		    " is an expression that will evaluate to T or NIL")
	       (:li (:span :class "general-keyword" "then")
		    " is an expression that is evaluated and its value returned if "(:em "test") " is T")
	       (:li (:span :class "general-keyword" "else")
		    " is an optional argument which is an expression to be evalued and its value returned if "
		    (:span :class "general-keyword" "test") " is "
		    (:span :class "value" "NIL") ".")
	       (:li "if "(:span :class "general-keyword" "else")" is not provided, but "
		    (:span :class "general-keyword" "test")
		    " returns "
		    (:span :class "value" "NIL") ", then "
		    (:span :class "special-operator" "if") " also returns "
		    (:span :class "value" "NIL"))))
      (str (repl-example (the repl-1)))

      (:p (:span :class "special-operator" "if") " only allows single expressions for the "
	  (:span :class "general-keyword" "then") " and "
	  (:span :class "general-keyword" "else") " arguments; if multiple expressions needs to be evaluated then the must be wrapped in a "
	  (:span :class "special-operator" "progn") " (which returns the last evaluated value in its body) or "
	  (:span :class "macro"  "prog1") " (which evaluates all expressions in its body in order and finally returns the value of the first). "
	  "Not that it matters much to the routine user, but note that "
	  (:span :class "special-operator" "progn") " is a special operator, whilst "
	  (:span :class "macro" "prog1") " is a macro (which you might imaging utilizes the former in its implementation).")
      (str (repl-example (the repl-2)))
      (:h3 (:span :class "macro" "when"))
      (:p (:span :class "macro" "when")" is a macro.")
      (:p "The syntax is " (:span :class "general-keyword" "(when test body)") ", where "
	  (:ul (:li (:span :class "general-keyword" "test") "is an expression that will evaluate to "
		    (:span :class "value" "T") " or " (:span :class "value" "NIL") ".")
	       (:li (:span :class "general-keyword" "body")
		    " is one or more expressions, an implicit "
		    (:span :class "special-operator" "progn")
		    " such that each expression will be evaluated in order, and finally the value of the last one returned.")
	       (:li "if " (:span :class "general-keyword" "test") " returns "
		    (:span :class "value" "NIL") ", then the "
		    (:span :class "macro" "when") " expression likewise returns "
		    (:span :class "value" "NIL"))))
      (str (repl-example (the repl-3)))
      (:h3 (:span :class "macro" "unless"))
      (:p (:span :class "macro" "unless")" is a macro and acts as the opposite of "
	  (:span :class "macro" "when")". It takes the same arguments, but "
	  (:span :class "general-keyword" "body")" is only evaluated when "
	  (:span :class "general-keyword" "test")" returns "
	  (:span :class "value" "NIL") ")")
      (:h3 (:span :class "macro" "cond"))
      (:pr  (:span :class "macro" "cond")
	    " is a macro which allows multiple conditions to be tested.
The code associated with each condition is wrapped in an implicit
progn allowing multiple expressions to be evaluated per
condition. Each "
	    (:span :class "general-keyword" "test") " and "
	    (:span :class "general-keyword"  "body") " of expressions is specified as a list and the "
	    (:span :class "general-keyword"  "body") " of expressions associated with the first "
	    (:span :class "general-keyword"  "test") " to evaluate to "
	    (:span :class "value" "T") " is evaluated and the value of the last expression in that "
	    (:span :class "general-keyword"  "body") " returned. Once a "
	    (:span :class "general-keyword"  "test") " has evaluated to "
	    (:span :class "value" "T") ", no further "
	    (:span :class "general-keyword"  "tests") " are evaluated.")
      (str (repl-example (the repl-4)))
      (:p "In the example above, the final "
	  (:span :class "general-keyword"  "test")" is always T so this is effectively the default if none of the prior "
	  (:span :class "general-keyword"  "test")" expressions return T")
      (:h3 (:span :class "macro" "case"))
      (:p (:span :class "macro" "case")" is a macro which takes an "
	  (:em "object")" as a first argument, followd by lists of "
	  (:em "key")"-"(:em "expression")" pairs. The "
	  (:em "key")-(:em "expression")" pairs are then tested in order, and where the "
	  (:em "object")" is "
	  (:span :class "function" "eql") " to the "
	  (:em "key")", or a member of the "
	  (:em "key")", then the corresponding "
	  (:em "expression")" is evaluated and returned. Optionally "
	  (:span :class "general-keyword" "T") " or "
	  (:span :class "general-keyword" "otherwise") " may be specified as a catchall "
	  (:em "key")" at the end of the "
	  (:span :class "macro" "case") " expression, meaning that its corresponsing "
	  (:em "expression")" will be evauated and returned if and only if "
	  (:em "object")" is not "
	  (:span :class "function" "eql") " to, or a member of, any of the "
	  (:em "keys") ". Without such a " (:span :class "general-keyword" "otherwise") " or "
	  (:span :class "general-keyword" "T") ", "
	  (:span :class "value" "NIL") " would be returned in such a non-matching situation.")
      (str (repl-example (the repl-5)))
      (:p)
      (:p "We could extend "
	  (:span :class "object" "assembly-8")
	  ", used in the Lists tutorial as follows to make it a bit more flexible, ensuring that the number of box centers defined will always match the number of boxes to be created")
      (str (code-example (the code-1)))
      (str (repl-example (the repl-6)))

      (:h2 "Resources")
      (str (the resource-links))))))


```

---

## gendl-intro.lisp - header
Source: gornschool-training/t2/source/gendl-intro.lisp
Type: tutorial

```
(in-package :training-2)


```

---

## gendl-intro.lisp - gendl-intro
Source: gornschool-training/t2/source/gendl-intro.lisp
Type: tutorial

```
(define-object gendl-intro (base-training-sheet)
  
  :computed-slots
  ((index-words (list "Emacs" "REPL" "YADD" "Geysr"))
  
   (body-content
    (with-cl-who-string()
      (:h3 "What is GendL?")
       "
GendL is a dynamic, declarative, object-oriented language environment
embedded in ANSI Common Lisp (CL).

It consists of a collection of predefined objects, code-expansion
macros, and functions which you, the GendL application developer, may
either use directly or extend, to solve problems of any level of
complexity decomposed into manageable units.

GendL includes geometric primitives (wireframe, surfaces and solids)
and has a built-in web server to facilitate cross-platform deployment.

Unlike many other programming languages which execute procedurally
line by line, GendL only evaluates expressions when they are demanded,
and the object, slot, and function specifications within an object
definition can be written in any order. Another useful and
distinguishing feature is GendL's built-in runtime dependency
tracking. This allows your applicaton code to make modifications to
the value of any slot at runtime, and the GendL runtime system will
immediately mark any downstream dependent slots as \"Unbound\",
forcing them to be re-computed with fresh data the next time they are
demanded."

      (:h3 "GendL development tools")
	    
      (:h4 "Emacs IDE")
      "

GendL is provided with a configuration for the Emacs Integrated
Development Environment (IDE) called \"Slime\", or the \"Superior Lisp
Interaction Mode for Emacs.\" Although any text editor can be used,
the Emacs/Slime combination provides a rich and evolving set of
features for developing GendL and CL code.

Emacs/Slime includes a Read-Eval-Print-Loop (REPL), which is an
interactive, editable command prompt connected to a live GendL
session (which itself is hosted in a running CL process). You will
find that the REPL proves useful for testing and debugging code as
you are writing it.
                          
Emacs also boasts a wide range of open-source plugins for
customization and automation, including plugins for the emerging AI
coding assistants such as Github Copilot.

"


    (:img :src (format nil "/~a-images/emacs.png" (the publish-prefix)) :style "width: auto; height: 200px;" )
      
    (:h4 "YADD")
      "
      Hyperlinked documentation for the objects, macros, functions, and
global parameters provided with GendL is available through any web
browser using YADD, a documentation generator built
using... GendL. Assuming you have a running session which is using
port 9000 for its webserver listener, then reference documentation for
the developer will be accessible at
[http://localhost:9000/yadd](http://localhost:9000/yadd).
		       
YADD may also be used to document your own user-defined objects,
macros, functions, and global parameters. The resulting documentation
available on the main YADD home page side by side with that for the
built-in operators.

In the picture on the right, you can see documentation links for
built-in GendL packages such as CL-LITE, GENDL, GEOM-BASE, GEYSR, GWL
and YADD, as well as user-defined packages such as PURSUIT-ANALYSIS,
WEB-PARTS, and WIND-TUNNEL.

From time to time, supplemental documentation may be published at [the
Genworks
Website](https://genworks.com/documentation){target=\"_new\"}."
                         
      (:img :src (format nil "/~a-images/yadd.png" (the publish-prefix)) :style "width: auto; height: 200px;" )
      (:div :class "grid-item"
	    (:h4 "Geysr")
            "
As code is developed it may be evaluated in the CL
Read-Eval-Print-Loop (REPL).

Additionally, GendL provides a model and object browser called
Geysr. This is a web-based UI which allows objects to be instantiated,
navigated and inspected. Assuming you have a running session which is
using port 9000 for its webserver listener, then Geysr will be
available
at [http://localhost:9000/geysr](http://localhost:9000/geysr).

Geysr is also integrated with the REPL. By invoking Geysr's \"Set
Self!\" action on any object in the object instance tree displayed
Geysr, that object will become bound to a global variable `self`,
allowing you to work with that object (including access to its
ancestor and descendant instances). As with YADD, Geysr is also built
using GendL itself."
                         
	    (:img :src (format nil "/~a-images/geysr.png" (the publish-prefix)) :style "width: auto; height: 200px;")
            (:div :class "main-page-item"
	          (:h2 "Resources")
	          (str (the resource-links))))))))


```

---

## object-definition-tw.lisp - header
Source: gornschool-training/t2/source/object-definition-tw.lisp
Type: tutorial

```
(in-package :training-2)

```

---

## object-definition-tw.lisp - object-definition
Source: gornschool-training/t2/source/object-definition-tw.lisp
Type: tutorial

```
(define-object object-definition (base-training-sheet)
  
  :computed-slots
  ((additional-header-content (with-cl-who-string()
                                ((:link :rel "stylesheet" :href "/training-style.css"))
                                (:link :rel "stylesheet" :href "/tw-css-build/tailwind-built.css")
                                (:title (str (the page-title)))))

   (index-words (list "define-object" "mixin" ":input-slots" ":computed-slots" ":objects" ":functions" "mixin precedence" "the"))

   (code-1 (list
	    "
```

---

## object-definition-tw.lisp - my-box-1a
Source: gornschool-training/t2/source/object-definition-tw.lisp
Type: tutorial

```
(define-object my-box-1a (box)"
	    " :input-slots"
	    " ((length 2)"
	    "  (width 3)"
	    "  (height 4)))"))
   (code-2 (list
	    "
```

---

## object-definition-tw.lisp - my-box-1b
Source: gornschool-training/t2/source/object-definition-tw.lisp
Type: tutorial

```
(define-object my-box-1b ()"
	    " :input-slots"
	    " ((length 2)"
	    "  (width 3)"
	    "  (height 4)))"
	    ""
	    "
```

---

## object-definition-tw.lisp - my-box-2
Source: gornschool-training/t2/source/object-definition-tw.lisp
Type: tutorial

```
(define-object my-box-2 (my-box-1b box))"
	    " "
	    "
```

---

## object-definition-tw.lisp - my-box-3
Source: gornschool-training/t2/source/object-definition-tw.lisp
Type: tutorial

```
(define-object my-box-3 (box my-box-1b))"
	    " "
	    ))
   (code-3 (list
	    "
```

---

## object-definition-tw.lisp - my-box-4
Source: gornschool-training/t2/source/object-definition-tw.lisp
Type: tutorial

```
(define-object my-box-4 (box)"
	    " :input-slots"
	    " (length"
	    "  (width 4)"
	    "  (height 4)))"))
   (code-4 (list
	    "
```

---

## object-definition-tw.lisp - my-box-4
Source: gornschool-training/t2/source/object-definition-tw.lisp
Type: tutorial

```
(define-object my-box-4 (box)"
	    " :input-slots"
	    " (length"
	    "  (width 4)"
	    "  (height 4)))"
	    " :computed-slots"
	    " ((density 7800)"
	    "  (mass (* (div (the volume) 1000000000) (the density)))))"))

   (code-5 (list
	    "
```

---

## object-definition-tw.lisp - assembly-1
Source: gornschool-training/t2/source/object-definition-tw.lisp
Type: tutorial

```
(define-object assembly-1 (base-object)"
	    " :objects"
	    " ((my-box :type 'my-box-4"
	    "          :length 10)"
	    "  (my-sphere :type 'sphere"
	    "             :radius (the my-box width))))"))
   
   (repl-1 (list (list :command "(setq self (make-object 'box)"
		       :output "#<BOX #x210348242D>")
		 (list :command "(the length)"
		       :output 0)
		 (list :command "(the width)"
		       :output 0)
		 (list :command "(the height)"
		       :output 0)
		 (list :command "(the volume)"
		       :output 0)
		 (list :command "(setq self (make-object 'my-box-1a)"
		       :output "#<BOX #x210348242D>")
		 (list :command "(the length)"
		       :output 2)
		 (list :command "(the width)"
		       :output 3)
		 (list :command "(the height)"
		       :output 4)
		 (list :command "(the volume)"
		       :output 24)))
   (repl-2 (list 
	    (list :command "(setq self (make-object 'my-box-2)"
		  :output "#<MY-BOX-2 #x210346742D>")
	    (list :command "(the length)"
		  :output 2)
	    (list :command "(setq self (make-object 'my-box-3)"
		  :output "#<MY-BOX-3 #x210349442D>")
	    (list :command "(the length)"
		  :output 0)))

   (repl-3 (list 
	    (list :command "(setq self (make-object 'my-box-4))"
		  :output "#<MY-BOX-4 #x2103467C4D>")
	    (list :command "(the length)"
		  :output "Invoking restart: Return to SLIME's top level."
		  :error "; Evaluation aborted on #<SIMPLE-ERROR #x21056CE7DD>.")
	    (list :command "(the volume)"
		  :output "Invoking restart: Return to SLIME's top level."
		  :error "; Evaluation aborted on #<SIMPLE-ERROR #x21056B76DD>.")
	    (list :command "(setq self (make-object 'my-box-4 :length 3))"
		  :output "#<MY-BOX-4 #x21034F4C4D>")
	    (list :command "(the length)"
		  :output 3)
	    (list :command "(the volume)"
		  :output 48)))

   (repl-4 (list (list :command "(setq self (make-object 'my-box-4 :length 3 :width 10))"
		       :output "#<MY-BOX-4 #x21034F4C4D>")
		 (list :command "(the volume)"
		       :output 120)))
   
   (main-sheet-body (with-cl-who-string()
		      (when gwl:*developing?* (str (the development-links)))
		      (:div :class "main-page-container"
		            (:div :class "main-page-item"
			          ((:h2 :class "text-lg text-indigo-600 font-semibold") (str (the page-header)))


                                  ((:h2 :class "text-lg text-indigo-600 font-semibold") "Hey Now")
                                  
		                  (:div :class "grid-container-2-650px"
			                (:div :class "grid-item"
		      
			                      (:p "Recall on the slide about "
                                                  ((:a :href (the instantiate-repl url)) "Instantiating Objects in the REPL")
                                                  ", we defined our own object using the Gendl "
			                          (:span :class "macro" "define-object") " macro similarly to the following:")
			                      (str (code-example (the code-1)))
			                      (:p "In this section we will look a bit deeper into " (:span :class "macro" "define-object") ".")
			                      (:p "The syntax for " (:span :class "macro" "define-object") " is:")
			                      (:p (wmd "`
```

---

## object-definition-tw.lisp - *definition-name*
Source: gornschool-training/t2/source/object-definition-tw.lisp
Type: tutorial

```
(define-object` *definition-name* `([`*mixins**`]) [`*specifications**`])`"))
                        
			                      (:p "where:"
                                                  (wmd "

- *definition-name* is a symbol naming the object being defined.
- *mixins** means zero or more symbols naming other object definitions, from which this object type will inherit characteristics.
- *specifications** comprise the body of the object definition and consist of a set
of keyword-value pairs which describe the characteristics and behaviors of any given instance of the object type."))
	                        
			                      (:h2 "mixins")
			                      (:p "In the above example, "
			                          (:span :class "object" "my-box-1a") " specifies "
			                          (:span :class "object" "box") " as a mixin. Because "
			                          (:span :class "object" "box") " includes a built-in slot "
			                          (:span :class "slot" "volume") " , "
			                          (:span :class "object" "my-box-1a") " will also include a "
                                                  (:span :class "slot" "volume") " slot. The primitive "
			                          (:span :class "object" "box") " also includes "
			                          (:span :class "slot" "length") ", "
			                          (:span :class "slot" "width") " and "
			                          (:span :class "slot" "height")", but specifies their default values each to be "
			                          (:span :class "value" "0") ". In the example above, because "
			                          (:span :class "object" "my-box-1a") " also includes "
			                          (:span :class "slot" "length") ", "
			                          (:span :class "slot" "width") ", and "
			                          (:span :class "slot" "height")", and assigns default values to them ("
                                                  (:span :class "value" "2") ", " (:span :class "value" "3") " and "
                                                  (:span :class "value" "4") ", respectively), 
these values override the default "
			                          (:span :class "value" "0") " values in any instances we may make of "
                                                  (:span :class "object" "my-box-1a")
			                          " as long as we do not feed in any other specific values at
instantiation-time.")

		                              (str (repl-example (the repl-1)))
                        
		                              (:p "If you specify multiple mixins, precedence on the slots is left to right (and depth-first).")
		                              (str (code-example (the code-2)))
		                              (str (repl-example (the repl-2)))
                        
			                      (:h2 "specifications")
			                      (:p "The specifications section is what really defines the object.
 This can be thought of as its computational DNA.")
			                      (:p "Each section of the specification is identified by one of a few supported keyword symbols.
The most common ones are:"
			                          (:ul (:li (:span :class "object-keyword" ":input-slots"))
				                       (:li (:span :class "object-keyword" ":computed-slots"))
				                       (:li (:span :class "object-keyword" ":objects"))
				                       (:li (:span :class "object-keyword" ":functions"))))
			                      (:h3 (:span :class "object-keyword" ":input-slots"))
			                      (:p  (:span :class "object-keyword" ":input-slots") " specify any required and/or optional
inputs to the object. Each input-slot may be "
			                           (:ul (:li "a symbol &ndash; in this case it is a required input for the object")
				                        (:li "a symbol-value pair enclosed in parentheses &ndash; in this case
the slot is provided with a default value which may be overridden by "
                                                             (:em "passing in")
                                                             " a different value, either from the parent object or from a toplevel
call to the "
                                                             (:span :class "function" "make-object")
                                                             " function.")))
			                      (:p "In the example below, "
			                          (:span :class "slot" "length") " is required, but "
			                          (:span :class "slot" "width") " and "
			                          (:span :class "slot" "height") " each default to "
			                          (:span :class "value" "4") ".")
			                      (str (code-example (the code-3)))
			                      (:p "If "
			                          (:span :class "object" "my-box-4") " is instantiated, and "
			                          (:span :class "slot" "length")" (or any attribute which depends on "
			                          (:span :class "slot" "length") ") is evaluated, then an error 
will result, as "
			                          (:span :class "slot" "length")
			                          " does not have a value. In other words, to be of any use,
 this object must be instantiated with " (:span :class "slot" "length")" passed in explicitly
as an input.")
			                      (str (repl-example (the repl-3)))
			                      (:p (:span :class "object-keyword" ":input-slots")" which have default 
values may have those values over-ridden when the object is instantiated")
			                      (str (repl-example (the repl-4)))
			                      (:h3 (:span :class "object-keyword"  ":computed-slots"))
			                      (:p  (:span :class "object-keyword" ":computed-slots") " can represent 
known values, intermediate results, or final outputs which may be computed by an object")
			                      (:p  "They are defined as symbol-value pairs enclosed in parenthases. 
The value can be any Common Lisp value or expression.")
			                      (:p  (:span :class "object-keyword" ":computed-slots")
			                           " can refer to the return values of other "
			                           (:span :class "object-keyword" ":computed-slots")
			                           " using the GendL macro " (:span :class "macro""the"))
			                      (str (code-example (the code-4)))
			                      (:p "In the example above, a computed-slot " (:span :class "slot" "density")
                                                  " has been created and set to " (:span :class "value" "7800")
                                                  " (the density of steel in kg/m" (:sup "3") "). A further slot has 
been created, " (:span :class "slot" "mass") ", which divides the "
			                          (:span :class "slot" "volume") " by "
                                                  (:span :class "value" "1000000") " and then multiplies that 
result by the value of "
                                                  (:span :class "slot" "density"))
			                      (:p (:em "A point to note here is the Gendl is dimensionless. 
It is the responsibility of the programmer to ensure units are correct when performing 
calculations. Implicit in the example here is that length, width and height are specified 
in mm and the resultant mass will be in kg"))
			                      (:p "We will cover the use of functions to perform calculations in 
more detail later in this tutorial")
		                              (:h3 (:span :class "object-keyword" ":objects"))
		                              (:p "The " (:span :class "object-keyword" ":objects")
			                          " section is where child objects are specified. 
This specification includes:"
			                          (:ul (:li (wmd "The object *name*, i.e. the name of the slot which 
will contain this child object instance"))
				                       (:li (wmd "The object *type*, which is expected to correspond to 
an object definition name specified in another ")
				                            (:span :class "function" "define-object") ".")
				                       (:li "The object input values"))
			                          "The object type is identified with the "
			                          (:code ":type")
			                          " keyword, followed by a literal (quoted) symbol or an expression
which evaluates to a symbol. The input values consist of a spliced-in plist of
keyword-value pairs.")
		                              (:p "In the definition below, the parent object ("
			                          (:span :class "object" "assembly-1")
			                          ") has two child objects: one called "
			                          (:span :class "object" "my-box")" based on "
			                          (:span :class "object" "my-box-4") " with "
			                          (:span :class "slot" "length") " set to "
			                          (:span :class "value" "10") ", and another called "
			                          (:span :class "object" "my-sphere") ", based on the GendL "
			                          (:span :class "object" "sphere") " object, with its "
			                          (:span :class "slot" "radius")" specified as being equal to the "
			                          (:span :class "slot" "width")" of "
			                          (:span :class "slot" "my-box"))
			                      (:p "Note that when specifying the inputs to an object"
			                          (:ul (:li "the name must match one of the symbols in the object's "
				                            (:span :class "object-keyword" ":input-slots") ".")
				                       (:li "the name must be specified as a keyword (ie preceeded by a "
				                            (:b (:code ":")) ") so an "
				                            (:span :class "object-keyword" ":input-slot") " "
				                            (:span :class "slot" "length") " is specified as "
				                            (:b ":length") " when specifying the inputs")))
		                              (str (code-example (the code-5)))
			                      (:p "Instantiating assembly-1 in Geysr and drawing the leaves will 
look like this")
			                      (:img :src (format nil "/~a-images/assembly-1.png" (the publish-prefix)) :style "width: auto; height: 200px;" )
			                      (:h3 (:span :class "object-keyword" ":functions"))
			                      (:p "The "
			                          (:span :class "object-keyword" ":functions") " section is where 
you put the names, argument lists and bodies for "
			                          (:span :class "object-keyword" ":functions") " which can operate 
within the context of the Gendl object they are defined in. "
			                          "They shouldn't be confused with Common Lisp functions which are 
defined with "
			                          (:em (:b"defun"))", although the syntax is very similar. The biggest 
difference is that a Gendl "
			                          (:span :class "object-keyword" ":function") " can access messages 
within the object it is defined in by using the Gendl "
			                          (:span :class "macro" "the") " macro")
			                      (:p "We will cover more on "
			                          (:span :class "object-keyword" ":functions") " in a later part of 
this tutorial"))))
		       
			
		            ((:div :class "main-page-item")
			     (:h2 "Resources")
			     (str (the resource-links))))))))






```

---

## lists.lisp - header
Source: gornschool-training/t2/source/lists.lisp
Type: tutorial

```
(in-package :training-2)


```

---

## lists.lisp - lists
Source: gornschool-training/t2/source/lists.lisp
Type: tutorial

```
(define-object lists (base-training-sheet)
  :computed-slots
  ((index-words (list "list" "quote" "'" "append" "cons" "push" "car" "cdr" "cadr"
                      "first" "second" "tenth" "nth" "nthcdr""last" "lastcar" 
		      "getf" "setf" "plist" "length" "list-elements" "the-element"
                      "the-child" "index"))
   (repl-1 (list (list :command "(setq a (list 1 2 3))"
		       :output "(1 2 3)")))
   (repl-2 (list (list :command "(setq b '(4 5 6 7 8 9))"
		       :output "(4 5 6 7 8 9)")))
   (repl-3 (list (list :command "(first a)"
			:output 1)
		  (list :command "(rest a)"
			:output "(2 3)")))
   (repl-4 (list (list :command "(car a)"
		       :output 1)
		 (list :command "(cdr a)"
		       :output "(2 3)")
		 (list :command "(caddr a)"
		       :output 3)))
   
   (repl-5 (list (list :command "(first a)"
		       :output 1)
		 (list :command "(fourth b)"
		       :output 7)))
   (repl-6 (list (list :command "(last a)"
		       :output "(3)")
		 (list :command "(lastcar a)"
		       :output 3)))
   (repl-7 (list (list :command "(nth 3 b)"
		       :output "7")
		 (list :command "(nthcdr 3 b)"
		       :output "(7 8 9)")))
   (repl-8 (list (list :command "(setq c (list :a 1 :b 2 :c 3))"
		       :output "(:A 1 :B 2 :C 3)")
		 (list :command "(getf c :b)"
		       :output 2)))
   (repl-9 (list (list :command "(setq c (list :a 1 :b 2 :c 3 :a 4))"
		       :output "(:A 1 :B 2 :C 3 :A 4)")
		 (list :command "(getf c :a)"
		       :output 1)))
   (repl-10 (list (list :command "(setq d (list :a 1 :b 2 :c 3))"
			:output "(:A 1 :B 2 :C 3)")
		  (list :command "(setf (getf d :b) 10)"
			:output 10)
		  (list :command "d"
			:output "(:A 1 :B 10 :C 3)")))

   (repl-11 (list (list :command "(append (list 1 2) (list 3 4))"
			:output "(1 2 3 4)")
		  (list :command "(append (list 1 2) (list 3 4) 5)"
			:output "(1 2 3 4 . 5)")
		  (list :command "(append 0 (list 1 2) (list 3 4))"
			:error "; Evaluation aborted on #<TYPE-ERROR #x2103F1595D>.")
		  (list :command "(append (list 1 2) 0 (list 3 4))"
			:error "; Evaluation aborted on #<TYPE-ERROR #x2103EBBCBD>.")))
		  
   (repl-12 (list (list :command "(cons 1 (list 2 3 4))"	  
			:output "(1 2 3 4)")
		  (list :command" (cons (list 1 2) (list 2 3 4))"	  
			:output "((1 2) 2 3 4)")))
   (repl-13 (list (list :command "(setq a nil)"
			:output "NIL")
		  (list :command "(push 1 a)"
			:output "(1)")
		  (list :command "(push 2 a)"
			:output "(2 1)")
		  (list :command "(push 2 a)"
			:output "(3 2 1)")
		  (list :command "a"
			:output "(3 2 1)")
		  (list :command "(push (list 1 2 3) a)"
			:output "((1 2 3) 3 2 1)")))

   (repl-14 (list (list :command "(setq self (make-object 'assembly-8))"
			:output "#<GDL-USER::ASSEMBLY-8 #x210456C58D>")
		  (list :command "(list-elements (the my-box))"
			:output "(#<BOX #x210456A86D> #<BOX #x210456F2BD> #<BOX #x210456EE9D>)")
		  (list :command "(the box-volumes)"
			:output "(4 10 16)")))


   (code-1 (list "
```

---

## lists.lisp - assembly-8
Source: gornschool-training/t2/source/lists.lisp
Type: tutorial

```
(define-object assembly-8 (base-object)"
		 ""
		 ":computed-slots"
		 "((box-lengths (list 2 5 8))"
		 " (box-centers (list (make-point 0 0 0)"
		 "		    (make-point 6 0 0)"
		 "		    (make-point 12 0 0)))"
		 " (number-of-boxes (length (the box-lengths)))"
		 " (box-volumes (list-elements (the my-box) (the-element volume)))"
		 " (box-1-volume (nth 0 (the box-volumes))))"
		 ""
		 ":objects"
		 "((my-box :type 'box"
		 "	 :sequence (:size (the number-of-boxes))"
		 "	 :length (nth (the-child index) (the box-lengths))"
		 "	 :width 2"
		 "	 :height 1"
		 "	 :center (nth (the-child index) (the box-centers)))))"))


   (body-content (with-cl-who-string()
		   (:p "Lists are fundamental to Lisp - LISt Processing")
		   (:p "A list is defined textually as zero or more elements enclosed by parentheses. The elements can actually be anything, e.g. numbers, strings, objects, more lists, keywords")
		   (:p (:span :class "general-keyword" "nil") " is defined an an empty list. It also acts as the Boolean value for false.
This is one of the few places in Lisp where a single value assumes more than one meaning")
		   (:h3 "Creating Lists")
		   (:p "There are 2 basic ways to create a list"
		       (:ul (:li "Using the "
				 (:span :class "function""list")" function"))
                       
		       (str (repl-example  (the repl-1)))
		       (:ul (:li "Quoting a literal list"))
		       (str (repl-example (the repl-2)))
		       "However, there are many other functions which return lists")
                   
		   (:h3 "Adding to lists")
                   
		   (:p "The Common Lisp function "
		       ((:span :class "function") "append") " takes 2 or more lists and returns a new list
which results from appending them into a single list. It does not modify the given input lists. If the arguments are not lists then "
		       ((:span :class "function") "append") " will behave in different ways depending on where the non-list element is"
		       (:ul (:li "If it is the last of the arguments append will generate a dotted list")
			    (:li "If it is in any other position an error will be generated")))
		   
		   (str (repl-example (the repl-11)))
                        
		   (:p "If the second argument to the Common Lisp function "
		       ((:span :class "function") "cons")" is a list, then "
		       ((:span :class "function") "cons")" will return a list with the first
argument prepended to the front of it. Contrast the difference in output compared to "
		       ((:span :class "function") "append")" when the first argument to "
		       ((:span :class "function") "cons")" is a list")
		   (str (repl-example (the repl-12)))
		   (:p "the Common Lisp macro "((:span :class "macro") "push")" works in much the same way as  "
		       ((:span :class "function") "cons") " but "
                       ((:span :class "function") "push") " modifies the list (its second argument) in-place,
while " ((:span :class "function") "cons") " simply returns a new list and does not modify its arguments in any way.")
                   
		   (str (repl-example (the repl-13))) 
		   
		   (:h3 "Accessing elements within a list")
		   (:p "The Lisp function "
		       ((:span :class "function") "first")" returns the first element of a list, whilst the Lisp function "
		       ((:span :class "function") "rest")" returns a list minus the first element")
		   (str (repl-example (the repl-3)))
		   
		   (:p "Lisp also defines some archaically-named list access functions which you may encounter in legacy code,such as  "
                       (:span :class "function" "car") " (synonym for "
                       (:span :class "function" "first") "), "
                       (:span :class "function" "cdr") " (synonym for "
                       (:span :class "function" "rest") "), and some compound ones such as "
                       ((:span :class "function")"cadr") ", "
                       ((:span :class "function")"caddr") ", and so on.")
                   (str (repl-example (the repl-4)))
		   (:p "If you encounter any of these archaic names, you may look them up in a standard CL reference.")
		   (:p "Common Lisp defines first through to tenth as functions to retrieve the corresponding element of a list")
		   (str (repl-example (the repl-5)))
		   (:p "The Common Lisp function "
		       (:span :class "function""last")" returns the last element of a list "
		       (:em (:b "as a list"))". To get the last element, use the GendL function "
		       (:span :class "function""lastcar"))
		   (str (repl-example (the repl-6)))
		   (:p "Finally to return any element in a list, Common Lisp provides the function "
		       (:span :class "function""nth")". "
		       (:span :class "function""nth")" takes an index number and a list and will return the the element at the position in the list defined by the index number. Note that nth is zero-based, so the first element in a list in (nth 0 [list]). There is a corresponding function "
		       (:span :class "function""nthcdr")", again taking an index number and list as arguments which will return the nth "
		       (:span :class "function""cdr")" of a list")
		   (str (repl-example (the repl-7)))
		   (:h3 "Plists")
		   (:p "Plists are a special type of list which is made up of keyword-value pairs. Rather than accessing elements of the list by position, the value element is accessed by referencing the keyword. Plists are a very convenient way to hold data and are widely used. The "
		       (:span :class "function""getf")" function, which takes a Plist and keyword as arguments returns the value immediately following the keyword")
		   (str (repl-example (the repl-8)))
		   (:p "Note that if a keyword is defined more than once in a plist, "
		       (:span :class "function""getf")" will return the value associated with the first occurrence of the keyword")
		   (str (repl-example (the repl-9)))
		   (:p "Values in a plist may be updated by using a combination of the Common Lisp "
		       (:span :class "function""setf")" and "
		       (:span :class "function""getf")" functions")
		   (str (repl-example (the repl-10)))
			
		   (:h3 "Using lists in GendL")
		   (:p "The example below is based on "
		       ((:span :class "object")"assembly-5")" from the Sequences of Objects tutorial, converted to use lists. A few features to observe"
		       (:ul (:li "The number of "
				 (:span :class "object""my-box")" objects is now calculated based on the elements in the list defined by the "
				 (:span :class "keyword"":computed-slot")" "
				 (:span :class "slot""box-lengths")", using the Common Lisp function "
				 (:span :class "function""length"))
			    (:li "The "
				 (:span :class "slot""length")" of each "
				 (:span :class "object""my-box")" object uses ("
				 (:span :class "macro""the-child")" "
				 (:span :class "slot""index")") to access the nth element of the "
				 (:span :class "slot""box-lengths")" list")
			    (:li "The "
				 (:span :class "keyword"":computed-slot")" "
				 (:span :class "slot""box-volumes")" makes use of the GendL macro "
				 (:span :class "macro""list-elements")" to list all of the "
				 (:span :class "object""my-box")" objects and then send the volume message to them using the GendL macro "(:span :class "macro""the-element"))))
                   (:div :class "main-page-container"
	                 (:div :class "main-page-item"
			       (str (code-example (the code-1))))
			 
			 (:div :class "main-page-item"
			       (:img :src (format nil "/~a-images/using-lists.png" (the publish-prefix)) :style "width: auto; height: 300px; margin: 1em 0 1em 3% ;" )))
		   (str (repl-example (the repl-14)))
			    
			    
		   (:div :class "main-page-item"
			 (:h2 "Resources")
			 (str (the resource-links)))))))
  


```

---

## positioning-and-orientation.lisp - header
Source: gornschool-training/t2/source/positioning-and-orientation.lisp
Type: tutorial

```
(in-package :training-2)


```

---

## positioning-and-orientation.lisp - positioning-and-orientation
Source: gornschool-training/t2/source/positioning-and-orientation.lisp
Type: tutorial

```
(define-object positioning-and-orientation (base-training-sheet)
  :computed-slots
  ((index-words (list "center" ":orientation" "alignment" "make-point" "translate-along-vector" "make-vector" "face-center" "face-normal-vector" "edge-center"
		      "vertex" ":top"":bottom"":front" ":rear" ":left" ":right" "the-child" "co-ordinate system" "axis system"))

   (code-1 (list "
```

---

## positioning-and-orientation.lisp - assembly-2
Source: gornschool-training/t2/source/positioning-and-orientation.lisp
Type: tutorial

```
(define-object assembly-2 (base-object)"
		 " :objects"
		 " ((box-1 :type 'box"
		 "         :length 5"
		 "         :width 1"
		 "         :height 1)"
		 "  (box-2 :type 'box"
		 "         :length 10"
		 "         :height 5"
		 "         :width 3"
		 "         :center (make-point 2 2 2))"
		 "  (box-3 :type 'box"
		 "         :length 5"
		 "         :height 5"
		 "         :width 5"
		 "         :center (translate-along-vector"
                 "                   (the box-2 center)"
		 "                   (make-vector 1 1 0)"
		 "                   5))))"))


   (code-2 (list "
```

---

## positioning-and-orientation.lisp - assembly-3
Source: gornschool-training/t2/source/positioning-and-orientation.lisp
Type: tutorial

```
(define-object assembly-3 (base-object)"
		 " :objects"
		 " ((box-1 :type 'box"
		 "         :length 5"
		 "         :width 1"
		 "         :height 1)"
		 "  (box-2 :type 'box"
		 "         :length 10"
		 "         :height 5"
		 "         :width 3"
		 "         :center (translate-along-vector"
                 "                   (the box-1 (face-center :rear))"
		 "                   (the box-1 (face-normal-vector :rear))"
                 "                   (half (the-child length))))"
		 "  (box-3 :type 'box"
		 "         :length 5"
		 "         :height 5"
		 "         :width 5"
		 "         :center (translate-along-vector"
                 "                    (the box-2 (face-center :rear))"
		 "                    (the box-2 (face-normal-vector :rear))"
		 "                    (half (the-child length)))))"))

   (code-3 (list "
```

---

## positioning-and-orientation.lisp - assembly-4
Source: gornschool-training/t2/source/positioning-and-orientation.lisp
Type: tutorial

```
(define-object assembly-4 (base-object)"
		 "  :objects"
		 "  ((box-1 :type 'box"
		 "          :length 5"
		 "          :width 1"
		 "          :height 1)"
		 "   (box-2 :type 'box"
		 "          :length 5"
		 "          :width 1"
		 "          :height 1"
		 "          :orientation (alignment :rear (the box-1 (face-normal-vector :top)))))"
		 "  )"))

   (code-4 (list "
```

---

## positioning-and-orientation.lisp - assembly-5
Source: gornschool-training/t2/source/positioning-and-orientation.lisp
Type: tutorial

```
(define-object assembly-5 (base-object)"
		 " :objects"
		 " ((box-1 :type 'box"
		 "         :length 5"
		 "         :width 1"
		 "         :height 1"
		 "         :orientation (alignment :rear (the (face-normal-vector :right))))"
		 "  (box-2 :type 'box"
		 "         :length 10"
		 "         :height 5"
		 "         :width 3"
		 "         :orientation (alignment :rear (the box-1 (face-normal-vector :rear)))"
		 "         :center (translate-along-vector (the box-1 (face-center :rear))"
		 "                                         (the box-1 (face-normal-vector :rear))"
		 "                                         (half (the-child length))))"
		 "  (box-3 :type 'box"
		 "         :length 5"
		 "         :height 5"
		 "         :width 5"
		 "         :orientation (alignment :rear (the box-2 (face-normal-vector :rear)))"		 
		 "         :center (translate-along-vector (the box-2 (face-center :rear))"
		 "                                         (the box-2 (face-normal-vector :rear))"
		 "                                         (half (the-child length)))"))
   
   (repl-1 (list (list :command "(make-point 2 2 2)"
		       :output "#(2.0 2.0 2.0)")))
   
   (repl-2 (list (list :command "(setq self (make-object 'assembly-2))"
		       :output "#<ASSEMBLY-2 #x210462875D>")
		 (list :command "(translate-along-vector (the box-2 center) (make-vector 1 1 0) 5)"
		       :output "#(5.535533905932738 5.535533905932738 2.0)")))

   (repl-3 (list (list :command "(setq self (make-object 'assembly-3))"
		       :output "#<ASSEMBLY-3 #x210494BB9D>")
		 (list :command "(the box-1 (face-center :rear))"
		       :output "#(0.0 2.5 0.0)")
		 (list :command "(the box-1 (face-normal-vector :rear))"
		       :output "#(0.0 1.0 0.0)")))


   (body-content (with-cl-who-string()
		   (when gwl:*developing?* (str (the development-links)))
		   (:h2 (str (the page-header)))
		      
		   (:div
                    :class "main-page-container"
		    (:div
                     :class "main-page-item"
		     (:div
		      :class "main-page-container"
		      (:div
                       :class "main-page-item"
		       (:p "For geometric objects, GendL has a Cartesian axis system to specify coördinates in the X, Y and Z dimensions. ")
		       (:p "When specifying a geometric object, a number of basic principles apply:")
		       (:ul (:li "The axis system applies to the object, but may be moved or re-oriented relative to other objects.")
			    (:li "The " (:span :class "slot" "center") " of the object is co-incident with the origin of its axis system "
                                 (:span :class "value" "#(0.0 0.0 0.0)") ".")
			    (:li "The " (:span :class "slot" "length") " dimension of the geometric object correponds with the Y axis.")
			    (:li "The " (:span :class "slot" "width") " dimension of the geometric object corresponds with the X axis.")
			    (:li "The " (:span :class "slot" "height") " dimension of the geometric object corresponds with the Z axis.")))
		      (:div :class "main-page-item")
		      (:div
                       :class "main-page-item"
		       (:img :src (format nil "/~a-images/xyz-length-width-height.png" (the publish-prefix)) :style "width: auto; height: 200px;" ))
		      (:div
		       :class "main-page-item")
			 
		      (:div
		       :class "main-page-item"
		       (:h3 "Positioning")


		       (:p "By default, the positioning of the center point of a child object is
identical to the center point of its parent. (This is because "
                           (:span :class "slot" "center") " is declared internally to be " (:em "trickle-down") ". We will cover more about "
                           (:span :class "object-keyword" "trickle-down-slots") " and the "
                           (:code "defaulting") " slot modifier later.")
                       (:p "If you would like to override the default "
                           (:span :class "slot" "center")
                           " which \"trickles down\" from the "
                           (:span :class "slot" "parent")
                           " or a higher ancestor in the tree, then you may position them explicitly,
either in an absolute manner (i.e. relative to the global coördinate system), or in a relative
manner (i.e. relative to another object, typically the direct parent), by feeding an explicit "
                           (:span :class "general-keyword" ":center")
                           " input into the child.  Depending on how you specify that "
                           (:span :class "general-keyword" ":center") ", the child will end up positioned relative to the parent (or to another object), or relative to the global coördinate system.")
		       (:p "Consider the following object definition and its display in Geysr:"))
		      (:div
                       :class "main-page-item")
			
		      (:div :class "main-page-item" (str (code-example (the code-1))))
		      (:div :class "main-page-item"
			    (:img :src (format nil "/~a-images/box-position.png"  (the publish-prefix))
                                  :style "width: auto; height: 300px; margin: 1em 0 1em 3% ;" ))
		      (:div :class "main-page-item"
			    (:p (:span :class "object" "box-1") " does not specify a "
			        (:span :class "slot" "center")", so its center will default to that of its parent, or "
                                (:span :class "value" "#(0.0 0.0 0.0)") ".  Note in this case the parent ("
                                (:span :class "object" "assembly-1") ") is defined to include the "
			        (:span :class "object" "base-object") " mixin - this provides the basic coördinate system without providing any explicit geometry.")
			    (:p (:span :class "object" "box-2") " defines an explicit "
			        (:span :class "slot" "center") " using the GendL "
			        (:span :class "macro" "make-point")" macro. The coördinates of this point are defined in absolute global coördinates by
passing numbers directly into that " (:span :class "function" "make-point") " macro."))
		      (:div :class "main-page-item")
		      (:div :class "main-page-item" 
			    (str (repl-example (the repl-1))))
		      (:div :class "main-page-item")
		      (:div :class "main-page-item"
			    (:p)
			    (:p "box-3 defines its "
			        (:span :class "slot" "center")" relative to the "
			        (:span :class "slot" "center")" of "
                                (:span :class "slot" "box-2") " instance by using the "
			        (:span :class "function" "translate-along-vector")" function. "
			        (:span :class "function" "translate-along-vector")" takes the following inputs:"
			        (:ul (:li "a 3d-point which is to be translated, in this case the center of box-2;")
				     (:li "a vector which specifies the direction of translation - in this case we are translating in the direction pointed to
by an arrow from the origin to one unit in the X direction, one unit in the Y direction and zero units in the Z direction;")
				     (:li "the distance by which the point it to be translated"))
			        "The coördinates of the resulting point are again defined in the coördinate system of the parent."))
		      (:div :class "main-page-item")
		      (:div :class "main-page-item" (str (repl-example (the repl-2))))
		      (:div :class "main-page-item")
		      (:div :class "main-page-item"
			    (:p "Sometimes it is useful to access points and vectors relative to the reference box of the objects you create. When you create a "
                                (:span :class "object" "box") " for example (whose reference box is identical with itself), the faces of the box are identified as "
			        (:span :class "general-keyword" ":top")", "
			        (:span :class "general-keyword" ":bottom")", "
			        (:span :class "general-keyword" ":left")", "
			        (:span :class "general-keyword" ":right")", "
			        (:span :class "general-keyword" ":front")" and "
			        (:span :class "general-keyword" ":rear")))
		      (:div :class "main-page-item")
		      (:div :class "main-page-item" 
			    (:img :src (format nil "/~a-images/face-notation.png"  (the publish-prefix)) :style "width: auto; height: 300px; margin: 1em 0 1em 3% ;" ))
		      (:div :class "main-page-item")
		      (:div :class "main-page-item"
			    (:p "Various points relative to the bounding-box may then be accessed using the GendL "
			        (:span :class "object-keyword" ":functions")" " 
			        (:span :class "function" "face-center")", "
			        (:span :class "function" "edge-center")" and "
			        (:span :class "function" "vertex")", whilst vectors may be created normal to a face using the GendL "
			        (:span :class "object-keyword" ":function") " "
			        (:span :class "function" "face-normal-vector")". "))
		      (:div :class "main-page-item")
		      (:div :class "main-page-item"
			    (:img :src (format nil "/~a-images/points-and-vector-from-faces.png"  (the publish-prefix)) :style "width: auto; height: 300px; margin: 1em 0 1em 3% ;" ))
		      (:div :class "main-page-item")
		      (:div :class "main-page-item"
			    (:p "So if you wished to position the 3 boxes in "
				(:span :class "object" "assemby-3")
				" such that the rear and front faces of adjoining boxes were coincident, you could achieve that with the following code:"))
		      (:div :class "main-page-item")
		      (:div :class "main-page-item"
			    (str (code-example (the code-2))))
		      (:div :class "main-page-item"
			    (:img :src (format nil "/~a-images/box-coincident-face.png"  (the publish-prefix)) :style "width: auto; height: 300px; margin: 1em 0 1em 3% ;" ))
		      (:div :class "main-page-item"
			    (:p "Two important concepts are also introduced here"
				(:ul (:li "When referencing a GendL function, you can use a normal referencing chain, but if the function has arguments
then you need to wrap that function and its arguments in parentheses. We see examples of this when using the "
					  (:span :class "function" "face-center")" and "
					  (:span :class "function" "face-normal-vector")" functions"))))
		      (:div :class "main-page-item")
		      (:div :class "main-page-item"
			    (str (repl-example (the repl-3))))
		      (:div :class "main-page-item")
		      (:div :class "main-page-item"
			    (:ul (:li "To refer to messages in the child object instances, you can use the macro "
				      (:span :class "macro" "the-child") ". In the case of "
                                      (:span :class "slot" "box-2") " and " (:span :class "slot" "box-3")
                                      ", we want to translate the point by half of each one's own "
				      (:span :class "slot" "length")))
			    (:p ". These points and vectors could, of cource be computed explicitly in
the parent object. However by computing them using expressions
relative to the child instances, you can ensure that the design will \"hang
together\" as intended, i.e. that front and rear faces of adjoining
boxes will continue to be touching, even if the "
                                (:span :class "slot" "center") " and/or " (:span :class "slot" "orientation") " of the first box is changed.")
			    (:h3 "Orientation")
			    (:p "By default, the " (:span :class "slot" "orientation") " of a child object is the same as that for its "
                                (:span :class "slot" "parent") " object. However, you can override that by specifying an "
			        (:span :class "object-keyword" ":orientation") " as an input to the child instance. To compute an orientation, you can use
the GendL "
                                (:span :class "function" "alignment") " "
                                (:span :class "object-keyword" "function") ", which will yield an orientation by aligning one, two, or three
specified axes of our child instance with one, two, or three specified vectors.")
                            (:p "If you compute those specified vectors relative to the coördinate system of the current "
                                (:span :class "variable-name" "self") " (i.e. the "
                                (:span :class "slot" "parent") ", then the resultant "
                                (:span :class "slot" "orientation") " will also end up relative and will automatically respond to any changes.")
                            (:p "The "
			        (:span :class "function" "alignment") " function requires at least one axis-vector pair,
but optionally can accept an additional two axis-vector pairs, providing the second axis
is orthogonal to the first and the third axis is orthogonal to the
first and second")
			    (:p "In the example below, the " (:span :class "general-keyword" ":rear") " axis of box-2 (aligned with the global positive Y axis, "
                                (:span :class "value" "#(0.0 1.0 0.0)") ", is aligned with the vector normal to the "
                                (:span :class "general-keyword" ":top") " face of "
                                (:span :class "slot" "box-1") ". The " (:span :class "slot" "center") " of each box remains the default "
                                (:span :class "value" "#(0.0 0.0 0.0)") "."))
		      (:div :class "main-page-item")
		      (:div :class "main-page-item"
			    (str (code-example (the code-3))))
		      (:div :class "main-page-item"
			    (:img :src (format nil "/~a-images/orientation-1.png" (the publish-prefix)) :style "width: auto; height: 300px; margin: 1em 0 1em 3% ;" ))
		      (:div :class "main-page-item"
			    (:p "In the example below, which is an extension of "
                                (:span :class "object" "assembly-4") ", by being more explicit about the "
			        (:span :class "slot" ":orientation")" being fed into "
                                (:span :class "slot" "box-2") " and "
                                (:span :class "slot" "box-3") ", and aligning them with the "
                                (:span :class "general-keyword" ":rear") " axis of each's neighbor, you can ensure that all boxes remain touching regardless of what "
			        (:span :class "slot" ":orientation")" may be specified in the future for "
                                (:span :class "slot" "box-1") "."))
		      (:div :class "main-page-item") 
		      (:div :class "main-page-item"
			    (str (code-example (the code-4))))
		      (:div :class "main-page-item"
			    (:img :src (format nil "/~a-images/orientation-2.png" (the publish-prefix)) :style "width: auto; height: 300px; margin: 1em 0 1em 3% ;" ))
		      (:div :class "main-page-item"
			    (:h3 "Points and Vectors")
			    (:p "GendL provides a number of functions for manipulating points and vectors. The Resources file points-and-vectors.lisp gives
examples of the most common ones, summarised below. Note that this is not an exhaustive list.")
			    (:h4 "Vectors")
			    (:ul (:li (:span :class "function" "make-vector")" - creates a vector")
			         (:li (:span :class "function" "face-normal-vector")" - (GendL " (:span :class "object-keyword" "function") ") returns a vector normal to a specified face")
			         (:li (:span :class "function" "subtract-vectors")" - takes 2 3d points and returns the vector from the second point to the first point")
			         (:li (:span :class "function" "cross-vectors")" - returns a vector that is orthogonal to the 2 input vectors")
			         (:li (:span :class "function" "rotate-vector")" - rotates a vector around a normal vecort by an angle specified in radians")
			         (:li (:span :class "function" "rotate-vector-d")" - rotates a vector around a normal vecort by an angle specified in degrees")
			         (:li (:span :class "function" "angle-between-vectors")" - returns the angle between 2 vectors in radians")
			         (:li (:span :class "function" "angle-between-vectors-d")" - returns the angle between 2 vectors in degrees"))
			    (:h4 "Points")
			    (:ul (:li  (:span :class "function" "make-point")" - creates a point")
			         (:li  (:span :class "function" "3d-distance")" - the distance between 2 points")
			         (:li  (:span :class "function" "translate-along-vector")" - returns a point resulting from translating the
specified point along the specified vector by the specified distance")
			         (:li  (:span :class "function" "face-center")" (GendL " (:span :class "object-keyword" "function")
                                       ") - returns a point which is the center of a specified reference-box face")
			         (:li  (:span :class "function" "edge-center")" (GendL " (:span :class "object-keyword" "function")
                                       ") - returns a point which is the center of a specified reference-box edge (coincidence of 2 specified reference-box faces)")
			         (:li  (:span :class "function" "vertex") " (GendL " (:span :class "object-keyword" "function")
                                       ") - returns a point which is a vertex (coincidence of 3 specified reference-box faces)")
			         (:li  (:span :class "function" "coincident-points?")" - returns T is 2 points are within a given tolerance of each other")
			         (:li  (:span :class "function" "get-x")", "
				       (:span :class "function" "get-y")", "
				       (:span :class "function" "get-z")" - returns the X, Y or Z value of a specified point"))
			    (:p "See the " (:a :href "/yadd" "online documentation") " relating to the "
                                (:span :class "package-name" ":GEOM-BASE") " package for exact syntax and further details.")))))
		   (:div :class "main-page-item"
			 (:h2 "Resources") (str (the resource-links)))))))




```

---

## building-example-2.lisp - header
Source: gornschool-training/t2/source/building-example-2.lisp
Type: tutorial

```
(in-package :training-2)


```

---

## building-example-2.lisp - building-example-2
Source: gornschool-training/t2/source/building-example-2.lisp
Type: tutorial

```
(define-object building-example-2 (base-training-sheet)
  :functions
  ((hint-1! () (the (set-slot! :hint-1 (not (the hint-1)))))
   (hint-2! () (the (set-slot! :hint-2 (not (the hint-2))))))

  :computed-slots
  ((index-words (list "format" "~f" "~{ ~}" "~a" ))
   (hint-1 nil :settable)
   (hint-2 nil :settable)
   (repl-1 (list (list :command "(make-self 'building)"
		       :output "#<BUILDING #x21045DF6AD>")
		  (list :command "(the building-materials)"
			:output (list "(:FULL-BRICKS 3109"
				      " :HALF-BRICKS 162"
				      " :MORTAR-VOLUME-M3 0.6665175"
				      " :BEAMS ((:LENGTH-MM 874.685657822283"
				      "          :QTY 3)"
				      "         (:LENGTH-MM 1727.7658984943535 "
				      "          :QTY 6)"
				      "         (:LENGTH-MM 3030 "
				      "          :QTY 3))"
				      " :ROOF-CLADDING (:LENGTH 4080 "
				      "                 :WIDTH 1806.755259207346 "
				      "                 :QTY 2))"))))

   (repl-2 (list (list :command (list "(format nil \"Bricks~%======~%  Full Bricks ~a~%  Half Bricks ~a~%\" "
				      "        (getf (the building-materials) :full-bricks)" 
				      "        (getf (the building-materials) :half-bricks))")
		       :output (list "\"Bricks\""
				     "======"
				     "  Full Bricks 3190"
				     "  Half Bricks 162"
				     "\""))))
   (repl-3 (list (list :command (list "(format nil \"Mortar~%======~%  Volume ~,2f m^3~%\""
				      "        (getf (the building-materials) :mortar-volume-m3))")
		       :output (list "\"Mortar"
				     "======"
				     "  Volume 0.67 m^3"
				     "\""))
		 (list :command (list "(format nil "
				      "  \"Roof Cladding~%======~%  Qty ~a~%  Dimensions (L x W x T) ~d x ~d x ~d\")"
				      "(getf (getf (the building-materials) :roof-cladding) :qty)"
				      "(round-to-nearest (getf (getf (the building-materials) :roof-cladding) "
				      "                        :length) 1)"
				      "(round-to-nearest (getf (getf (the building-materials) :roof-cladding) "
				      "                        :width) 1)"
				      "(the cladding-thickness)))")
		       
		       :output (list "\"Roof Cladding"
				     "======"
				     "  Qty 2"
				     "  Dimensions (L x W x T) 4080 x 1807 x 10"
				     "\""))))

   (repl-4 (list (list :command (list "(let* "
				      "  ((bom (the building-materials))"
				      "   (cladding (getf bom :roof-cladding))"
				      "   (bricks (format nil \"Bricks~%======~%  Full Bricks ~a~%  Half Bricks ~a~%\" "
				      "		          (getf bom :full-bricks) "
				      "		          (getf bom :half-bricks)))"
				      "   (mortar (format nil \"Mortar~%======~%  Volume ~,2f m^3~%\" "
				      "		          (getf bom :mortar-volume-m3)))"
				      "   (l (round-to-nearest (getf cladding :length) 1))"
				      "   (w (round-to-nearest (getf cladding :width) 1))"
				      "   (roof "
				      "      (format nil "
				      "       \"Roof Cladding~%======~%  Qty ~a~%  Dimensions (L x W x T) ~d x ~d x ~d~%\" "
				      "         (getf cladding :qty)"
				      "         l w (the cladding-thickness))))"
				      "  (format nil \"~@{~a~}\" bricks mortar roof))")
		       :output (list "\"Bricks"
				     "======"
				     "  Full Bricks 3109"
				     "  Half Bricks 162"
				     "Mortar"
				     "======"
				     "  Volume 0.67 m^3"
				     "Roof Cladding"
				     "======"
				     "  Qty 2"
				     "  Dimensions (L x W x T) 4080 x 1807 x 10"
				     "\""))))

   (repl-5 (list (list :command (list "(let* ((beams (getf (the building-materials) :beams))"
				      "       (beams-list (flatten "
				      "                       (mapcar #'(lambda(a)"
				      "                            (list (getf a :qty) "
				      "                                  (round-to-nearest (getf a :length-mm) 1)))"
				      "		                    beams)))"
				      "       (header (format nil \"Beams~%=====~%\"))"
				      "       (dimensions (format nil \"Section (H x W x T) ~a x ~a x ~a~%\" "
				      "                           (the beam-height) "
				      "                           (the beam-width) "
				      "                           (the wall-thickness)))"
				      "       (lengths (format nil \"~{  Qty ~a Length ~a~%~}\" beams-list)))"
				      " (format nil \"~@{~a~}\" header dimensions lengths))")
		       :output (list "\"Beams"
				     "====="
				     "Section (H x W x T) 50 x 40 x 3"
				     "  Qty 3 Length 875"
				     "  Qty 6 Length 1728"
				     "  Qty 3 Length 3030"
				     "\""))))

   (code-1 (list "define-object building(box)"
		 ":computed-slots"
		 "(..."
		 " ..."
		 "(bom-formatted "
		 "   (let* "
		 "     ((bom (the building-materials))"
		 "	(cladding (getf bom :roof-cladding))"
		 "	(bricks (format nil \"Bricks~%======~%  Full Bricks ~a~%  Half Bricks ~a~%\" "
		 "			(getf bom :full-bricks) "
		 "			(getf bom :half-bricks)))"
		 "	(mortar (format nil \"Mortar~%======~%  Volume ~,3f m^3~%\" "
		 "		        (getf bom :mortar-volume-m3)))"
		 "	(l (round-to-nearest (getf cladding :length) 1))"
		 "	(w (round-to-nearest (getf cladding :width) 1))"
		 "	(roof "
		 "        (format "
		 "           nil "
		 "            \"Roof Cladding~%======~%  Qty ~a~%  Dimensions (L x W x T) ~d x ~d x ~d~%\" "
		 "	        (getf cladding :qty) "
		 "              l w (the cladding-thickness)))"
		 "	(beams (getf (the building-materials) :beams))"
		 "	(beams-list (flatten"
		 "		      (mapcar #'(lambda(a)"
		 "				  (list (getf a :qty) "
		 "                                      (round-to-nearest (getf a :length-mm) 1)))"
		 "			  beams)))"
		 "	"
		 "	    (beams-header" 
		 "             (format "
		 "                nil "
		 "                 \"Beams~%=====~%  Section (H x W x T) ~a x ~a x ~a~%\""
		 "		 (the beam-height) (the beam-width) (the wall-thickness)))"
		 "	    (beam-lengths (format nil \"~{  Qty ~a Length ~a~%~}\" beams-list)))"
		 "   (format nil \"~@{~a~}\" bricks mortar roof beams-header beam-lengths))) "
		 "))"))

   (code-2 (list "
```

---

## building-example-2.lisp - building-bom
Source: gornschool-training/t2/source/building-example-2.lisp
Type: tutorial

```
(defun building-bom (&key (nominal-height 3000)"
		 "                          (nominal-width 3000)"
		 "                          (nominal-length 3000)"
		 "                          (roof-angle 30))"
		 "   (let ((obj (make-object 'building"
		 "			     :nominal-height nominal-height"
		 "			     :nominal-width nominal-width"
		 "			     :nominal-length nominal-length"
		 "			     :truss-angle roof-angle)))"
		 "    (theo obj bom-formatted)))"))

   (repl-6 (list (list :command "(building-bom :nominal-width 4000)"
		       :output (list "\"Bricks"
				     "======"
				     "  Full Bricks 2834"
				     "  Half Bricks 162"
				     "Mortar"
				     "======"
				     "  Volume 0.608 m^3"
				     "Roof Cladding"
				     "======"
				     "  Qty 2"
				     "  Dimensions (L x W x T) 3130 x 2355 x 10"
				     "Beams"
				     "====="
				     "  Section (H x W x T) 50 x 40 x 3"
				     "  Qty 3 Length 1149"
				     "  Qty 6 Length 2276"
				     "  Qty 3 Length 3980"
				     "\""))))
   
   (body-content (with-cl-who-string()
		      ((:div :class "main-page-container")
		       ((:div :class "main-page-item")
			(str (the intro-section main-div))
			(str (the hint-1-section main-div))
			(str (the hint-2-section main-div))
			)
		       ((:div :class "main-page-item")
			(:h2 "Resources")
			(str (the resource-links)))))))

  :objects
  ((intro-section :type 'sheet-section
		  :inner-html (with-cl-who-string()
				(:div :class "grid-container-2-650px"
				      (:div :class "grid-item"
					    (:p "The previous worked example generated a model of a building and a list of materials required.
In this example you will create a formatted bill of materials")
					    (:h3 "Brief")
					    (:p "Based on the building model, deliver a formatted bill of materials. Dimensions where required should be
to the nearest mm and morter volume to the nearest 0.01 m"(:sup "3") " The BOM shuld be delivered as a single formatted string")
                                            (str (the (hint-button :function-key :hint-1!)))))))

		  
   (hint-1-section :type 'sheet-section
		   :inner-html (with-cl-who-string()
				 (when (the hint-1)
				   (htm
				    (:div :class "grid-container-2-650px"
					  (:div :class "grid-item"
						(:p "If you instantiate the "
					((:span :class "object") "building")" object from the previous example and request "
					((:span :class "slot") "(the building-materials)")", you will see the plist identifying the material requirements.")
				   (str (repl-example (the repl-1) ))
				   (:p "Starting with the bricks, we can easily construct a formatted string:")
				   (str (repl-example (the repl-2) ))

				   (:p "And similarly we can construct for the morter and roof cladding.
Note that for the roof cladding we have to round the value before passing to "
				       ((:span :class "object")"format")", this is because if you were to use a "(:em ",0")" prefix parameter with the "
				       ((:span :class "format-directive")"~f")" directive you would still end up with a trailing decimal point.
With the Mortar, on the other hand, "
				       ((:span :class "format-directive")"~,2f")" delivers exactly what you want")

				   (str (repl-example (the repl-3) ))
				   (:p "You can tidy this up and make it a bit more efficient with the use of "
				       ((:span :class "macro") "let*") " with some local bindings")
				   (str (repl-example (the repl-4) ))

				   (:p "Now you need to process the beams")
                                   (str (the (hint-button :function-key :hint-2!)))))))))

   (hint-2-section :type 'sheet-section
		   :inner-html (with-cl-who-string()
				 (when (the hint-2)
				   (htm
				    (:div :class "grid-container-2-650px"
					  (:div :class "grid-item"
						(:p "If you can put the beams data into a list of pairs containing quantity and length for each distinct length, you can then use "
					((:span :class "object") "format")" with its iteration directive "
					((:span :class "format-directive")"~{ ~}") " to write the main output.")
				    (str (repl-example (the repl-5 ) ))
				    (:p "Putting it all together now, you can define a new "
					((:span :class "keyword")":computed-slot")" at the top level of "
					((:span :class "object")"building")" called "
					((:span :class "slot")"bom-formatted")", to deliver the required formatted bill of materials")

				    (str (code-example (the code-1) ))
				    (:p "Additionally you could define a custom constructor function with some keyword inputs which makes a "
					((:span :class "object") "building")" object instance using said inputs, and then calls "
					((:span :class "slot") "bom-formatted")" to yield the formatted BOM output.")
				    
				    (str (code-example (the code-2) ))
				    (str (repl-example (the repl-6 ) ))))))))))


```

---

## strings.lisp - header
Source: gornschool-training/t2/source/strings.lisp
Type: tutorial

```
(in-package :training-2)


```

---

## strings.lisp - strings
Source: gornschool-training/t2/source/strings.lisp
Type: tutorial

```
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

```

---

## truss-example.lisp - header
Source: gornschool-training/t2/source/truss-example.lisp
Type: tutorial

```
(in-package :training-2)


```

---

## truss-example.lisp - truss-example
Source: gornschool-training/t2/source/truss-example.lisp
Type: tutorial

```
(define-object truss-example (base-training-sheet)
  :computed-slots
  ((hint-1 nil :settable)
   (hint-2 nil :settable)
   (hint-3 nil :settable)
   (hint-4 nil :settable)
   (hint-5 nil :settable)
   (hint-6 nil :settable)
   (hint-7 nil :settable)
   (hint-8 nil :settable)
   
   (code-1 (list "
```

---

## truss-example.lisp - truss
Source: gornschool-training/t2/source/truss-example.lisp
Type: tutorial

```
(define-object truss ()"
		 "  :objects"
		 "  ((lower-beam :type 'beam)"
		 "   (vertical-beam :type 'beam)"
		 "   (front-slope-beam :type 'beam)"
		 "   (rear-slope-beam :type 'beam)))"
		 ""
		 "
```

---

## truss-example.lisp - beam
Source: gornschool-training/t2/source/truss-example.lisp
Type: tutorial

```
(define-object beam ()"
		 "  :objects"
		 "  ((outer :type 'box)"
		 "   (inner :type 'box)))"))

   (code-2 (list "
```

---

## truss-example.lisp - beam
Source: gornschool-training/t2/source/truss-example.lisp
Type: tutorial

```
(define-object beam (box)"
		 "  :input-slots"
		 "  ((beam-length 1000)"
		 "   (beam-width 40)"
		 "   (beam-height 50)"
		 "   (wall-thickness 2)"
		 "   (material-density 7800)"
		 "   (tonne-rate 500))"
		 ""
		 "  :computed-slots"
		 "  ((length (the beam-length))"
		 "   (width (the beam-width))"
		 "   (height (the beam-height))"
		 ""
		 "   (beam-volume (- (the outer volume) (the inner volume)))"
		 "   (beam-mass (* (div (the beam-volume) 1000000000) (the material-density)))"
		 "   (beam-cost (* (the tonne-rate) (div (the beam-mass) 1000)))"
		 "   (beam-properties (list :volume-mm3 (the beam-volume)"
		 "			    :mass-kg (round-to-nearest (the beam-mass) 0.01)"
		 "			    :cost-gbp (round-to-nearest (the beam-cost) 0.01)"
		 "			    :length-mm (the beam-length)"
		 "			    :width-mm (the beam-width)"
		 "			    :height-mm (the beam-height)"
		 "			   :thickness-mm (the wall-thickness))))"
		 "   "
		 "  :objects"
		 "  ((outer :type 'box"
		 "	    :length (the beam-length)"
		 "	    :width (the beam-width)"
		 "	    :height (the beam-height))"
		 ""
		 "   (inner :type 'box"
		 "	    :length (the beam-length)"
		 "	    :width (- (the beam-width) (twice (the wall-thickness)))"
		 "	   :height (- (the beam-height) (twice (the wall-thickness))))))"))


  (code-3 (list "
```

---

## truss-example.lisp - degrees-to-radians
Source: gornschool-training/t2/source/truss-example.lisp
Type: tutorial

```
(defun degrees-to-radians (degrees)"
		"   (div (* degrees pi) 180))"
		""
		"
```

---

## truss-example.lisp - truss
Source: gornschool-training/t2/source/truss-example.lisp
Type: tutorial

```
(define-object truss (box)"
		"  :input-slots"
		"  ((truss-length 2000)"
		"   (truss-height 800)"
		"   (truss-angle nil)"
		""
		"   (beam-width 50)"
		"   (beam-height 50)"
		"   (wall-thickness 3))"
		""
		"  :computed-slots"
		"  ("
		"   ;; bounding box dimensions"
		"   (length (the truss-length))"
		"   (height (cond ((the truss-height)(the truss-height))"
		"		 ((the truss-angle) (+ (* (half (the truss-length))"
		"					  (tan (degrees-to-radians (the truss-angle))))"
		"				       (the beam-height)))))"
		"   (width (the beam-width)))"
		""		       
		"  :objects"
		"  ((lower-beam :type 'beam"
		"		:beam-height (the beam-height)"
		"		:beam-width (the beam-width)"
		"		:beam-length (the truss-length)"
		"		:center (translate-along-vector "
		"                          (the (face-center :bottom))"
		"			    (the (face-normal-vector :top))"
		"			    (half (the beam-height))))"
		"    ;;(vertical-beam :type 'beam)"
		"    ;;(front-slope-beam :type 'beam)"
		"    ;;(rear-slope-beam :type 'beam))"
		" ))" ))

   (code-4 (list "
```

---

## truss-example.lisp - truss
Source: gornschool-training/t2/source/truss-example.lisp
Type: tutorial

```
(define-object truss (box)"
		 "..."
		 "..."
		 "  :objects"
		 "  ((lower-beam :type 'beam"
		 "	       :beam-height (the beam-height)"
		 "	       :beam-width (the beam-width)"
		 "	       :beam-length (the truss-length)"
		 "	       :center (translate-along-vector "
		 "                         (the (face-center :bottom))"
		 "                         (the (face-normal-vector :top))"
		 "			   (half (the beam-height))))"
		 "   (vertical-beam :type 'beam"
		 "		  :beam-length (- (the height) (the beam-height))"
		 "		  :beam-height (the beam-height)"
		 "		  :beam-width (the beam-width)"
		 "		  :orientation (alignment :rear (the (face-normal-vector :top))"
		 "                                        :right (the (face-normal-vector :right)))"
		 "		  :center (translate-along-vector "
		 "                           (the lower-beam (face-center :top))"
		 "			     (the lower-beam (face-normal-vector :top))"
		 "			     (half (the-child beam-length))))"
		 " ;;(front-slope-beam :type 'beam)"
		 " ;;(rear-slope-beam :type 'beam))"
		 "))"))

   (code-5 (list "
```

---

## truss-example.lisp - truss
Source: gornschool-training/t2/source/truss-example.lisp
Type: tutorial

```
(define-object truss (box)"
		 "  ..."
		 "  ..."
		 ":computed-slots"
		 " ("
		 "  ..."
		 "  ..."
		 "  (truss-front-slope-vector (subtract-vectors "
		 "                               (the vertical-beam (edge-center :rear :top))"
		 "		                 (the lower-beam (edge-center :front :top))))"
		 " )"
		 ":objects"
		 "  ..."
		 "  ..."
		 ";;(front-slope-beam :type 'beam)"
		 ";;(right-slope-beam :type 'beam)"
		 ""
		 "   (pt-1 :type 'sphere"
		 "	 :radius 5"
		 "	 :display-controls (list :color :green)"
		 "	 :center (the lower-beam (edge-center :front :top)))"
		 "   (pt-2 :type 'sphere"
		 "	 :radius 5"
		 "	 :display-controls (list :color :red)"
		 "	 :center (the vertical-beam (edge-center :rear :top)))"
		 "   (vector-line :type 'vector-line"
		 "		:start-point (the pt-1 center)"
		 "		:vector (the truss-front-slope-vector)"
		 "		:length 150)))"
		 ""
		 "
```

---

## truss-example.lisp - vector-line
Source: gornschool-training/t2/source/truss-example.lisp
Type: tutorial

```
(define-object vector-line (box)"
		 "  :input-slots"
		 "  ((start-point (make-point 0 0 0))"
		 "   (vector (make-vector 1 0 1))"
		 "   (length 50)"
		 "   (width 1))"
		 "  :computed-slots"
		 "  ((height (div (the length) 5)))"
		 "  :objects"
		 "  ((v-line :type 'line"
		 "	   :start (the start-point)"
		 "	   :display-controls (list :color :red)"
		 "	   :end (translate-along-vector (the start-point)"
		 "					(the vector)"
		 "					(the length)))"
		 "   (arrow :type 'cone"
		 "	  :radius-1 0"
		 "	  :radius-2 (div (the length) 50)"
		 "	  :length (div (the length) 5)"
		 "	  :display-controls (list :color :red)"
		 "	  :center (translate-along-vector (the v-line end)"
		 "					  (the vector)"
		 "					  (half (the-child length)))"
		 "	  :orientation (alignment :front (the vector)))))"))

   (code-6 (list "
```

---

## truss-example.lisp - truss
Source: gornschool-training/t2/source/truss-example.lisp
Type: tutorial

```
(define-object truss (box)"
		 "  ..."
		 "  ..."
		 ":computed-slots"
		 " ("
		 "  ..."
		 "  ..."
		 "  (truss-front-slope-vector (subtract-vectors" 
		 "                              (the vertical-beam (edge-center :rear :top))"
		 "			        (the lower-beam (edge-center :front :top))))"
		 "  (front-slope-length (3d-distance "
		 "                         (the vertical-beam (edge-center :rear :top))"
		 "		          (the lower-beam (edge-center :front :top))))"
		 "  (front-slope-center (translate-along-vector "
		 "                         (the front-slope-construction-line center)"
		 "			  (the front-slope-beam (face-normal-vector :bottom))"
		 "			  (half (the beam-height))))"
		 ")"
		 ":objects"
		 "  ..."
		 "  ..."
		 "  (front-slope-beam :type 'beam"
		 "                    :beam-length (the front-slope-length)"
		 "                    :beam-height (the beam-height)"
		 "                    :beam-width (the beam-width)"
		 "                    :center (the front-slope-center)"
		 "                    :orientation (alignment :rear (the truss-front-slope-vector)"
		 "                                          :right (the (face-normal-vector :right))))"
		 ""
		 "  (front-slope-construction-line :type 'line"
		 "				 :start (the lower-beam (edge-center :front :top))"
		 "				 :end (the vertical-beam (edge-center :rear :top)))"
		 "  (mid-pt :type 'sphere"
		 "	    :display-controls (list :color :blue)"
		 "	    :radius 5"
		 "	    :center (the front-slope-construction-line center))"
		 "  ..."
		 "  ..."
		 " ))"))


    (code-7 (list "
```

---

## truss-example.lisp - truss
Source: gornschool-training/t2/source/truss-example.lisp
Type: tutorial

```
(define-object truss (box)"
		  "  ..."
		  "  ..."
		  ":computed-slots"
		  " ("
		  "  (truss-front-slope-vector (subtract-vectors "
		  "                              (the vertical-beam (edge-center :rear :top))"
		  "			        (the lower-beam (edge-center :front :top))))"
		  "  (front-slope-length (3d-distance "
		  "                         (the vertical-beam (edge-center :rear :top))"
		  "			  (the lower-beam (edge-center :front :top))))"
		  "  (front-slope-center (translate-along-vector "
		  "                         (the front-slope-construction-line center)"
		  "			  (the front-slope-beam (face-normal-vector :bottom))"			    
		  "                         (half (the beam-height))))"
		  "  (truss-rear-slope-vector (subtract-vectors "
		  "                             (the vertical-beam (edge-center :rear :bottom))"
		  "			       (the lower-beam (edge-center :rear :top))))"
		  "  (rear-slope-length (3d-distance "
		  "                        (the vertical-beam (edge-center :rear :bottom))"
		  "			  (the lower-beam (edge-center :rear :top))))"
		  "  (rear-slope-center (translate-along-vector "
		  "                        (the rear-slope-construction-line center)"
		  "			  (the rear-slope-beam (face-normal-vector :bottom))"
		  "			  (half (the beam-height))))"
		  "  ..."
		  "  ..."
		  "))"))

   (code-8 (list "
```

---

## truss-example.lisp - truss
Source: gornschool-training/t2/source/truss-example.lisp
Type: tutorial

```
(define-object truss (box)"
		 "  ..."
		 "  ..."
		 " :computed-slots"
		 " ("
		 "  ..."
		 "  ..."
		 "  (truss-front-slope-vector (the (get-slope-vector! :front)))"
		 "  (truss-rear-slope-vector (the (get-slope-vector! :rear)))"
		 ""
		 "  (front-slope-length (the (get-slope-length! :front)))"
		 "  (rear-slope-length (the (get-slope-length! :rear)))"
		 "  "
		 "  (front-slope-center (the (get-slope-center! :front)))"
		 "  (rear-slope-center (the (get-slope-center! :rear)))"
		 " )"
		 ""
		 "  :functions"
		 "  ((get-slope-vector! (beam-side)"
		 "		      (let ((v-key (the (get-v-key! beam-side)))"
		 "			    (l-key (the (get-l-key! beam-side))))"
		 "		      (subtract-vectors (the vertical-beam (edge-center :rear v-key))"
		 "					(the lower-beam (edge-center l-key :top)))))"
		 "   (get-slope-length! (beam-side)"
		 "		      (let ((v-key (the (get-v-key! beam-side)))"
		 "			    (l-key (the (get-l-key! beam-side))))"
		 "			(3d-distance (the vertical-beam (edge-center :rear v-key))"
		 "				     (the lower-beam (edge-center l-key :top)))))"
		 ""
		 "   (get-slope-center! "
		 "     (beam-side)"
		 "     (let ((pt (case beam-side"
		 "		   (:front (the front-slope-construction-line center))"
		 "		   (:rear  (the rear-slope-construction-line center))))"
		 "	     (norm-vector (case beam-side"
		 "                          (:front (the front-slope-beam (face-normal-vector :bottom)))"
		 "                          (:rear (the rear-slope-beam (face-normal-vector :bottom))))))"
		 "	(translate-along-vector "
		 "                           pt"
		 "			     norm-vector"
		 "			     (half (the beam-height)))))"
		 "   (get-v-key! (beam-side)"
		 "	       (case beam-side"
		 "		 (:front :top)"
		 "		 (:rear :bottom)))"
		 "   (get-l-key! (beam-side)"
		 "	       (case beam-side"
		 "		 (:front :front)"
		 "		 (:rear :rear))))"))

   (code-9 (list "
```

---

## truss-example.lisp - truss
Source: gornschool-training/t2/source/truss-example.lisp
Type: tutorial

```
(define-object truss (box)"
		 "  ..."
		 "  ..."
		 " :computed-slots"
		 " ("
		 "  ..."
		 "  ..."
		 "  (beam-properties (mapsend (the children) :beam-properties)) "
		 "  (total-mass (round-to-nearest"
		 "                   (apply '+"
		 "                          (mapcar #'(lambda(a) (getf a :mass-kg))"
		 "	                              (the beam-properties)))"
		 "               0.001)))"
		 "  (total-cost (round-to-nearest"
		 "                   (apply '+"
		 "                          (mapcar #'(lambda(a) (getf a :cost-gbp))"
		 "	                              (the beam-properties)))"
		 "	           0.01)))"
		 " )"
		 ")"))
   
   (repl-1 (list (list :command "(setq self (make-object 'truss))"
		       :output "#<TRUSS #x2104B7CAED>")
		 (list :command "(the total-mass)"
		       :output 22.92)
		 (list :command "(the total-cost)"
		       :output 10.32)))

   (body-content (with-cl-who-string()
		   ((:div :class "main-page-container")
		    ((:div :class "main-page-item")
		     (str (the start-section main-div))
		     (str (the hint-1-section main-div))
		     (str (the hint-2-section main-div))
		     (str (the hint-3-section main-div))
		     (str (the hint-4-section main-div))
		     (str (the hint-5-section main-div))
		     (str (the hint-6-section main-div))
		     (str (the hint-7-section main-div))
		     (str (the hint-8-section main-div)))
		    
		    
		    ((:div :class "main-page-item")
		     (:h2 "Resources")
		     (str (the resource-links-section main-div)))))))


   :functions
  ((hint-1! () (the (set-slot! :hint-1 (not (the hint-1)))))
   (hint-2! () (the (set-slot! :hint-2 (not (the hint-2)))))
   (hint-3! () (the (set-slot! :hint-3 (not (the hint-3)))))
   (hint-4! () (the (set-slot! :hint-4 (not (the hint-4)))))
   (hint-5! () (the (set-slot! :hint-5 (not (the hint-5)))))
   (hint-6! () (the (set-slot! :hint-6 (not (the hint-6)))))
   (hint-7! () (the (set-slot! :hint-7 (not (the hint-7)))))
   (hint-8! () (the (set-slot! :hint-8 (not (the hint-8))))))

   :objects
  ((resource-links-section :type 'sheet-section
			   :inner-html (with-cl-who-string()
					 (:table
					     (let ((icon "/common-images/lisp-file.png")
						   (lis (list (list :available (the hint-1) :file "truss-hint-1.lisp")
							      (list :available (the hint-2) :file "truss-hint-2.lisp")
							      (list :available (the hint-3) :file "truss-hint-3.lisp")
							      (list :available (the hint-4) :file "truss-hint-4.lisp")
							      (list :available (the hint-5) :file "truss-hint-5.lisp")
							      (list :available (the hint-6) :file "truss-hint-6.lisp")
							      (list :available (the hint-6) :file "truss-hint-6a.lisp")
							      (list :available (the hint-7) :file "truss-hint-7.lisp")
							      (list :available (the hint-8) :file "truss-hint-8.lisp"))))
					       (dolist (l lis)
						 (let* ((f (getf l :file))
						       (link (format nil "/t2-resources/~a" f)))
						   (htm (:tr (when (getf l :available))
							     (htm (:td ((:a :href link) ((:img :src icon :style "width: 40px; height: auto;"))))
						                  (:td ((:a :href link) (str f))))))))))))

   (start-section :type 'sheet-section
		  :inner-html (with-cl-who-string()
				(:p "This example builds on the wall example, but adds some orientation and also considers creation and use of custom objects.")
				(:h3 "Example Brief")
				(:p "A roof truss is made from a number of rectangular section hollow steel beams. The beams are H mm high, W mm wide
and have a wall thickness t mm. The length of the truss is L mm. Height may be specified explicitly as Ht mm, or an angle D
degrees may be specified (one or the other, if both are specified then the H dimension should be used.
A sketch of the design is shown. ")
				(:p "For a given beam dimension (H X W X t) and overall truss dimensions (H x L or L with a D degree slope), calculate the total
mass of the truss. If it is made from steel which has a density of 7800 kg/m" (:sup "3") " and the cost of the beam is £450 per metric tonne,
calculate the mass of the truss and the material cost. Joint overlaps may be ignored")
		                (:img :src (format nil "/~a-images/roof-truss-brief.png"  (the publish-prefix)) :style "width: auto; height: 300px; margin: 1em 0 1em 3% ;" )
		                (:p (str (the (hint-button :function-key :hint-1!))))))

   (hint-1-section :type 'sheet-section
	   :inner-html (with-cl-who-string()
			 (when (the hint-1)
			   (htm (:p "As with the wall example, we start by thinking of the object structure. There will be"
				    (:ul (:li "A horizontal beam")
					 (:li "A vertical beam with its bottom end sat on the mid-point of the horizontal beam")
					 (:li "2 sloping beams joining the ends of the horizontal beam with the top end of the vertical beam"))
				    "All beams are the same cross section and we need to gather information about each beam to compute the overall mass and cost.
It would make sense therefore to design a beam object which generates a beam and answers the required property messages.
The 2 sloping beams are potentially candidates for a "
				    ((:span :class "object-keyword")":sequence")
				    ", but on closer consideration the start and end point positioning are not generic and there will always be exactly two
sloping beams, so two explicit child objects are probably the best way forward in this case. With GendL, if this turns out not to be the case and we
see more genericness during coding the we first thought would be present, its easy to change to a different solution approach.
So and object structure could look like this")
				(str (code-example (the code-1)))
				(:p "The next task would be to define our custom beam object")
                                (str (the (hint-button :function-key :hint-2!)))))))

   (hint-2-section :type 'sheet-section
	   :inner-html (with-cl-who-string()
			 (when (the hint-2)
			   (htm (:p "To satisfy the brief requirements we need to calculate the "
				    ((:span :class "slot")"mass")" and "
				    ((:span :class "slot")"cost")" of each beam. To do this we will need to know the "
				    ((:span :class "slot")"volume")" of each beam. Because we are using wireframe we will need to create a box representing the
outer of the beam, a box representing the hollow inner, and subtract the inner box "
				    ((:span :class "slot")"volume")" from the outer box "
				    ((:span :class "slot")"volume")". Of course we could do this analytically without the need of geometry,
but for the sake of this example and potentially some downstream benefits we will probe the geometric entities directly.")
				(:p "So our beam will have two child objects of type "
				    ((:span :class "object") "box")", from which we can get the beam "
				    ((:span :class "slot") "volume")". To calculate the box dimensions we need beam "
				    ((:span :class "slot") "height")", "
				    ((:span :class "slot") "width")" and "
				    ((:span :class "slot") "thickness")", plus the "
				    ((:span :class "slot")"length")". To caculate "
				    ((:span :class "slot")"mass")" we will need inputs to the object for "
				    ((:span :class "slot") "material-density")" and "
				    ((:span :class "slot") "tonne-rate")". So effectively out custom beam object will look like below. By defining some default values for the "
				    ((:span :class "object-keyword") ":input-slots")" we can instantiate this object standalone in Geysr to review the geometry
graphics and output values")
				((:div :class "main-page-container")
				 ((:div :class "main-page-item")
				  (str (code-example (the code-2))))
				 ((:div :class "main-page-item")
				  (:img :src (format nil "/~a-images/geysr-truss-1.png"  (the publish-prefix)) :style "width: auto; height: 300px; margin: 1em 0 1em 3% ;" )))
				(:p "A couple of things to note"
				    (:ul (:li "The properties of the beam have been assembled into a plist to make it easier to return multiple properties at once.")
					 (:li ((:span :class "slot")"beam-volume")", "
					      ((:span :class "slot")"beam-mass")" and "
					      ((:span :class "slot")"beam-cost")" could all have been defined as local variables using a "
					      ((:span :class "special-operator")"let")" binding when calculating the "
					      ((:span :class "slot")"beam-properties")". This would probably have been more efficient, but it reduces flexibility
a bit at this early stage of development. If it turns out that performance improvements are required at a later stage in the development we could easily
implement this by converting these slots into simple "
                                              (:span :class "general-keyword" "let")
                                              " variables, but at this stage the benefit in terms of variable access and visibility is considered more useful")))
				(:p "Now that we have defined the custom "
				    ((:span :class "object")"beam")" object and the "
				    ((:span :class "object-keyword")":input-slots")" it needs to function we need to start working from the top down to pass values through")
                                (str (the (hint-button :function-key :hint-3!)))))))
				

   (hint-3-section :type 'sheet-section
	   :inner-html (with-cl-who-string()
			 (when (the hint-3)
			   (htm (:p "We can break the next step down into 2 stages"
				    (:ul (:li "Define the size of the overall bounding box for the " ((:span :class "object")"truss"))
					 (:li "Pass down the required arguments for the beams which make up the " ((:span :class "object")"truss")" assembly"))
				    "To simplify this step and to allow the top level object to be instantiated in Geysr, all of the child objects except the "
				    ((:span :class "object")"lower-beam")" beam have been commented out")
				(:p ((:span :class "slot")"length")" and "
				    ((:span :class "slot")"width")" of the assembly bounding box are simple; they are the "
				    (:em "length of the truss")" and the "
				    (:em "width of the beam")". As per the spec, if the "
				    (:em "truss height")" is specified, then we should use it, and this will be the height of the bounding box.
However, if height isn't provided and the "
				    (:em "truss slope angle") " is provided, then we can use a bit of triganometry to calculate the height. Two things to note here:"
				    (:ul (:li "the Lisp function "
					      ((:span :class "function")"tan") " takes an angle in "
					      (:em (:b "radians"))  " as its argument, yet the specification says the angle will be specified in degrees.
To make the conversion, a simple function "
					      ((:span :class "function")"degrees-to-radians") ", defined by a supported Gendl package, is used.")
					 (:li "We need to add the "
					      (:em "beam height")" to the "
					      (:em"slope height")" to get the overall bounding box height.")))
				(:p "Now that the bounding box is defined, we can position the "
				    ((:span :class "object") "lower-beam inside that bounding-box by specifying its "
                                     (:span :class "slot" "center") "."))
				((:div :class "main-page-container")
				 ((:div :class "main-page-item")
				  (str (code-example (the code-3))))
				 ((:div :class "main-page-item")
				  (:img :src (format nil "/~a-images/geysr-truss-2.png"  (the publish-prefix)) :style "width: auto; height: 300px; margin: 1em 0 1em 3% ;" )))
				(:p "We can now uncomment the "
				    ((:span :class "object")"vertical-beam")", provide the input values and position and orient it")
                                (str (the (hint-button :function-key :hint-4!)))))))


   (hint-4-section :type 'sheet-section
	   :inner-html (with-cl-who-string()
			 (when (the hint-4)
			   (htm (:p "The "((:span :class "object")"vertical-beam")" is oriented so that its "
				    ((:span :class "object-keyword")":rear")" axis aligns with the "
				    ((:span :class "function")"(face-center :top)")" of the assembly bounding box and its "
				    ((:span :class "object-keyword")":right")" axis aligns with the "
				    ((:span :class "function")"(face-center :top)")" of the assembly bounding box. We then position its center relative to the "
				    ((:span :class "function")"(face-center :top)")" of the "
				    ((:span :class "object")"lower-beam")". Because it sits on top of the "
				    ((:span :class "object")"lower-beam")", its length is defined as the overall assembly "
				    ((:span :class "slot")"height")" minus the "
				    ((:span :class "slot")"height")" of the "
				    ((:span :class "object")"lower-beam"))
				((:div :class "main-page-container")
				 ((:div :class "main-page-item")
				  (str (code-example (the code-4))))
				 ((:div :class "main-page-item")
				  (:img :src (format nil "/~a-images/geysr-truss-3.png" (the publish-prefix)) :style "width: auto; height: 300px; margin: 1em 0 1em 3% ;" )))
				(:p "With the "((:span :class "object")"lower-beam")" and "
				    ((:span :class "object")"vertical-beam")" now defined, we can move on to calculation the "
				    ((:span :class "slot")"length")", "
				    ((:span :class "object-keyword")"position")" and "
				    ((:span :class "object-keyword")"orientation")" of the sloping beams")
                                (str (the (hint-button :function-key :hint-5!)))))))


   (hint-5-section :type 'sheet-section
	   :inner-html (with-cl-who-string()
			 (when (the hint-5)
			   (htm (:p "To determine the "
				    ((:span :class "object-keyword")"orientation")" and "
				    ((:span :class "object-keyword")"position")" of the sloping beam, we can use some construction geometry. Often, when working
with different orientations it can also be helpful to visualise specific points and directions, and we will adopt that approach here.")
				(:p "The "((:span :class "object")"front-slope-beam")" "
				    ((:span :class "function")"(edge-center :front :top)")" and "
				    ((:span :class "function")"(edge-center :rear :top)")" need to be coincident with the "
				    ((:span :class "object")"lower-beam")" "
				    ((:span :class "function")"(edge-center :front :top)")" and the "
				    ((:span :class "object")"vertical-beam")" "
				    ((:span :class "function")"(edge-center :rear :top)")" respectively and its "
				    ((:span :class "object-keyword")"orientation")" will be a vector from the "
				    ((:span :class "object")"lower-beam")" "
				    ((:span :class "function")"(edge-center :front :top)")" to the "
				    ((:span :class "object")"vertical-beam")" "
				    ((:span :class "function")"(edge-center :rear :top)")". To verify this visually, we can include graphical representations of these points and make an object which will display a vector visually. To further help with visual identification we can use the objects "
				    ((:span :class "object-keyword")":display-controls")" to set different colours. The code below includes 2 visual points (using the "
				    ((:span :class "object")"sphere")" primative) and a "
				    ((:span :class "object")"vector-line")" object, which is a custom object to display a vectors orientation. Note the calculation of the slope vector, using the GendL "
				    ((:span :class "function")"subtract-vectors")" function. Of particular interest is the the inputs are actually both points,
but as a point is defined with the same data structure as a vector this will return the vector pointing from the second point argument to the first point argument")
				((:div :class "main-page-container")
				 ((:div :class "main-page-item")
				  (str (code-example (the code-5))))
				 ((:div :class "main-page-item")
				  (:img :src (format nil "/~a-images/geysr-truss-4.png"  (the publish-prefix)) :style "width: auto; height: 300px; margin: 1em 0 1em 3% ;" )))
				(:P "So now we have verified we are using the correct construction points, and that the vector is correct, we need to determine the length of the sloping beam and its position.")
                                (str (the (hint-button :function-key :hint-6!)))))))


   (hint-6-section :type 'sheet-section
	   :inner-html (with-cl-who-string()
			 (when (the hint-6)
			   (htm (:p "Getting the "
				    ((:span :class "slot")"length")" is fairly simple; we use the GendL "
				    ((:span :class "function")"3d-distance")" function, which takes 2 points and returns the distance between them. To determine the center point of the sloping beam, we will use a construction line between the 2 construction points and take its "
				    ((:span :class "slot")"center")" point. (As an alternative we could use the GendL function "
				    ((:span :class "function")"midpoint")") We also need to offset this point in a direction normal to bottom face of the beam by half the "
				    ((:span :class "slot")"beam-height"))
				(:p "Again, we have included a construction point to visualise the mid-point of our construction line. The Geysr screen
shot below shows the beam in Right view with all the construction/visualisation elements turned on to give a visual check that the beam is
positioned correctly"
				((:div :class "main-page-container")
				 ((:div :class "main-page-item")
				  (str (code-example (the code-6))))
				 ((:div :class "main-page-item")
				  (:img :src (format nil "/~a-images/geysr-truss-5.png"  (the publish-prefix)) :style "width: auto; height: 300px; margin: 1em 0 1em 3% ;" )))
				(:p "The process is then repeated for the "
				    ((:span :class "object")"rear-slope-beam")", although some changes need making to calculate the slope vector, beam
center and beam length to reflect the other construction points being used")
				((:div :class "main-page-container")
				 ((:div :class "main-page-item")
				  (str (code-example (the code-7))))
				 ((:div :class "main-page-item")
				  (:img :src (format nil "/~a-images/geysr-truss-6.png"  (the publish-prefix)) :style "width: auto; height: 300px; margin: 1em 0 1em 3% ;" )))
				(:p "At this point we have all the geometry construction complete, but before proceeding to delivering the required
calculation outputs, there are a number of oppertunities to tidy up both the code and the display in Geysr")
                                (str (the (hint-button :function-key :hint-7!))))))))


   
   (hint-7-section :type 'sheet-section
	   :inner-html (with-cl-who-string()
			 (when (the hint-7)
			   (htm (:p "If we examine the code for the "
				    (:em "length")", "
				    (:em "vector")" and "
				    (:em "center")" of the "
				    ((:span :class "object")"front-slope-beam")" and "
				    ((:span :class "object")"rear-slope-beam")", we can see patterns/similarities between them as well as some differences.
These are candidates for replacing with "
				    ((:span :class "object-keyword")":functions")" which will"
				    (:ul (:li "Clean up the code a bit")
					 (:li "Make the code easier to maintain")
					 (:li "Explicitly capture the logic used to differentiate between the "
					      ((:span :class "object")"front-slop-beam")" and the "
					      ((:span :class "object")"rear-slope-beam")))
				    "The principal differences between the vector and distance calculations relate to the "
				    (:em "keywords")" used to identify the construction points and the objects used for points and vectors.
Further examination reveals that the same logic is applied in each case. So it makes sense to have common "
				    ((:span :class "object-keyword")":functions")" to return the respective keywords ("
				    ((:span :class "function")"get-v-key!")" to be used on the "
				    ((:span :class "object")"vertical-beam")" and "
				    ((:span :class "function")"get-l-key!")" to be used on the "
				    ((:span :class "object")"lower-beam")"). These functions are then called from the other "
				    ((:span :class "object-keyword")":functions")" which return the vector and length for each beam")
				(:p "The "((:span :class "object-keyword")":computed-slots")" are also updated to make use of these "
				    ((:span :class "object-keyword")":functions"))
				    
				(:p "If we look in Geysr we see all of the construction geometry. Once the code has been developed we don't particularly
need to see these objects in the model tree. Whilst the points could be commented out, we actually need the construction line as it's part of the
slope beams center calculation. To hide these from displaying in Geysr, rather than include them in the "
				    ((:span :class "object-keyword")":objects")" section of the assembly object, we put them in a "
				    ((:span :class "object-keyword")":hidden-objects")" section. Child-objects defined as "
				    ((:span :class "object-keyword")":hidden-objects")" exist in the model and can be referenced, but don't show up in
the graphics window in Geysr and also do not show in the object tree on the LHS window in Geysr")
				((:div :class "main-page-container")
				 ((:div :class "main-page-item")
				  (str (code-example (the code-8))))
				 ((:div :class "main-page-item")
				  (:img :src (format nil "/~a-images/geysr-truss-7.png"  (the publish-prefix)) :style "width: auto; height: 300px; margin: 1em 0 1em 3% ;" )))
				(:p "The final task is to calculate the "
				    ((:span :class "slot")"total-mass")" and "
				    ((:span :class "slot")"total-cost")" for the truss assembly")
                                (str (the (hint-button :function-key :hint-8!)))))))


   (hint-8-section :type 'sheet-section
	   :inner-html (with-cl-who-string()
			 (when (the hint-8)
			   (htm (:p "The "
				    ((:span :class "object") "beam")" object supports a message "
				    ((:span :class "slot") "beam-properties")" which we need to get from each of the truss assembly beams.
Because we only have beam objects in the "
				    ((:span :class "object-keyword")":objects") " section (having moved construction geometry into "
				    ((:span :class "object-keyword")":hidden-objects") ") we can use one of the base-object messages "
				    ((:span :class "slot")":children")" to get a list of all of the beams. Once we have this list we can use the GendL function "
				    ((:span :class "function")"mapsend")" to send the "
				    ((:span :class "slot")":beam-properties")" message to each of these objects and return a list of "
				    ((:span :class "slot")":beam-properties")" messages. This will be a list of plists and the 2 keyword elements we are interested in are "
				    (:em (:b ":mass-kg"))" and "(:em (:b ":cost-gbp")))
				(:p "Using "
				    ((:span :class "function")"mapcar")" and a "
				    (:em (:b "lambda function"))", we can process this list of plists to retrieve a list of masses and a list of costs. Because the values are in a list, we cannot use the "
				    ((:span :class "function")"+")" function directly to sum these values, we need to use the Lisp "
				    ((:span :class "function")"apply")" function to sum the list values")
			((:div :class "main-page-container")
				 ((:div :class "main-page-item")
				  (str (code-example (the code-9))))
			 ((:div :class "main-page-item")
			  (str (repl-example (the repl-1)))))))))))
				    
   

```

---

## numbers-and-arithmetic.lisp - header
Source: gornschool-training/t2/source/numbers-and-arithmetic.lisp
Type: tutorial

```
(in-package :training-2)


```

---

## numbers-and-arithmetic.lisp - numbers-and-arithmetic
Source: gornschool-training/t2/source/numbers-and-arithmetic.lisp
Type: tutorial

```
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

   
	      
   (code-1 (list"
```

---

## numbers-and-arithmetic.lisp - my-box-4
Source: gornschool-training/t2/source/numbers-and-arithmetic.lisp
Type: tutorial

```
(define-object my-box-4 (box)"
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

```

---

## instantiate-repl.lisp - header
Source: gornschool-training/t2/source/instantiate-repl.lisp
Type: tutorial

```
(in-package :training-2)


```

---

## instantiate-repl.lisp - instantiate-repl
Source: gornschool-training/t2/source/instantiate-repl.lisp
Type: tutorial

```
(define-object instantiate-repl (base-training-sheet)
  :computed-slots
  ((index-words (list "make-object" "the-object" "theo" "setq" "self" ":input-slots" "define-object" "REPL" "compile code"))

   (repl-1 (list (list :command "(setq my-box (make-object 'box :length 2 :width 3 :height 4))"
		       :output "#<BOX #x210348222D>")
		 (list :command "(the-object my-box length)"
		       :output 2)
		 (list :command "(the-object my-box width)"
		       :output 3)
		 (list :command "(the-object my-box height)"
		       :output 4)))

   (repl-2 (list (list :command "(the-object my-box volume)"
		       :output 24)))

   (repl-3 (list (list :command "(theo my-box volume)"
		       :output 24)))
   
   (repl-4 (list (list :command "(setq self (make-object 'box :length 2 :width 3 :height 4))"
		       :output "#<BOX #x210348242D>")
		 (list :command "(the volume)"
		       :output 24)
                 (list :command "(the-object self volume)"
		       :output 24)
                 (list :command "(theo self volume)"
		       :output 24)))

   (repl-4b (list (list :command "(make-self 'box :length 1 :width 2 :height 3)"
		        :output "#<BOX #x210348242D>")
		  (list :command "(the volume)"
		        :output 6)
                  (list :command "(the-object self volume)"
		        :output 6)
                  (list :command "(theo self volume)"
		        :output 6)))

   (repl-5 (list (list :command "(setq self (make-object 'my-box-1))"
		       :output "#<BOX #x210344342D>")
		 (list :command "(the volume)"
		       :output 24)))

   (repl-6 (list (list :command "(setq self (make-object 'my-box-1 :length 8))"
		       :output "#<BOX #x210478242D>")
		 (list :command "(the volume)"
		       :output 96)))

   (setq-repl (list (list :command "(setq a 1)"
			  :output 1)
		    (list :command "(setq b 2 c \"three\" d nil)"
			  :output "NIL")
		    (list :command "b"
			  :output 2)
		    (list :command "c"
			  :output "three")
		    (list :command "d"
			  :output "NIL")))
   
   (code-1 (list
	    "
```

---

## instantiate-repl.lisp - my-box-1
Source: gornschool-training/t2/source/instantiate-repl.lisp
Type: tutorial

```
(define-object my-box-1 (box)"
	    " :input-slots"
	    " ((length 2)"
	    "  (width 3)"
	    "  (height 4)))"))

   (body-content (with-cl-who-string()
		   (:p "GendL includes a collection of object definitions which you can use
directly or customise/enhance to suit particular needs.")
		   (:p "To work with instances of these objects you may choose to "
                       (:em "instantiate")" an object in the REPL. For example, to instantiate a GendL object of type "
                       (:span :class "object" "box") " in the REPL, we call GendL's general-purpose "
                       (:span :class "function" "make-object")
                       " constructor function, which yields a freshly instantiated object which we assign to a symbol ("
                       (:span :class "variable-name"  "my-box") ") in the below example), using the Lisp special operator "
                       (:span :class "special-operator" "setq") ". The "
                       (:span :class "function" "make-object")
                       " function takes an object name as its required argument, followed by a
\"spliced-in\" list of alternating slot-names and corresponding
desired initial values. This type of list which is made up from
alternating key-value pairs is called a " (:i "plist") ". You will learn more
about plists later.
                            
In the example below we are instantiating a wireframe volumetric " (:span :class "object" "box") " and initializing its "
                       (:span :class "slot" "length")", "
                       (:span :class "slot" "width") " and "
                       (:span :class "slot" "height") " slots to "
                       (:span :class "value" "2") ", " (:span :class "value" "3") ", and "
                       (:span :class "value" "4") ", respectively.")
		   (:p "Using the macro "
		       (:span :class "macro" "the-object") 
                       ", you can then \"send a message\" to this object, which yields the current
value of the requested slot.")
		      
		   (str (repl-example (the repl-1)))
		   (:p "Note that " (:span :class "special-operator" "setq")
                       " is a standard CL special operator which takes one or more symbol-value
pairs, assigns the respective values to the symbols, and finally returns the last value.")
		   (str (repl-example (the setq-repl)))

		   (:p "As with most built-in primitives, The GendL " (:span :class "object" "box")
                       " supports a number of built-in messages, for example "
		       (:span :class "slot" "volume")
                       ". To send the " (:span :class "slot" "volume") " message to our "
                       (:span :class "variable-name" "my-box")
                       " instance and get its value returned, we will need to use a "
                       (:i "reference chain") ". A reference chain consists of a reference operator such as "
                       (:span :class "macro" "the-object") ", followed by an expression which is expected
to return an object instance (the example below this expression is simply that "
                       (:span :class "variable-name" "my-box") " symbol).")
                        
		   (str (repl-example (the repl-2)))

		   (:p "You can use the abbreviation "
		       ((:span :class "macro")"theo") " rather than "
		       ((:span :class "macro")"the-object") ". It does the same thing with less typing.")
		   (str (repl-example (the repl-3)))
		       
		   (:p "As an added convenience, you can assign the object to the special variable "
                       (:span :class "variable-name" "self")
                       " rather than to an arbitrarily-named variable. If you take this approach, then rather than using "
		       (:span :class "macro" "theo") " or "
		       (:span :class "macro" "the-object") " to refer to an object expression explicitly when
sending a message, you now have the option of using the GendL macro "
		       (:span :class "macro" "the") " to send a message to the object, and automatically "
                       (:span :class "variable-name" "self") " will be assumed as the object to receive the message. ")

                   (:p "In the following REPL interaction, the last three expressions are equivalent with each other:")

                   (str (repl-example (the repl-4)))

                        
		   (:p "While " (:span :class "variable-name" "self")
                       " is a normal symbol, it has a special meaning within the context of GendL.
It is fine to use "
                       (:span :class "variable-name") "self" " in the REPL, you should never bind or set/modify "
                       (:span :class "variable-name") "self" " within the body of a GendL object definition (created using the "
                       (:span :class "macro")"define-object" " macro)")


                   (:p "As yet an additional convenience, you can use the built-in GendL function "
                       (:span :class "function" "make-self")
                       " to instantiate an object and set it to the toplevel "
                       (:span :class "variable-name" "self")
                       ", all in one fell swoop:")

                   (str (repl-example (the repl-4b)))
                        
                        
		   (:p "If you'd like to create a named \"blueprint\"
of an object instance, e.g. for being able to instantiate customized
boxes without needing to feed in customized slot values, then you may
use GendL's "
                       (:span :class "macro" "define-object") " macro. In the example below we are
defining a new object "
                       (:span :class "object" "my-box")
                            
                       ",  which is effectively a customisation of the default Gendl "
		       (:span :class "object" "box") " object, by specifying "
		       (:span :class "object" "box") " as part of its "
                       (:i "mixin list") ". More about "
		       (:span :class "macro""define-object") " later.")
                        
		   (str (code-example (the code-1)))
                        
		   (:p "After this object definition has been compiled & loaded into the
running system (in Emacs/Slime this can be done with the cursor in the object and pressing C-c C-c),
then you can instantiate it in the REPL. Note that because the new object "
		       (:span :class "object" "my-box") " specifies default values for the "
		       (:span :class "object-keyword" ":input-slots")" , you do not need to feed them in when making instances of "
                       (:span :class "object" "my-box") " in the REPL")
		   (str (repl-example (the repl-5)))

		   (:p "However, you continue to have the ability to feed in alternative values of "
		       (:span :class "object-keyword" ":input-slots") " for "
		       (:span :class "object" "my-box"))
		   (str (repl-example (the repl-6)))
  
		   (str (the resource-links))))))

```

---

## instantiate-geysr.lisp - header
Source: gornschool-training/t2/source/instantiate-geysr.lisp
Type: tutorial

```
(in-package :training-2)


```

---

## instantiate-geysr.lisp - instantiate-geysr
Source: gornschool-training/t2/source/instantiate-geysr.lisp
Type: tutorial

```
(define-object instantiate-geysr (base-training-sheet)
  
  :computed-slots
  ((body-content (with-cl-who-string()
		   (:p "As a companion to the REPL interaction described on the previous slide, you can use the built-in
Geysr object browser.")
		   (:p "Geysr is particularly helpful when working with geometry,
as it includes a viewport for rendering the geometry.")
		   (:p "Assuming the Gendl web server is running on port 9000, Geysr may be accessed at "
                       ((:a :href "http://localhost:9000/geysr") "http://localhost:9000/geysr") ".")
		   ((:div :class "grid-container-2")
		    ((:div :class "grid-item")
		     (:p "When Geysr is opened, a splash screen is displayed:"))
		    ((:div :class "grid-item")
		     (:img :src (format nil "/~a-images/geysr-splash.png" (the publish-prefix)) :style "width: auto; height: 200px;" ))
		    ((:div :class "grid-item")
		     (:p "To instantiate a compiled object, select:")
		     (:p (:b "File..New") ", and in the text field enter the desired package name and the object
type name separated by " (:b "::"))
                     (:p "For the example code in the Resources section, this will be " (:code "gdl-user::my-box-1") ".")
		     (:p "Press " (:b "Enter") " to instantiate the object."))
		    ((:div :class "grid-item")
		     (:img :src (format nil "/~a-images/geysr-file-new.png" (the publish-prefix)) :style "width: auto; height: 100px;" ) (:br)
		     (:img :src (format nil "/~a-images/geysr-package-object.png" (the publish-prefix)) :style "width: auto; height: 100px;" ))
		    ((:div :class "grid-item")
		     (:p "The object will be instantiated, and all of its available slots will
be displayed in the "
                         (:i "Inspector") " pane in the lower Left-hand section of the browser window.")
		     (:p "Note that most slots will initially be shown as " (:em "unbound") ".")
		     (:p "Clicking on the " (:em "unbound") " link will cause the value of the associated
slot to be evaluated. In the lower screenshot the slot "
			 ((:span :class "slot") "length") " has been evaluated and returns "
                         ((:span :class "value") "4") "."))
		    ((:div :class "grid-item")
		     (:img :src (format nil "/~a-images/geysr-instantiate.png" (the publish-prefix)) :style "width: auto; height: 200px;" ) (:br)
		     (:img :src (format nil "/~a-images/geysr-eval.png" (the publish-prefix)) :style "width: auto; height: 200px;" ))
		    ((:div :class "grid-item")
		     (:p "To draw the geometry: first click or tap ")
		     (:p (:b "Mode") " and ensure " (:b "Add Leaves") " is selected with a checkmark"))
		    ((:div :class "grid-item")
		     (:img :src (format nil "/~a-images/geysr-add-leaves.png" (the publish-prefix)):style "width: auto; height: 100px;" ))
		    ((:div :class "grid-item")
		     (:p "Then left mouse click or tap on the object name just below the menu bar (in this case "
			 (:code "GDL-USER::MY-BOX-1") "), and the geometry will be displayed in the main viewport")
		     (:p "The geometry camera view may be changed by selecting "
			 (:b "View..Perspective") " and picking any of the pre-defined orientations such a top or left.")
		     (:p "The geometry viewport may be cleared by selecting "
			 (:b "View..Clear!")))
		    ((:div :class "grid-item")
		     (:img :src (format nil "/~a-images/geysr-geometry.png" (the publish-prefix)):style "width: auto; height: 400px;" ))
		    ((:div :class "grid-item")
		     (:p "By selecting " (:b "Mode..Set Self...") " then clicking on any object in the tree pane at upper-left,
the toplevel value of "
                         ((:span :class "variable-name") "self") " will be set to that object, to similar effect
as typing "
                         (:code "(setq self (make-object 'my-box-1))") " at the REPL. You may then evaluate any slot in
my-box-1 at the REPL"))
		    ((:div :class "grid-item")
		     (str (repl-example (the repl-set-self)))))
		   (:h3 "Creating Fresh Instances")
		   (:p 

                    (wmd "__Note__: __File..New__ is currently __only__ supported for a *Fresh Web Browser Tab*.
It is not currently supported to make a fresh instance in an existing Geysr browser tab or window.

So, if you'd like to make a new, fresh instance (of the same or a different type),
then follow these steps:

1. Open a *new* Web Browser tab or window.
1. Visit the toplevel  [Geysr url](/geysr).
1. Click or tap __File..New__ and proceed as in the __File..New__ section above.


"))
                   (:h3 "Updating Existing Instances after a Code Change")
                   (:p "If you want to see the results of a change you've made to the code
and loaded into the running session (don't worry, you will learn how
compile & load code changes later), then select the "
                       (:b "Update") " mode (click or tap " (:b "Mode..Update...") "),
and finally click in the tree on the object for which you would like
to unload any cached results it may be holding, and let the system
recompute fresh everything based on your latest code (typically the
node you click to "
                       (:b "Update")
                       " will simply be the root object in the tree).")
                        
                   (:p (wmd "
This recomputation forms one of the steps of a five-step iterative
process you'll typically follow when developing using Geysr:

1. Author your desired code (new code or changes to existing code).
1. Compile the code and load it into the running session (compiling is optional but recommended).
1. Update the instance in the running Geysr session to reflect the new code.
1. Inspect and poke around with the results in the various panes of Geysr
1. Based on said results, return to Step 1 above and repeat.

Your typical development session will include many iterations of the
above five steps. Depending on the situation at hand it is possible to
complete a single iteration very quickly, sometimes in less than one
minute. This rapid iteration potential is one of the upsides of
becoming proficient with GendL combined with Geysr."))

                   (:h3 "A Note on Symbol Names")
                   (:p (wmd "Note that object type names in Gendl (as in Common Lisp) have two parts:

1. a Package name
2. a Symbol name

These can be written together separated by a double colon (`::`).")

                       " For example, the symbol naming our " ((:span :class "object") "my-box-1")
                       " definition was introduced into the system while the "
                       ((:span :class "package-name") "gdl-user") " package was active, or
\"current\" &mdash; therefore you can refer to this definition with the fully qualified symbol
name: "
                       ((:span :class "package-name") "gdl-user")
                       (:code "::")
                       ((:span :class "object") "my-box-1")
                       (wmd
                        "Sometimes it is possible to use a single colon (`:`) rather than a
double colon (`::`), and often it is possible to omit the package name
entirely when referring to a symbol.

That's enough on Packages and Symbols for now. You will learn more about Packages and Symbols later."))
			    
		   (:h2 "Resources")
		   (str (the resource-links))))

   (repl-set-self (list (list :comment "Self is now set to to #<MY-BOX-1 #x210500D41D>, you may use command-line interaction....")
			(list :comment "127.0.0.1 - - [Thu, 02 Jun 2022 09:00:48 GMT] \"POST /gdlAjax HTTP/1.1\" 200 222")
			(list :command "(the)"
			      :output "#<MY-BOX-1 #x210500D41D>")
			(list :command "(the length)"
			      :output 2)
			(list :command "(the height)"
			      :output 4)
			(list :command "(the width)"
			      :output 3)
			(list :command "(the volume)"
			     :output 24))))

   )

```

---

## object-definition.lisp - header
Source: gornschool-training/t2/source/object-definition.lisp
Type: tutorial

```
(in-package :training-2)


```

---

## object-definition.lisp - object-definition
Source: gornschool-training/t2/source/object-definition.lisp
Type: tutorial

```
(define-object object-definition (base-training-sheet)
  
  :computed-slots
  (
   (index-words (list "define-object" "mixin" ":input-slots" ":computed-slots" ":objects" ":functions" "mixin precedence" "the"))

   (code-1 (list
	    "
```

---

## object-definition.lisp - my-box-1a
Source: gornschool-training/t2/source/object-definition.lisp
Type: tutorial

```
(define-object my-box-1a (box)"
	    " :input-slots"
	    " ((length 2)"
	    "  (width 3)"
	    "  (height 4)))"))
   (code-2 (list
	    "
```

---

## object-definition.lisp - my-box-1b
Source: gornschool-training/t2/source/object-definition.lisp
Type: tutorial

```
(define-object my-box-1b ()"
	    " :input-slots"
	    " ((length 2)"
	    "  (width 3)"
	    "  (height 4)))"
	    ""
	    "
```

---

## object-definition.lisp - my-box-2
Source: gornschool-training/t2/source/object-definition.lisp
Type: tutorial

```
(define-object my-box-2 (my-box-1b box))"
	    " "
	    "
```

---

## object-definition.lisp - my-box-3
Source: gornschool-training/t2/source/object-definition.lisp
Type: tutorial

```
(define-object my-box-3 (box my-box-1b))"
	    " "
	    ))
   (code-3 (list
	    "
```

---

## object-definition.lisp - my-box-4
Source: gornschool-training/t2/source/object-definition.lisp
Type: tutorial

```
(define-object my-box-4 (box)"
	    " :input-slots"
	    " (length"
	    "  (width 4)"
	    "  (height 4)))"))
   (code-4 (list
	    "
```

---

## object-definition.lisp - my-box-4
Source: gornschool-training/t2/source/object-definition.lisp
Type: tutorial

```
(define-object my-box-4 (box)"
	    " :input-slots"
	    " (length"
	    "  (width 4)"
	    "  (height 4)))"
	    " :computed-slots"
	    " ((density 7800)"
	    "  (mass (* (div (the volume) 1000000000) (the density)))))"))

   (code-5 (list
	    "
```

---

## object-definition.lisp - assembly-1
Source: gornschool-training/t2/source/object-definition.lisp
Type: tutorial

```
(define-object assembly-1 (base-object)"
	    " :objects"
	    " ((my-box :type 'my-box-4"
	    "          :length 10)"
	    "  (my-sphere :type 'sphere"
	    "             :radius (the my-box width))))"))
   
   (repl-1 (list (list :command "(setq self (make-object 'box)"
		       :output "#<BOX #x210348242D>")
		 (list :command "(the length)"
		       :output 0)
		 (list :command "(the width)"
		       :output 0)
		 (list :command "(the height)"
		       :output 0)
		 (list :command "(the volume)"
		       :output 0)
		 (list :command "(setq self (make-object 'my-box-1a)"
		       :output "#<BOX #x210348242D>")
		 (list :command "(the length)"
		       :output 2)
		 (list :command "(the width)"
		       :output 3)
		 (list :command "(the height)"
		       :output 4)
		 (list :command "(the volume)"
		       :output 24)))
   (repl-2 (list 
	    (list :command "(setq self (make-object 'my-box-2)"
		  :output "#<MY-BOX-2 #x210346742D>")
	    (list :command "(the length)"
		  :output 2)
	    (list :command "(setq self (make-object 'my-box-3)"
		  :output "#<MY-BOX-3 #x210349442D>")
	    (list :command "(the length)"
		  :output 0)))

   (repl-3 (list 
	    (list :command "(setq self (make-object 'my-box-4))"
		  :output "#<MY-BOX-4 #x2103467C4D>")
	    (list :command "(the length)"
		  :output "Invoking restart: Return to SLIME's top level."
		  :error "; Evaluation aborted on #<SIMPLE-ERROR #x21056CE7DD>.")
	    (list :command "(the volume)"
		  :output "Invoking restart: Return to SLIME's top level."
		  :error "; Evaluation aborted on #<SIMPLE-ERROR #x21056B76DD>.")
	    (list :command "(setq self (make-object 'my-box-4 :length 3))"
		  :output "#<MY-BOX-4 #x21034F4C4D>")
	    (list :command "(the length)"
		  :output 3)
	    (list :command "(the volume)"
		  :output 48)))

   (repl-4 (list (list :command "(setq self (make-object 'my-box-4 :length 3 :width 10))"
		       :output "#<MY-BOX-4 #x21034F4C4D>")
		 (list :command "(the volume)"
		       :output 120)))
   
   (body-content (with-cl-who-string()
		   (:p "Recall on the slide about "
                       ((:a :href (the instantiate-repl url)) "Instantiating Objects in the REPL")
                       ", we defined our own object using the Gendl "
		       (:span :class "macro" "define-object") " macro similarly to the following:")
		   (str (code-example (the code-1)))
		   (:p "In this section we will look a bit deeper into " (:span :class "macro" "define-object") ".")
		   (:p "The syntax for " (:span :class "macro" "define-object") " is:")
		   (:p (str (code-example (list "
```

---

## object-definition.lisp - definition-name
Source: gornschool-training/t2/source/object-definition.lisp
Type: tutorial

```
(define-object definition-name ([mixins*]) [specifications*])"))))
                        
		   (:p "where:"
                       (:dl
                        (:dt "definition-name")
                        (:dd "is a symbol naming the object being defined.")
                        (:dt "mixins*")
                        (:dd "means zero or more symbols naming other object definitions,
from which this object type will inherit characteristics.")
                        (:dt "specifications*")
                        (:dd "comprise the body of the object definition and consist of a set
of keyword-value pairs which describe the characteristics and behaviors of any given instance of the object type.")))
	                                      
		   (:h2 "mixins")
		   (:p "In the above example, "
		       (:span :class "object" "my-box-1a") " specifies "
		       (:span :class "object" "box") " as a mixin. Because "
		       (:span :class "object" "box") " includes a built-in slot "
		       (:span :class "slot" "volume") " , "
		       (:span :class "object" "my-box-1a") " will also include a "
                       (:span :class "slot" "volume") " slot. The primitive "
		       (:span :class "object" "box") " also includes "
		       (:span :class "slot" "length") ", "
		       (:span :class "slot" "width") " and "
		       (:span :class "slot" "height")", but specifies their default values each to be "
		       (:span :class "value" "0") ". In the example above, because "
		       (:span :class "object" "my-box-1a") " also includes "
		       (:span :class "slot" "length") ", "
		       (:span :class "slot" "width") ", and "
		       (:span :class "slot" "height")", and assigns default values to them ("
                       (:span :class "value" "2") ", " (:span :class "value" "3") " and "
                       (:span :class "value" "4") ", respectively), 
these values override the default "
		       (:span :class "value" "0") " values in any instances we may make of "
                       (:span :class "object" "my-box-1a")
		       " as long as we do not feed in any other specific values at
instantiation-time.")

		   (str (repl-example (the repl-1)))
                        
		   (:p "If you specify multiple mixins, precedence on the slots is left to right (and depth-first).")

                   (str (code-example (the code-2)))
                   :br 
		   (str (repl-example (the repl-2)))
                        
		   (:h2 "specifications")
		   (:p "The specifications section is what really defines the object.
 This can be thought of as its computational DNA.")
		   (:p "Each section of the specification is identified by one of a few supported keyword symbols.
The most common ones are:"
		       (:ul :class "list-disc list-outside ml-3 mr-8 bg-zinc-100"
                            (:li (:span :class "object-keyword" ":input-slots"))
			    (:li (:span :class "object-keyword" ":computed-slots"))
			    (:li (:span :class "object-keyword" ":objects"))
			    (:li (:span :class "object-keyword" ":functions"))))
		   (:h3 (:span :class "object-keyword" ":input-slots"))
		   (:p  (:span :class "object-keyword" ":input-slots") " specify any required and/or optional
inputs to the object. Each input-slot may be "

                        (:dl (:dt "a symbol")
                             (:dd "In this case it is a required input for the object.")
                             (:dt "a symbol-value pair enclosed in parentheses")
                             (:dd "in this case the slot is provided with a default value which may be overridden by "
                                  (:em "passing in")
                                  " a different value, either from the parent object or from a toplevel call to the "
                                  (:span :class "function" "make-object") " function.")))
		   (:p "In the example below, "
		       (:span :class "slot" "length") " is required, but "
		       (:span :class "slot" "width") " and "
		       (:span :class "slot" "height") " each default to "
		       (:span :class "value" "4") ".")
                                        
		   (str (code-example (the code-3)))
                                        
		   (:p "If "
		       (:span :class "object" "my-box-4") " is instantiated, and "
		       (:span :class "slot" "length")" (or any attribute which depends on "
		       (:span :class "slot" "length") ") is evaluated, then an error 
will result, as "
		       (:span :class "slot" "length")
		       " does not have a value. In other words, to be of any use,
 this object must be instantiated with " (:span :class "slot" "length")" passed in explicitly
as an input.")
		   (str (repl-example (the repl-3)))
		   (:p (:span :class "object-keyword" ":input-slots")" which have default 
values may have those values over-ridden when the object is instantiated")
                                              
		   ;;(str (repl-example (the repl-4) :class "repl-example-wide"))

                   (str (repl-example (the repl-4) :class "repl-example"))
                                              
		   (:h3 (:span :class "object-keyword"  ":computed-slots"))
		   (:p  (:span :class "object-keyword" ":computed-slots") " can represent 
known values, intermediate results, or final outputs which may be computed by an object")
		   (:p  "They are defined as symbol-value pairs enclosed in parentheses. 
The value can be any Common Lisp value or expression.")
		   (:p  (:span :class "object-keyword" ":computed-slots")
			" can refer to the return values of other "
			(:span :class "object-keyword" ":computed-slots")
			" using the GendL macro " (:span :class "macro""the"))
		   (str (code-example (the code-4)))
		   (:p "In the example above, a computed-slot " (:span :class "slot" "density")
                       " has been created and set to " (:span :class "value" "7800")
                       " (the density of steel in kg/m" (:sup "3") "). A further slot has 
been created, " (:span :class "slot" "mass") ", which divides the "
		       (:span :class "slot" "volume") " by "
                       (:span :class "value" "1000000") " and then multiplies that 
result by the value of "
                       (:span :class "slot" "density"))
		   (:p (:em "A point to note here is the Gendl is dimensionless. 
It is the responsibility of the programmer to ensure units are correct when performing 
calculations. Implicit in the example here is that length, width and height are specified 
in mm and the resultant mass will be in kg"))
		   (:p "We will cover the use of functions to perform calculations in 
more detail later in this tutorial")
		   (:h3 (:span :class "object-keyword" ":objects"))
		   (:p "The " (:span :class "object-keyword" ":objects")
		       " section is where child objects are specified. 
This specification includes:"
		       (:dl (:dt "The object name")
                            (:dd "i.e. the name of the slot which will contain this child object instance")
                            (:dt "The object type")
                            (:dd "This is expected to correspond to an object definition name specified in another "
				 (:span :class "function" "define-object") ". This can be a literal (quoted) symbol or
an expression which yields a symbol when evaluated (that is, object type can be determined dynamically at runtime).")
                            (:dt "The object input values")
                            (:dd "these are keyword-tagged expressions which specify the values to be passed (when and if demanded)
into the child object instance. They keywords should match existing "
                                 (:span :class "slot" "input-slots")
                                 " in the child object's type, otherwise a compiler warning will be generated.")))
		   (:p "In the definition below, the parent object ("
		       (:span :class "object" "assembly-1")
		       ") has two child objects: one called "
		       (:span :class "object" "my-box")" based on "
		       (:span :class "object" "my-box-4") " with "
		       (:span :class "slot" "length") " set to "
		       (:span :class "value" "10") ", and another called "
		       (:span :class "object" "my-sphere") ", based on the GendL "
		       (:span :class "object" "sphere") " object, with its "
		       (:span :class "slot" "radius")" specified as being equal to the "
		       (:span :class "slot" "width")" of "
		       (:span :class "slot" "my-box"))
		   (:p "Note that when specifying the inputs to an object"
		       (:ul :class "list-disc list-outside ml-3 mr-8 bg-zinc-100"
                            (:li "the name must match one of the symbols in the object's "
				 (:span :class "object-keyword" ":input-slots") ".")
			    (:li "the name must be specified as a keyword (ie preceeded by a "
				 (:b (:code ":")) ") so an "
				 (:span :class "object-keyword" ":input-slot") " "
				 (:span :class "slot" "length") " is specified as "
				 (:b ":length") " when specifying the inputs.")))
		   (str (code-example (the code-5)))
		   (:p "Instantiating assembly-1 in Geysr and drawing the leaves will 
look like this")
		   (:img :src (format nil "/~a-images/assembly-1.png" (the publish-prefix)) :style "width: auto; height: 200px;" )
		   (:h3 (:span :class "object-keyword" ":functions"))
		   (:p "The "
		       (:span :class "object-keyword" ":functions") " section is where 
you put the names, argument lists and bodies for "
		       (:span :class "object-keyword" ":functions") " which can operate 
within the context of the Gendl object they are defined in. "
		       "They shouldn't be confused with Common Lisp functions which are 
defined with "
		       (:em (:b"defun"))", although the syntax is very similar. The biggest 
difference is that a Gendl "
		       (:span :class "object-keyword" ":function") " can access messages 
within the object it is defined in by using the Gendl "
		       (:span :class "macro" "the") " macro")
		   (:p "We will cover more on "
		       (:span :class "object-keyword" ":functions") " in a later part of this tutorial")
		       
			
                   ((:div :class "main-page-item")
		    (:h2 "Resources")
		    (str (the resource-links)))))))







```

---

## functions-and-functions.lisp - header
Source: gornschool-training/t2/source/functions-and-functions.lisp
Type: tutorial

```
(in-package :training-2)


```

---

## functions-and-functions.lisp - functions-and-functions
Source: gornschool-training/t2/source/functions-and-functions.lisp
Type: tutorial

```
(define-object functions-and-functions (base-training-sheet)
  :computed-slots
  ((index-words (list "Named Functions" ":functions" "GendL object functions"
                      "&optional" "&key" "&rest"))

   (repl-1 (list (list :command "
```

---

## functions-and-functions.lisp - add2
Source: gornschool-training/t2/source/functions-and-functions.lisp
Type: tutorial

```
(defun add2 (x) (+ x 2))"
		       :output "ADD2")
		 (list :command "(add2 4)"
		       :output 6)
		 (list :command (list "
```

---

## functions-and-functions.lisp - kinetic-energy
Source: gornschool-training/t2/source/functions-and-functions.lisp
Type: tutorial

```
(defun kinetic-energy (mass velocity)"
				      "   (div (* mass velocity velocity) 2))")
		       :output "KINETIC-ENERGY")
		 (list :command "(kinetic-energy 5 12)"
		       :output "360.0")))

   (repl-2 (list (list  :command (list "
```

---

## functions-and-functions.lisp - kinetic-energy
Source: gornschool-training/t2/source/functions-and-functions.lisp
Type: tutorial

```
(defun kinetic-energy (&optional (mass 5) (velocity 12)"
				       "   (div (* mass velocity velocity) 2))")
			:output "KINETIC-ENERGY")
		 (list :command "(kinetic-energy)"
		       :output "360.0")
		 (list :command "(kinetic-energy 10)"
		       :output "720.0")
		 (list :command "(kinetic-energy 10 24)"
		       :output "2880.0")))

   (repl-3 (list (list  :command (list "
```

---

## functions-and-functions.lisp - kinetic-energy
Source: gornschool-training/t2/source/functions-and-functions.lisp
Type: tutorial

```
(defun kinetic-energy (&key (mass 5) (velocity 12)"
				       "   (div (* mass velocity velocity) 2))")
			:output "KINETIC-ENERGY")
		 (list :command "(kinetic-energy)"
		       :output "360.0")
		 (list :command "(kinetic-energy :mass 10)"
		       :output "720.0")
		 (list :command "(kinetic-energy :velocity 24)"
		       :output "1440.0")
		 (list :command "(kinetic-energy :velocity 24 :mass 10)"
		       :output "2880.0") ))

   (code-1 (list "
```

---

## functions-and-functions.lisp - kinetic-energy
Source: gornschool-training/t2/source/functions-and-functions.lisp
Type: tutorial

```
(defun kinetic-energy (&key (mass 5) (velocity 12)"
		 "	 (div (* mass velocity velocity) 2)))"
		 ""
		 "
```

---

## functions-and-functions.lisp - function-example
Source: gornschool-training/t2/source/functions-and-functions.lisp
Type: tutorial

```
(define-object function-example(base-object)"
		 ""
		 ":computed-slots"
		 "((mass 5)"
		 " (velocity 12)"
		 " (ke-1 (the kinetic-energy-1))"
		 " (ke-2 (the kinetic-energy-2))"
		 " (ke-3 (the (kinetic-energy-2 :mass 10)))"
		 " (ke-4 (the (kinetic-energy-2 :velocity 24)))"
		 " (ke-5 (the (kinetic-energy-2 :velocity 24 :mass 10)))"
		 ""
		 " (ke-6 (kinetic-energy :mass 10 :velocity 24))"
		 " )"
		 ""
		 ":functions"
		 "((kinetic-energy-1 () (div (* (the mass) (the velocity) (the velocity)) 2))"
		 ""
		 " (kinetic-energy-2 (&key (mass (the mass)) (velocity (the velocity)))"
		 "		      (div (* mass velocity velocity) 2))"
		 " )"
		 ")"))

   (repl-4 (list (list :command "(setq self (make-object 'function-example))"
		       :output "#<GDL-USER::FUNCTION-EXAMPLE #x210397F77D>")
		 (list :command "(the ke-1)"
		       :output "360.0")
		 (list :command "(the ke-2)"
		       :output "360.0")
		 (list :command "(the ke-3)"
		       :output "720.0")
		 (list :command "(the ke-4)"
		       :output "1440.0")
		 (list :command "(the ke-5)"
		       :output "2880.0")
		 (list :command "(the ke-6)"
		       :output "2880.0"))) 

   

   (body-content (with-cl-who-string()
		   (:div
                    :class "main-page-container"
		    (:div
                     :class "main-page-item"
                     (:h2 (str (the page-header)))
		     (:div
                      :class "grid-container-2-650px"
		      (:div
                       :class "grid-item"
		       (:p (wmd "Functions are Lisp objects which can be _called_, or invoked, to
accept some _inputs_ (or \"arguments\") and yield corresponding _outputs_ or \"return values.\"

Typically, functions will simply return new values without modifying
any of the inputs. If you program mostly with such a non-modifying
style, you are doing _Functional Programming."))
                       (:p "GendL offers two kinds of functions:"
			   (:ul (:li "Functions defined using the "
				     ((:span :class "macro")"defun")" macro, Sometimes called "
				     (:em (:b "Named Functions"))
                                     ", such functions become objects in memory which have a symbol for a name. Usually, variables mentioned within the body of such functions
are \"passed in\" to the function call as arguments, Variables with
Global and Dynamic Scope can also be accessed within function bodies,
but we will cover those types of variables later.")
				(:li "Functions defined within the "
				     ((:span :class "object-keyword")":functions")
                                     " section of a GendL object definition. As with toplevel "
                                     (:span :class "macro" "defun")
                                     " functions, variables may be passed into "
                                     (:span :class "object-keyword" ":functions")
                                     " as arguments and the function bodies may access
global data. But additionally, "
                                     (:span :class "object-keyword" ":functions")

                                     " can also access any other "(:em (:b "messages"))" defined within
the same GendL object type where they occur. " (:em (:b "Messages"))" can be any of "
                                     (:ul
                                      (:li (:span :class "object-keyword" ":input-slots"))
                                      (:li (:span :class "object-keyword" ":computed-slots"))
                                      (:li (:span :class "object-keyword" ":objects")))
                                     "... and other "
                                     (:span :class "object-keyword" ":functions")
                                     " also count as messages!

When you want to access the value of other mesages within the same object instance, you can use the "
                                     (:span :class "macro" "the") (:em " referencing macro")".")))
		       (:h3 "Named functions")
		       (:p "We have already encountered a number of Named Functions, for example"
			   (:ul (:li (:span :class "function" "+")" which returns the sum of
the numbers supplied as arguments")
				(:li (:span :class "function" "length")" which returns
the length of a list supplied as an argument")))
                       (:p "Named functions defined using the "
                           (:span :class "macro" "defun")
                           " macro use "
			   (:em (:b "Prefix Notation")) ". In other words, the function
name is specified first, followed by its arguments. Named functions can accept "
			   (:ul (:li "Specific standard arguments, which immediately follow
the function name")
				(:li "Specific optional arguments, identified using "
				     (:em (:b "&optional")) " in the argument list")
				(:li "Specific keyword arguments, identified using "
				     (:em (:b "&key")) " in the argument list")
				(:li "Any number of remaining arguments, identified using "
				     (:em (:b "&rest")) " in the argument list"))
			   "We'll discuss standard, "
			   (:em (:b "&optional")) " and "
			   (:em (:b "&keyword")) " arguments in this tutorial and come back to "
			   (:em (:b "&rest arguments")) " in a later tutorial")
		       (:h4 "Standard arguments")
		       (:p "Here we define two functions with one and two arguments respectively.
In the second function, the ordering of the arguments is what determines which input value ends
up in which variable in the body of the function.")
		       (str (repl-example (the repl-1)))
		       (:h4 "Optional arguments")
		       (:p "The syntax for optional arguments is"
			   (:ul (:li (:code "defun function-name (&optional (arg1-name arg1-default) (arg2-name arg2-default))"))
				(:li "In principle there is no limit to the number of optional arguments you can specify")
				(:li "Optional arguments are order dependant as with Standard ones, so if "
                                     (:span :class "general-keyword" "arg2")
                                     " is to be specified, "
                                     (:span :class "general-keyword" "arg1")
                                     " must be specified as well.")))
		       (str (repl-example (the repl-2)))
		       (:p "The first call to "

			   ((:span :class "function")"kinetic-energy")
                           " passes in no values for the arguments and so it ends up using both default optional values.
The second call uses a supplied value for "
			   ((:span :class "slot") "mass")" but the default value for "
			   ((:span :class "slot") "velocity")". And the third call to "
			   ((:span :class "function") "kinetic-energy")" uses supplied values for "
			   ((:span :class "slot") "mass")" and "
			   ((:span :class "slot")"velocity")
                           ". Note that when defined in this way, in order to used a specified "
			   ((:span :class "slot") "velocity") ", then "
			   ((:span :class "slot") "mass")" must be supplied, even if given simply 
as its default value")

			    
		       (:h4 "Keyword arguments")	      
		       (:p "The syntax for keyword arguments is"
			   (:ul (:li (:code "defun function-name (&key (arg1-key arg1-default) (arg2-key arg2-deefault))"))
				(:li "As with Optional arguments, there is no limit to the number of
keyword arguments you can specify")
                                (:li "Contrasted with Standard and Optional arguments, you can call a function with
Keyword arguments with those argument values specified in any order (because each one is identified explicitly
by a keyword symbol in the definition of the function. This is a significant advantage over Optional and Standard
arguments, especially when you want to extend a function to accept new arguments without breaking existing code
which is calling it.")
			        (:li "As with Optional arguments, you only need to specify the keyword arguments you want to use
when calling the function, and any arguments which you do not include in the call will take on their default values when
the body of the function is being evaluated.")))
		       (str (repl-example (the repl-3)))
		       (:p "Above we can see how the order-dependency is removed")

		       (:p "Some general recomendations and observations:"
			   (:ul (:li "If there is only one argument then it's reasonable to use a Standard argument")
				(:li "Optional arguments will be easy to work with if there is only one of
them, a bit less easy if there are two, and will start becoming
unwieldy to work with if you use more than 2 Optional
arguments.

Additionally, thought needs to be given to the order in
which they are defined to avoid having to specify optional arguments
just to occupy a space in the argument list (e.g. if you have two optional arguments,
list first the one which is more likely to be passed in as a non-default value in calls to that function.")
				(:li "Keyword arguments work well when the function has more than 1 argument, and help with readability")
				(:li "Optional and Keyword arguments are relatively 'expensive' at runtime
compared with Standard arguments, so particularly with Keyword
arguments you may be trading Convenience for Speed in some cases (such cases can be shaken out
with Runtime Profiling)")))
                        
		       (:h3 "GendL object "((:span :class "object-keyword")":functions"))
		       (:p "A GendL object function is defined in the "
			   ((:span :class "object-keyword")":functions")" section of an object definition made with the "
                           (:span :class "macro" "define-object")
                           " macro. The function only \"exists\" in the context of the object
definition itself, but it can reference all of the slots within the
object directly, rather than you having to pass in values as
arguments. But of course it can also accept passed-in arguments. The
argument syntax is identical to that for Named Functions (this style
of argument list is called a \"lambda list\". The Named
Function "
			   ((:span :class "function")"kinetic-energy")" shown above,
when defined as a GendL object function, may look like this:")))
		     ((:div :class "main-page-container")
		      ((:div :class "main-page-item")

		       (str (code-example (the code-1))))
		      (:div :class "main-page-item"
			    (str (repl-example (the repl-4)))))
		     (:div :class "grid-container-2-650px"
			   (:div :class "grid-item"
			         (:p "Some points to note from the above example"
			             (:ul (:li "The "
				               (:span :class "object-keyword" ":function")" "
				               (:span :class "function" "kinetic-energy-1")" has no arguments but it references the "
				               (:span :class "object-keyword" ":computed-slots")" "
				               (:span :class "slot" "mass")" and "
				               (:span :class "slot" "velocity")" directly using the "
				               (:span :class "macro" "the")" macro")
				          (:li "The "

				               (:span :class "object-keyword" ":function") " "
				               (:span :class "function" "kinetic-energy-2")
                                               " defines keyword arguments for "
				               (:span :class "slot" "mass")" and "
				               (:span :class "slot" "veocity")
                                               " and sets their default values to the values of the "
				               (:span :class "object-keyword" ":computed-slots")" "
				               (:span :class "slot" "mass")" and "
				               (:span :class "slot" "velocity") ".")
				          (:li "When a "
				               (:span :class "object-keyword" ":function")" is called without arguments, the "
				               (:span :class "macro" "the")" referencing macro is still used but the "
				               (:span :class "object-keyword" ":function")" does not need enclosing in parentheses (so
such a reference would look just like a reference to a "

                                               (:span :class "object-keyword" ":computed-slot") " (see "
                                               (:span :class "object-keyword" ":computed-slots") " above)")

				          (:li "When a "
                                               (:span :class "object-keyword" ":function")" is called with arguments, whilst the "
				               (:span :class "macro" "the")"
referencing macro is again used, you must wrap the function name and
arguments with parentheses, similar to calling a normal Named function
with the only difference being an outer wrapping referencing macro e.g. "
                                               (:code "(the ... )")
                                               " (see the "
				               (:span :class "object-keyword"":computed-slots")" "
				               (:span :class "slot" "ke-3")", "
				               (:span :class "slot" "ke-4")" and "
				               (:span :class "slot" "ke-5"))
				          (:li "Compare and contrast these "
				               (:span :class "object-keyword" ":computed-slots")" with the way in which the "
				               (:span :class "function" "kinetic-energy")" Named Function is used for "
				               (:span :class "slot" "ke-6")))))))
			
		    (:div :class "main-page-item"
			  (:h2 "Resources")
			  (str (the resource-links))))))))




```

---

## more-on-lists.lisp - header
Source: gornschool-training/t2/source/more-on-lists.lisp
Type: tutorial

```
(in-package :training-2)


```

---

## more-on-lists.lisp - more-on-lists
Source: gornschool-training/t2/source/more-on-lists.lisp
Type: tutorial

```
(define-object more-on-lists (base-training-sheet)
  :input-slots
  (prior-tutorial-url)

  :computed-slots
  ((index-words (list "length" "member" "position" "subseq" "remove" "delete" "distructive functions"
		      "remove-duplicates" "flatten" "reverse" "nreverse" "sort" "safe-sort"))

   (repl-1 (list (list :command "(setq a (list 1 2 3))"
		       :output "(1 2 3)")
		 (list :command "(length a)"
		       :output 3)
		 (list :command "(setq b (list 1 2 (list 3 4 5 6)))"
		       :output "(1 2 (3 4 5 6))")
		 (list :command "(length b)"
		       :output 3)))
   (repl-2 (list (list :command "(member 2 a)"
		       :output "(2 3)")
		 (list :command "(member 4 a)"
		       :output "NIL")))
   (repl-3 (list (list :command "(setq c (list \"a\" \"b\" \"c\"))"
		       :output "(\"a\" \"b\" \"c\")")
		 (list :command "(member \"a\" c)"
		       :output "NIL")
		 (list :command "(member \"a\" c :test 'equal)"
		       :output "(\"a\" \"b\" \"c\")")))

   (repl-4 (list (list :command "(position 2 a)"
		       :output 1)
		 (list :command "(position \"b\" c)"
		       :output "NIL")
		 (list :command "(position \"b\" c :test 'equal)"
		       :output 2)))

   (repl-5 (list (list :command "(setq lis (list 1 2 3 \"a\" \"B\" (list 1 2 3)))"
		       :output "(1 2 3 \"a\" \"B\" (1 2 3))")
		 (list :command "(subseq lis 2)"
		       :output "(3 \"a\" \"B\" (1 2 3))")
		 (list :command "(subseq lis 2 4)"
		       :output "(3 \"a\")")))

   (repl-6 (list (list :command "(setq rm (list 1 2 3 4 3 4 5 6 4 5 4 5 3 1))"
		       :output "(1 2 3 4 3 4 5 6 4 5 4 5 3 1)")
		 (list :command "(remove 3 rm)"
		       :output "(1 2 4 4 5 6 4 5 4 5 1)")
		 (list :command "(remove 3 rm :count 2)"
		       :output "(1 2 4 4 5 6 4 5 4 5 3 1)")
		 (list :command "(remove 3 rm :start 3)"
		       :output "(1 2 3 4 4 5 6 4 5 4 5 1)")
		 (list :command "(remove 3 rm :start 3 :end 6)"
		       :output "(1 2 3 4 4 5 6 4 5 4 5 3 1)")
		 (list :command "(setq rm1 (list (list 1 2) (list 3 4) (list 1 3)))"
		       :output "((1 2) (3 4) (1 3))")
		 (list :command "(remove (list 3 4) rm1 :test 'equal)"
		       :output "((1 2) (1 3))")))
   
   (repl-7 (list (list :command "rm1"
		       :output "((1 2) (3 4) (1 3))")
		 (list :command "(delete (list 3 4) rm1 :test 'equal)"
		       :output "((1 2) (1 3))")
		 (list :command "rm1"
		       :output "((1 2) (1 3))")))

   (repl-8 (list (list :command "(setq rm (list 1 2 3 4 3 4 5 6 4 5 4 5 3 1))"
		 :output "(1 2 3 4 3 4 5 6 4 5 4 5 3 1)")
	   (list :command "(remove-duplicates rm)"
		 :output "(2 6 4 5 3 1)")
	   (list :command "(setq rm (list \"a\" \"b\" \"c\" \"a\" \"A\"))"
		 :output "(\"a\" \"b\" \"c\" \"a\" \"A\")")
	   (list :command "(remove-duplicates rm)"
		 :output "(\"a\" \"b\" \"c\" \"a\" \"A\")")
	   (list :command "(remove-duplicates rm :test 'equal)"
		 :output "(\"b\" \"c\" \"a\" \"A\")")
	   (list :command "(remove-duplicates rm :test 'equalp)"
		 :output "(\"b\" \"c\" \"A\")")))
	   
   (repl-9 (list (list :command "(setq lis (list 1 2 3 (list 1 4 5) (list \"a\" \"b\") (list :a 1 :b 2)))"
		       :output "(1 2 3 1 4 5 \"a\" \"b\" :A 1 :B 2)")
		 (list :command "(flatten lis)"
		       :output "(1 2 3 1 4 5 \"a\" \"b\" :A 1 :B 2)")
		 (list :command "(setq lis (list 1 2 3 nil (list 1 4 5) (list \"a\" \"b\") nil (list :a 1 :b 2)))"
		       :output "(1 2 3 1 nil 4 5 \"a\" \"b\" nil :A 1 :B 2)")
		 (list :command "(flatten lis)"
		       :output "(1 2 3 1 4 5 \"a\" \"b\" :A 1 :B 2)")))

   (repl-10 (list (list :command "(setq a (list 1 2 3 4 5))"
			:output "(1 2 3 4 5)")
		  (list :command "(reverse a)"
			:output "(5 4 3 2 1)")
		  (list :command "a"
			:output "(1 2 3 4 5)")
		  (list :command "(nreverse a)"
			:output "(5 4 3 2 1)")
		  (list :command "a"
			:output "(1)")))

   (repl-11 (list (list :command "(setq a (list 1 3 2 5 4))"
			:output "(1 3 2 5 4)")
		  (list :command "(setq b (list 1 3 2 5 4)"
			:output "(1 3 2 5 4)")
		  (list :command "(sort a '<)"
			:output "(1 2 3 4 5)")
		  (list :command "a"
			:output "(1 2 3 4 5)")
		  (list :command "(safe-sort b '<)"
			:output "(1 2 3 4 5)")
		  (list :command "b"
			:output "(1 3 2 5 4)")))

   (repl-12 (list (list :command "(setq a (list (list 2 4) (list 1 6) (list 4 3) (list 3 5)))"
			:output "((2 4) (1 6) (4 3) (3 5))")
		  (list :command "(safe-sort a '< :key 'first)"
			:output "((1 6) (2 4) (3 5) (4 3))")))
		  
   
   (body-content (with-cl-who-string()
		   (:p "In the earlier tutorial on " ((:a :href (the prior-tutorial-url)) "Lists")
		       " we covered Creating Lists, Adding to Lists, Accessing elements
within a List, and treating Lists as Property Lists (Plists). In this
tutorial we will continue to look at working with lists.")
		   (:h3 "List Properties")
		   (:p "Common Lisp provides a number of functions for obtaining properties of lists"
		       (:ul ((:div :class "grid-container-2")
			     ((:div :class "main-page-item")
			      (:li (:span :class "function" "length") " takes a "
				   (:em "list") " as its argument and returns an "
				   (:em "integer") " corresponding to the number of elements within that list. ")
			      "Note that if any of the elements of the list passed to "
			      (:span :class "function" "length")
			      " is
itself a list or other aggregate data type, it still counts as a single element for the purposes of "
			      (:span :class "function" "length")
			      ".")
			     ((:div :class "main-page-item")
			      (str (repl-example (the repl-1))))
			     ((:div :class "main-page-item")
			      (:li ((:span :class "function")"member")" takes an "
				   (:em "object") " and "
				   (:em "list") " as arguments and returns all the values in the list starting with the first element matching object.")
			      "If there is no match the function returns nil")
			     ((:div :class "main-page-item")
			      (str (repl-example (the repl-2))))
			     ((:div :class "main-page-item")
			      ((:span :class "function") "member") " uses the equality test "
			      ((:span :class "function") "eql")" as the default test. An alternative test may be provided using the "
			      ((:span :class "object-keyword") ":test") " keyword input. Whatever function is specified for"
			      ((:span :class "object-keyword") ":test") " needs to be "(:em (:b "quoted")))
			     ((:div :class "main-page-item")	
			      (str (repl-example (the repl-3))))
			     ((:div :class "main-page-item")
			      (:li ((:span :class "function")"position")" takes an "
				   (:em "object")" and a "
				   (:em "list")" as inputs and returns the index number of the first object in the list matching object.")
			      "The index number is zero-based. Again, the default test for equality is "
			      ((:span :class "function") "eql")" but you can override this by specifying a "
			      ((:span :class "object-keyword")":test")" keyword input")
			     ((:div :class "main-page-item")
			      (str (repl-example (the repl-4)))))))
			
		   (:h3 "List Processing")
			
		   (:p "There are a number of Common Lisp functions which allow us to process lists in various ways,
such as returning part of a list, returning a new list with elements removed, and re-ordering or sorting a list"
		       (:ul ((:div :class "grid-container-2")
			     ((:div :class "main-page-item")
			      (:li ((:span :class "function")"subseq")" treats a list as a sequence. Its takes a "
				   (:em "list")" and "
				   (:em "start")" position and optionally an "
				   (:em "end")" position, returning a list comprising the elements from and including the start position up to but excluding the end position. "
				   "Positions are zero-based. Note that if either "
				   (:em "start")" or "
				   (:em "end")" is a higher index than the maximum index number in the list, then an error will be generated"))
			     ((:div :class "main-page-item")
			      (str (repl-example (the repl-5))))
			     ((:div :class "main-page-item")
			      (:li ((:span :class "function")"remove")" takes an "
				   (:em "object")" and a "
				   (:em "list")" as inputs and returns the list with "
				   (:em "object")" removed from it. "
				   "By default the equality test is "((:span :class "function")"EQL")", but an alternative may be specified using the "
				   ((:span :class "object-keyword")":test")" keyword input. If "
				   ((:span :class "object-keyword")":start")" or "
				   ((:span :class "object-keyword")":end")" keywords are provided, only elements between those positions are tested for a match and removed. Finally if "
				   ((:span :class "object-keyword")":count")" is provided only the first "
				   (:em "count")" instances of "(:em "object")" are removed"))
			     ((:div :class "main-page-item")
			      (str (repl-example (the repl-6))))
			     ((:div :class "main-page-item")
			      (:li ((:span :class "function")"delete")" is identical to "
				   ((:span :class "function")"remove")" except it "
				   (:em (:b "modifies the input list"))))
			     ((:div :class "main-page-item")
			      (str (repl-example (the repl-7))))
			     ((:div :class "main-page-item")
			      (:li ((:span :class "function")"remove-duplicates")" removes any duplicate values from a list, where duplicate is tested by default using "
				   ((:span :class "function") "eql")". As with "
				   ((:span :class "function") "remove")", "
				   ((:span :class "object-keyword") ":test")", "
				   ((:span :class "object-keyword") ":start")" and "
				   ((:span :class "object-keyword") ":end")" keyword inputs may be specified. ")
			      "When duplicates are found, only the last occurrence of the duplicate is retained in the return value")
                                  
			     ((:div :class "main-page-item")
			      (str (repl-example (the repl-8))))
			     ((:div :class "main-page-item")
			      (:li ((:span :class "function") "flatten")" (a GendL function, not included in standard Common Lisp) takes a "
				   (:em "list")" as input, which may comprise sub-lists, and returns a one dimensional list. Because nil is an empty list, any occurences of nil in a list passed to "
				   ((:span :class "function")"flatten")" will be removed"))
			     ((:div :class "main-page-item")
			      (str (repl-example (the repl-9))))
			     ((:div :class "main-page-item")
			      (:li ((:span :class "function")"reverse")" takes a "
				   (:em "list")" as input and returns a list but with the elements in the reverse order."
				   ((:span :class "function")"nreverse") "is the destructive version of reverse, ie it may alter the supplied list"))
			     ((:div :class "main-page-item")
			      (str (repl-example (the repl-10)))))))
		   (:h3 "List Sorting")
		   (:ul ((:div :class "grid-container-2")
			 ((:div :class "main-page-item")
			  (:li ((:span :class "function")"sort")" is a Common Lisp function which takes a "
			       (:em "List")" and a "
			       (:em "predicate") "as arguments and returns a list such that no 2 successive elements [x] and [y] returns false for (predicate x y) and true for (predicate y x)")
			  (:p "A significant downside of using "
			      ((:span :class "function")"sort")" is that it is distructive - it can modify the input list. GendL therefore defines a function "
			      ((:span :class "function")"safe-sort")" which mirrors "
			      ((:span :class "function")"sort")" in every respect apart from "
			      (:em (:b "it does not modify the input list"))". It is "
			      (:em (:b "strongly recomended"))" to use "
			      ((:span :class "function")"safe-sort")" for any list sorting")
			  (:p "An obvious but sometimes overlooked equirement for the input list is that for "
			      ((:span :class "function")"sort")" or "
			      ((:span :class "function")"safe-sort")" to be meaningful, the elements in the list must be of types which may be compared with the same predicate"))
			 ((:div :class "main-page-item")
			  (str (repl-example (the repl-11))))
			 ((:div :class "main-page-item")
			  ((:span :class "function")"sort")" and "
			  ((:span :class "function")"safe-sort")" both have a keyword input "
			  ((:span :class "object-keyword")":key")" which allows the element on which the sorting is to be performed to be identified. "
			  ((:span :class "object-keyword")":key")" is specified as a "
			  (:em "quoted")" function which is applied to each element in the input list and then the sort is applied to that return value")
			 (str (repl-example (the repl-12)))))
			    
			
		   ((:div :class "main-page-item")
		    (:h2 "Resources")
		    (str (the resource-links)))))))

```

---

## truss-hint-4.lisp - header
Source: gornschool-training/t2/resources/source/truss-hint-4.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## truss-hint-4.lisp - degrees-to-radians
Source: gornschool-training/t2/resources/source/truss-hint-4.lisp
Type: tutorial

```
(defun degrees-to-radians (degrees)
  (div (* degrees pi) 180))


```

---

## truss-hint-4.lisp - truss
Source: gornschool-training/t2/resources/source/truss-hint-4.lisp
Type: tutorial

```
(define-object truss (box)
  :input-slots
  ((truss-length 2000)
   (truss-height 800)
   (truss-angle nil)

   (beam-width 50)
   (beam-height 50)
   (wall-thickness 3))

  :computed-slots
  ((length (the truss-length))
   (height (cond ((the truss-height)(the truss-height))
		 ((the truss-angle) (+ (* (half (the truss-length))
					  (tan (degrees-to-radians (the truss-angle))))
				       (the beam-height)))))
   (width (the beam-width)))
				       
  :objects
  ((lower-beam :type 'beam
	       :beam-height (the beam-height)
	       :beam-width (the beam-width)
	       :beam-length (the truss-length)
	       :center (translate-along-vector (the (face-center :bottom))
					       (the (face-normal-vector :top))
					       (half (the beam-height))))
   (vertical-beam :type 'beam
		  :beam-length (- (the height) (the beam-height))
		  :beam-height (the beam-height)
		  :beam-width (the beam-width)
		  :orientation (alignment :rear (the (face-normal-vector :top)))
		  :center (translate-along-vector (the lower-beam (face-center :top))
						  (the lower-beam (face-normal-vector :top))
						  (half (the-child beam-length))))
   ;;(front-slope-beam :type 'beam)
   ;;(rear-slope-beam :type 'beam))
   ))




```

---

## truss-hint-4.lisp - beam
Source: gornschool-training/t2/resources/source/truss-hint-4.lisp
Type: tutorial

```
(define-object beam (box)
  :input-slots
  ((beam-length 1000)
   (beam-width 40)
   (beam-height 50)
   (wall-thickness 2)
   (material-density 7800)
   (tonne-rate 500))

  :computed-slots
  ((length (the beam-length))
   (width (the beam-width))
   (height (the beam-height))

   (beam-volume (- (the outer volume) (the inner volume)))
   (beam-mass (* (div (the beam-volume) 1000000000) (the material-density)))
   (beam-cost (* (the tonne-rate) (div (the beam-mass) 1000)))
   (beam-properties (list :volume-mm3 (the beam-volume)
			  :mass-kg (round-to-nearest (the beam-mass) 0.01)
			  :cost-gbp (round-to-nearest (the beam-cost) 0.01)
			  :length-mm (the beam-length)
			  :width-mm (the beam-width)
			  :height-mm (the beam-height)
			  :thickness-mm (the wall-thickness))))
   
  :objects
  ((outer :type 'box
	  :length (the beam-length)
	  :width (the beam-width)
	  :height (the beam-height))

   (inner :type 'box
	  :length (the beam-length)
	  :width (- (the beam-width) (twice (the wall-thickness)))
	  :height (- (the beam-height) (twice (the wall-thickness)))))
  )

```

---

## wall-hint-7.lisp - header
Source: gornschool-training/t2/resources/source/wall-hint-7.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## wall-hint-7.lisp - wall
Source: gornschool-training/t2/resources/source/wall-hint-7.lisp
Type: tutorial

```
(define-object wall(box)
  :input-slots
  ((brick-height 45)
   (brick-length 180)
   (brick-width 90)
   (mortar-joint-width 10)
   (wall-length 3700)
   (wall-height 900))

  :computed-slots
  ((row-height (+ (the brick-height) (the mortar-joint-width)))
   (number-of-rows (round-to-nearest (div (the wall-height) (the row-height)) 1))
   (actual-wall-height (* (the row-height) (the number-of-rows)))
   
   ;; for the wall-length we need the number of full bricks
   ;; if there are n full bricks then there will be (n-1) mortar joints
   ;; so n*brick-length + n-1*mortar-joint-width = overall-length
   ;; or n(brick-length + mortar-join-width) - mortar-joint-width = overall-length
   ;; or n = (overall-length - mortar-joint-width)/(brick-length + mortar-joint-width)
   (number-of-bricks (round-to-nearest (div (- (the wall-length) (the mortar-joint-width))
					    (+ (the brick-length) (the mortar-joint-width)))
				       1))
   (actual-wall-length (+ (* (the number-of-bricks) (the brick-length))
			  (* (- (the number-of-bricks) 1) (the mortar-joint-width))))

   ;; box inputs - gives the wall bounding box
   (height (the actual-wall-height))
   (width (the brick-width))
   (length (the actual-wall-length))

   (full-bricks (apply '+ (list-elements (the row) (the-element full-bricks))))
   (half-bricks (apply '+ (list-elements (the row) (the-element half-bricks))))
   (mortar-volume (apply '+ (list-elements (the row) (the-element mortar-volume))))
   (mortar-density 2162)
   (mortar-mass (* (the mortar-density) (div (the mortar-volume) 1000000000)))
   )
   
     
  :objects
  ((row :type 'row
     :sequence (:size (the number-of-rows))
     :center (translate-along-vector (the (face-center :bottom))
				     (the (face-normal-vector :top))
				     (+ (half (the-child height))
					(* (the-child index) (the-child height))))
     :height (+ (the brick-height) (the mortar-joint-width))
     :full-bricks-per-row (the number-of-bricks)
     :pass-down (brick-height
		 brick-length
		 brick-width
		 mortar-joint-width
		 length
		 width))))


```

---

## wall-hint-7.lisp - row
Source: gornschool-training/t2/resources/source/wall-hint-7.lisp
Type: tutorial

```
(define-object row (box)
  :input-slots
  (full-bricks-per-row
   brick-height
   brick-length
   brick-width
   mortar-joint-width)

  :computed-slots
  ((full-brick-row? (or (zerop (the index)) (evenp (the index))))
   (full-bricks (the bricks-and-mortar full-bricks))
   (half-bricks (the bricks-and-mortar half-bricks))
   (mortar-volume (+ (the bricks-and-mortar mortar-joint-volume)
		     (the mortar-bed volume))))

  :objects
  ((bricks-and-mortar :type 'bricks-and-mortar
		      :height (the brick-height)
		      :center (translate-along-vector (the mortar-bed (face-center :top))
						      (the mortar-bed (face-normal-vector :top))
						      (half (the-child height)))
		      :pass-down (width
				  length
				  full-brick-row?
				  brick-height
				  brick-length
				  brick-width
				  mortar-joint-width
				  full-bricks-per-row))
   (mortar-bed :type 'box
	       :height (the mortar-joint-width)
	       :center (translate-along-vector (the (face-center :bottom))
					       (the (face-normal-vector :top))
					       (half (the-child height)))
	       :pass-down (width
			   length))))


```

---

## wall-hint-7.lisp - bricks-and-mortar
Source: gornschool-training/t2/resources/source/wall-hint-7.lisp
Type: tutorial

```
(define-object bricks-and-mortar (box)
  :input-slots
  (full-brick-row?
   full-bricks-per-row
   brick-height
   brick-length
   brick-width
   mortar-joint-width)

  :computed-slots
  ((first-full-brick-start-point (if (the full-brick-row?)
				     (the (face-center :front))
				     (the (mortar-joint 0) (face-center :rear))))
   
   (first-mortar-joint-start-point (if (the full-brick-row?)
				       (the (full-brick 0) (face-center :rear))
				       (the (half-brick 0) (face-center :rear))))

   (number-of-full-bricks (if (the full-brick-row?)
			      (the full-bricks-per-row)
			      (- (the full-bricks-per-row) 1)))

   (number-of-mortar-joints (if (the full-brick-row?)
				(- (the number-of-full-bricks) 1)
				(+ (the number-of-full-bricks) 1)))

   ;; if it isn't a full brick row then there will be an extra joint because one
   ;; full brick is replaced with 2 half bricks so without correcting the
   ;; mortar-joint-width the ends of a full brick row and one starting and
   ;; finishing with half bricks won't align. So we need to correct
   ;; the mortar-joint-width
   (corrected-joint-width (if (the full-brick-row?)
				     (the mortar-joint-width)
				     (let ((total-gap (* (- (the number-of-mortar-joints) 1)
									(the mortar-joint-width))))
				       (div total-gap (the number-of-mortar-joints)))))
   ;; collating the output. We could do this analytically, but for this example we'll use the geometry
   (full-bricks (length (list-elements (the full-brick))))
   (half-bricks (length (list-elements (the half-brick))))
   (mortar-joint-volume (apply '+ (list-elements (the mortar-joint)
						 (the-element volume))))
   )
  
  :functions
  ((first-full-brick-center!
    ()
    (translate-along-vector (the first-full-brick-start-point)
			    (the (face-normal-vector :rear))
			    (half (the brick-length))))
   
   (other-full-brick-center!
    (index)
    ;; if its a full brick row, (full-brick 1) is positioned relative to (joint 0)
    ;; if its a half brick row, (full-brick 1) is positioned relative to (joint 1)
    (let ((ind (if (the full-brick-row?) (- index 1) index)))
	  (translate-along-vector (the (mortar-joint ind) (face-center :rear))
				  (the (face-normal-vector :rear))
				  (half (the brick-length)))))
   
    (first-joint-center!
     ()
     (translate-along-vector (the first-mortar-joint-start-point)
			     (the (face-normal-vector :rear))
			     (half (the corrected-joint-width))))
   (other-joint-center!
    (index)
    ;; if its a full brick row, (joint 1) is positioned relative to (full-brick 1)
    ;; if its a half brick row, (joint 1) is positioned relative to (full-brick 0)
    (let ((ind (if (the full-brick-row?) index (- index 1))))
      (translate-along-vector (the (full-brick ind) (face-center :rear))
			      (the (face-normal-vector :rear))
			      (half (the corrected-joint-width)))))

    (first-half-brick-center!
     ()
     (translate-along-vector (the (face-center :front))
			     (the (face-normal-vector :rear))
			     (half (half (the brick-length)))))
   
   (last-half-brick-center!
     ()
     (translate-along-vector (theo (the mortar-joint last) (face-center :rear))
			     (the (face-normal-vector :rear))
			     (half (half (the brick-length))))))
 			     
  
  :objects
  ((full-brick :type 'box
	       :sequence (:size (the number-of-full-bricks))
	       :center (if (= (the-child index) 0)
			   (the first-full-brick-center!)
			   (the (other-full-brick-center! (the-child index))))
	       :length (the brick-length)
	       :height (the brick-height)
	       :width (the brick-width))

   (half-brick :type 'box
	       :sequence (:size (if (the full-brick-row?) 0 2))
	       :center (if (= (the-child index) 0)
			   (the first-half-brick-center!)
			   (the last-half-brick-center!))
	       :length (half (the brick-length))
	       :height (the brick-height)
	       :width (the brick-width))

   (mortar-joint :type 'box
		 :sequence (:size (the number-of-mortar-joints))
		 :center (if (= (the-child index) 0)
			     (the first-joint-center!)
			  (the (other-joint-center! (the-child index))))   
		 :height (the brick-height)
		 :width (the brick-width)
		 :length (the corrected-joint-width))))







```

---

## my-box-1.lisp - header
Source: gornschool-training/t2/resources/source/my-box-1.lisp
Type: tutorial

```
(in-package :gdl-user)

;; a basic customisation of the box object


```

---

## my-box-1.lisp - my-box-1
Source: gornschool-training/t2/resources/source/my-box-1.lisp
Type: tutorial

```
(define-object my-box-1 (box)
  :input-slots
  ((length 2)
   (width 3)
   (height 4)))

```

---

## iteration-and-mapping.lisp - header
Source: gornschool-training/t2/resources/source/iteration-and-mapping.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## iteration-and-mapping.lisp - make-integer-list
Source: gornschool-training/t2/resources/source/iteration-and-mapping.lisp
Type: tutorial

```
(defun make-integer-list (elements &key (start 0) (interval 1))
  ;; build a list if integers a specified number of elements long
  ;; by default it starts at zero and increments each element by 1, but these values may be ammended

  (let ((result nil))
    ;; we can use nreverse here for the return value, since while it is distructive
    ;; result will never be used once the return value has been sent
    (dotimes (e elements (nreverse result))
      (if (= e 0)
	  (push start result)
	  (push (+ start (* e interval)) result)))))

;; GDL-USER> (make-integer-list 5)
;; (0 1 2 3 4)
;; GDL-USER> (make-integer-list 5 :start 8)
;; (8 9 10 11 12)
;; GDL-USER> (make-integer-list 5 :start 8 :interval 2)
;; (8 10 12 14 16)
;; GDL-USER> (make-integer-list 5 :interval 2)
;; (0 2 4 6 8)


```

---

## iteration-and-mapping.lisp - get-plist-keys-dolist
Source: gornschool-training/t2/resources/source/iteration-and-mapping.lisp
Type: tutorial

```
(defun get-plist-keys-dolist (plist)
  ;; get just keywords from a plist
  ;; in contrast to make-integer-list we have not set result to nil explicitly
  ;; if no value is assigned it will automatically be nil
  (let ((result))
    (dolist (p plist (nreverse result))
    (when (keywordp p) (push p result)))))


```

---

## iteration-and-mapping.lisp - get-plist-keys-mapcar
Source: gornschool-training/t2/resources/source/iteration-and-mapping.lisp
Type: tutorial

```
(defun get-plist-keys-mapcar (plist)
  (remove nil (mapcar #'(lambda(a) (when (keywordp a) a)) plist)))

;; GDL-USER> (setq plist (list :k1 1 :k2 2 :k3 3 :k4 4 :k5 5 :k6 6))
;; (:K1 1 :K2 2 :K3 3 :K4 4 :K5 5 :K6 6)
;; GDL-USER> (get-plist-keys-dolist plist)
;; (:K1 :K2 :K3 :K4 :K5 :K6)
;; GDL-USER> (get-plist-keys-mapcar plist)
;; (:K1 :K2 :K3 :K4 :K5 :K6)




```

---

## iteration-and-mapping.lisp - mapping
Source: gornschool-training/t2/resources/source/iteration-and-mapping.lisp
Type: tutorial

```
(define-object mapping (base-object)
  :computed-slots
  ((integer-list (list 1 2 3 4 5 6))
   (plist (list :k1 1 :k2 2 :k3 3 :k4 4 :k5 5 :k6 6))
  

   (add-2 (mapcar #'(lambda(i) (+ i 2)) (the integer-list)))
   ;; (3 4 5 6 7 8)
   
   (keys-only (remove nil (mapcar #'(lambda(p) (when (keywordp p) p)) (the plist))))
   ;; (:k1 :k2 :k3 :k4 :k5 :k6)

   (values-only (remove nil (mapcar #'(lambda(p) (when (not (keywordp p)) p)) (the plist))))
   ;; (1 2 3 4 5 6)

   (spliced-plist (mapcan #'list (the keys-only) (the add-2)))
   ;; (:K1 3 :K2 4 :K3 5 :K4 6 :K5 7 :K6 8)

   (add-plists (mapcar #'(lambda(k) (+ (getf (the plist) k)
				       (getf (the spliced-plist) k)))
		       (the keys-only)))
   ;; (4 6 8 10 12 14)

   ;; slightly more efficient, making only a single 'the' call for each plist
   (add-plists-2 (let ((plist-1 (the plist))
		       (plist-2 (the spliced-plist)))
		   (mapcar #'(lambda(k) (+ (getf plist-1 k)
					   (getf plist-2 k)))
			   (the keys-only))))
   ;; (4 6 8 10 12 14)

   (add-plists-3 (let ((result)
		       (plist-1 (the plist))
		       (plist-2 (the spliced-plist)))
		   (mapc #'(lambda(x) (when (keywordp x)
					(push (+ (getf plist-1 x) (or (getf plist-2 x) 0)) result)))
			 plist-1)
		   (nreverse result)))

    ;; (4 6 8 10 12 14)
		   
   )
  )

```

---

## equality.lisp - header
Source: gornschool-training/t2/resources/source/equality.lisp
Type: tutorial

```
   
(in-package :gdl-user)


```

---

## equality.lisp - equality
Source: gornschool-training/t2/resources/source/equality.lisp
Type: tutorial

```
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
   

  
   
   

```

---

## building-hint-5.lisp - header
Source: gornschool-training/t2/resources/source/building-hint-5.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## building-hint-5.lisp - building
Source: gornschool-training/t2/resources/source/building-hint-5.lisp
Type: tutorial

```
(define-object building (box)
  :input-slots
  ((nominal-height 3000)
   (nominal-width 3000)
   (nominal-length 4000)
   (brick-height 45)
   (brick-length 180)
   (brick-width 90)
   (mortar-joint-width 10)
   (truss-angle 30)
   (beam-width 40)
   (beam-height 50)
   (wall-thickness 3)
   (material-density 7800)
   (roof-overhang 50)
   (cladding-thickness 10)
   (max-beam-spacing 1500))

  :computed-slots
  ((length (the left-wall length))
   (width (the rear-wall length))
   (height (+ (the left-wall height) (the (roof-truss 0) height)))

   (number-of-roof-trusses (let ((trusses (ceiling (the left-wall length) 1500)))
			     (max trusses 2)))

   (truss-spacing (div (- (the left-wall length) (the beam-width))
		       (- (the number-of-roof-trusses) 1)))
   (truss-offsets (let ((res nil))
		    (dotimes (n (the number-of-roof-trusses) (nreverse res))
		      (push (+ (half (the beam-width))
			       (* n (the truss-spacing))) res))))

   (roof-length (+ (the left-wall length) (twice (the roof-overhang))))
   (roof-width (the cladding-thickness))
   (roof-height (let ((apex (the (roof-truss 0) apex-point))
		      (gutter (the (roof-truss 0) front-gutter-point)))
		  (+ (3d-distance apex gutter) (the roof-overhang))))

   ;; building properties
   (walls (remove nil (mapcar #'(lambda(a) (when (typep a 'wall) a)) (the children))))
   (full-bricks (apply '+ (mapsend (the walls) :full-bricks)))
   (half-bricks (apply '+ (mapsend (the walls) :half-bricks)))
   (mortar-volume (apply '+ (mapsend (the walls) :mortar-volume)))
   (cladding-dimensions (list :length (the roof-length)
			      :width (the roof-height)))
   (beam-properties (the (roof-truss 0) beam-properties))
   (beam-qty-by-size (let ((res nil))
		       (dolist (plis (the beam-properties) )
			 (let* ((trusses (the number-of-roof-trusses))
				(l (getf plis :length-mm))
				(p (position l res :key #'(lambda(a) (getf a :length-mm))))
				(qty (when p (getf (nth p res) :qty))))
			   (if p (setf (getf (nth p res) :qty) (+ qty trusses))
			       (setq res (append (list (list :length-mm l :qty trusses)) res)))))
		       (safe-sort res '< :key #'(lambda(a) (getf a :length-mm)))))
		       

   (roof-truss-mass (* (apply '+ (mapcar #'(lambda(a) (getf a :mass-kg))
					 (the beam-properties)))
		       (the number-of-roof-trusses)))

   (building-materials (list :full-bricks (the full-bricks)
			     :half-bricks (the half-bricks)
			     :mortar-volume-m3 (div (the mortar-volume) 1000000000)
			     :beams (the beam-qty-by-size)
			     :roof-cladding (append (the cladding-dimensions) (list :qty 2))))
   
		
   )

  :functions
  ((get-roof-mid-point! (first-gutter last-gutter last-index)
		       (let*((mid-gutter (midpoint first-gutter last-gutter))
			     (first-apex (the (roof-truss 0) apex-point))
			     (last-apex (the (roof-truss last-index) apex-point))
			     (mid-apex (midpoint first-apex last-apex))
			     (vec (subtract-vectors mid-gutter mid-apex))
			     (mid-edge (translate-along-vector mid-gutter vec (the roof-overhang))))
			 (midpoint mid-apex mid-edge))) )
  
  :objects
  ((left-wall :type 'wall
	      :pass-down (brick-height
			  brick-length
			  brick-width
			  mortar-joint-width)
	      :center (translate-along-vector
		       (translate-along-vector (the (edge-center :bottom :left))
					       (the (face-normal-vector :right))
					       (half (the-child width)))
		       (the (face-normal-vector :top))
		       (half (the-child height)))
	      :wall-length (the nominal-length)
	      :wall-height (the nominal-height))

   (right-wall :type 'wall
	       :pass-down (brick-height
			   brick-length
			   brick-width
			   mortar-joint-width)
	       :center (translate-along-vector
			(translate-along-vector (the (edge-center :bottom :right))
						(the (face-normal-vector :left))
						(half (the-child width)))
			(the (face-normal-vector :top))
			(half (the-child height)))
	       :wall-length (the nominal-length)
	       :wall-height (the nominal-height))

   (rear-wall :type 'wall
	      :pass-down (brick-height
			  brick-length
			  brick-width
			  mortar-joint-width)
	      :center (translate-along-vector
			(translate-along-vector (the (edge-center :bottom :rear))
						(the (face-normal-vector :front))
						(half (the-child width)))
			(the (face-normal-vector :top))
			(half (the-child height)))
	      :orientation (alignment :rear (the (face-normal-vector :right)))
	      :wall-length (the nominal-width)
	      :wall-height (the nominal-height))

   (roof-truss :type 'truss
	       :sequence (:size (the number-of-roof-trusses))
	       :truss-length (the rear-wall length)
	       :center (translate-along-vector
			(translate-along-vector (the (edge-center :front :top))
						(the (face-normal-vector :bottom))
						(half (the-child height)))
			(the (face-normal-vector :rear))
			(nth (the-child index) (the truss-offsets)))
	       :orientation (alignment :rear (the (face-normal-vector :right))
				       )		
	       :pass-down (truss-angle
			   beam-width
			   beam-height
			   wall-thickness
			   material-density))
   
   (roof-cladding-left
    :type 'box
    :length (the roof-length)
    :height (the roof-height)
    :width (the cladding-thickness)
    :orientation (alignment :left (the (roof-truss 0) front-slope-normal))
    :center (let* ((last-index (- (the number-of-roof-trusses) 1))
		   (first-gutter (the (roof-truss 0) front-gutter-point))
		   (last-gutter (the (roof-truss last-index) front-gutter-point))
		   (mid-ctr (the (get-roof-mid-point! first-gutter last-gutter last-index))))
	      (translate-along-vector mid-ctr
				      (the (roof-truss 0) front-slope-normal)
				      (half (the cladding-thickness)))))
   
   (roof-cladding-right :type 'box
			:length (the roof-length)
			:height (the roof-height)
			:width (the cladding-thickness)
			:orientation (alignment :left (the (roof-truss 0) rear-slope-normal))
			 :center (let* ((last-index (- (the number-of-roof-trusses) 1))
					(first-gutter (the (roof-truss 0) rear-gutter-point))
					(last-gutter (the (roof-truss last-index) rear-gutter-point))
					(mid-ctr (the (get-roof-mid-point! first-gutter last-gutter last-index))))
				 (translate-along-vector mid-ctr
							 (the (roof-truss 0) rear-slope-normal)
							 (half (the cladding-thickness)))))
   )

  
		       
  )



```

---

## building-hint-5.lisp - wall
Source: gornschool-training/t2/resources/source/building-hint-5.lisp
Type: tutorial

```
(define-object wall(box)
  :input-slots
  ((brick-height 45)
   (brick-length 180)
   (brick-width 90)
   (mortar-joint-width 10)
   (wall-length 3700)
   (wall-height 900)
   (first-row :start-full)
   (front-edge :full)
   (rear-edge :full))

  :computed-slots
  ((row-height (+ (the brick-height) (the mortar-joint-width)))
   (number-of-rows (round-to-nearest (div (the wall-height) (the row-height)) 1))
   (actual-wall-height (* (the row-height) (the number-of-rows)))
   
   ;; for the wall-length we need the number of full bricks
   ;; if there are n full bricks then there will be (n-1) mortar joints
   ;; so n*brick-length + n-1*mortar-joint-width = overall-length
   ;; or n(brick-length + mortar-join-width) - mortar-joint-width = overall-length
   ;; or n = (overall-length - mortar-joint-width)/(brick-length + mortar-joint-width)
   (number-of-bricks (round-to-nearest (div (- (the wall-length) (the mortar-joint-width))
					    (+ (the brick-length) (the mortar-joint-width)))
				       1))
   (actual-wall-length (+ (* (the number-of-bricks) (the brick-length))
			  (* (- (the number-of-bricks) 1) (the mortar-joint-width))))

   ;; box inputs - gives the wall bounding box
   (height (the actual-wall-height))
   (width (the brick-width))
   (length (the actual-wall-length))

   (full-bricks (apply '+ (list-elements (the row) (the-element full-bricks))))
   (half-bricks (apply '+ (list-elements (the row) (the-element half-bricks))))
   (mortar-volume (apply '+ (list-elements (the row) (the-element mortar-volume))))
   (mortar-density 2162)
   (mortar-mass (* (the mortar-density) (div (the mortar-volume) 1000000000)))
   )
   
     
  :objects
  ((row :type 'row
     :sequence (:size (the number-of-rows))
     :center (translate-along-vector (the (face-center :bottom))
				     (the (face-normal-vector :top))
				     (+ (half (the-child height))
					(* (the-child index) (the-child height))))
     :height (+ (the brick-height) (the mortar-joint-width))
     :full-bricks-per-row (the number-of-bricks)
     :pass-down (brick-height
		 brick-length
		 brick-width
		 mortar-joint-width
		 length
		 width
		 first-row
		 front-edge
		 rear-edge))))


```

---

## building-hint-5.lisp - row
Source: gornschool-training/t2/resources/source/building-hint-5.lisp
Type: tutorial

```
(define-object row (box)
  :input-slots
  (full-bricks-per-row
   brick-height
   brick-length
   brick-width
   mortar-joint-width
   first-row
   front-edge
   rear-edge )

  :computed-slots
  ((full-brick-row? (cond ((eq (the first-row) :start-full)
			   (or (zerop (the index)) (evenp (the index))))
			  ((eq (the first-row) :start-half)
			   (not (or (zerop (the index)) (evenp (the index)))))))
		    
   (full-bricks (the bricks-and-mortar full-bricks))
   (half-bricks (the bricks-and-mortar half-bricks))
   (mortar-volume (+ (the bricks-and-mortar mortar-joint-volume)
		     (the mortar-bed volume))))

  :objects
  ((bricks-and-mortar :type 'bricks-and-mortar
		      :height (the brick-height)
		      :center (translate-along-vector (the mortar-bed (face-center :top))
						      (the mortar-bed (face-normal-vector :top))
						      (half (the-child height)))
		      :pass-down (width
				  length
				  full-brick-row?
				  brick-height
				  brick-length
				  brick-width
				  mortar-joint-width
				  full-bricks-per-row
				  front-edge
				  rear-edge))
   (mortar-bed :type 'box
	       :height (the mortar-joint-width)
	       :center (translate-along-vector (the (face-center :bottom))
					       (the (face-normal-vector :top))
					       (half (the-child height)))
	       :pass-down (width
			   length))))


```

---

## building-hint-5.lisp - bricks-and-mortar
Source: gornschool-training/t2/resources/source/building-hint-5.lisp
Type: tutorial

```
(define-object bricks-and-mortar (box)
  :input-slots
  (full-brick-row?
   full-bricks-per-row
   brick-height
   brick-length
   brick-width
   mortar-joint-width
   front-edge
   rear-edge)

  :computed-slots
  ((first-full-brick-start-point (if (the full-brick-row?)
				     (the (face-center :front))
				     (the (mortar-joint 0) (face-center :rear))))
   
   (first-mortar-joint-start-point (cond ((the full-brick-row?) (the (full-brick 0) (face-center :rear)))
					 ((eq (the front-edge) :full) (the (half-brick 0) (face-center :rear)))
					 ((eq (the front-edge) :keyed) (translate-along-vector (the (face-center :front))
											       (the (face-normal-vector :rear))
											       (half (the brick-length))))))

   (number-of-full-bricks (if (the full-brick-row?)
			      (the full-bricks-per-row)
			      (- (the full-bricks-per-row) 1)))

   (number-of-half-bricks (cond ((the full-brick-row?) 0)
				((and (eq (the front-edge) :full)(eq (the rear-edge) :full)) 2)
				((and (eq (the front-edge) :keyed) (eq (the rear-edge) :full)) 1)
				((and (eq (the front-edge) :full) (eq (the rear-edge) :keyed)) 1)
				((and (eq (the front-edge) :keyed) (eq (the rear-edge) :keyed)) 0)))

   ;; whether or not the ends are :full or :keyed, the number of mortar joints remains the same since the mortar joint
   ;; when it is :keyed is used to connect to the full brick of the other wall
   (number-of-mortar-joints (if (the full-brick-row?)
				(- (the number-of-full-bricks) 1)
				(+ (the number-of-full-bricks) 1)))

   ;; if it isn't a full brick row then there will be an extra joint because one
   ;; full brick is replaced with 2 half bricks so without correcting the
   ;; mortar-joint-width the ends of a full brick row and one starting and
   ;; finishing with half bricks won't align. So we need to correct
   ;; the mortar-joint-width
   (corrected-joint-width (if (the full-brick-row?)
				     (the mortar-joint-width)
				     (let ((total-gap (* (- (the number-of-mortar-joints) 1)
									(the mortar-joint-width))))
				       (div total-gap (the number-of-mortar-joints)))))
   ;; collating the output. We could do this analytically, but for this example we'll use the geometry
   (full-bricks (length (list-elements (the full-brick))))
   (half-bricks (length (list-elements (the half-brick))))
   (mortar-joint-volume (apply '+ (list-elements (the mortar-joint)
						 (the-element volume))))
   )
  
  :functions
  ((first-full-brick-center!
    ()
    (translate-along-vector (the first-full-brick-start-point)
			    (the (face-normal-vector :rear))
			    (half (the brick-length))))
   
   (other-full-brick-center!
    (index)
    ;; if its a full brick row, (full-brick 1) is positioned relative to (joint 0)
    ;; if its a half brick row, (full-brick 1) is positioned relative to (joint 1)
    (let ((ind (if (the full-brick-row?) (- index 1) index)))
	  (translate-along-vector (the (mortar-joint ind) (face-center :rear))
				  (the (face-normal-vector :rear))
				  (half (the brick-length)))))
   
    (first-joint-center!
     ()
     (translate-along-vector (the first-mortar-joint-start-point)
			     (the (face-normal-vector :rear))
			     (half (the corrected-joint-width))))
   (other-joint-center!
    (index)
    ;; if its a full brick row, (joint 1) is positioned relative to (full-brick 1)
    ;; if its a half brick row, (joint 1) is positioned relative to (full-brick 0)
    (let ((ind (if (the full-brick-row?) index (- index 1))))
      (translate-along-vector (the (full-brick ind) (face-center :rear))
			      (the (face-normal-vector :rear))
			      (half (the corrected-joint-width)))))

    (first-half-brick-center!
     ()
     (translate-along-vector (the (face-center :front))
			     (the (face-normal-vector :rear))
			     (half (half (the brick-length)))))
   
   (last-half-brick-center!
     ()
     (translate-along-vector (theo (the mortar-joint last) (face-center :rear))
			     (the (face-normal-vector :rear))
			     (half (half (the brick-length))))))
 			     
  
  :objects
  ((full-brick :type 'box
	       :sequence (:size (the number-of-full-bricks))
	       :center (if (= (the-child index) 0)
			   (the first-full-brick-center!)
			   (the (other-full-brick-center! (the-child index))))
	       :length (the brick-length)
	       :height (the brick-height)
	       :width (the brick-width))

   (half-brick :type 'box
	       :sequence (:size (the number-of-half-bricks))
	       :center (cond ((and (= (the-child index) 0)
				   (eq (the front-edge) :full)) (the first-half-brick-center!))
			     ((and (= (the-child index) 0)
				   (eq (the front-edge) :keyed)
				   (eq (the rear-edge) :full)) (the last-half-brick-center!))
			     ((eq (the rear-edge) :full) (the last-half-brick-center!)))
	       :length (half (the brick-length))
	       :height (the brick-height)
	       :width (the brick-width))

   (mortar-joint :type 'box
		 :sequence (:size (the number-of-mortar-joints))
		 :center (if (= (the-child index) 0)
			     (the first-joint-center!)
			  (the (other-joint-center! (the-child index))))   
		 :height (the brick-height)
		 :width (the brick-width)
		 :length (the corrected-joint-width))))



```

---

## building-hint-5.lisp - degrees-to-radians
Source: gornschool-training/t2/resources/source/building-hint-5.lisp
Type: tutorial

```
(defun degrees-to-radians (degrees)
  (div (* degrees pi) 180))



```

---

## building-hint-5.lisp - truss
Source: gornschool-training/t2/resources/source/building-hint-5.lisp
Type: tutorial

```
(define-object truss (box)
  :input-slots
  ((truss-length 2000)
   (truss-height nil)
   (truss-angle 30)

   (beam-width 30)
   (beam-height 50)
   (wall-thickness 3)

   (material-density 7800)
   (tonne-rate 450))

  :computed-slots
  ((length (the truss-length))
   (height (cond ((the truss-height)(the truss-height))
		 ((the truss-angle) (+ (* (half (the truss-length))
					  (tan (degrees-to-radians (the truss-angle))))
				       (the beam-height)))))
   (width (the beam-width))
  
   (truss-front-slope-vector (the (get-slope-vector! :front)))
   (truss-rear-slope-vector (the (get-slope-vector! :rear)))

   (front-slope-length (the (get-slope-length! :front)))
   (rear-slope-length (the (get-slope-length! :rear)))
   
   (front-slope-center (the (get-slope-center! :front)))
   (rear-slope-center (the (get-slope-center! :rear)))
   

   (beam-properties (mapsend (the children) :beam-properties))
   (total-mass (round-to-nearest
		(apply '+
		       (mapcar #'(lambda(a) (getf a :mass-kg))
			       (the beam-properties)))
		0.001))
   (total-cost (round-to-nearest
		(apply '+
		       (mapcar #'(lambda(a) (getf a :cost-gbp))
			       (the beam-properties)))
		0.01))

   ;; messages to support roof cladding sizing and positioning
   (apex-point (inter-line-plane (the rear-slope-construction-line end)
			   (the truss-rear-slope-vector)
			   (the lower-beam center)
				 (the (face-normal-vector :rear))))
   (front-gutter-point (the front-slope-construction-line start))
   (rear-gutter-point (the rear-slope-construction-line start))
   (front-slope-normal (the front-slope-beam (face-normal-vector :top)))
   (rear-slope-normal (the rear-slope-beam (face-normal-vector :top)))
   )

  :functions
  ((get-slope-vector! (beam-side)
		      (let ((v-key (the (get-v-key! beam-side)))
			    (l-key (the (get-l-key! beam-side))))
		      (subtract-vectors (the vertical-beam (edge-center :rear v-key))
					(the lower-beam (edge-center l-key :top)))))
   (get-slope-length! (beam-side)
		      (let ((v-key (the (get-v-key! beam-side)))
			    (l-key (the (get-l-key! beam-side))))
			(3d-distance (the vertical-beam (edge-center :rear v-key))
				     (the lower-beam (edge-center l-key :top)))))
   (get-slope-center!
    (beam-side)
    (let ((pt (case beam-side
		(:front (the front-slope-construction-line center))
		(:rear  (the rear-slope-construction-line center))))
	  (norm-vector (case beam-side
			 (:front (the front-slope-beam (face-normal-vector :bottom)))
			 (:rear (the rear-slope-beam (face-normal-vector :bottom))))))
      (translate-along-vector pt
			      norm-vector
			      (half (the beam-height)))))
   (get-v-key! (beam-side)
	       (case beam-side
		 (:front :top)
		 (:rear :bottom)))
   (get-l-key! (beam-side)
	       (case beam-side
		 (:front :front)
		 (:rear :rear)))

   
   )
   
  
  
  :objects
  ((lower-beam :type 'beam
	       :pass-down (:beam-height :beam-width :wall-thickness :material-density :tonne-rate)
	       :beam-length (the truss-length)
	       :center (translate-along-vector (the (face-center :bottom))
					       (the (face-normal-vector :top))
					       (half (the beam-height))))
   (vertical-beam :type 'beam
		  :pass-down (:beam-height :beam-width :wall-thickness :material-density :tonne-rate)
		  
		  :beam-length (- (the height) (the beam-height))
		  :orientation (alignment :rear (the (face-normal-vector :top))
					  :right (the (face-normal-vector :right)))  
		  :center (translate-along-vector (the lower-beam (face-center :top))
						  (the lower-beam (face-normal-vector :top))
						  (half (the-child beam-length))))
   (front-slope-beam :type 'beam
		     :pass-down (:beam-height :beam-width :wall-thickness :material-density :tonne-rate)
		     :beam-length (the front-slope-length)
		     :center (the front-slope-center)
		     :orientation (alignment :rear (the truss-front-slope-vector)
					     :right (the (face-normal-vector :right))))

   (rear-slope-beam :type 'beam
		    :pass-down (:beam-height :beam-width :wall-thickness :material-density :tonne-rate)
		    :beam-length (the rear-slope-length)
		    :center (the rear-slope-center)
		    :orientation (alignment :rear (the truss-rear-slope-vector)
					    :left (the (face-normal-vector :right))))
   
   )

  :hidden-objects
  ((apex-pt :type 'sphere
	    :radius 5
	    :display-controls (list :color :green)
	    :center (the apex-point))
   (front-slope-construction-line :type 'line
				  :start (the lower-beam (edge-center :front :top))
				  :end (the vertical-beam (edge-center :rear :top)))
   (rear-slope-construction-line :type 'line
				 :start (the lower-beam (edge-center :rear :top))
				 :end (the vertical-beam (edge-center :rear :bottom)))
   
   (front-mid-pt :type 'sphere
		 :display-controls (list :color :blue)
		 :radius 5
		 :center (the front-slope-construction-line center))
   (rear-mid-pt :type 'sphere
		:display-controls (list :color :blue)
		:center (the rear-slope-construction-line center)
		:radius 5)
   (pt-1 :type 'sphere
	 :radius 5
	 :display-controls (list :color :green)
	 :center (the lower-beam (edge-center :rear :top)))
   (pt-2 :type 'sphere
	 :radius 5
	 :display-controls (list :color :red)
	 :center (the vertical-beam (edge-center :rear :bottom)) )

   

   (vector-line :type 'vector-line
		:start-point (the rear-slope-construction-line center)
		:vector (the rear-slope-beam (face-normal-vector :bottom))
		:length 150)
   
   ))


```

---

## building-hint-5.lisp - vector-line
Source: gornschool-training/t2/resources/source/building-hint-5.lisp
Type: tutorial

```
(define-object vector-line (box)
  :input-slots
  ((start-point (make-point 0 0 0))
   (vector (make-vector 1 0 1))
   (length 50)
   (width 1))
  :computed-slots
  ((height (div (the length) 5)))
  :objects
  ((v-line :type 'line
	   :start (the start-point)
	   :display-controls (list :color :red)
	   :end (translate-along-vector (the start-point)
					(the vector)
					(the length)))
   (arrow :type 'cone
	  :radius-1 0
	  :radius-2 (div (the length) 50)
	  :length (div (the length) 5)
	  :display-controls (list :color :red)
	  :center (translate-along-vector (the v-line end)
					  (the vector)
					  (half (the-child length)))
	  :orientation (alignment :front (the vector)))))

			 
		  

```

---

## building-hint-5.lisp - beam
Source: gornschool-training/t2/resources/source/building-hint-5.lisp
Type: tutorial

```
(define-object beam (box)
  :input-slots
  ((beam-length 1000)
   (beam-width 40)
   (beam-height 50)
   (wall-thickness 2)
   (material-density 7800)
   (tonne-rate 500))

  :computed-slots
  ((length (the beam-length))
   (width (the beam-width))
   (height (the beam-height))

   (beam-volume (- (the outer volume) (the inner volume)))
   (beam-mass (* (div (the beam-volume) 1000000000) (the material-density)))
   (beam-cost (* (the tonne-rate) (div (the beam-mass) 1000)))
   (beam-properties (list :volume-mm3 (the beam-volume)
			  :mass-kg (round-to-nearest (the beam-mass) 0.01)
			  :cost-gbp (round-to-nearest (the beam-cost) 0.01)
			  :length-mm (the beam-length)
			  :width-mm (the beam-width)
			  :height-mm (the beam-height)
			  :thickness-mm (the wall-thickness))))
   
  :objects
  ((outer :type 'box
	  :length (the beam-length)
	  :width (the beam-width)
	  :height (the beam-height))

   (inner :type 'box
	  :length (the beam-length)
	  :width (- (the beam-width) (twice (the wall-thickness)))
	  :height (- (the beam-height) (twice (the wall-thickness)))))
  )










```

---

## format.lisp - header
Source: gornschool-training/t2/resources/source/format.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## format.lisp - top-10-formatting
Source: gornschool-training/t2/resources/source/format.lisp
Type: tutorial

```
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
   

```

---

## truss-hint-6a.lisp - header
Source: gornschool-training/t2/resources/source/truss-hint-6a.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## truss-hint-6a.lisp - degrees-to-radians
Source: gornschool-training/t2/resources/source/truss-hint-6a.lisp
Type: tutorial

```
(defun degrees-to-radians (degrees)
  (div (* degrees pi) 180))


```

---

## truss-hint-6a.lisp - truss
Source: gornschool-training/t2/resources/source/truss-hint-6a.lisp
Type: tutorial

```
(define-object truss (box)
  :input-slots
  ((truss-length 2000)
   (truss-height 800)
   (truss-angle nil)

   (beam-width 30)
   (beam-height 50)
   (wall-thickness 3))

  :computed-slots
  ((length (the truss-length))
   (height (cond ((the truss-height)(the truss-height))
		 ((the truss-angle) (+ (* (half (the truss-length))
					  (tan (degrees-to-radians (the truss-angle))))
				       (the beam-height)))))
   (width (the beam-width))
   (truss-front-slope-vector (subtract-vectors (the vertical-beam (edge-center :rear :top))
					       (the lower-beam (edge-center :front :top))))
   (front-slope-length (3d-distance (the vertical-beam (edge-center :rear :top))
				    (the lower-beam (edge-center :front :top))))
   (front-slope-center (translate-along-vector (the front-slope-construction-line center)
					       (the front-slope-beam (face-normal-vector :bottom))
					       (half (the beam-height))))
   (truss-rear-slope-vector (subtract-vectors (the vertical-beam (edge-center :rear :bottom))
					      (the lower-beam (edge-center :rear :top))))
   (rear-slope-length (3d-distance (the vertical-beam (edge-center :rear :bottom))
				   (the lower-beam (edge-center :rear :top))))
   (rear-slope-center (translate-along-vector (the rear-slope-construction-line center)
					      (the rear-slope-beam (face-normal-vector :bottom))
					      (half (the beam-height))))
   )
				       
  :objects
  ((lower-beam :type 'beam
	       :beam-height (the beam-height)
	       :beam-width (the beam-width)
	       :beam-length (the truss-length)
	       :center (translate-along-vector (the (face-center :bottom))
					       (the (face-normal-vector :top))
					       (half (the beam-height))))
   (vertical-beam :type 'beam
		  :beam-length (- (the height) (the beam-height))
		  :beam-height (the beam-height)
		  :beam-width (the beam-width)
		  :orientation (alignment :rear (the (face-normal-vector :top))
					  :right (the (face-normal-vector :right)))
		  :center (translate-along-vector (the lower-beam (face-center :top))
						  (the lower-beam (face-normal-vector :top))
						  (half (the-child beam-length))))
   (front-slope-beam :type 'beam
		     :beam-length (the front-slope-length)
		     :beam-height (the beam-height)
		     :beam-width (the beam-width)
		     :center (the front-slope-center)
		     :orientation (alignment :rear (the truss-front-slope-vector)
					     :right (the (face-normal-vector :right))))

   (front-slope-construction-line :type 'line
				  :start (the lower-beam (edge-center :front :top))
				  :end (the vertical-beam (edge-center :rear :top)))
   
   (front-mid-pt :type 'sphere
		 :display-controls (list :color :blue)
		 :radius 5
		 :center (the front-slope-construction-line center))
   
   
   (rear-slope-beam :type 'beam
		     :beam-length (the rear-slope-length)
		     :beam-height (the beam-height)
		     :beam-width (the beam-width)
		     :center (the rear-slope-center)
		     :orientation (alignment :rear (the truss-rear-slope-vector)
					     :left (the (face-normal-vector :right))))
   
   (rear-slope-construction-line :type 'line
				 :start (the lower-beam (edge-center :rear :top))
				 :end (the vertical-beam (edge-center :rear :bottom)))
   (rear-mid-pt :type 'sphere
		:display-controls (list :color :blue)
		:center (the rear-slope-construction-line center)
		:radius 5)
   
   (pt-1 :type 'sphere
	 :radius 5
	 :display-controls (list :color :green)
	 :center (the lower-beam (edge-center :front :top)))
   (pt-2 :type 'sphere
	 :radius 5
	 :display-controls (list :color :red)
	 :center (the vertical-beam (edge-center :rear :top)) )
   (vector-line :type 'vector-line
		:start-point (the pt-1 center)
		:vector (the truss-front-slope-vector)
		:length 150)))


```

---

## truss-hint-6a.lisp - vector-line
Source: gornschool-training/t2/resources/source/truss-hint-6a.lisp
Type: tutorial

```
(define-object vector-line (box)
  :input-slots
  ((start-point (make-point 0 0 0))
   (vector (make-vector 1 0 1))
   (length 50)
   (width 1))
  :computed-slots
  ((height (div (the length) 5)))
  :objects
  ((v-line :type 'line
	   :start (the start-point)
	   :display-controls (list :color :red)
	   :end (translate-along-vector (the start-point)
					(the vector)
					(the length)))
   (arrow :type 'cone
	  :radius-1 0
	  :radius-2 (div (the length) 50)
	  :length (div (the length) 5)
	  :display-controls (list :color :red)
	  :center (translate-along-vector (the v-line end)
					  (the vector)
					  (half (the-child length)))
	  :orientation (alignment :front (the vector)))))


```

---

## truss-hint-6a.lisp - beam
Source: gornschool-training/t2/resources/source/truss-hint-6a.lisp
Type: tutorial

```
(define-object beam (box)
  :input-slots
  ((beam-length 1000)
   (beam-width 40)
   (beam-height 50)
   (wall-thickness 2)
   (material-density 7800)
   (tonne-rate 500))

  :computed-slots
  ((length (the beam-length))
   (width (the beam-width))
   (height (the beam-height))

   (beam-volume (- (the outer volume) (the inner volume)))
   (beam-mass (* (div (the beam-volume) 1000000000) (the material-density)))
   (beam-cost (* (the tonne-rate) (div (the beam-mass) 1000)))
   (beam-properties (list :volume-mm3 (the beam-volume)
			  :mass-kg (round-to-nearest (the beam-mass) 0.01)
			  :cost-gbp (round-to-nearest (the beam-cost) 0.01)
			  :length-mm (the beam-length)
			  :width-mm (the beam-width)
			  :height-mm (the beam-height)
			  :thickness-mm (the wall-thickness))))
   
  :objects
  ((outer :type 'box
	  :length (the beam-length)
	  :width (the beam-width)
	  :height (the beam-height))

   (inner :type 'box
	  :length (the beam-length)
	  :width (- (the beam-width) (twice (the wall-thickness)))
	  :height (- (the beam-height) (twice (the wall-thickness)))))
  )

```

---

## wall-hint-2.lisp - header
Source: gornschool-training/t2/resources/source/wall-hint-2.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## wall-hint-2.lisp - wall
Source: gornschool-training/t2/resources/source/wall-hint-2.lisp
Type: tutorial

```
(define-object wall()
  :input-slots
  ((brick-height 45)
   (brick-length 180)
   (brick-width 90)
   (mortar-joint-width 10)
   (wall-length 3700)
   (wall-height 900))
     
  :objects
  ((row :type 'row
     :brick-height(the brick-height)
     :brick-length (the brick-length)
     :brick-width (the brick-width)
     :mortar-joint (the mortar-joint-width))))


```

---

## wall-hint-2.lisp - row
Source: gornschool-training/t2/resources/source/wall-hint-2.lisp
Type: tutorial

```
(define-object row ()
  :input-slots
  (brick-height
   brick-length
   brick-width
   mortar-joint-width)

  :objects
  ((bricks-and-mortar :type 'bricks-and-mortar
		      :brick-height (the brick-height)
		      :brick-length (the brick-length)
		      :brick-width (the brick-width)
		      :mortar-joint-width (the mortar-joint-width))
   (mortar-bed :type 'box
	       :height (the mortar-joint-width))
   )
  )


```

---

## wall-hint-2.lisp - bricks-and-mortar
Source: gornschool-training/t2/resources/source/wall-hint-2.lisp
Type: tutorial

```
(define-object bricks-and-mortar ()
  :input-slots
  (brick-height
   brick-length
   brick-width
   mortar-joint-width)
  
  :objects
  ((full-brick :type 'box
	       :length (the brick-length)
	       :height (the brick-height)
	       :width (the brick-width))
   (half-brick :type 'box
	       :length (half (the brick-length))
	       :height (the brick-height)
	       :width (the brick-width))
   (mortar-joint :type 'box
		 :height (the brick-height)
		 :width (the brick-width)
		 :length (the mortar-joint-width))))

```

---

## sequences.lisp - header
Source: gornschool-training/t2/resources/source/sequences.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## sequences.lisp - assembly-6
Source: gornschool-training/t2/resources/source/sequences.lisp
Type: tutorial

```
(define-object assembly-6 (base-object)

  :input-slots
  ((number-of-boxes 3))

  :computed-slots
  ((first-box-volume-1 (the (my-box 0) volume))
   (first-box-volume-2 (the-object (the my-box last) volume))
   (second-box-volume (the (my-box 1) volume))
   (last-box-volume-1 (the-object (the my-box last) volume)))

  :objects
  ((my-box :type 'box
	   :sequence (:size (the number-of-boxes))
	   :length (+ 2 (* (the-child index) 3))
	   :width 2
	   :height 1
	   :center (make-point (* (the-child index) 6) 0 0))))


```

---

## sequences.lisp - assembly-7
Source: gornschool-training/t2/resources/source/sequences.lisp
Type: tutorial

```
(define-object assembly-7 (base-object)
		 
  :input-slots
  ((number-of-boxes 3))
		 
  :objects
  ((my-box :type 'box
	   :sequence (:radial (the number-of-boxes))
	   :length (+ 2 (* (the-child index) 3))
	   :width 2
	   :height 1)))


```

---

## building-hint-3.lisp - header
Source: gornschool-training/t2/resources/source/building-hint-3.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## building-hint-3.lisp - building
Source: gornschool-training/t2/resources/source/building-hint-3.lisp
Type: tutorial

```
(define-object building (box)
  :input-slots
  ((nominal-height 3000)
   (nominal-width 3000)
   (nominal-length 4000)
   (brick-height 45)
   (brick-length 180)
   (brick-width 90)
   (mortar-joint-width 10)
   (truss-angle 30)
   (beam-width 40)
   (beam-height 50)
   (wall-thickness 3)
   (material-density 7800)

   (max-beam-spacing 1500))

  :computed-slots
  ((length (the left-wall length))
   (width (the rear-wall length))
   (height (+ (the left-wall height) (the (roof-truss 0) height)))

   (number-of-roof-trusses (let ((trusses (ceiling (the left-wall length) 1500)))
			     (max trusses 2)))

   (truss-spacing (div (- (the left-wall length) (the beam-width))
		       (- (the number-of-roof-trusses) 1)))
   (truss-offsets (let ((res nil))
		    (dotimes (n (the number-of-roof-trusses) (nreverse res))
		      (push (+ (half (the beam-width))
			       (* n (the truss-spacing))) res))))
   )

   
  
  :objects
  ((left-wall :type 'wall
	      :pass-down (brick-height
			  brick-length
			  brick-width
			  mortar-joint-width)
	      :center (translate-along-vector
		       (translate-along-vector (the (edge-center :bottom :left))
					       (the (face-normal-vector :right))
					       (half (the-child width)))
		       (the (face-normal-vector :top))
		       (half (the-child height)))
	      :wall-length (the nominal-length)
	      :wall-height (the nominal-height))

   (right-wall :type 'wall
	       :pass-down (brick-height
			   brick-length
			   brick-width
			   mortar-joint-width)
	       :center (translate-along-vector
			(translate-along-vector (the (edge-center :bottom :right))
						(the (face-normal-vector :left))
						(half (the-child width)))
			(the (face-normal-vector :top))
			(half (the-child height)))
	       :wall-length (the nominal-length)
	       :wall-height (the nominal-height))

   (rear-wall :type 'wall
	      :pass-down (brick-height
			  brick-length
			  brick-width
			  mortar-joint-width)
	      :center (translate-along-vector
			(translate-along-vector (the (edge-center :bottom :rear))
						(the (face-normal-vector :front))
						(half (the-child width)))
			(the (face-normal-vector :top))
			(half (the-child height)))
	      :orientation (alignment :rear (the (face-normal-vector :right)))
	      :wall-length (the nominal-width)
	      :wall-height (the nominal-height))

   (roof-truss :type 'truss
	       :sequence (:size (the number-of-roof-trusses))
	       :truss-length (the rear-wall length)
	       :center (translate-along-vector
			(translate-along-vector (the (edge-center :front :top))
						(the (face-normal-vector :bottom))
						(half (the-child height)))
			(the (face-normal-vector :rear))
			(nth (the-child index) (the truss-offsets)))
	       :orientation (alignment :rear (the (face-normal-vector :right))
				       )		
	       :pass-down (truss-angle
			   beam-width
			   beam-height
			   wall-thickness
			   material-density))
   
   (roof-cladding-left :type 'box)
   (roof-cladding-right :type 'box)
   )
  )

```

---

## building-hint-3.lisp - full-start-wall
Source: gornschool-training/t2/resources/source/building-hint-3.lisp
Type: tutorial

```
(define-object full-start-wall (wall)
  :input-slots
  ((first-row :start-full)))


```

---

## building-hint-3.lisp - half-start-wall
Source: gornschool-training/t2/resources/source/building-hint-3.lisp
Type: tutorial

```
(define-object half-start-wall (wall)
  :input-slots
  ((first-row :start-half)))


```

---

## building-hint-3.lisp - half-start-wall-front-key
Source: gornschool-training/t2/resources/source/building-hint-3.lisp
Type: tutorial

```
(define-object half-start-wall-front-key (wall)
  :input-slots
  ((first-row :start-half)
   (front-edge :keyed)))


```

---

## building-hint-3.lisp - half-start-wall-rear-key
Source: gornschool-training/t2/resources/source/building-hint-3.lisp
Type: tutorial

```
(define-object half-start-wall-rear-key (wall)
  :input-slots
  ((first-row :start-half)
   (rear-edge :keyed)))


```

---

## building-hint-3.lisp - half-start-wall-both-key
Source: gornschool-training/t2/resources/source/building-hint-3.lisp
Type: tutorial

```
(define-object half-start-wall-both-key (wall)
  :input-slots
  ((first-row :start-half)
   (front-edge :keyed)
   (rear-edge :keyed)))


```

---

## building-hint-3.lisp - wall
Source: gornschool-training/t2/resources/source/building-hint-3.lisp
Type: tutorial

```
(define-object wall(box)
  :input-slots
  ((brick-height 45)
   (brick-length 180)
   (brick-width 90)
   (mortar-joint-width 10)
   (wall-length 3700)
   (wall-height 900)
   (first-row :start-full)
   (front-edge :full)
   (rear-edge :full))

  :computed-slots
  ((row-height (+ (the brick-height) (the mortar-joint-width)))
   (number-of-rows (round-to-nearest (div (the wall-height) (the row-height)) 1))
   (actual-wall-height (* (the row-height) (the number-of-rows)))
   
   ;; for the wall-length we need the number of full bricks
   ;; if there are n full bricks then there will be (n-1) mortar joints
   ;; so n*brick-length + n-1*mortar-joint-width = overall-length
   ;; or n(brick-length + mortar-join-width) - mortar-joint-width = overall-length
   ;; or n = (overall-length - mortar-joint-width)/(brick-length + mortar-joint-width)
   (number-of-bricks (round-to-nearest (div (- (the wall-length) (the mortar-joint-width))
					    (+ (the brick-length) (the mortar-joint-width)))
				       1))
   (actual-wall-length (+ (* (the number-of-bricks) (the brick-length))
			  (* (- (the number-of-bricks) 1) (the mortar-joint-width))))

   ;; box inputs - gives the wall bounding box
   (height (the actual-wall-height))
   (width (the brick-width))
   (length (the actual-wall-length))

   (full-bricks (apply '+ (list-elements (the row) (the-element full-bricks))))
   (half-bricks (apply '+ (list-elements (the row) (the-element half-bricks))))
   (mortar-volume (apply '+ (list-elements (the row) (the-element mortar-volume))))
   (mortar-density 2162)
   (mortar-mass (* (the mortar-density) (div (the mortar-volume) 1000000000)))
   )
   
     
  :objects
  ((row :type 'row
     :sequence (:size (the number-of-rows))
     :center (translate-along-vector (the (face-center :bottom))
				     (the (face-normal-vector :top))
				     (+ (half (the-child height))
					(* (the-child index) (the-child height))))
     :height (+ (the brick-height) (the mortar-joint-width))
     :full-bricks-per-row (the number-of-bricks)
     :pass-down (brick-height
		 brick-length
		 brick-width
		 mortar-joint-width
		 length
		 width
		 first-row
		 front-edge
		 rear-edge))))


```

---

## building-hint-3.lisp - row
Source: gornschool-training/t2/resources/source/building-hint-3.lisp
Type: tutorial

```
(define-object row (box)
  :input-slots
  (full-bricks-per-row
   brick-height
   brick-length
   brick-width
   mortar-joint-width
   first-row
   front-edge
   rear-edge )

  :computed-slots
  ((full-brick-row? (cond ((eq (the first-row) :start-full)
			   (or (zerop (the index)) (evenp (the index))))
			  ((eq (the first-row) :start-half)
			   (not (or (zerop (the index)) (evenp (the index)))))))
		    
   (full-bricks (the bricks-and-mortar full-bricks))
   (half-bricks (the bricks-and-mortar half-bricks))
   (mortar-volume (+ (the bricks-and-mortar mortar-joint-volume)
		     (the mortar-bed volume))))

  :objects
  ((bricks-and-mortar :type 'bricks-and-mortar
		      :height (the brick-height)
		      :center (translate-along-vector (the mortar-bed (face-center :top))
						      (the mortar-bed (face-normal-vector :top))
						      (half (the-child height)))
		      :pass-down (width
				  length
				  full-brick-row?
				  brick-height
				  brick-length
				  brick-width
				  mortar-joint-width
				  full-bricks-per-row
				  front-edge
				  rear-edge))
   (mortar-bed :type 'box
	       :height (the mortar-joint-width)
	       :center (translate-along-vector (the (face-center :bottom))
					       (the (face-normal-vector :top))
					       (half (the-child height)))
	       :pass-down (width
			   length))))


```

---

## building-hint-3.lisp - bricks-and-mortar
Source: gornschool-training/t2/resources/source/building-hint-3.lisp
Type: tutorial

```
(define-object bricks-and-mortar (box)
  :input-slots
  (full-brick-row?
   full-bricks-per-row
   brick-height
   brick-length
   brick-width
   mortar-joint-width
   front-edge
   rear-edge)

  :computed-slots
  ((first-full-brick-start-point (if (the full-brick-row?)
				     (the (face-center :front))
				     (the (mortar-joint 0) (face-center :rear))))
   
   (first-mortar-joint-start-point (cond ((the full-brick-row?) (the (full-brick 0) (face-center :rear)))
					 ((eq (the front-edge) :full) (the (half-brick 0) (face-center :rear)))
					 ((eq (the front-edge) :keyed) (translate-along-vector (the (face-center :front))
											       (the (face-normal-vector :rear))
											       (half (the brick-length))))))

   (number-of-full-bricks (if (the full-brick-row?)
			      (the full-bricks-per-row)
			      (- (the full-bricks-per-row) 1)))

   (number-of-half-bricks (cond ((the full-brick-row?) 0)
				((and (eq (the front-edge) :full)(eq (the rear-edge) :full)) 2)
				((and (eq (the front-edge) :keyed) (eq (the rear-edge) :full)) 1)
				((and (eq (the front-edge) :full) (eq (the rear-edge) :keyed)) 1)
				((and (eq (the front-edge) :keyed) (eq (the rear-edge) :keyed)) 0)))

   ;; whether or not the ends are :full or :keyed, the number of mortar joints remains the same since the mortar joint
   ;; when it is :keyed is used to connect to the full brick of the other wall
   (number-of-mortar-joints (if (the full-brick-row?)
				(- (the number-of-full-bricks) 1)
				(+ (the number-of-full-bricks) 1)))

   ;; if it isn't a full brick row then there will be an extra joint because one
   ;; full brick is replaced with 2 half bricks so without correcting the
   ;; mortar-joint-width the ends of a full brick row and one starting and
   ;; finishing with half bricks won't align. So we need to correct
   ;; the mortar-joint-width
   (corrected-joint-width (if (the full-brick-row?)
				     (the mortar-joint-width)
				     (let ((total-gap (* (- (the number-of-mortar-joints) 1)
									(the mortar-joint-width))))
				       (div total-gap (the number-of-mortar-joints)))))
   ;; collating the output. We could do this analytically, but for this example we'll use the geometry
   (full-bricks (length (list-elements (the full-brick))))
   (half-bricks (length (list-elements (the half-brick))))
   (mortar-joint-volume (apply '+ (list-elements (the mortar-joint)
						 (the-element volume))))
   )
  
  :functions
  ((first-full-brick-center!
    ()
    (translate-along-vector (the first-full-brick-start-point)
			    (the (face-normal-vector :rear))
			    (half (the brick-length))))
   
   (other-full-brick-center!
    (index)
    ;; if its a full brick row, (full-brick 1) is positioned relative to (joint 0)
    ;; if its a half brick row, (full-brick 1) is positioned relative to (joint 1)
    (let ((ind (if (the full-brick-row?) (- index 1) index)))
	  (translate-along-vector (the (mortar-joint ind) (face-center :rear))
				  (the (face-normal-vector :rear))
				  (half (the brick-length)))))
   
    (first-joint-center!
     ()
     (translate-along-vector (the first-mortar-joint-start-point)
			     (the (face-normal-vector :rear))
			     (half (the corrected-joint-width))))
   (other-joint-center!
    (index)
    ;; if its a full brick row, (joint 1) is positioned relative to (full-brick 1)
    ;; if its a half brick row, (joint 1) is positioned relative to (full-brick 0)
    (let ((ind (if (the full-brick-row?) index (- index 1))))
      (translate-along-vector (the (full-brick ind) (face-center :rear))
			      (the (face-normal-vector :rear))
			      (half (the corrected-joint-width)))))

    (first-half-brick-center!
     ()
     (translate-along-vector (the (face-center :front))
			     (the (face-normal-vector :rear))
			     (half (half (the brick-length)))))
   
   (last-half-brick-center!
     ()
     (translate-along-vector (theo (the mortar-joint last) (face-center :rear))
			     (the (face-normal-vector :rear))
			     (half (half (the brick-length))))))
 			     
  
  :objects
  ((full-brick :type 'box
	       :sequence (:size (the number-of-full-bricks))
	       :center (if (= (the-child index) 0)
			   (the first-full-brick-center!)
			   (the (other-full-brick-center! (the-child index))))
	       :length (the brick-length)
	       :height (the brick-height)
	       :width (the brick-width))

   (half-brick :type 'box
	       :sequence (:size (the number-of-half-bricks))
	       :center (cond ((and (= (the-child index) 0)
				   (eq (the front-edge) :full)) (the first-half-brick-center!))
			     ((and (= (the-child index) 0)
				   (eq (the front-edge) :keyed)
				   (eq (the rear-edge) :full)) (the last-half-brick-center!))
			     ((eq (the rear-edge) :full) (the last-half-brick-center!)))
	       :length (half (the brick-length))
	       :height (the brick-height)
	       :width (the brick-width))

   (mortar-joint :type 'box
		 :sequence (:size (the number-of-mortar-joints))
		 :center (if (= (the-child index) 0)
			     (the first-joint-center!)
			  (the (other-joint-center! (the-child index))))   
		 :height (the brick-height)
		 :width (the brick-width)
		 :length (the corrected-joint-width))))









```

---

## building-hint-3.lisp - truss
Source: gornschool-training/t2/resources/source/building-hint-3.lisp
Type: tutorial

```
(define-object truss (box)
  :input-slots
  ((truss-length 2000)
   (truss-height nil)
   (truss-angle 30)

   (beam-width 30)
   (beam-height 50)
   (wall-thickness 3)

   (material-density 7800)
   (tonne-rate 450))

  :computed-slots
  ((length (the truss-length))
   (height (cond ((the truss-height)(the truss-height))
		 ((the truss-angle) (+ (* (half (the truss-length))
					  (tan (degrees-to-radians (the truss-angle))))
				       (the beam-height)))))
   (width (the beam-width))
  
   (truss-front-slope-vector (the (get-slope-vector! :front)))
   (truss-rear-slope-vector (the (get-slope-vector! :rear)))

   (front-slope-length (the (get-slope-length! :front)))
   (rear-slope-length (the (get-slope-length! :rear)))
   
   (front-slope-center (the (get-slope-center! :front)))
   (rear-slope-center (the (get-slope-center! :rear)))
   

   (beam-properties (mapsend (the children) :beam-properties))
   (total-mass (round-to-nearest
		(apply '+
		       (mapcar #'(lambda(a) (getf a :mass-kg))
			       (the beam-properties)))
		0.001))
   (total-cost (round-to-nearest
		(apply '+
		       (mapcar #'(lambda(a) (getf a :cost-gbp))
			       (the beam-properties)))
		0.01))
   )

  :functions
  ((get-slope-vector! (beam-side)
		      (let ((v-key (the (get-v-key! beam-side)))
			    (l-key (the (get-l-key! beam-side))))
		      (subtract-vectors (the vertical-beam (edge-center :rear v-key))
					(the lower-beam (edge-center l-key :top)))))
   (get-slope-length! (beam-side)
		      (let ((v-key (the (get-v-key! beam-side)))
			    (l-key (the (get-l-key! beam-side))))
			(3d-distance (the vertical-beam (edge-center :rear v-key))
				     (the lower-beam (edge-center l-key :top)))))
   (get-slope-center!
    (beam-side)
    (let ((pt (case beam-side
		(:front (the front-slope-construction-line center))
		(:rear  (the rear-slope-construction-line center))))
	  (norm-vector (case beam-side
			 (:front (the front-slope-beam (face-normal-vector :bottom)))
			 (:rear (the rear-slope-beam (face-normal-vector :bottom))))))
      (translate-along-vector pt
			      norm-vector
			      (half (the beam-height)))))
   (get-v-key! (beam-side)
	       (case beam-side
		 (:front :top)
		 (:rear :bottom)))
   (get-l-key! (beam-side)
	       (case beam-side
		 (:front :front)
		 (:rear :rear)))

   
   )
   
				       
  :objects
  ((lower-beam :type 'beam
	       :pass-down (:beam-height :beam-width :wall-thickness :material-density :tonne-rate)
	       :beam-length (the truss-length)
	       :center (translate-along-vector (the (face-center :bottom))
					       (the (face-normal-vector :top))
					       (half (the beam-height))))
   (vertical-beam :type 'beam
		  :pass-down (:beam-height :beam-width :wall-thickness :material-density :tonne-rate)
		  
		  :beam-length (- (the height) (the beam-height))
		  :orientation (alignment :rear (the (face-normal-vector :top))
					  :right (the (face-normal-vector :right)))  
		  :center (translate-along-vector (the lower-beam (face-center :top))
						  (the lower-beam (face-normal-vector :top))
						  (half (the-child beam-length))))
   (front-slope-beam :type 'beam
		     :pass-down (:beam-height :beam-width :wall-thickness :material-density :tonne-rate)
		     :beam-length (the front-slope-length)
		     :center (the front-slope-center)
		     :orientation (alignment :rear (the truss-front-slope-vector)
					     :right (the (face-normal-vector :right))))

   (rear-slope-beam :type 'beam
		     :pass-down (:beam-height :beam-width :wall-thickness :material-density :tonne-rate)
		     :beam-length (the rear-slope-length)
		     :center (the rear-slope-center)
		    :orientation (alignment :rear (the truss-rear-slope-vector)
					    :left (the (face-normal-vector :right))))
   
   )

  :hidden-objects
  ((front-slope-construction-line :type 'line
				  :start (the lower-beam (edge-center :front :top))
				  :end (the vertical-beam (edge-center :rear :top)))
   (rear-slope-construction-line :type 'line
				 :start (the lower-beam (edge-center :rear :top))
				 :end (the vertical-beam (edge-center :rear :bottom)))
   
   (front-mid-pt :type 'sphere
		 :display-controls (list :color :blue)
		 :radius 5
		 :center (the front-slope-construction-line center))
   (rear-mid-pt :type 'sphere
		:display-controls (list :color :blue)
		:center (the rear-slope-construction-line center)
		:radius 5)
   (pt-1 :type 'sphere
	 :radius 5
	 :display-controls (list :color :green)
	 :center (the lower-beam (edge-center :rear :top)))
   (pt-2 :type 'sphere
	 :radius 5
	 :display-controls (list :color :red)
	 :center (the vertical-beam (edge-center :rear :bottom)) )

   

   (vector-line :type 'vector-line
		:start-point (the rear-slope-construction-line center)
		:vector (the rear-slope-beam (face-normal-vector :bottom))
		:length 150)
   
   )))


```

---

## building-hint-3.lisp - vector-line
Source: gornschool-training/t2/resources/source/building-hint-3.lisp
Type: tutorial

```
(define-object vector-line (box)
  :input-slots
  ((start-point (make-point 0 0 0))
   (vector (make-vector 1 0 1))
   (length 50)
   (width 1))
  :computed-slots
  ((height (div (the length) 5)))
  :objects
  ((v-line :type 'line
	   :start (the start-point)
	   :display-controls (list :color :red)
	   :end (translate-along-vector (the start-point)
					(the vector)
					(the length)))
   (arrow :type 'cone
	  :radius-1 0
	  :radius-2 (div (the length) 50)
	  :length (div (the length) 5)
	  :display-controls (list :color :red)
	  :center (translate-along-vector (the v-line end)
					  (the vector)
					  (half (the-child length)))
	  :orientation (alignment :front (the vector)))))


```

---

## building-hint-3.lisp - beam
Source: gornschool-training/t2/resources/source/building-hint-3.lisp
Type: tutorial

```
(define-object beam (box)
  :input-slots
  ((beam-length 1000)
   (beam-width 40)
   (beam-height 50)
   (wall-thickness 2)
   (material-density 7800)
   (tonne-rate 500))

  :computed-slots
  ((length (the beam-length))
   (width (the beam-width))
   (height (the beam-height))

   (beam-volume (- (the outer volume) (the inner volume)))
   (beam-mass (* (div (the beam-volume) 1000000000) (the material-density)))
   (beam-cost (* (the tonne-rate) (div (the beam-mass) 1000)))
   (beam-properties (list :volume-mm3 (the beam-volume)
			  :mass-kg (round-to-nearest (the beam-mass) 0.01)
			  :cost-gbp (round-to-nearest (the beam-cost) 0.01)
			  :length-mm (the beam-length)
			  :width-mm (the beam-width)
			  :height-mm (the beam-height)
			  :thickness-mm (the wall-thickness))))
   
  :objects
  ((outer :type 'box
	  :length (the beam-length)
	  :width (the beam-width)
	  :height (the beam-height))

   (inner :type 'box
	  :length (the beam-length)
	  :width (- (the beam-width) (twice (the wall-thickness)))
	  :height (- (the beam-height) (twice (the wall-thickness)))))
  )














```

---

## wall-hint-6.lisp - header
Source: gornschool-training/t2/resources/source/wall-hint-6.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## wall-hint-6.lisp - wall
Source: gornschool-training/t2/resources/source/wall-hint-6.lisp
Type: tutorial

```
(define-object wall(box)
  :input-slots
  ((brick-height 45)
   (brick-length 180)
   (brick-width 90)
   (mortar-joint-width 10)
   (wall-length 3700)
   (wall-height 900))

  :computed-slots
  ((row-height (+ (the brick-height) (the mortar-joint-width)))
   (number-of-rows (round-to-nearest (div (the wall-height) (the row-height)) 1))
   (actual-wall-height (* (the row-height) (the number-of-rows)))
   
   ;; for the wall-length we need the number of full bricks
   ;; if there are n full bricks then there will be (n-1) mortar joints
   ;; so n*brick-length + n-1*mortar-joint-width = overall-length
   ;; or n(brick-length + mortar-join-width) - mortar-joint-width = overall-length
   ;; or n = (overall-length - mortar-joint-width)/(brick-length + mortar-joint-width)
   (number-of-bricks (round-to-nearest (div (- (the wall-length) (the mortar-joint-width))
					    (+ (the brick-length) (the mortar-joint-width)))
				       1))
   (actual-wall-length (+ (* (the number-of-bricks) (the brick-length))
			  (* (- (the number-of-bricks) 1) (the mortar-joint-width))))

   ;; box inputs - gives the wall bounding box
   (height (the actual-wall-height))
   (width (the brick-width))
   (length (the actual-wall-length)))
   
     
  :objects
  ((row :type 'row
     :sequence (:size (the number-of-rows))
     :center (translate-along-vector (the (face-center :bottom))
				     (the (face-normal-vector :top))
				     (+ (half (the-child height))
					(* (the-child index) (the-child height))))
     :height (+ (the brick-height) (the mortar-joint-width))
     :full-bricks-per-row (the number-of-bricks)
     :pass-down (brick-height
		 brick-length
		 brick-width
		 mortar-joint-width
		 length
		 width))))


```

---

## wall-hint-6.lisp - row
Source: gornschool-training/t2/resources/source/wall-hint-6.lisp
Type: tutorial

```
(define-object row (box)
  :input-slots
  (full-bricks-per-row
   brick-height
   brick-length
   brick-width
   mortar-joint-width)

  :computed-slots
  ((full-brick-row? (or (zerop (the index)) (evenp (the index)))))

  :objects
  ((bricks-and-mortar :type 'bricks-and-mortar
		      :height (the brick-height)
		      :center (translate-along-vector (the mortar-bed (face-center :top))
						      (the mortar-bed (face-normal-vector :top))
						      (half (the-child height)))
		      :pass-down (width
				  length
				  full-brick-row?
				  brick-height
				  brick-length
				  brick-width
				  mortar-joint-width
				  full-bricks-per-row))
   (mortar-bed :type 'box
	       :height (the mortar-joint-width)
	       :center (translate-along-vector (the (face-center :bottom))
					       (the (face-normal-vector :top))
					       (half (the-child height)))
	       :pass-down (width
			   length))))


```

---

## wall-hint-6.lisp - bricks-and-mortar
Source: gornschool-training/t2/resources/source/wall-hint-6.lisp
Type: tutorial

```
(define-object bricks-and-mortar (box)
  :input-slots
  (full-brick-row?
   full-bricks-per-row
   brick-height
   brick-length
   brick-width
   mortar-joint-width)

  :computed-slots
  ((first-full-brick-start-point (if (the full-brick-row?)
				     (the (face-center :front))
				     (the (mortar-joint 0) (face-center :rear))))
   
   (first-mortar-joint-start-point (if (the full-brick-row?)
				       (the (full-brick 0) (face-center :rear))
				       (the (half-brick 0) (face-center :rear))))

   (number-of-full-bricks (if (the full-brick-row?)
			      (the full-bricks-per-row)
			      (- (the full-bricks-per-row) 1)))

   (number-of-mortar-joints (if (the full-brick-row?)
				(- (the number-of-full-bricks) 1)
				(+ (the number-of-full-bricks) 1)))

   ;; if it isn't a full brick row then there will be an extra joint because one
   ;; full brick is replaced with 2 half bricks so without correcting the
   ;; mortar-joint-width the ends of a full brick row and one starting and
   ;; finishing with half bricks won't align. So we need to correct
   ;; the mortar-joint-width
   (corrected-joint-width (if (the full-brick-row?)
				     (the mortar-joint-width)
				     (let ((total-gap (* (- (the number-of-mortar-joints) 1)
									(the mortar-joint-width))))
				       (div total-gap (the number-of-mortar-joints))))))
  
  :functions
  ((first-full-brick-center!
    ()
    (translate-along-vector (the first-full-brick-start-point)
			    (the (face-normal-vector :rear))
			    (half (the brick-length))))
   
   (other-full-brick-center!
    (index)
    ;; if its a full brick row, (full-brick 1) is positioned relative to (joint 0)
    ;; if its a half brick row, (full-brick 1) is positioned relative to (joint 1)
    (let ((ind (if (the full-brick-row?) (- index 1) index)))
	  (translate-along-vector (the (mortar-joint ind) (face-center :rear))
				  (the (face-normal-vector :rear))
				  (half (the brick-length)))))
   
    (first-joint-center!
     ()
     (translate-along-vector (the first-mortar-joint-start-point)
			     (the (face-normal-vector :rear))
			     (half (the corrected-joint-width))))
   (other-joint-center!
    (index)
    ;; if its a full brick row, (joint 1) is positioned relative to (full-brick 1)
    ;; if its a half brick row, (joint 1) is positioned relative to (full-brick 0)
    (let ((ind (if (the full-brick-row?) index (- index 1))))
      (translate-along-vector (the (full-brick ind) (face-center :rear))
			      (the (face-normal-vector :rear))
			      (half (the corrected-joint-width)))))

    (first-half-brick-center!
     ()
     (translate-along-vector (the (face-center :front))
			     (the (face-normal-vector :rear))
			     (half (half (the brick-length)))))
   
   (last-half-brick-center!
     ()
     (translate-along-vector (theo (the mortar-joint last) (face-center :rear))
			     (the (face-normal-vector :rear))
			     (half (half (the brick-length))))))
 			     
  
  :objects
  ((full-brick :type 'box
	       :sequence (:size (the number-of-full-bricks))
	       :center (if (= (the-child index) 0)
			   (the first-full-brick-center!)
			   (the (other-full-brick-center! (the-child index))))
	       :length (the brick-length)
	       :height (the brick-height)
	       :width (the brick-width))

   (half-brick :type 'box
	       :sequence (:size (if (the full-brick-row?) 0 2))
	       :center (if (= (the-child index) 0)
			   (the first-half-brick-center!)
			   (the last-half-brick-center!))
	       :length (half (the brick-length))
	       :height (the brick-height)
	       :width (the brick-width))

   (mortar-joint :type 'box
		 :sequence (:size (the number-of-mortar-joints))
		 :center (if (= (the-child index) 0)
			     (the first-joint-center!)
			  (the (other-joint-center! (the-child index))))   
		 :height (the brick-height)
		 :width (the brick-width)
		 :length (the corrected-joint-width))))

```

---

## truss-hint-8.lisp - header
Source: gornschool-training/t2/resources/source/truss-hint-8.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## truss-hint-8.lisp - degrees-to-radians
Source: gornschool-training/t2/resources/source/truss-hint-8.lisp
Type: tutorial

```
(defun degrees-to-radians (degrees)
  (div (* degrees pi) 180))

```

---

## truss-hint-8.lisp - truss-assembly
Source: gornschool-training/t2/resources/source/truss-hint-8.lisp
Type: tutorial

```
(define-object truss-assembly (box)
  :computed-slots
  ((length 200)
   (height 200)
   (width 200)
   (number-of-roof-trusses 2)
   (truss-angle 30)
   (beam-width 30)
   (beam-height 50)
   (wall-thickness 3)
   (material-density 7800)
   (truss-offsets (list 0 500))
   )
  :objects
  ((roof-truss :type 'truss
	       :sequence (:size (the number-of-roof-trusses))
	       :truss-length 1500
	       :center (translate-along-vector
			(translate-along-vector (the (edge-center :front :top))
						(the (face-normal-vector :top))
						(half (the-child height)))
			(the (face-normal-vector :rear))
			(nth (the-child index) (the truss-offsets)))
	       :orientation (alignment :rear (the (face-normal-vector :right))
				       :top (the (face-normal-vector :top))
				       )		
	       :pass-down (truss-angle
			   beam-width
			   beam-height
			   wall-thickness
			   material-density)))
  )


```

---

## truss-hint-8.lisp - truss
Source: gornschool-training/t2/resources/source/truss-hint-8.lisp
Type: tutorial

```
(define-object truss (box)
  :input-slots
  ((truss-length 2000)
   (truss-height nil)
   (truss-angle 30)

   (beam-width 30)
   (beam-height 50)
   (wall-thickness 3)

   (material-density 7800)
   (tonne-rate 450))

  :computed-slots
  ((length (the truss-length))
   (height (cond ((the truss-height)(the truss-height))
		 ((the truss-angle) (+ (* (half (the truss-length))
					  (tan (degrees-to-radians (the truss-angle))))
				       (the beam-height)))))
   (width (the beam-width))
  
   (truss-front-slope-vector (the (get-slope-vector! :front)))
   (truss-rear-slope-vector (the (get-slope-vector! :rear)))

   (front-slope-length (the (get-slope-length! :front)))
   (rear-slope-length (the (get-slope-length! :rear)))
   
   (front-slope-center (the (get-slope-center! :front)))
   (rear-slope-center (the (get-slope-center! :rear)))
   

   (beam-properties (mapsend (the children) :beam-properties))
   (total-mass (round-to-nearest
		(apply '+
		       (mapcar #'(lambda(a) (getf a :mass-kg))
			       (the beam-properties)))
		0.001))
   (total-cost (round-to-nearest
		(apply '+
		       (mapcar #'(lambda(a) (getf a :cost-gbp))
			       (the beam-properties)))
		0.01))
   )

  :functions
  ((get-slope-vector! (beam-side)
		      (let ((v-key (the (get-v-key! beam-side)))
			    (l-key (the (get-l-key! beam-side))))
		      (subtract-vectors (the vertical-beam (edge-center :rear v-key))
					(the lower-beam (edge-center l-key :top)))))
   (get-slope-length! (beam-side)
		      (let ((v-key (the (get-v-key! beam-side)))
			    (l-key (the (get-l-key! beam-side))))
			(3d-distance (the vertical-beam (edge-center :rear v-key))
				     (the lower-beam (edge-center l-key :top)))))
   (get-slope-center!
    (beam-side)
    (let ((pt (case beam-side
		(:front (the front-slope-construction-line center))
		(:rear  (the rear-slope-construction-line center))))
	  (norm-vector (case beam-side
			 (:front (the front-slope-beam (face-normal-vector :bottom)))
			 (:rear (the rear-slope-beam (face-normal-vector :bottom))))))
      (translate-along-vector pt
			      norm-vector
			      (half (the beam-height)))))
   (get-v-key! (beam-side)
	       (case beam-side
		 (:front :top)
		 (:rear :bottom)))
   (get-l-key! (beam-side)
	       (case beam-side
		 (:front :front)
		 (:rear :rear)))

   
   )
   
				       
  :objects
  ((lower-beam :type 'beam
	       :pass-down (:beam-height :beam-width :wall-thickness :material-density :tonne-rate)
	       :beam-length (the truss-length)
	       :center (translate-along-vector (the (face-center :bottom))
					       (the (face-normal-vector :top))
					       (half (the beam-height))))
   (vertical-beam :type 'beam
		  :pass-down (:beam-height :beam-width :wall-thickness :material-density :tonne-rate)
		  
		  :beam-length (- (the height) (the beam-height))
		  :orientation (alignment :rear (the (face-normal-vector :top))
					  :right (the (face-normal-vector :right)))  
		  :center (translate-along-vector (the lower-beam (face-center :top))
						  (the lower-beam (face-normal-vector :top))
						  (half (the-child beam-length))))
   (front-slope-beam :type 'beam
		     :pass-down (:beam-height :beam-width :wall-thickness :material-density :tonne-rate)
		     :beam-length (the front-slope-length)
		     :center (the front-slope-center)
		     :orientation (alignment :rear (the truss-front-slope-vector)
					     :right (the (face-normal-vector :right))))

   (rear-slope-beam :type 'beam
		     :pass-down (:beam-height :beam-width :wall-thickness :material-density :tonne-rate)
		     :beam-length (the rear-slope-length)
		     :center (the rear-slope-center)
		    :orientation (alignment :rear (the truss-rear-slope-vector)
					    :left (the (face-normal-vector :right))))
   
   )

  :hidden-objects
  ((front-slope-construction-line :type 'line
				  :start (the lower-beam (edge-center :front :top))
				  :end (the vertical-beam (edge-center :rear :top)))
   (rear-slope-construction-line :type 'line
				 :start (the lower-beam (edge-center :rear :top))
				 :end (the vertical-beam (edge-center :rear :bottom)))
   
   (front-mid-pt :type 'sphere
		 :display-controls (list :color :blue)
		 :radius 5
		 :center (the front-slope-construction-line center))
   (rear-mid-pt :type 'sphere
		:display-controls (list :color :blue)
		:center (the rear-slope-construction-line center)
		:radius 5)
   (pt-1 :type 'sphere
	 :radius 5
	 :display-controls (list :color :green)
	 :center (the lower-beam (edge-center :rear :top)))
   (pt-2 :type 'sphere
	 :radius 5
	 :display-controls (list :color :red)
	 :center (the vertical-beam (edge-center :rear :bottom)) )

   

   (vector-line :type 'vector-line
		:start-point (the rear-slope-construction-line center)
		:vector (the rear-slope-beam (face-normal-vector :bottom))
		:length 150)
   
   ))


```

---

## truss-hint-8.lisp - vector-line
Source: gornschool-training/t2/resources/source/truss-hint-8.lisp
Type: tutorial

```
(define-object vector-line (box)
  :input-slots
  ((start-point (make-point 0 0 0))
   (vector (make-vector 1 0 1))
   (length 50)
   (width 1))
  :computed-slots
  ((height (div (the length) 5)))
  :objects
  ((v-line :type 'line
	   :start (the start-point)
	   :display-controls (list :color :red)
	   :end (translate-along-vector (the start-point)
					(the vector)
					(the length)))
   (arrow :type 'cone
	  :radius-1 0
	  :radius-2 (div (the length) 50)
	  :length (div (the length) 5)
	  :display-controls (list :color :red)
	  :center (translate-along-vector (the v-line end)
					  (the vector)
					  (half (the-child length)))
	  :orientation (alignment :front (the vector)))))


```

---

## truss-hint-8.lisp - beam
Source: gornschool-training/t2/resources/source/truss-hint-8.lisp
Type: tutorial

```
(define-object beam (box)
  :input-slots
  ((beam-length 1000)
   (beam-width 40)
   (beam-height 50)
   (wall-thickness 2)
   (material-density 7800)
   (tonne-rate 500))

  :computed-slots
  ((length (the beam-length))
   (width (the beam-width))
   (height (the beam-height))

   (beam-volume (- (the outer volume) (the inner volume)))
   (beam-mass (* (div (the beam-volume) 1000000000) (the material-density)))
   (beam-cost (* (the tonne-rate) (div (the beam-mass) 1000)))
   (beam-properties (list :volume-mm3 (the beam-volume)
			  :mass-kg (round-to-nearest (the beam-mass) 0.01)
			  :cost-gbp (round-to-nearest (the beam-cost) 0.01)
			  :length-mm (the beam-length)
			  :width-mm (the beam-width)
			  :height-mm (the beam-height)
			  :thickness-mm (the wall-thickness))))
   
  :objects
  ((outer :type 'box
	  :length (the beam-length)
	  :width (the beam-width)
	  :height (the beam-height))

   (inner :type 'box
	  :length (the beam-length)
	  :width (- (the beam-width) (twice (the wall-thickness)))
	  :height (- (the beam-height) (twice (the wall-thickness)))))
  )

```

---

## mixins.lisp - header
Source: gornschool-training/t2/resources/source/mixins.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## mixins.lisp - object-1
Source: gornschool-training/t2/resources/source/mixins.lisp
Type: tutorial

```
(define-object object-1 (box)

  :input-slots
  ((length 2)
   (width 3)
   (height 4))
  )


```

---

## mixins.lisp - my-object
Source: gornschool-training/t2/resources/source/mixins.lisp
Type: tutorial

```
(define-object my-object (base-object)
  :computed-slots
  ((volume "not computed"))
  )
   


```

---

## mixins.lisp - object-2
Source: gornschool-training/t2/resources/source/mixins.lisp
Type: tutorial

```
(define-object object-2 (object-1 my-object)
  )


```

---

## mixins.lisp - object-3
Source: gornschool-training/t2/resources/source/mixins.lisp
Type: tutorial

```
(define-object object-3 (my-object object-1)
  )

;;
;; make object-1, volume is the standard box version of volume (* length width height)
;;
;; GDL-USER> (make-self 'object-1)
;; #<OBJECT-1 #x21041A35AD>
;; GDL-USER> (the volume)
;; 24
;;
;; now make object-2, whilst my-object defines volume, because its mixed in after object-1, the
;; object-1 definition for volume, which comes from box, takes precedence
;;
;; GDL-USER> (make-self 'object-2)
;; #<OBJECT-2 #x21041981AD>
;; GDL-USER> (the volume)
;; 24
;;
;; now make object-3. the definition of volume from my-ovject takes precedence over the definition in
;; object-1 which is derived from box
;;
;; GDL-USER> (make-self 'object-3)
;; #<OBJECT-3 #x21042FBB5D>
;; GDL-USER> (the volume)
;; "not computed"
;;
;; to get an idea of whats happening, with object-3 still set as self we can send it the message (the mixins)
;; which returns us the mixins list for object-3
;; GDL-USER> (the mixins)
;; (MY-OBJECT OBJECT-1)
;;
;; not hugely helpful. But if we send it (the all-mixins) message we get a list back of all of the mixins
;; used in object-3 which is derived recursively, placed in order of priority and any duplicates removed
;;
;; GDL-USER> (the all-mixins)
;; (MY-OBJECT BASE-OBJECT VANILLA-MIXIN VANILLA-MIXIN* STANDARD-OBJECT T GENDL::GDL-BASIS OBJECT-1 BOX)
;;
;; so first we get my-object, but it has base-object mixed in. base-object however mixies in vanilla-mixin
;; which in turn mixes in vanilla-mixin* etc. when all of the mixins used in my-object have been listed
;; we then start with object-1, which has box mixed into it, but then nothing after box...
;;
;; so if we make box and send it (the all-mixins) message we can see that it mixes in base-object, which we
;; have already covered from my-object
;;
;; GDL-USER> (make-self 'box)
;; #<BOX #x21044BA30D>
;; GDL-USER> (the all-mixins)
;; (BASE-OBJECT VANILLA-MIXIN VANILLA-MIXIN* STANDARD-OBJECT T GENDL::GDL-BASIS)
;;
;; (the all-mixins) effectively gives us all of the mixins in use for the current object ordered left to
;; right by priority

```

---

## truss-hint-2.lisp - header
Source: gornschool-training/t2/resources/source/truss-hint-2.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## truss-hint-2.lisp - truss
Source: gornschool-training/t2/resources/source/truss-hint-2.lisp
Type: tutorial

```
(define-object truss ()

  :objects
  ((lower-beam :type 'beam)
   (vertical-beam :type 'beam)
   (front-slope-beam :type 'beam)
   (rear-slope-beam :type 'beam)))


```

---

## truss-hint-2.lisp - beam
Source: gornschool-training/t2/resources/source/truss-hint-2.lisp
Type: tutorial

```
(define-object beam (box)
  :input-slots
  ((beam-length 1000)
   (beam-width 40)
   (beam-height 50)
   (wall-thickness 2)
   (material-density 7800)
   (tonne-rate 500))

  :computed-slots
  ((length (the beam-length))
   (width (the beam-width))
   (height (the beam-height))

   (beam-volume (- (the outer volume) (the inner volume)))
   (beam-mass (* (div (the beam-volume) 1000000000) (the material-density)))
   (beam-cost (* (the tonne-rate) (div (the beam-mass) 1000)))
   (beam-properties (list :volume-mm3 (the beam-volume)
			  :mass-kg (round-to-nearest (the beam-mass) 0.01)
			  :cost-gbp (round-to-nearest (the beam-cost) 0.01)
			  :length-mm (the beam-length)
			  :width-mm (the beam-width)
			  :height-mm (the beam-height)
			  :thickness-mm (the wall-thickness))))
   
  :objects
  ((outer :type 'box
	  :length (the beam-length)
	  :width (the beam-width)
	  :height (the beam-height))

   (inner :type 'box
	  :length (the beam-length)
	  :width (- (the beam-width) (twice (the wall-thickness)))
	  :height (- (the beam-height) (twice (the wall-thickness)))))
  )

```

---

## using-conditionals.lisp - header
Source: gornschool-training/t2/resources/source/using-conditionals.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## using-conditionals.lisp - assembly-8
Source: gornschool-training/t2/resources/source/using-conditionals.lisp
Type: tutorial

```
(define-object assembly-8 (base-object)
  :input-slots
  ((box-lengths (list 2 5 8 12)))
  :computed-slots
  ((number-of-boxes (if (> (length (the box-lengths)) 3)
			3
			(length (the box-lengths))))

   (box-centers (case (the number-of-boxes)
		  (1 (list (make-point 0 0 0)))
		  (2 (list (make-point 0 0 0)
			   (make-point 6 0 0)))
		  (3 (list (make-point 0 0 0)
			    (make-point 6 0 0)
			    (make-point 12 0 0)))))
   
   
   (box-volumes (list-elements (the my-box) (the-element volume)))
   (box-1-volume (nth 0 (the box-volumes))))

  :objects
  ((my-box :type 'box
	   :sequence (:size (the number-of-boxes))
	   :length (nth (the-child index) (the box-lengths))
	   :width 2
	   :height 1
	   :center (nth (the-child index) (the box-centers)))))

```

---

## numbers.lisp - header
Source: gornschool-training/t2/resources/source/numbers.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## numbers.lisp - numbers
Source: gornschool-training/t2/resources/source/numbers.lisp
Type: tutorial

```
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

   

```

---

## wall-hint-5.lisp - header
Source: gornschool-training/t2/resources/source/wall-hint-5.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## wall-hint-5.lisp - wall
Source: gornschool-training/t2/resources/source/wall-hint-5.lisp
Type: tutorial

```
(define-object wall(box)
  :input-slots
  ((brick-height 45)
   (brick-length 180)
   (brick-width 90)
   (mortar-joint-width 10)
   (wall-length 3700)
   (wall-height 900))

  :computed-slots
  ((row-height (+ (the brick-height) (the mortar-joint-width)))
   (number-of-rows (round-to-nearest (div (the wall-height) (the row-height)) 1))
   (actual-wall-height (* (the row-height) (the number-of-rows)))
   
   ;; for the wall-length we need the number of full bricks
   ;; if there are n full bricks then there will be (n-1) mortar joints
   ;; so n*brick-length + n-1*mortar-joint-width = overall-length
   ;; or n(brick-length + mortar-join-width) - mortar-joint-width = overall-length
   ;; or n = (overall-length - mortar-joint-width)/(brick-length + mortar-joint-width)
   (number-of-bricks (round-to-nearest (div (- (the wall-length) (the mortar-joint-width))
					    (+ (the brick-length) (the mortar-joint-width)))
				       1))
   (actual-wall-length (+ (* (the number-of-bricks) (the brick-length))
			  (* (- (the number-of-bricks) 1) (the mortar-joint-width))))

   ;; box inputs - gives the wall bounding box
   (height (the actual-wall-height))
   (width (the brick-width))
   (length (the actual-wall-length)))
   
     
  :objects
  ((row :type 'row
     :sequence (:size (the number-of-rows))
     :center (translate-along-vector (the (face-center :bottom))
				     (the (face-normal-vector :top))
				     (+ (half (the-child height))
					(* (the-child index) (the-child height))))
     :length (the length)
     :width (the width)
     :height (+ (the brick-height) (the mortar-joint-width))
     :bricks-per-row (the number-of-bricks)
     :brick-height(the brick-height)
     :brick-length (the brick-length)
     :brick-width (the brick-width)
     :mortar-joint-width (the mortar-joint-width))))


```

---

## wall-hint-5.lisp - row
Source: gornschool-training/t2/resources/source/wall-hint-5.lisp
Type: tutorial

```
(define-object row (box)
  :input-slots
  (brick-height
   brick-length
   brick-width
   mortar-joint-width)

  :objects
  ((bricks-and-mortar :type 'bricks-and-mortar
		      :width (the width)
		      :length (the length)
		      :height (the brick-height)
		      :center (translate-along-vector (the mortar-bed (face-center :top))
						      (the mortar-bed (face-normal-vector :top))
						      (half (the-child height)))
		      :brick-height (the brick-height)
		      :brick-length (the brick-length)
		      :brick-width (the brick-width)
		      :mortar-joint-width (the mortar-joint-width))
   (mortar-bed :type 'box
	       :height (the mortar-joint-width)
	       :width (the width)
	       :length (the length)
	       :center (translate-along-vector (the (face-center :bottom))
					       (the (face-normal-vector :top))
					       (half (the-child height))))
   )
  )


```

---

## wall-hint-5.lisp - bricks-and-mortar
Source: gornschool-training/t2/resources/source/wall-hint-5.lisp
Type: tutorial

```
(define-object bricks-and-mortar ()
  :input-slots
  (brick-height
   brick-length
   brick-width
   mortar-joint-width)
  
  :objects
  ((full-brick :type 'box
	       :length (the brick-length)
	       :height (the brick-height)
	       :width (the brick-width))
   (half-brick :type 'box
	       :length (half (the brick-length))
	       :height (the brick-height)
	       :width (the brick-width))
   (mortar-joint :type 'box
		 :height (the brick-height)
		 :width (the brick-width)
		 :length (the mortar-joint-width))))

```

---

## building-hint-2.lisp - header
Source: gornschool-training/t2/resources/source/building-hint-2.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## building-hint-2.lisp - building
Source: gornschool-training/t2/resources/source/building-hint-2.lisp
Type: tutorial

```
(define-object building (box)
  :input-slots
  ((nominal-height 3000)
   (nominal-width 3000)
   (nominal-length 4000)
   (brick-height 45)
   (brick-length 180)
   (brick-width 90)
   (mortar-joint-width 10)
   (truss-angle 30)
   (beam-width 40)
   (beam-height 50)
   (wall-thickness 3)
   (material-density 7800))

  :computed-slots
  ((length (the left-wall length))
   (width (the rear-wall length))
   (height (+ (the left-wall height) (the (roof-truss 0) height)))
   (number-of-roof-trusses 3)
   (truss-spacing (div (- (the left-wall length) (the beam-width))
		       (- (the number-of-roof-trusses) 1)))
   (truss-offsets (let ((res nil))
		    (dotimes (n (the number-of-roof-trusses) (nreverse res))
		      (push (+ (half (the beam-width))
			       (* n (the truss-spacing))) res))))
   )

   
  
  :objects
  ((left-wall :type 'wall
	      :pass-down (brick-height
			  brick-length
			  brick-width
			  mortar-joint-width)
	      :center (translate-along-vector
		       (translate-along-vector (the (edge-center :bottom :left))
					       (the (face-normal-vector :right))
					       (half (the-child width)))
		       (the (face-normal-vector :top))
		       (half (the-child height)))
	      :wall-length (the nominal-length)
	      :wall-height (the nominal-height))

   (right-wall :type 'wall
	       :pass-down (brick-height
			   brick-length
			   brick-width
			   mortar-joint-width)
	       :center (translate-along-vector
			(translate-along-vector (the (edge-center :bottom :right))
						(the (face-normal-vector :left))
						(half (the-child width)))
			(the (face-normal-vector :top))
			(half (the-child height)))
	       :wall-length (the nominal-length)
	       :wall-height (the nominal-height))

   (rear-wall :type 'wall
	      :pass-down (brick-height
			  brick-length
			  brick-width
			  mortar-joint-width)
	      :center (translate-along-vector
			(translate-along-vector (the (edge-center :bottom :rear))
						(the (face-normal-vector :front))
						(half (the-child width)))
			(the (face-normal-vector :top))
			(half (the-child height)))
	      :orientation (alignment :rear (the (face-normal-vector :right)))
	      :wall-length (the nominal-width)
	      :wall-height (the nominal-height))

   (roof-truss :type 'truss
	       :sequence (:size (the number-of-roof-trusses))
	       :truss-length (the rear-wall length)
	       :center (translate-along-vector
			(translate-along-vector (the (edge-center :front :top))
						(the (face-normal-vector :bottom))
						(half (the-child height)))
			(the (face-normal-vector :rear))
			(nth (the-child index) (the truss-offsets)))
	       :orientation (alignment :rear (the (face-normal-vector :right)))		
	       :pass-down (truss-angle
			   beam-width
			   beam-height
			   wall-thickness
			   material-density))
   
   (roof-cladding-left :type 'box)
   (roof-cladding-right :type 'box)
   )
  )


```

---

## building-hint-2.lisp - full-start-wall
Source: gornschool-training/t2/resources/source/building-hint-2.lisp
Type: tutorial

```
(define-object full-start-wall (wall)
  :input-slots
  ((first-row :start-full)))


```

---

## building-hint-2.lisp - half-start-wall
Source: gornschool-training/t2/resources/source/building-hint-2.lisp
Type: tutorial

```
(define-object half-start-wall (wall)
  :input-slots
  ((first-row :start-half)))


```

---

## building-hint-2.lisp - half-start-wall-front-key
Source: gornschool-training/t2/resources/source/building-hint-2.lisp
Type: tutorial

```
(define-object half-start-wall-front-key (wall)
  :input-slots
  ((first-row :start-half)
   (front-edge :keyed)))


```

---

## building-hint-2.lisp - half-start-wall-rear-key
Source: gornschool-training/t2/resources/source/building-hint-2.lisp
Type: tutorial

```
(define-object half-start-wall-rear-key (wall)
  :input-slots
  ((first-row :start-half)
   (rear-edge :keyed)))


```

---

## building-hint-2.lisp - half-start-wall-both-key
Source: gornschool-training/t2/resources/source/building-hint-2.lisp
Type: tutorial

```
(define-object half-start-wall-both-key (wall)
  :input-slots
  ((first-row :start-half)
   (front-edge :keyed)
   (rear-edge :keyed)))


```

---

## building-hint-2.lisp - wall
Source: gornschool-training/t2/resources/source/building-hint-2.lisp
Type: tutorial

```
(define-object wall(box)
  :input-slots
  ((brick-height 45)
   (brick-length 180)
   (brick-width 90)
   (mortar-joint-width 10)
   (wall-length 3700)
   (wall-height 900)
   (first-row :start-full)
   (front-edge :full)
   (rear-edge :full))

  :computed-slots
  ((row-height (+ (the brick-height) (the mortar-joint-width)))
   (number-of-rows (round-to-nearest (div (the wall-height) (the row-height)) 1))
   (actual-wall-height (* (the row-height) (the number-of-rows)))
   
   ;; for the wall-length we need the number of full bricks
   ;; if there are n full bricks then there will be (n-1) mortar joints
   ;; so n*brick-length + n-1*mortar-joint-width = overall-length
   ;; or n(brick-length + mortar-join-width) - mortar-joint-width = overall-length
   ;; or n = (overall-length - mortar-joint-width)/(brick-length + mortar-joint-width)
   (number-of-bricks (round-to-nearest (div (- (the wall-length) (the mortar-joint-width))
					    (+ (the brick-length) (the mortar-joint-width)))
				       1))
   (actual-wall-length (+ (* (the number-of-bricks) (the brick-length))
			  (* (- (the number-of-bricks) 1) (the mortar-joint-width))))

   ;; box inputs - gives the wall bounding box
   (height (the actual-wall-height))
   (width (the brick-width))
   (length (the actual-wall-length))

   (full-bricks (apply '+ (list-elements (the row) (the-element full-bricks))))
   (half-bricks (apply '+ (list-elements (the row) (the-element half-bricks))))
   (mortar-volume (apply '+ (list-elements (the row) (the-element mortar-volume))))
   (mortar-density 2162)
   (mortar-mass (* (the mortar-density) (div (the mortar-volume) 1000000000)))
   )
   
     
  :objects
  ((row :type 'row
     :sequence (:size (the number-of-rows))
     :center (translate-along-vector (the (face-center :bottom))
				     (the (face-normal-vector :top))
				     (+ (half (the-child height))
					(* (the-child index) (the-child height))))
     :height (+ (the brick-height) (the mortar-joint-width))
     :full-bricks-per-row (the number-of-bricks)
     :pass-down (brick-height
		 brick-length
		 brick-width
		 mortar-joint-width
		 length
		 width
		 first-row
		 front-edge
		 rear-edge))))


```

---

## building-hint-2.lisp - row
Source: gornschool-training/t2/resources/source/building-hint-2.lisp
Type: tutorial

```
(define-object row (box)
  :input-slots
  (full-bricks-per-row
   brick-height
   brick-length
   brick-width
   mortar-joint-width
   first-row
   front-edge
   rear-edge )

  :computed-slots
  ((full-brick-row? (cond ((eq (the first-row) :start-full)
			   (or (zerop (the index)) (evenp (the index))))
			  ((eq (the first-row) :start-half)
			   (not (or (zerop (the index)) (evenp (the index)))))))
		    
   (full-bricks (the bricks-and-mortar full-bricks))
   (half-bricks (the bricks-and-mortar half-bricks))
   (mortar-volume (+ (the bricks-and-mortar mortar-joint-volume)
		     (the mortar-bed volume))))

  :objects
  ((bricks-and-mortar :type 'bricks-and-mortar
		      :height (the brick-height)
		      :center (translate-along-vector (the mortar-bed (face-center :top))
						      (the mortar-bed (face-normal-vector :top))
						      (half (the-child height)))
		      :pass-down (width
				  length
				  full-brick-row?
				  brick-height
				  brick-length
				  brick-width
				  mortar-joint-width
				  full-bricks-per-row
				  front-edge
				  rear-edge))
   (mortar-bed :type 'box
	       :height (the mortar-joint-width)
	       :center (translate-along-vector (the (face-center :bottom))
					       (the (face-normal-vector :top))
					       (half (the-child height)))
	       :pass-down (width
			   length))))


```

---

## building-hint-2.lisp - bricks-and-mortar
Source: gornschool-training/t2/resources/source/building-hint-2.lisp
Type: tutorial

```
(define-object bricks-and-mortar (box)
  :input-slots
  (full-brick-row?
   full-bricks-per-row
   brick-height
   brick-length
   brick-width
   mortar-joint-width
   front-edge
   rear-edge)

  :computed-slots
  ((first-full-brick-start-point (if (the full-brick-row?)
				     (the (face-center :front))
				     (the (mortar-joint 0) (face-center :rear))))
   
   (first-mortar-joint-start-point (cond ((the full-brick-row?) (the (full-brick 0) (face-center :rear)))
					 ((eq (the front-edge) :full) (the (half-brick 0) (face-center :rear)))
					 ((eq (the front-edge) :keyed) (translate-along-vector (the (face-center :front))
											       (the (face-normal-vector :rear))
											       (half (the brick-length))))))

   (number-of-full-bricks (if (the full-brick-row?)
			      (the full-bricks-per-row)
			      (- (the full-bricks-per-row) 1)))

   (number-of-half-bricks (cond ((the full-brick-row?) 0)
				((and (eq (the front-edge) :full)(eq (the rear-edge) :full)) 2)
				((and (eq (the front-edge) :keyed) (eq (the rear-edge) :full)) 1)
				((and (eq (the front-edge) :full) (eq (the rear-edge) :keyed)) 1)
				((and (eq (the front-edge) :keyed) (eq (the rear-edge) :keyed)) 0)))

   ;; whether or not the ends are :full or :keyed, the number of mortar joints remains the same since the mortar joint
   ;; when it is :keyed is used to connect to the full brick of the other wall
   (number-of-mortar-joints (if (the full-brick-row?)
				(- (the number-of-full-bricks) 1)
				(+ (the number-of-full-bricks) 1)))

   ;; if it isn't a full brick row then there will be an extra joint because one
   ;; full brick is replaced with 2 half bricks so without correcting the
   ;; mortar-joint-width the ends of a full brick row and one starting and
   ;; finishing with half bricks won't align. So we need to correct
   ;; the mortar-joint-width
   (corrected-joint-width (if (the full-brick-row?)
				     (the mortar-joint-width)
				     (let ((total-gap (* (- (the number-of-mortar-joints) 1)
									(the mortar-joint-width))))
				       (div total-gap (the number-of-mortar-joints)))))
   ;; collating the output. We could do this analytically, but for this example we'll use the geometry
   (full-bricks (length (list-elements (the full-brick))))
   (half-bricks (length (list-elements (the half-brick))))
   (mortar-joint-volume (apply '+ (list-elements (the mortar-joint)
						 (the-element volume))))
   )
  
  :functions
  ((first-full-brick-center!
    ()
    (translate-along-vector (the first-full-brick-start-point)
			    (the (face-normal-vector :rear))
			    (half (the brick-length))))
   
   (other-full-brick-center!
    (index)
    ;; if its a full brick row, (full-brick 1) is positioned relative to (joint 0)
    ;; if its a half brick row, (full-brick 1) is positioned relative to (joint 1)
    (let ((ind (if (the full-brick-row?) (- index 1) index)))
	  (translate-along-vector (the (mortar-joint ind) (face-center :rear))
				  (the (face-normal-vector :rear))
				  (half (the brick-length)))))
   
    (first-joint-center!
     ()
     (translate-along-vector (the first-mortar-joint-start-point)
			     (the (face-normal-vector :rear))
			     (half (the corrected-joint-width))))
   (other-joint-center!
    (index)
    ;; if its a full brick row, (joint 1) is positioned relative to (full-brick 1)
    ;; if its a half brick row, (joint 1) is positioned relative to (full-brick 0)
    (let ((ind (if (the full-brick-row?) index (- index 1))))
      (translate-along-vector (the (full-brick ind) (face-center :rear))
			      (the (face-normal-vector :rear))
			      (half (the corrected-joint-width)))))

    (first-half-brick-center!
     ()
     (translate-along-vector (the (face-center :front))
			     (the (face-normal-vector :rear))
			     (half (half (the brick-length)))))
   
   (last-half-brick-center!
     ()
     (translate-along-vector (theo (the mortar-joint last) (face-center :rear))
			     (the (face-normal-vector :rear))
			     (half (half (the brick-length))))))
 			     
  
  :objects
  ((full-brick :type 'box
	       :sequence (:size (the number-of-full-bricks))
	       :center (if (= (the-child index) 0)
			   (the first-full-brick-center!)
			   (the (other-full-brick-center! (the-child index))))
	       :length (the brick-length)
	       :height (the brick-height)
	       :width (the brick-width))

   (half-brick :type 'box
	       :sequence (:size (the number-of-half-bricks))
	       :center (cond ((and (= (the-child index) 0)
				   (eq (the front-edge) :full)) (the first-half-brick-center!))
			     ((and (= (the-child index) 0)
				   (eq (the front-edge) :keyed)
				   (eq (the rear-edge) :full)) (the last-half-brick-center!))
			     ((eq (the rear-edge) :full) (the last-half-brick-center!)))
	       :length (half (the brick-length))
	       :height (the brick-height)
	       :width (the brick-width))

   (mortar-joint :type 'box
		 :sequence (:size (the number-of-mortar-joints))
		 :center (if (= (the-child index) 0)
			     (the first-joint-center!)
			  (the (other-joint-center! (the-child index))))   
		 :height (the brick-height)
		 :width (the brick-width)
		 :length (the corrected-joint-width))))









```

---

## building-hint-2.lisp - truss
Source: gornschool-training/t2/resources/source/building-hint-2.lisp
Type: tutorial

```
(define-object truss (box)
  :input-slots
  ((truss-length 2000)
   (truss-height nil)
   (truss-angle 30)

   (beam-width 30)
   (beam-height 50)
   (wall-thickness 3)

   (material-density 7800)
   (tonne-rate 450))

  :computed-slots
  ((length (the truss-length))
   (height (cond ((the truss-height)(the truss-height))
		 ((the truss-angle) (+ (* (half (the truss-length))
					  (tan (degrees-to-radians (the truss-angle))))
				       (the beam-height)))))
   (width (the beam-width))
  
   (truss-front-slope-vector (the (get-slope-vector! :front)))
   (truss-rear-slope-vector (the (get-slope-vector! :rear)))

   (front-slope-length (the (get-slope-length! :front)))
   (rear-slope-length (the (get-slope-length! :rear)))
   
   (front-slope-center (the (get-slope-center! :front)))
   (rear-slope-center (the (get-slope-center! :rear)))
   

   (beam-properties (mapsend (the children) :beam-properties))
   (total-mass (round-to-nearest
		(apply '+
		       (mapcar #'(lambda(a) (getf a :mass-kg))
			       (the beam-properties)))
		0.001))
   (total-cost (round-to-nearest
		(apply '+
		       (mapcar #'(lambda(a) (getf a :cost-gbp))
			       (the beam-properties)))
		0.01))
   )

  :functions
  ((get-slope-vector! (beam-side)
		      (let ((v-key (the (get-v-key! beam-side)))
			    (l-key (the (get-l-key! beam-side))))
		      (subtract-vectors (the vertical-beam (edge-center :rear v-key))
					(the lower-beam (edge-center l-key :top)))))
   (get-slope-length! (beam-side)
		      (let ((v-key (the (get-v-key! beam-side)))
			    (l-key (the (get-l-key! beam-side))))
			(3d-distance (the vertical-beam (edge-center :rear v-key))
				     (the lower-beam (edge-center l-key :top)))))
   (get-slope-center!
    (beam-side)
    (let ((pt (case beam-side
		(:front (the front-slope-construction-line center))
		(:rear  (the rear-slope-construction-line center))))
	  (norm-vector (case beam-side
			 (:front (the front-slope-beam (face-normal-vector :bottom)))
			 (:rear (the rear-slope-beam (face-normal-vector :bottom))))))
      (translate-along-vector pt
			      norm-vector
			      (half (the beam-height)))))
   (get-v-key! (beam-side)
	       (case beam-side
		 (:front :top)
		 (:rear :bottom)))
   (get-l-key! (beam-side)
	       (case beam-side
		 (:front :front)
		 (:rear :rear)))

   
   )
   
				       
  :objects
  ((lower-beam :type 'beam
	       :pass-down (:beam-height :beam-width :wall-thickness :material-density :tonne-rate)
	       :beam-length (the truss-length)
	       :center (translate-along-vector (the (face-center :bottom))
					       (the (face-normal-vector :top))
					       (half (the beam-height))))
   (vertical-beam :type 'beam
		  :pass-down (:beam-height :beam-width :wall-thickness :material-density :tonne-rate)
		  
		  :beam-length (- (the height) (the beam-height))
		  :orientation (alignment :rear (the (face-normal-vector :top))
					  :right (the (face-normal-vector :right)))  
		  :center (translate-along-vector (the lower-beam (face-center :top))
						  (the lower-beam (face-normal-vector :top))
						  (half (the-child beam-length))))
   (front-slope-beam :type 'beam
		     :pass-down (:beam-height :beam-width :wall-thickness :material-density :tonne-rate)
		     :beam-length (the front-slope-length)
		     :center (the front-slope-center)
		     :orientation (alignment :rear (the truss-front-slope-vector)
					     :right (the (face-normal-vector :right))))

   (rear-slope-beam :type 'beam
		     :pass-down (:beam-height :beam-width :wall-thickness :material-density :tonne-rate)
		     :beam-length (the rear-slope-length)
		     :center (the rear-slope-center)
		    :orientation (alignment :rear (the truss-rear-slope-vector)
					    :left (the (face-normal-vector :right))))
   
   )

  :hidden-objects
  ((front-slope-construction-line :type 'line
				  :start (the lower-beam (edge-center :front :top))
				  :end (the vertical-beam (edge-center :rear :top)))
   (rear-slope-construction-line :type 'line
				 :start (the lower-beam (edge-center :rear :top))
				 :end (the vertical-beam (edge-center :rear :bottom)))
   
   (front-mid-pt :type 'sphere
		 :display-controls (list :color :blue)
		 :radius 5
		 :center (the front-slope-construction-line center))
   (rear-mid-pt :type 'sphere
		:display-controls (list :color :blue)
		:center (the rear-slope-construction-line center)
		:radius 5)
   (pt-1 :type 'sphere
	 :radius 5
	 :display-controls (list :color :green)
	 :center (the lower-beam (edge-center :rear :top)))
   (pt-2 :type 'sphere
	 :radius 5
	 :display-controls (list :color :red)
	 :center (the vertical-beam (edge-center :rear :bottom)) )

   

   (vector-line :type 'vector-line
		:start-point (the rear-slope-construction-line center)
		:vector (the rear-slope-beam (face-normal-vector :bottom))
		:length 150)
   
   )))


```

---

## building-hint-2.lisp - vector-line
Source: gornschool-training/t2/resources/source/building-hint-2.lisp
Type: tutorial

```
(define-object vector-line (box)
  :input-slots
  ((start-point (make-point 0 0 0))
   (vector (make-vector 1 0 1))
   (length 50)
   (width 1))
  :computed-slots
  ((height (div (the length) 5)))
  :objects
  ((v-line :type 'line
	   :start (the start-point)
	   :display-controls (list :color :red)
	   :end (translate-along-vector (the start-point)
					(the vector)
					(the length)))
   (arrow :type 'cone
	  :radius-1 0
	  :radius-2 (div (the length) 50)
	  :length (div (the length) 5)
	  :display-controls (list :color :red)
	  :center (translate-along-vector (the v-line end)
					  (the vector)
					  (half (the-child length)))
	  :orientation (alignment :front (the vector)))))


```

---

## building-hint-2.lisp - beam
Source: gornschool-training/t2/resources/source/building-hint-2.lisp
Type: tutorial

```
(define-object beam (box)
  :input-slots
  ((beam-length 1000)
   (beam-width 40)
   (beam-height 50)
   (wall-thickness 2)
   (material-density 7800)
   (tonne-rate 500))

  :computed-slots
  ((length (the beam-length))
   (width (the beam-width))
   (height (the beam-height))

   (beam-volume (- (the outer volume) (the inner volume)))
   (beam-mass (* (div (the beam-volume) 1000000000) (the material-density)))
   (beam-cost (* (the tonne-rate) (div (the beam-mass) 1000)))
   (beam-properties (list :volume-mm3 (the beam-volume)
			  :mass-kg (round-to-nearest (the beam-mass) 0.01)
			  :cost-gbp (round-to-nearest (the beam-cost) 0.01)
			  :length-mm (the beam-length)
			  :width-mm (the beam-width)
			  :height-mm (the beam-height)
			  :thickness-mm (the wall-thickness))))
   
  :objects
  ((outer :type 'box
	  :length (the beam-length)
	  :width (the beam-width)
	  :height (the beam-height))

   (inner :type 'box
	  :length (the beam-length)
	  :width (- (the beam-width) (twice (the wall-thickness)))
	  :height (- (the beam-height) (twice (the wall-thickness)))))
  )








```

---

## define-object.lisp - header
Source: gornschool-training/t2/resources/source/define-object.lisp
Type: tutorial

```
(in-package :gdl-user)

```

---

## define-object.lisp - my-box-1a
Source: gornschool-training/t2/resources/source/define-object.lisp
Type: tutorial

```
(define-object my-box-1a (box)
  :input-slots
  ((length 2)
   (width 3)
   (height 4)))


```

---

## define-object.lisp - my-box-1b
Source: gornschool-training/t2/resources/source/define-object.lisp
Type: tutorial

```
(define-object my-box-1b ()
  :input-slots
  ((length 2)
   (width 3)
   (height 4)))




```

---

## define-object.lisp - my-box-2
Source: gornschool-training/t2/resources/source/define-object.lisp
Type: tutorial

```
(define-object my-box-2 (my-box-1b box))


```

---

## define-object.lisp - my-box-3
Source: gornschool-training/t2/resources/source/define-object.lisp
Type: tutorial

```
(define-object my-box-3 (box my-box-1b))


```

---

## define-object.lisp - my-box-4
Source: gornschool-training/t2/resources/source/define-object.lisp
Type: tutorial

```
(define-object my-box-4 (box)
  :input-slots
  (length
   (width 4)
   (height 4))

  :computed-slots
  ((density 7800)
   (mass (* (div (the volume) 1000000000) (the density)))))



```

---

## define-object.lisp - assembly-1
Source: gornschool-training/t2/resources/source/define-object.lisp
Type: tutorial

```
(define-object assembly-1 (base-object)
  :objects
  ((my-box :type 'my-box-4
	   :length 10)
   
   (my-sphere :type 'sphere
	      :radius (the my-box width))))

```

---

## truss-hint-6.lisp - header
Source: gornschool-training/t2/resources/source/truss-hint-6.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## truss-hint-6.lisp - degrees-to-radians
Source: gornschool-training/t2/resources/source/truss-hint-6.lisp
Type: tutorial

```
(defun degrees-to-radians (degrees)
  (div (* degrees pi) 180))


```

---

## truss-hint-6.lisp - truss
Source: gornschool-training/t2/resources/source/truss-hint-6.lisp
Type: tutorial

```
(define-object truss (box)
  :input-slots
  ((truss-length 2000)
   (truss-height 800)
   (truss-angle nil)

   (beam-width 50)
   (beam-height 50)
   (wall-thickness 3))

  :computed-slots
  ((length (the truss-length))
   (height (cond ((the truss-height)(the truss-height))
		 ((the truss-angle) (+ (* (half (the truss-length))
					  (tan (degrees-to-radians (the truss-angle))))
				       (the beam-height)))))
   (width (the beam-width))
   (truss-front-slope-vector (subtract-vectors (the vertical-beam (edge-center :rear :top))
					       (the lower-beam (edge-center :front :top))))
   (front-slope-length (3d-distance (the vertical-beam (edge-center :rear :top))
				    (the lower-beam (edge-center :front :top))))
   (front-slope-center (translate-along-vector (the front-slope-construction-line center)
					       (the front-slope-beam (face-normal-vector :bottom))
					       (half (the beam-height))))
   )
				       
  :objects
  ((lower-beam :type 'beam
	       :beam-height (the beam-height)
	       :beam-width (the beam-width)
	       :beam-length (the truss-length)
	       :center (translate-along-vector (the (face-center :bottom))
					       (the (face-normal-vector :top))
					       (half (the beam-height))))
   (vertical-beam :type 'beam
		  :beam-length (- (the height) (the beam-height))
		  :beam-height (the beam-height)
		  :beam-width (the beam-width)
		  :orientation (alignment :rear (the (face-normal-vector :top))
					  :right (the (face-normal-vector :right)))
		  :center (translate-along-vector (the lower-beam (face-center :top))
						  (the lower-beam (face-normal-vector :top))
						  (half (the-child beam-length))))
   (front-slope-beam :type 'beam
		     :beam-length (the front-slope-length)
		     :beam-height (the beam-height)
		     :beam-width (the beam-width)
		     :center (the front-slope-center)
		     :orientation (alignment :rear (the truss-front-slope-vector)
					     :right (the (face-normal-vector :right))))

   (front-slope-construction-line :type 'line
				  :start (the lower-beam (edge-center :front :top))
				  :end (the vertical-beam (edge-center :rear :top)))
   (mid-pt :type 'sphere
	   :display-controls (list :color :blue)
	   :radius 5
	   :center (the front-slope-construction-line center))
   
   ;;(right-slope-beam :type 'beam))

   (pt-1 :type 'sphere
	 :radius 5
	 :display-controls (list :color :green)
	 :center (the lower-beam (edge-center :front :top)))
   (pt-2 :type 'sphere
	 :radius 5
	 :display-controls (list :color :red)
	 :center (the vertical-beam (edge-center :rear :top)) )
   (vector-line :type 'vector-line
		:start-point (the pt-1 center)
		:vector (the truss-front-slope-vector)
		:length 150)))


```

---

## truss-hint-6.lisp - vector-line
Source: gornschool-training/t2/resources/source/truss-hint-6.lisp
Type: tutorial

```
(define-object vector-line (box)
  :input-slots
  ((start-point (make-point 0 0 0))
   (vector (make-vector 1 0 1))
   (length 50)
   (width 1))
  :computed-slots
  ((height (div (the length) 5)))
  :objects
  ((v-line :type 'line
	   :start (the start-point)
	   :display-controls (list :color :red)
	   :end (translate-along-vector (the start-point)
					(the vector)
					(the length)))
   (arrow :type 'cone
	  :radius-1 0
	  :radius-2 (div (the length) 50)
	  :length (div (the length) 5)
	  :display-controls (list :color :red)
	  :center (translate-along-vector (the v-line end)
					  (the vector)
					  (half (the-child length)))
	  :orientation (alignment :front (the vector)))))


```

---

## truss-hint-6.lisp - beam
Source: gornschool-training/t2/resources/source/truss-hint-6.lisp
Type: tutorial

```
(define-object beam (box)
  :input-slots
  ((beam-length 1000)
   (beam-width 40)
   (beam-height 50)
   (wall-thickness 2)
   (material-density 7800)
   (tonne-rate 500))

  :computed-slots
  ((length (the beam-length))
   (width (the beam-width))
   (height (the beam-height))

   (beam-volume (- (the outer volume) (the inner volume)))
   (beam-mass (* (div (the beam-volume) 1000000000) (the material-density)))
   (beam-cost (* (the tonne-rate) (div (the beam-mass) 1000)))
   (beam-properties (list :volume-mm3 (the beam-volume)
			  :mass-kg (round-to-nearest (the beam-mass) 0.01)
			  :cost-gbp (round-to-nearest (the beam-cost) 0.01)
			  :length-mm (the beam-length)
			  :width-mm (the beam-width)
			  :height-mm (the beam-height)
			  :thickness-mm (the wall-thickness))))
   
  :objects
  ((outer :type 'box
	  :length (the beam-length)
	  :width (the beam-width)
	  :height (the beam-height))

   (inner :type 'box
	  :length (the beam-length)
	  :width (- (the beam-width) (twice (the wall-thickness)))
	  :height (- (the beam-height) (twice (the wall-thickness)))))
  )

```

---

## truss-hint-3.lisp - header
Source: gornschool-training/t2/resources/source/truss-hint-3.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## truss-hint-3.lisp - degrees-to-radians
Source: gornschool-training/t2/resources/source/truss-hint-3.lisp
Type: tutorial

```
(defun degrees-to-radians (degrees)
  (div (* degrees pi) 180))


```

---

## truss-hint-3.lisp - truss
Source: gornschool-training/t2/resources/source/truss-hint-3.lisp
Type: tutorial

```
(define-object truss (box)
  :input-slots
  ((truss-length 2000)
   (truss-height 800)
   (truss-angle nil)

   (beam-width 50)
   (beam-height 50)
   (wall-thickness 3))

  :computed-slots
  ((length (the truss-length))
   (height (cond ((the truss-height)(the truss-height))
		 ((the truss-angle) (+ (* (half (the truss-length))
					  (tan (degrees-to-radians (the truss-angle))))
				       (the beam-height)))))
   (width (the beam-width)))
				       
  :objects
   ((lower-beam :type 'beam
		:beam-height (the beam-height)
		:beam-width (the beam-width)
		:beam-length (the truss-length)
		:center (translate-along-vector (the (face-center :bottom))
						(the (face-normal-vector :top))
						(half (the beam-height))))
    ;;(vertical-beam :type 'beam)
    ;;(front-slope-beam :type 'beam)
    ;;(rear-slope-beam :type 'beam))
    ))


```

---

## truss-hint-3.lisp - beam
Source: gornschool-training/t2/resources/source/truss-hint-3.lisp
Type: tutorial

```
(define-object beam (box)
  :input-slots
  ((beam-length 1000)
   (beam-width 40)
   (beam-height 50)
   (wall-thickness 2)
   (material-density 7800)
   (tonne-rate 500))

  :computed-slots
  ((length (the beam-length))
   (width (the beam-width))
   (height (the beam-height))

   (beam-volume (- (the outer volume) (the inner volume)))
   (beam-mass (* (div (the beam-volume) 1000000000) (the material-density)))
   (beam-cost (* (the tonne-rate) (div (the beam-mass) 1000)))
   (beam-properties (list :volume-mm3 (the beam-volume)
			  :mass-kg (round-to-nearest (the beam-mass) 0.01)
			  :cost-gbp (round-to-nearest (the beam-cost) 0.01)
			  :length-mm (the beam-length)
			  :width-mm (the beam-width)
			  :height-mm (the beam-height)
			  :thickness-mm (the wall-thickness))))
   
  :objects
  ((outer :type 'box
	  :length (the beam-length)
	  :width (the beam-width)
	  :height (the beam-height))

   (inner :type 'box
	  :length (the beam-length)
	  :width (- (the beam-width) (twice (the wall-thickness)))
	  :height (- (the beam-height) (twice (the wall-thickness)))))
  )

```

---

## building-hint-4.lisp - header
Source: gornschool-training/t2/resources/source/building-hint-4.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## building-hint-4.lisp - building
Source: gornschool-training/t2/resources/source/building-hint-4.lisp
Type: tutorial

```
(define-object building (box)
  :input-slots
  ((nominal-height 3000)
   (nominal-width 3000)
   (nominal-length 4000)
   (brick-height 45)
   (brick-length 180)
   (brick-width 90)
   (mortar-joint-width 10)
   (truss-angle 30)
   (beam-width 40)
   (beam-height 50)
   (wall-thickness 3)
   (material-density 7800)
   (roof-overhang 50)
   (cladding-thickness 10)
   (max-beam-spacing 1500))

  :computed-slots
  ((length (the left-wall length))
   (width (the rear-wall length))
   (height (+ (the left-wall height) (the (roof-truss 0) height)))

   (number-of-roof-trusses (let ((trusses (ceiling (the left-wall length) 1500)))
			     (max trusses 2)))

   (truss-spacing (div (- (the left-wall length) (the beam-width))
		       (- (the number-of-roof-trusses) 1)))
   (truss-offsets (let ((res nil))
		    (dotimes (n (the number-of-roof-trusses) (nreverse res))
		      (push (+ (half (the beam-width))
			       (* n (the truss-spacing))) res))))

   (roof-length (+ (the left-wall length) (twice (the roof-overhang))))
   (roof-width (the cladding-thickness))
   (roof-height (let ((apex (the (roof-truss 0) apex-point))
		      (gutter (the (roof-truss 0) front-gutter-point)))
		 (+ (3d-distance apex gutter) (the roof-overhang))))
   )

  :functions
  ((get-roof-mid-point! (first-gutter last-gutter last-index)
		       (let*((mid-gutter (midpoint first-gutter last-gutter))
			     (first-apex (the (roof-truss 0) apex-point))
			     (last-apex (the (roof-truss last-index) apex-point))
			     (mid-apex (midpoint first-apex last-apex))
			     (vec (subtract-vectors mid-gutter mid-apex))
			     (mid-edge (translate-along-vector mid-gutter vec (the roof-overhang))))
			 (midpoint mid-apex mid-edge))) )
  
  :objects
  ((left-wall :type 'wall
	      :pass-down (brick-height
			  brick-length
			  brick-width
			  mortar-joint-width)
	      :center (translate-along-vector
		       (translate-along-vector (the (edge-center :bottom :left))
					       (the (face-normal-vector :right))
					       (half (the-child width)))
		       (the (face-normal-vector :top))
		       (half (the-child height)))
	      :wall-length (the nominal-length)
	      :wall-height (the nominal-height))

   (right-wall :type 'wall
	       :pass-down (brick-height
			   brick-length
			   brick-width
			   mortar-joint-width)
	       :center (translate-along-vector
			(translate-along-vector (the (edge-center :bottom :right))
						(the (face-normal-vector :left))
						(half (the-child width)))
			(the (face-normal-vector :top))
			(half (the-child height)))
	       :wall-length (the nominal-length)
	       :wall-height (the nominal-height))

   (rear-wall :type 'wall
	      :pass-down (brick-height
			  brick-length
			  brick-width
			  mortar-joint-width)
	      :center (translate-along-vector
			(translate-along-vector (the (edge-center :bottom :rear))
						(the (face-normal-vector :front))
						(half (the-child width)))
			(the (face-normal-vector :top))
			(half (the-child height)))
	      :orientation (alignment :rear (the (face-normal-vector :right)))
	      :wall-length (the nominal-width)
	      :wall-height (the nominal-height))

   (roof-truss :type 'truss
	       :sequence (:size (the number-of-roof-trusses))
	       :truss-length (the rear-wall length)
	       :center (translate-along-vector
			(translate-along-vector (the (edge-center :front :top))
						(the (face-normal-vector :bottom))
						(half (the-child height)))
			(the (face-normal-vector :rear))
			(nth (the-child index) (the truss-offsets)))
	       :orientation (alignment :rear (the (face-normal-vector :right))
				       )		
	       :pass-down (truss-angle
			   beam-width
			   beam-height
			   wall-thickness
			   material-density))
   
   (roof-cladding-left
    :type 'box
    :length (the roof-length)
    :height (the roof-height)
    :width (the cladding-thickness)
    :orientation (alignment :left (the (roof-truss 0) front-slope-normal))
    :center (let* ((last-index (- (the number-of-roof-trusses) 1))
		   (first-gutter (the (roof-truss 0) front-gutter-point))
		   (last-gutter (the (roof-truss last-index) front-gutter-point))
		   (mid-ctr (the (get-roof-mid-point! first-gutter last-gutter last-index))))
	      (translate-along-vector mid-ctr
				      (the (roof-truss 0) front-slope-normal)
				      (half (the cladding-thickness)))))
   
   (roof-cladding-right :type 'box
			:length (the roof-length)
			:height (the roof-height)
			:width (the cladding-thickness)
			:orientation (alignment :left (the (roof-truss 0) rear-slope-normal))
			 :center (let* ((last-index (- (the number-of-roof-trusses) 1))
					(first-gutter (the (roof-truss 0) rear-gutter-point))
					(last-gutter (the (roof-truss last-index) rear-gutter-point))
					(mid-ctr (the (get-roof-mid-point! first-gutter last-gutter last-index))))
				 (translate-along-vector mid-ctr
							 (the (roof-truss 0) rear-slope-normal)
							 (half (the cladding-thickness)))))
   )

  
		       
  )


```

---

## building-hint-4.lisp - full-start-wall
Source: gornschool-training/t2/resources/source/building-hint-4.lisp
Type: tutorial

```
(define-object full-start-wall (wall)
  :input-slots
  ((first-row :start-full)))


```

---

## building-hint-4.lisp - half-start-wall
Source: gornschool-training/t2/resources/source/building-hint-4.lisp
Type: tutorial

```
(define-object half-start-wall (wall)
  :input-slots
  ((first-row :start-half)))


```

---

## building-hint-4.lisp - half-start-wall-front-key
Source: gornschool-training/t2/resources/source/building-hint-4.lisp
Type: tutorial

```
(define-object half-start-wall-front-key (wall)
  :input-slots
  ((first-row :start-half)
   (front-edge :keyed)))


```

---

## building-hint-4.lisp - half-start-wall-rear-key
Source: gornschool-training/t2/resources/source/building-hint-4.lisp
Type: tutorial

```
(define-object half-start-wall-rear-key (wall)
  :input-slots
  ((first-row :start-half)
   (rear-edge :keyed)))


```

---

## building-hint-4.lisp - half-start-wall-both-key
Source: gornschool-training/t2/resources/source/building-hint-4.lisp
Type: tutorial

```
(define-object half-start-wall-both-key (wall)
  :input-slots
  ((first-row :start-half)
   (front-edge :keyed)
   (rear-edge :keyed)))


```

---

## building-hint-4.lisp - wall
Source: gornschool-training/t2/resources/source/building-hint-4.lisp
Type: tutorial

```
(define-object wall(box)
  :input-slots
  ((brick-height 45)
   (brick-length 180)
   (brick-width 90)
   (mortar-joint-width 10)
   (wall-length 3700)
   (wall-height 900)
   (first-row :start-full)
   (front-edge :full)
   (rear-edge :full))

  :computed-slots
  ((row-height (+ (the brick-height) (the mortar-joint-width)))
   (number-of-rows (round-to-nearest (div (the wall-height) (the row-height)) 1))
   (actual-wall-height (* (the row-height) (the number-of-rows)))
   
   ;; for the wall-length we need the number of full bricks
   ;; if there are n full bricks then there will be (n-1) mortar joints
   ;; so n*brick-length + n-1*mortar-joint-width = overall-length
   ;; or n(brick-length + mortar-join-width) - mortar-joint-width = overall-length
   ;; or n = (overall-length - mortar-joint-width)/(brick-length + mortar-joint-width)
   (number-of-bricks (round-to-nearest (div (- (the wall-length) (the mortar-joint-width))
					    (+ (the brick-length) (the mortar-joint-width)))
				       1))
   (actual-wall-length (+ (* (the number-of-bricks) (the brick-length))
			  (* (- (the number-of-bricks) 1) (the mortar-joint-width))))

   ;; box inputs - gives the wall bounding box
   (height (the actual-wall-height))
   (width (the brick-width))
   (length (the actual-wall-length))

   (full-bricks (apply '+ (list-elements (the row) (the-element full-bricks))))
   (half-bricks (apply '+ (list-elements (the row) (the-element half-bricks))))
   (mortar-volume (apply '+ (list-elements (the row) (the-element mortar-volume))))
   (mortar-density 2162)
   (mortar-mass (* (the mortar-density) (div (the mortar-volume) 1000000000)))
   )
   
     
  :objects
  ((row :type 'row
     :sequence (:size (the number-of-rows))
     :center (translate-along-vector (the (face-center :bottom))
				     (the (face-normal-vector :top))
				     (+ (half (the-child height))
					(* (the-child index) (the-child height))))
     :height (+ (the brick-height) (the mortar-joint-width))
     :full-bricks-per-row (the number-of-bricks)
     :pass-down (brick-height
		 brick-length
		 brick-width
		 mortar-joint-width
		 length
		 width
		 first-row
		 front-edge
		 rear-edge))))


```

---

## building-hint-4.lisp - row
Source: gornschool-training/t2/resources/source/building-hint-4.lisp
Type: tutorial

```
(define-object row (box)
  :input-slots
  (full-bricks-per-row
   brick-height
   brick-length
   brick-width
   mortar-joint-width
   first-row
   front-edge
   rear-edge )

  :computed-slots
  ((full-brick-row? (cond ((eq (the first-row) :start-full)
			   (or (zerop (the index)) (evenp (the index))))
			  ((eq (the first-row) :start-half)
			   (not (or (zerop (the index)) (evenp (the index)))))))
		    
   (full-bricks (the bricks-and-mortar full-bricks))
   (half-bricks (the bricks-and-mortar half-bricks))
   (mortar-volume (+ (the bricks-and-mortar mortar-joint-volume)
		     (the mortar-bed volume))))

  :objects
  ((bricks-and-mortar :type 'bricks-and-mortar
		      :height (the brick-height)
		      :center (translate-along-vector (the mortar-bed (face-center :top))
						      (the mortar-bed (face-normal-vector :top))
						      (half (the-child height)))
		      :pass-down (width
				  length
				  full-brick-row?
				  brick-height
				  brick-length
				  brick-width
				  mortar-joint-width
				  full-bricks-per-row
				  front-edge
				  rear-edge))
   (mortar-bed :type 'box
	       :height (the mortar-joint-width)
	       :center (translate-along-vector (the (face-center :bottom))
					       (the (face-normal-vector :top))
					       (half (the-child height)))
	       :pass-down (width
			   length))))


```

---

## building-hint-4.lisp - bricks-and-mortar
Source: gornschool-training/t2/resources/source/building-hint-4.lisp
Type: tutorial

```
(define-object bricks-and-mortar (box)
  :input-slots
  (full-brick-row?
   full-bricks-per-row
   brick-height
   brick-length
   brick-width
   mortar-joint-width
   front-edge
   rear-edge)

  :computed-slots
  ((first-full-brick-start-point (if (the full-brick-row?)
				     (the (face-center :front))
				     (the (mortar-joint 0) (face-center :rear))))
   
   (first-mortar-joint-start-point (cond ((the full-brick-row?) (the (full-brick 0) (face-center :rear)))
					 ((eq (the front-edge) :full) (the (half-brick 0) (face-center :rear)))
					 ((eq (the front-edge) :keyed) (translate-along-vector (the (face-center :front))
											       (the (face-normal-vector :rear))
											       (half (the brick-length))))))

   (number-of-full-bricks (if (the full-brick-row?)
			      (the full-bricks-per-row)
			      (- (the full-bricks-per-row) 1)))

   (number-of-half-bricks (cond ((the full-brick-row?) 0)
				((and (eq (the front-edge) :full)(eq (the rear-edge) :full)) 2)
				((and (eq (the front-edge) :keyed) (eq (the rear-edge) :full)) 1)
				((and (eq (the front-edge) :full) (eq (the rear-edge) :keyed)) 1)
				((and (eq (the front-edge) :keyed) (eq (the rear-edge) :keyed)) 0)))

   ;; whether or not the ends are :full or :keyed, the number of mortar joints remains the same since the mortar joint
   ;; when it is :keyed is used to connect to the full brick of the other wall
   (number-of-mortar-joints (if (the full-brick-row?)
				(- (the number-of-full-bricks) 1)
				(+ (the number-of-full-bricks) 1)))

   ;; if it isn't a full brick row then there will be an extra joint because one
   ;; full brick is replaced with 2 half bricks so without correcting the
   ;; mortar-joint-width the ends of a full brick row and one starting and
   ;; finishing with half bricks won't align. So we need to correct
   ;; the mortar-joint-width
   (corrected-joint-width (if (the full-brick-row?)
				     (the mortar-joint-width)
				     (let ((total-gap (* (- (the number-of-mortar-joints) 1)
									(the mortar-joint-width))))
				       (div total-gap (the number-of-mortar-joints)))))
   ;; collating the output. We could do this analytically, but for this example we'll use the geometry
   (full-bricks (length (list-elements (the full-brick))))
   (half-bricks (length (list-elements (the half-brick))))
   (mortar-joint-volume (apply '+ (list-elements (the mortar-joint)
						 (the-element volume))))
   )
  
  :functions
  ((first-full-brick-center!
    ()
    (translate-along-vector (the first-full-brick-start-point)
			    (the (face-normal-vector :rear))
			    (half (the brick-length))))
   
   (other-full-brick-center!
    (index)
    ;; if its a full brick row, (full-brick 1) is positioned relative to (joint 0)
    ;; if its a half brick row, (full-brick 1) is positioned relative to (joint 1)
    (let ((ind (if (the full-brick-row?) (- index 1) index)))
	  (translate-along-vector (the (mortar-joint ind) (face-center :rear))
				  (the (face-normal-vector :rear))
				  (half (the brick-length)))))
   
    (first-joint-center!
     ()
     (translate-along-vector (the first-mortar-joint-start-point)
			     (the (face-normal-vector :rear))
			     (half (the corrected-joint-width))))
   (other-joint-center!
    (index)
    ;; if its a full brick row, (joint 1) is positioned relative to (full-brick 1)
    ;; if its a half brick row, (joint 1) is positioned relative to (full-brick 0)
    (let ((ind (if (the full-brick-row?) index (- index 1))))
      (translate-along-vector (the (full-brick ind) (face-center :rear))
			      (the (face-normal-vector :rear))
			      (half (the corrected-joint-width)))))

    (first-half-brick-center!
     ()
     (translate-along-vector (the (face-center :front))
			     (the (face-normal-vector :rear))
			     (half (half (the brick-length)))))
   
   (last-half-brick-center!
     ()
     (translate-along-vector (theo (the mortar-joint last) (face-center :rear))
			     (the (face-normal-vector :rear))
			     (half (half (the brick-length))))))
 			     
  
  :objects
  ((full-brick :type 'box
	       :sequence (:size (the number-of-full-bricks))
	       :center (if (= (the-child index) 0)
			   (the first-full-brick-center!)
			   (the (other-full-brick-center! (the-child index))))
	       :length (the brick-length)
	       :height (the brick-height)
	       :width (the brick-width))

   (half-brick :type 'box
	       :sequence (:size (the number-of-half-bricks))
	       :center (cond ((and (= (the-child index) 0)
				   (eq (the front-edge) :full)) (the first-half-brick-center!))
			     ((and (= (the-child index) 0)
				   (eq (the front-edge) :keyed)
				   (eq (the rear-edge) :full)) (the last-half-brick-center!))
			     ((eq (the rear-edge) :full) (the last-half-brick-center!)))
	       :length (half (the brick-length))
	       :height (the brick-height)
	       :width (the brick-width))

   (mortar-joint :type 'box
		 :sequence (:size (the number-of-mortar-joints))
		 :center (if (= (the-child index) 0)
			     (the first-joint-center!)
			  (the (other-joint-center! (the-child index))))   
		 :height (the brick-height)
		 :width (the brick-width)
		 :length (the corrected-joint-width))))


```

---

## building-hint-4.lisp - degrees-to-radians
Source: gornschool-training/t2/resources/source/building-hint-4.lisp
Type: tutorial

```
(defun degrees-to-radians (degrees)
  (div (* degrees pi) 180))



```

---

## building-hint-4.lisp - truss
Source: gornschool-training/t2/resources/source/building-hint-4.lisp
Type: tutorial

```
(define-object truss (box)
  :input-slots
  ((truss-length 2000)
   (truss-height nil)
   (truss-angle 30)

   (beam-width 30)
   (beam-height 50)
   (wall-thickness 3)

   (material-density 7800)
   (tonne-rate 450))

  :computed-slots
  ((length (the truss-length))
   (height (cond ((the truss-height)(the truss-height))
		 ((the truss-angle) (+ (* (half (the truss-length))
					  (tan (degrees-to-radians (the truss-angle))))
				       (the beam-height)))))
   (width (the beam-width))
  
   (truss-front-slope-vector (the (get-slope-vector! :front)))
   (truss-rear-slope-vector (the (get-slope-vector! :rear)))

   (front-slope-length (the (get-slope-length! :front)))
   (rear-slope-length (the (get-slope-length! :rear)))
   
   (front-slope-center (the (get-slope-center! :front)))
   (rear-slope-center (the (get-slope-center! :rear)))
   

   (beam-properties (mapsend (the children) :beam-properties))
   (total-mass (round-to-nearest
		(apply '+
		       (mapcar #'(lambda(a) (getf a :mass-kg))
			       (the beam-properties)))
		0.001))
   (total-cost (round-to-nearest
		(apply '+
		       (mapcar #'(lambda(a) (getf a :cost-gbp))
			       (the beam-properties)))
		0.01))

   ;; messages to support roof cladding sizing and positioning
   (apex-point (inter-line-plane (the rear-slope-construction-line end)
			   (the truss-rear-slope-vector)
			   (the lower-beam center)
				 (the (face-normal-vector :rear))))
   (front-gutter-point (the front-slope-construction-line start))
   (rear-gutter-point (the rear-slope-construction-line start))
   (front-slope-normal (the front-slope-beam (face-normal-vector :top)))
   (rear-slope-normal (the rear-slope-beam (face-normal-vector :top)))
   )

  :functions
  ((get-slope-vector! (beam-side)
		      (let ((v-key (the (get-v-key! beam-side)))
			    (l-key (the (get-l-key! beam-side))))
		      (subtract-vectors (the vertical-beam (edge-center :rear v-key))
					(the lower-beam (edge-center l-key :top)))))
   (get-slope-length! (beam-side)
		      (let ((v-key (the (get-v-key! beam-side)))
			    (l-key (the (get-l-key! beam-side))))
			(3d-distance (the vertical-beam (edge-center :rear v-key))
				     (the lower-beam (edge-center l-key :top)))))
   (get-slope-center!
    (beam-side)
    (let ((pt (case beam-side
		(:front (the front-slope-construction-line center))
		(:rear  (the rear-slope-construction-line center))))
	  (norm-vector (case beam-side
			 (:front (the front-slope-beam (face-normal-vector :bottom)))
			 (:rear (the rear-slope-beam (face-normal-vector :bottom))))))
      (translate-along-vector pt
			      norm-vector
			      (half (the beam-height)))))
   (get-v-key! (beam-side)
	       (case beam-side
		 (:front :top)
		 (:rear :bottom)))
   (get-l-key! (beam-side)
	       (case beam-side
		 (:front :front)
		 (:rear :rear)))

   
   )
   
  :computed-slots
  (
   
   )
  
  :objects
  ((lower-beam :type 'beam
	       :pass-down (:beam-height :beam-width :wall-thickness :material-density :tonne-rate)
	       :beam-length (the truss-length)
	       :center (translate-along-vector (the (face-center :bottom))
					       (the (face-normal-vector :top))
					       (half (the beam-height))))
   (vertical-beam :type 'beam
		  :pass-down (:beam-height :beam-width :wall-thickness :material-density :tonne-rate)
		  
		  :beam-length (- (the height) (the beam-height))
		  :orientation (alignment :rear (the (face-normal-vector :top))
					  :right (the (face-normal-vector :right)))  
		  :center (translate-along-vector (the lower-beam (face-center :top))
						  (the lower-beam (face-normal-vector :top))
						  (half (the-child beam-length))))
   (front-slope-beam :type 'beam
		     :pass-down (:beam-height :beam-width :wall-thickness :material-density :tonne-rate)
		     :beam-length (the front-slope-length)
		     :center (the front-slope-center)
		     :orientation (alignment :rear (the truss-front-slope-vector)
					     :right (the (face-normal-vector :right))))

   (rear-slope-beam :type 'beam
		    :pass-down (:beam-height :beam-width :wall-thickness :material-density :tonne-rate)
		    :beam-length (the rear-slope-length)
		    :center (the rear-slope-center)
		    :orientation (alignment :rear (the truss-rear-slope-vector)
					    :left (the (face-normal-vector :right))))
   
   )

  :hidden-objects
  ((apex-pt :type 'sphere
	    :radius 5
	    :display-controls (list :color :green)
	    :center (the apex))
   (front-slope-construction-line :type 'line
				  :start (the lower-beam (edge-center :front :top))
				  :end (the vertical-beam (edge-center :rear :top)))
   (rear-slope-construction-line :type 'line
				 :start (the lower-beam (edge-center :rear :top))
				 :end (the vertical-beam (edge-center :rear :bottom)))
   
   (front-mid-pt :type 'sphere
		 :display-controls (list :color :blue)
		 :radius 5
		 :center (the front-slope-construction-line center))
   (rear-mid-pt :type 'sphere
		:display-controls (list :color :blue)
		:center (the rear-slope-construction-line center)
		:radius 5)
   (pt-1 :type 'sphere
	 :radius 5
	 :display-controls (list :color :green)
	 :center (the lower-beam (edge-center :rear :top)))
   (pt-2 :type 'sphere
	 :radius 5
	 :display-controls (list :color :red)
	 :center (the vertical-beam (edge-center :rear :bottom)) )

   

   (vector-line :type 'vector-line
		:start-point (the rear-slope-construction-line center)
		:vector (the rear-slope-beam (face-normal-vector :bottom))
		:length 150)
   
   )))


```

---

## building-hint-4.lisp - vector-line
Source: gornschool-training/t2/resources/source/building-hint-4.lisp
Type: tutorial

```
(define-object vector-line (box)
  :input-slots
  ((start-point (make-point 0 0 0))
   (vector (make-vector 1 0 1))
   (length 50)
   (width 1))
  :computed-slots
  ((height (div (the length) 5)))
  :objects
  ((v-line :type 'line
	   :start (the start-point)
	   :display-controls (list :color :red)
	   :end (translate-along-vector (the start-point)
					(the vector)
					(the length)))
   (arrow :type 'cone
	  :radius-1 0
	  :radius-2 (div (the length) 50)
	  :length (div (the length) 5)
	  :display-controls (list :color :red)
	  :center (translate-along-vector (the v-line end)
					  (the vector)
					  (half (the-child length)))
	  :orientation (alignment :front (the vector)))))


```

---

## building-hint-4.lisp - beam
Source: gornschool-training/t2/resources/source/building-hint-4.lisp
Type: tutorial

```
(define-object beam (box)
  :input-slots
  ((beam-length 1000)
   (beam-width 40)
   (beam-height 50)
   (wall-thickness 2)
   (material-density 7800)
   (tonne-rate 500))

  :computed-slots
  ((length (the beam-length))
   (width (the beam-width))
   (height (the beam-height))

   (beam-volume (- (the outer volume) (the inner volume)))
   (beam-mass (* (div (the beam-volume) 1000000000) (the material-density)))
   (beam-cost (* (the tonne-rate) (div (the beam-mass) 1000)))
   (beam-properties (list :volume-mm3 (the beam-volume)
			  :mass-kg (round-to-nearest (the beam-mass) 0.01)
			  :cost-gbp (round-to-nearest (the beam-cost) 0.01)
			  :length-mm (the beam-length)
			  :width-mm (the beam-width)
			  :height-mm (the beam-height)
			  :thickness-mm (the wall-thickness))))
   
  :objects
  ((outer :type 'box
	  :length (the beam-length)
	  :width (the beam-width)
	  :height (the beam-height))

   (inner :type 'box
	  :length (the beam-length)
	  :width (- (the beam-width) (twice (the wall-thickness)))
	  :height (- (the beam-height) (twice (the wall-thickness)))))
  )








```

---

## lists.lisp - header
Source: gornschool-training/t2/resources/source/lists.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## lists.lisp - lists
Source: gornschool-training/t2/resources/source/lists.lisp
Type: tutorial

```
(define-object lists (base-object)

  :computed-slots
  ((my-number-list (list 1 2 3 4))
   (my-plist (list :uk "London" :france "Paris" :belgium "Brussels"))
   
   ;; Length of a list
   (length-1 (length (the my-number-list)))    ;;; 4
   (length-2 (length nil))                     ;;; 0
   

   ;; referencing in a list (from welcome-to-lisp)
    ;; accessing the first element of a list
   (access-first-1 (first (the my-number-list)))    ;;; 1
   (access-first-2 (car (the my-number-list)))      ;;; 1
   (access-first-3 (nth 0 (the my-number-list)))    ;;; 1

   ;; accessing the second element of a list
   (access-second-1 (second (the my-number-list)))     ;;; 2
   (access-second-2 (car (cdr (the my-number-list))))  ;;; 2
   (access-second-3 (cadr (the my-number-list)))       ;;; 2
   (access-second-4 (nth 1 (the my-number-list)))      ;;; 2

   ;; accessing all but the first element in a list
   (access-rest-1 (cdr (the my-number-list)))          ;;; (2 3 4))
   (access-rest-2 (rest (the my-number-list)))         ;;; (2 3 4))

   ;; accesing the last element of a list
   (access-last-1 (car (last (the my-number-list))))                               ;;; 4 !! caution - last returns a list !!
   (access-last-2 (nth (- (length (the my-number-list)) 1) (the my-number-list)))  ;;; 4
   (access-last-3 (car (reverse (the my-number-list))))                            ;;; 4
   (access-last-4 (lastcar (the my-number-list)))                                  ;;; 4

   ;; access all but the last element of the list
   (access-but-last (butlast (the my-number-list)))   ;;; (1 2 3 )

   ;; appending lists
   (add-1 (append  (the my-number-list) (the my-string-list))) ;;; (1 2 3 4 "peter" "paul" "mike" "john")

   (add-2 (append (the my-number-list) (list 1)))	      ;;; (1 2 3 4 1)
   (add-3 (append (the my-number-list) 5))                    ;;; (1 2 3 4 . 5) if the first argument is a list this generates a dotted list.
   (add-4 (ignore-errors (append 0 (the my-number-list))))    ;;; if the first argument isn't a list append will error
                                                              ;;; NIL
                                                              ;;; #<TYPE-ERROR #x210376AEED>
   
   (add-5 (cons 0 (the my-number-list))) ;; (0 1 2 3 4) if the second argument is a list cons will return a list
   (add-6 (cons (the my-number-list) 5)) ;; ((1 2 3 4) . 5) note the different behaviour to append

   ;; plists
   (plist-1 (getf (the my-plist) :uk))                ;;; "London"
   )
  )

```

---

## positioning-and-orientation.lisp - header
Source: gornschool-training/t2/resources/source/positioning-and-orientation.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## positioning-and-orientation.lisp - assembly-2
Source: gornschool-training/t2/resources/source/positioning-and-orientation.lisp
Type: tutorial

```
(define-object assembly-2 (base-object)

  :objects
  ((box-1 :type 'box
	  :length 5
	  :width 1
	  :height 1)

   (box-2 :type 'box
	  :length 10
	  :height 5
	  :width 3
	  :center (make-point 2 2 2))

   (box-3 :type 'box
	  :length 5
	  :height 5
	  :width 5
	  :center (translate-along-vector (the box-2 center)
					  (make-vector 1 1 0)
					  5))))

```

---

## positioning-and-orientation.lisp - assembly-3
Source: gornschool-training/t2/resources/source/positioning-and-orientation.lisp
Type: tutorial

```
(define-object assembly-3 (base-object)

  :objects
  ((box-1 :type 'box
	  :length 5
	  :width 1
	  :height 1)

   (box-2 :type 'box
	  :length 10
	  :height 5
	  :width 3
	  :center (translate-along-vector (the box-1 (face-center :rear))
					  (the box-1 (face-normal-vector :rear))
					  (half (the-child length))))

   (box-3 :type 'box
	  :length 5
	  :height 5
	  :width 5
	  :center (translate-along-vector (the box-2 (face-center :rear))
					  (the box-2 (face-normal-vector :rear))
					  (half (the-child length))))))

  
```

---

## positioning-and-orientation.lisp - assembly-4
Source: gornschool-training/t2/resources/source/positioning-and-orientation.lisp
Type: tutorial

```
(define-object assembly-4 (base-object)

  :objects
  ((box-1 :type 'box
	  :length 5
	  :width 1
	  :height 1)

   (box-2 :type 'box
	  :length 5
	  :width 1
	  :height 1
	  :orientation (alignment :rear (the box-1 (face-normal-vector :top))))
   ))


```

---

## positioning-and-orientation.lisp - assembly-5
Source: gornschool-training/t2/resources/source/positioning-and-orientation.lisp
Type: tutorial

```
(define-object assembly-5 (base-object)

  :objects
  ((box-1 :type 'box
	  :length 5
	  :width 1
	  :orientation (alignment :rear (the (face-normal-vector :top)))
	  :height 1)

   (box-2 :type 'box
	  :length 10
	  :height 5
	  :width 3
	 :orientation (alignment :rear (the box-1 (face-normal-vector :rear)))
	  :center (translate-along-vector (the box-1 (face-center :rear))
					  (the box-1 (face-normal-vector :rear))
					  (half (the-child length))))

   (box-3 :type 'box
	  :length 5
	  :height 5
	  :width 5
	  :orientation (alignment :rear (the box-2 (face-normal-vector :rear)))
	  :center (translate-along-vector (the box-2 (face-center :rear))
					  (the box-2 (face-normal-vector :rear))
					  (half (the-child length))))))

```

---

## strings.lisp - header
Source: gornschool-training/t2/resources/source/strings.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## strings.lisp - strings
Source: gornschool-training/t2/resources/source/strings.lisp
Type: tutorial

```
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

   

```

---

## wall-hint-3.lisp - header
Source: gornschool-training/t2/resources/source/wall-hint-3.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## wall-hint-3.lisp - wall
Source: gornschool-training/t2/resources/source/wall-hint-3.lisp
Type: tutorial

```
(define-object wall(box)
  :input-slots
  ((brick-height 45)
   (brick-length 180)
   (brick-width 90)
   (mortar-joint-width 10)
   (wall-length 3700)
   (wall-height 900))

  :computed-slots
  ((row-height (+ (the brick-height) (the mortar-joint-width)))
   (number-of-rows (round-to-nearest (div (the wall-height) (the row-height)) 1))
   (actual-wall-height (* (the row-height) (the number-of-rows)))
   
   ;; for the wall-length we need the number of full bricks
   ;; if there are n full bricks then there will be (n-1) mortar joints
   ;; so n*brick-length + n-1*mortar-joint-width = overall-length
   ;; or n(brick-length + mortar-join-width) - mortar-joint-width = overall-length
   ;; or n = (overall-length - mortar-joint-width)/(brick-length + mortar-joint-width)
   (number-of-bricks (round-to-nearest (div (- (the wall-length) (the mortar-joint-width))
					    (+ (the brick-length) (the mortar-joint-width)))
				       1))
   (actual-wall-length (+ (* (the number-of-bricks) (the brick-length))
			  (* (- (the number-of-bricks) 1) (the mortar-joint-width))))

   ;; box inputs - gives the wall bounding box
   (height (the actual-wall-height))
   (width (the brick-width))
   (length (the actual-wall-length)))
   
     
  :objects
  ((row :type 'row
     :sequence (:size (the number-of-rows))
     :bricks-per-row (the number-of-bricks)
     :length (the length)
     :width (the width)
     :brick-height(the brick-height)
     :brick-length (the brick-length)
     :brick-width (the brick-width)
     :mortar-joint (the mortar-joint-width))))


```

---

## wall-hint-3.lisp - row
Source: gornschool-training/t2/resources/source/wall-hint-3.lisp
Type: tutorial

```
(define-object row ()
  :input-slots
  (brick-height
   brick-length
   brick-width
   mortar-joint-width)

  :objects
  ((bricks-and-mortar :type 'bricks-and-mortar
		      :brick-height (the brick-height)
		      :brick-length (the brick-length)
		      :brick-width (the brick-width)
		      :mortar-joint-width (the mortar-joint-width))
   (mortar-bed :type 'box
	       :height (the mortar-joint-width))
   )
  )


```

---

## wall-hint-3.lisp - bricks-and-mortar
Source: gornschool-training/t2/resources/source/wall-hint-3.lisp
Type: tutorial

```
(define-object bricks-and-mortar ()
  :input-slots
  (brick-height
   brick-length
   brick-width
   mortar-joint-width)
  
  :objects
  ((full-brick :type 'box
	       :length (the brick-length)
	       :height (the brick-height)
	       :width (the brick-width))
   (half-brick :type 'box
	       :length (half (the brick-length))
	       :height (the brick-height)
	       :width (the brick-width))
   (mortar-joint :type 'box
		 :height (the brick-height)
		 :width (the brick-width)
		 :length (the mortar-joint-width))))

```

---

## functions.lisp - header
Source: gornschool-training/t2/resources/source/functions.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## functions.lisp - kinetic-energy
Source: gornschool-training/t2/resources/source/functions.lisp
Type: tutorial

```
(defun kinetic-energy (&key (mass 5) (velocity 12)
			 (div (* mass velocity velocity) 2)))


```

---

## functions.lisp - function-example
Source: gornschool-training/t2/resources/source/functions.lisp
Type: tutorial

```
(define-object function-example(base-object)

  :computed-slots
  ((mass 5)
   (velocity 12)
   (ke-1 (the kinetic-energy-1!))
   (ke-2 (the kinetic-energy-2!))
   (ke-3 (the (kinetic-energy-2! :mass 10)))
   (ke-4 (the (kinetic-energy-2! :velocity 24)))
   (ke-5 (the (kinetic-energy-2! :velocity 24 :mass 10)))

   (ke-6 (kinetic-energy :mass 10 :velocity 24))
   )

  :functions
  ((kinetic-energy-1! () (div (* (the mass) (the velocity) (the velocity)) 2))

   (kinetic-energy-2! (&key (mass (the mass)) (velocity (the velocity)))
		      (div (* mass velocity velocity) 2))
   )
  )

```

---

## point-and-vector-examples.lisp - header
Source: gornschool-training/t2/resources/source/point-and-vector-examples.lisp
Type: tutorial

```
(in-package :gdl-user)

```

---

## point-and-vector-examples.lisp - assembly
Source: gornschool-training/t2/resources/source/point-and-vector-examples.lisp
Type: tutorial

```
(define-object assembly (box)
  :computed-slots
  ((length 1)
  (width 1)
  (height 1)
  (alignment-vector-1 (the (face-normal-vector :left)))
  (alignment-vector-2 (the (face-normal-vector :rear)))
  ;; some other ways to make vectors
  ;; effectively the vector from 0,0,0 to 1,1,1
  (alignment-3 (make-vector 1 1 1))

  ;; points and vectors have the same structure so make-point and make-vector can be used interchangably
  (pt-1 (make-point 1 0 0))
  (pt-2 (make-point 2 5 6))
  ;; subtract vectors is used to get the vector from the second point to the first point
  ;; conceptualy (make-vector 1 1 1) is equivalent to (subtract-vectors (make-vector 1 1 1) (make-vector 0 0 0)))
  (vector-1 (subtract-vectors (the pt-2) (the pt-1) ))

  ;; distance between 2 points
  (distance-between-points (3d-distance (the pt-1) (the pt-2)))

  ;; translating a point
  (new-point (translate-along-vector (the pt-1) (the vector-1) (the distance-between-points)))

  ;; new-point should be exactly the same as pt-2, but we see some rounding has crept in as the value is #(2.0 5.0 6.000000000000001)
  ;; to test if 2 points are the 'same' use coincident-point? the tolerance defaults to *ZERO-EPSILON* (0.001 by default)
  (same-point? (coincident-point? (the pt-2) (the new-point) :tolerance 0.000001))

  ;; getting individual x y and z values from a point
  (new-point-x (get-x (the new-point)))
  (new-point-y (get-y (the new-point)))
  (new-point-z (get-z (the new-point)))

  ;; orthoginal vector
  ;; use cross vectors to get a vector that is orthogonal to the 2 input vectors
  (ortho-vector (let ((v1 (make-vector 1 0 0))
		      (v2 (make-vector 0 1 0)))
		  (cross-vectors v1 v2)))
  ;; returns #(0.0 0.0 1.0)

  (vector-angle-rads (let ((v1 (make-vector 1 0 0))
		      (v2 (make-vector 0 1 0)))
		       (angle-between-vectors v1 v2)))
  ;; returns 1.5707963267948966

  (vector-angle-degrees (let ((v1 (make-vector 1 0 0))
		      (v2 (make-vector 0 1 0)))
			  (angle-between-vectors-d v1 v2)))
  ;; returns 90
  ;; note that by default angle-between-vectors/angle-between-vectors-d returns the smallest angle.
  ;; However if a third vector is given it calculates the angle
  ;; based on the RH rule, around this third vector

  (vector-angle-degrees-1 (let ((v1 (make-vector 1 0 0))
				(v2 (make-vector 0 1 0))
				(ref-v (make-vector 0 0 1)))
			    (angle-between-vectors-d v1 v2 ref-v)))
  ;; returns 90
  (vector-angle-degrees-2 (let ((v1 (make-vector 1 0 0))
				(v2 (make-vector 0 1 0))
				(ref-v (make-vector 0 0 -1)))
			    (angle-between-vectors-d v1 v2 ref-v)))	       
  ;; returns 270
  )
  
  
  :objects
  ((bottom-post :type 'post
		;; in this example we align the :top axis of the post with alignment-vector-1
		;; if we change this to bottom it will rotate the post by 180 degrees
		:orientation (alignment :top
					(the alignment-vector-1)))
   (next-post :type 'post
		:orientation (alignment :top
					(the alignment-vector-2))))
  )


```

---

## point-and-vector-examples.lisp - post
Source: gornschool-training/t2/resources/source/point-and-vector-examples.lisp
Type: tutorial

```
(define-object post (box)

  :computed-slots
  ((height (+ (the post-main height) (the base height)))
   (length (max (the post-main length) (the base length)))
   (width (max (the post-main width ) (the base width))))
  
  :objects
  ((post-main :type 'box
	      :height 1000
	      :width 50
	      :length 50
	      :center (translate-along-vector (the (face-center :top))
					      (the (face-normal-vector :bottom))
					      (half (the-child height))))
   (base :type 'box
	 :height 10
	 :width 200
	 :length 200
	 ;; the centre of the base will always be half the base height below the bottom of the post
	 :center (translate-along-vector (the post-main (face-center :bottom))
					 (the post-main (face-normal-vector :bottom))
					 (half (the-child height)))))
  )
  

```

---

## truss-hint-5.lisp - header
Source: gornschool-training/t2/resources/source/truss-hint-5.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## truss-hint-5.lisp - degrees-to-radians
Source: gornschool-training/t2/resources/source/truss-hint-5.lisp
Type: tutorial

```
(defun degrees-to-radians (degrees)
  (div (* degrees pi) 180))


```

---

## truss-hint-5.lisp - truss
Source: gornschool-training/t2/resources/source/truss-hint-5.lisp
Type: tutorial

```
(define-object truss (box)
  :input-slots
  ((truss-length 2000)
   (truss-height 800)
   (truss-angle nil)

   (beam-width 30)
   (beam-height 50)
   (wall-thickness 3))

  :computed-slots
  ((length (the truss-length))
   (height (cond ((the truss-height)(the truss-height))
		 ((the truss-angle) (+ (* (half (the truss-length))
					  (tan (degrees-to-radians (the truss-angle))))
				       (the beam-height)))))
   (width (the beam-width))
   (truss-front-slope-vector (subtract-vectors (the vertical-beam (edge-center :rear :top))
					       (the lower-beam (edge-center :front :top))))

  
   )
				       
  :objects
  ((lower-beam :type 'beam
	       :beam-height (the beam-height)
	       :beam-width (the beam-width)
	       :beam-length (the truss-length)
	       :center (translate-along-vector (the (face-center :bottom))
					       (the (face-normal-vector :top))
					       (half (the beam-height))))
   (vertical-beam :type 'beam
		  :beam-length (- (the height) (the beam-height))
		  :beam-height (the beam-height)
		  :beam-width (the beam-width)
		  :orientation (alignment :rear (the (face-normal-vector :top))
					  :right (the (face-normal-vector :right)))
		  :center (translate-along-vector (the lower-beam (face-center :top))
						  (the lower-beam (face-normal-vector :top))
						  (half (the-child beam-length))))
   ;;(front-slope-beam :type 'beam)
   ;;(right-slope-beam :type 'beam))

   (pt-1 :type 'sphere
	 :radius 5
	 :display-controls (list :color :green)
	 :center (the lower-beam (edge-center :front :top)))
   (pt-2 :type 'sphere
	 :radius 5
	 :display-controls (list :color :red)
	 :center (the vertical-beam (edge-center :rear :top)) )
   (vector-line :type 'vector-line
		:start-point (the pt-1 center)
		:vector (the truss-front-slope-vector)
		:length 150)))


```

---

## truss-hint-5.lisp - vector-line
Source: gornschool-training/t2/resources/source/truss-hint-5.lisp
Type: tutorial

```
(define-object vector-line (box)
  :input-slots
  ((start-point (make-point 0 0 0))
   (vector (make-vector 1 0 1))
   (length 50)
   (width 1))
  :computed-slots
  ((height (div (the length) 5)))
  :objects
  ((v-line :type 'line
	   :start (the start-point)
	   :display-controls (list :color :red)
	   :end (translate-along-vector (the start-point)
					(the vector)
					(the length)))
   (arrow :type 'cone
	  :radius-1 0
	  :radius-2 (div (the length) 50)
	  :length (div (the length) 5)
	  :display-controls (list :color :red)
	  :center (translate-along-vector (the v-line end)
					  (the vector)
					  (half (the-child length)))
	  :orientation (alignment :front (the vector)))))


```

---

## truss-hint-5.lisp - beam
Source: gornschool-training/t2/resources/source/truss-hint-5.lisp
Type: tutorial

```
(define-object beam (box)
  :input-slots
  ((beam-length 1000)
   (beam-width 40)
   (beam-height 50)
   (wall-thickness 2)
   (material-density 7800)
   (tonne-rate 500))

  :computed-slots
  ((length (the beam-length))
   (width (the beam-width))
   (height (the beam-height))

   (beam-volume (- (the outer volume) (the inner volume)))
   (beam-mass (* (div (the beam-volume) 1000000000) (the material-density)))
   (beam-cost (* (the tonne-rate) (div (the beam-mass) 1000)))
   (beam-properties (list :volume-mm3 (the beam-volume)
			  :mass-kg (round-to-nearest (the beam-mass) 0.01)
			  :cost-gbp (round-to-nearest (the beam-cost) 0.01)
			  :length-mm (the beam-length)
			  :width-mm (the beam-width)
			  :height-mm (the beam-height)
			  :thickness-mm (the wall-thickness))))
   
  :objects
  ((outer :type 'box
	  :length (the beam-length)
	  :width (the beam-width)
	  :height (the beam-height))

   (inner :type 'box
	  :length (the beam-length)
	  :width (- (the beam-width) (twice (the wall-thickness)))
	  :height (- (the beam-height) (twice (the wall-thickness)))))
  )

```

---

## wall-hint-1.lisp - header
Source: gornschool-training/t2/resources/source/wall-hint-1.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## wall-hint-1.lisp - wall
Source: gornschool-training/t2/resources/source/wall-hint-1.lisp
Type: tutorial

```
(define-object wall()
  :objects
  ((row :type 'row)))


```

---

## wall-hint-1.lisp - row
Source: gornschool-training/t2/resources/source/wall-hint-1.lisp
Type: tutorial

```
(define-object row ()
  :objects
  ((bricks-and-mortar :type 'bricks-and-mortar)
   (mortar-bed :type 'box)))


```

---

## wall-hint-1.lisp - bricks-and-mortar
Source: gornschool-training/t2/resources/source/wall-hint-1.lisp
Type: tutorial

```
(define-object bricks-and-mortar ()
  :objects
  ((full-brick :type 'box)
   (half-brick :type 'box)
   (mortar-joint :type 'box)))

```

---

## wall-hint-4.lisp - header
Source: gornschool-training/t2/resources/source/wall-hint-4.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## wall-hint-4.lisp - wall
Source: gornschool-training/t2/resources/source/wall-hint-4.lisp
Type: tutorial

```
(define-object wall(box)
  :input-slots
  ((brick-height 45)
   (brick-length 180)
   (brick-width 90)
   (mortar-joint-width 10)
   (wall-length 3700)
   (wall-height 900))

  :computed-slots
  ((row-height (+ (the brick-height) (the mortar-joint-width)))
   (number-of-rows (round-to-nearest (div (the wall-height) (the row-height)) 1))
   (actual-wall-height (* (the row-height) (the number-of-rows)))
   
   ;; for the wall-length we need the number of full bricks
   ;; if there are n full bricks then there will be (n-1) mortar joints
   ;; so n*brick-length + n-1*mortar-joint-width = overall-length
   ;; or n(brick-length + mortar-join-width) - mortar-joint-width = overall-length
   ;; or n = (overall-length - mortar-joint-width)/(brick-length + mortar-joint-width)
   (number-of-bricks (round-to-nearest (div (- (the wall-length) (the mortar-joint-width))
					    (+ (the brick-length) (the mortar-joint-width)))
				       1))
   (actual-wall-length (+ (* (the number-of-bricks) (the brick-length))
			  (* (- (the number-of-bricks) 1) (the mortar-joint-width))))

   ;; box inputs - gives the wall bounding box
   (height (the actual-wall-height))
   (width (the brick-width))
   (length (the actual-wall-length)))
   
     
  :objects
  ((row :type 'row
     :sequence (:size (the number-of-rows))
     :center (translate-along-vector (the (face-center :bottom))
				     (the (face-normal-vector :top))
				     (+ (half (the-child height))
					(* (the-child index) (the-child height))))
     :length (the length)
     :width (the width)
     :height (+ (the brick-height) (the mortar-joint-width))
     :bricks-per-row (the number-of-bricks)
     :brick-height(the brick-height)
     :brick-length (the brick-length)
     :brick-width (the brick-width)
     :mortar-joint-width (the mortar-joint-width))))


```

---

## wall-hint-4.lisp - row
Source: gornschool-training/t2/resources/source/wall-hint-4.lisp
Type: tutorial

```
(define-object row (box)
  :input-slots
  (brick-height
   brick-length
   brick-width
   mortar-joint-width)

  

  :objects
  ((bricks-and-mortar :type 'bricks-and-mortar
		      :brick-height (the brick-height)
		      :brick-length (the brick-length)
		      :brick-width (the brick-width)
		      :mortar-joint-width (the mortar-joint-width))
   (mortar-bed :type 'box
	       :height (the mortar-joint-width))
   )
  )


```

---

## wall-hint-4.lisp - bricks-and-mortar
Source: gornschool-training/t2/resources/source/wall-hint-4.lisp
Type: tutorial

```
(define-object bricks-and-mortar ()
  :input-slots
  (brick-height
   brick-length
   brick-width
   mortar-joint-width)
  
  :objects
  ((full-brick :type 'box
	       :length (the brick-length)
	       :height (the brick-height)
	       :width (the brick-width))
   (half-brick :type 'box
	       :length (half (the brick-length))
	       :height (the brick-height)
	       :width (the brick-width))
   (mortar-joint :type 'box
		 :height (the brick-height)
		 :width (the brick-width)
		 :length (the mortar-joint-width))))

```

---

## building-hint-1.lisp - header
Source: gornschool-training/t2/resources/source/building-hint-1.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## building-hint-1.lisp - building
Source: gornschool-training/t2/resources/source/building-hint-1.lisp
Type: tutorial

```
(define-object building (box)
  :input-slots
  ((nominal-height 3000)
   (nominal-width 3000)
   (nominal-length 4000)
   (brick-height 45)
   (brick-length 180)
   (brick-width 90)
   (mortar-joint-width 10)
   (truss-angle 30)
   (beam-width 40)
   (beam-height 50)
   (wall-thickness 3)
   (material-density 7800))

  :computed-slots
  ((length (the left-wall length))
   (width (the rear-wall length))
   (height (+ (the left-wall height) (the (roof-truss 0) height)))
   (number-of-roof-trusses 3)
   (truss-spacing (div (- (the left-wall length) (the beam-width))
		       (- (the number-of-roof-trusses) 1)))
   (truss-offsets (let ((res nil))
		    (dotimes (n (the number-of-roof-trusses) (nreverse res))
		      (push (+ (half (the beam-width))
			       (* n (the truss-spacing))) res))))
   )

   
  
  :objects
  ((left-wall :type 'wall
	      :pass-down (brick-height
			  brick-length
			  brick-width
			  mortar-joint-width)
	      :center (translate-along-vector
		       (translate-along-vector (the (edge-center :bottom :left))
					       (the (face-normal-vector :right))
					       (half (the-child width)))
		       (the (face-normal-vector :top))
		       (half (the-child height)))
	      :wall-length (the nominal-length)
	      :wall-height (the nominal-height))

   (right-wall :type 'wall
	       :pass-down (brick-height
			   brick-length
			   brick-width
			   mortar-joint-width)
	       :center (translate-along-vector
			(translate-along-vector (the (edge-center :bottom :right))
						(the (face-normal-vector :left))
						(half (the-child width)))
			(the (face-normal-vector :top))
			(half (the-child height)))
	       :wall-length (the nominal-length)
	       :wall-height (the nominal-height))

   (rear-wall :type 'wall
	      :pass-down (brick-height
			  brick-length
			  brick-width
			  mortar-joint-width)
	      :center (translate-along-vector
			(translate-along-vector (the (edge-center :bottom :rear))
						(the (face-normal-vector :front))
						(half (the-child width)))
			(the (face-normal-vector :top))
			(half (the-child height)))
	      :orientation (alignment :rear (the (face-normal-vector :right)))
	      :wall-length (the nominal-width)
	      :wall-height (the nominal-height))

   (roof-truss :type 'truss
	       :sequence (:size (the number-of-roof-trusses))
	       :truss-length (the rear-wall length)
	       :center (translate-along-vector
			(translate-along-vector (the (edge-center :front :top))
						(the (face-normal-vector :bottom))
						(half (the-child height)))
			(the (face-normal-vector :rear))
			(nth (the-child index) (the truss-offsets)))
	       :orientation (alignment :rear (the (face-normal-vector :right)))		
	       :pass-down (truss-angle
			   beam-width
			   beam-height
			   wall-thickness
			   material-density))
   
   (roof-cladding-left :type 'box)
   (roof-cladding-right :type 'box)
   )
  )


```

---

## building-hint-1.lisp - wall
Source: gornschool-training/t2/resources/source/building-hint-1.lisp
Type: tutorial

```
(define-object wall(box)
  :input-slots
  ((brick-height 45)
   (brick-length 180)
   (brick-width 90)
   (mortar-joint-width 10)
   (wall-length 3700)
   (wall-height 900))

  :computed-slots
  ((row-height (+ (the brick-height) (the mortar-joint-width)))
   (number-of-rows (round-to-nearest (div (the wall-height) (the row-height)) 1))
   (actual-wall-height (* (the row-height) (the number-of-rows)))
   
   ;; for the wall-length we need the number of full bricks
   ;; if there are n full bricks then there will be (n-1) mortar joints
   ;; so n*brick-length + n-1*mortar-joint-width = overall-length
   ;; or n(brick-length + mortar-join-width) - mortar-joint-width = overall-length
   ;; or n = (overall-length - mortar-joint-width)/(brick-length + mortar-joint-width)
   (number-of-bricks (round-to-nearest (div (- (the wall-length) (the mortar-joint-width))
					    (+ (the brick-length) (the mortar-joint-width)))
				       1))
   (actual-wall-length (+ (* (the number-of-bricks) (the brick-length))
			  (* (- (the number-of-bricks) 1) (the mortar-joint-width))))

   ;; box inputs - gives the wall bounding box
   (height (the actual-wall-height))
   (width (the brick-width))
   (length (the actual-wall-length))

   (full-bricks (apply '+ (list-elements (the row) (the-element full-bricks))))
   (half-bricks (apply '+ (list-elements (the row) (the-element half-bricks))))
   (mortar-volume (apply '+ (list-elements (the row) (the-element mortar-volume))))
   (mortar-density 2162)
   (mortar-mass (* (the mortar-density) (div (the mortar-volume) 1000000000)))
   )
   
     
  :objects
  ((row :type 'row
     :sequence (:size (the number-of-rows))
     :center (translate-along-vector (the (face-center :bottom))
				     (the (face-normal-vector :top))
				     (+ (half (the-child height))
					(* (the-child index) (the-child height))))
     :height (+ (the brick-height) (the mortar-joint-width))
     :full-bricks-per-row (the number-of-bricks)
     :pass-down (brick-height
		 brick-length
		 brick-width
		 mortar-joint-width
		 length
		 width))))


```

---

## building-hint-1.lisp - row
Source: gornschool-training/t2/resources/source/building-hint-1.lisp
Type: tutorial

```
(define-object row (box)
  :input-slots
  (full-bricks-per-row
   brick-height
   brick-length
   brick-width
   mortar-joint-width)

  :computed-slots
  ((full-brick-row? (or (zerop (the index)) (evenp (the index))))
   (full-bricks (the bricks-and-mortar full-bricks))
   (half-bricks (the bricks-and-mortar half-bricks))
   (mortar-volume (+ (the bricks-and-mortar mortar-joint-volume)
		     (the mortar-bed volume))))

  :objects
  ((bricks-and-mortar :type 'bricks-and-mortar
		      :height (the brick-height)
		      :center (translate-along-vector (the mortar-bed (face-center :top))
						      (the mortar-bed (face-normal-vector :top))
						      (half (the-child height)))
		      :pass-down (width
				  length
				  full-brick-row?
				  brick-height
				  brick-length
				  brick-width
				  mortar-joint-width
				  full-bricks-per-row))
   (mortar-bed :type 'box
	       :height (the mortar-joint-width)
	       :center (translate-along-vector (the (face-center :bottom))
					       (the (face-normal-vector :top))
					       (half (the-child height)))
	       :pass-down (width
			   length))))


```

---

## building-hint-1.lisp - bricks-and-mortar
Source: gornschool-training/t2/resources/source/building-hint-1.lisp
Type: tutorial

```
(define-object bricks-and-mortar (box)
  :input-slots
  (full-brick-row?
   full-bricks-per-row
   brick-height
   brick-length
   brick-width
   mortar-joint-width)

  :computed-slots
  ((first-full-brick-start-point (if (the full-brick-row?)
				     (the (face-center :front))
				     (the (mortar-joint 0) (face-center :rear))))
   
   (first-mortar-joint-start-point (if (the full-brick-row?)
				       (the (full-brick 0) (face-center :rear))
				       (the (half-brick 0) (face-center :rear))))

   (number-of-full-bricks (if (the full-brick-row?)
			      (the full-bricks-per-row)
			      (- (the full-bricks-per-row) 1)))

   (number-of-mortar-joints (if (the full-brick-row?)
				(- (the number-of-full-bricks) 1)
				(+ (the number-of-full-bricks) 1)))

   ;; if it isn't a full brick row then there will be an extra joint because one
   ;; full brick is replaced with 2 half bricks so without correcting the
   ;; mortar-joint-width the ends of a full brick row and one starting and
   ;; finishing with half bricks won't align. So we need to correct
   ;; the mortar-joint-width
   (corrected-joint-width (if (the full-brick-row?)
				     (the mortar-joint-width)
				     (let ((total-gap (* (- (the number-of-mortar-joints) 1)
									(the mortar-joint-width))))
				       (div total-gap (the number-of-mortar-joints)))))
   ;; collating the output. We could do this analytically, but for this example we'll use the geometry
   (full-bricks (length (list-elements (the full-brick))))
   (half-bricks (length (list-elements (the half-brick))))
   (mortar-joint-volume (apply '+ (list-elements (the mortar-joint)
						 (the-element volume))))
   )
  
  :functions
  ((first-full-brick-center!
    ()
    (translate-along-vector (the first-full-brick-start-point)
			    (the (face-normal-vector :rear))
			    (half (the brick-length))))
   
   (other-full-brick-center!
    (index)
    ;; if its a full brick row, (full-brick 1) is positioned relative to (joint 0)
    ;; if its a half brick row, (full-brick 1) is positioned relative to (joint 1)
    (let ((ind (if (the full-brick-row?) (- index 1) index)))
	  (translate-along-vector (the (mortar-joint ind) (face-center :rear))
				  (the (face-normal-vector :rear))
				  (half (the brick-length)))))
   
    (first-joint-center!
     ()
     (translate-along-vector (the first-mortar-joint-start-point)
			     (the (face-normal-vector :rear))
			     (half (the corrected-joint-width))))
   (other-joint-center!
    (index)
    ;; if its a full brick row, (joint 1) is positioned relative to (full-brick 1)
    ;; if its a half brick row, (joint 1) is positioned relative to (full-brick 0)
    (let ((ind (if (the full-brick-row?) index (- index 1))))
      (translate-along-vector (the (full-brick ind) (face-center :rear))
			      (the (face-normal-vector :rear))
			      (half (the corrected-joint-width)))))

    (first-half-brick-center!
     ()
     (translate-along-vector (the (face-center :front))
			     (the (face-normal-vector :rear))
			     (half (half (the brick-length)))))
   
   (last-half-brick-center!
     ()
     (translate-along-vector (theo (the mortar-joint last) (face-center :rear))
			     (the (face-normal-vector :rear))
			     (half (half (the brick-length))))))
 			     
  
  :objects
  ((full-brick :type 'box
	       :sequence (:size (the number-of-full-bricks))
	       :center (if (= (the-child index) 0)
			   (the first-full-brick-center!)
			   (the (other-full-brick-center! (the-child index))))
	       :length (the brick-length)
	       :height (the brick-height)
	       :width (the brick-width))

   (half-brick :type 'box
	       :sequence (:size (if (the full-brick-row?) 0 2))
	       :center (if (= (the-child index) 0)
			   (the first-half-brick-center!)
			   (the last-half-brick-center!))
	       :length (half (the brick-length))
	       :height (the brick-height)
	       :width (the brick-width))

   (mortar-joint :type 'box
		 :sequence (:size (the number-of-mortar-joints))
		 :center (if (= (the-child index) 0)
			     (the first-joint-center!)
			  (the (other-joint-center! (the-child index))))   
		 :height (the brick-height)
		 :width (the brick-width)
		 :length (the corrected-joint-width))))










```

---

## building-hint-1.lisp - degrees-to-radians
Source: gornschool-training/t2/resources/source/building-hint-1.lisp
Type: tutorial

```
(defun degrees-to-radians (degrees)
  (div (* degrees pi) 180))

```

---

## building-hint-1.lisp - truss-assembly
Source: gornschool-training/t2/resources/source/building-hint-1.lisp
Type: tutorial

```
(define-object truss-assembly (box)
  :computed-slots
  ((length 200)
   (height 200)
   (width 200)
   (number-of-roof-trusses 2)
   (truss-angle 30)
   (beam-width 30)
   (beam-height 50)
   (wall-thickness 3)
   (material-density 7800)
   (truss-offsets (list 0 500))
   )
  :objects
  ((roof-truss :type 'truss
	       :sequence (:size (the number-of-roof-trusses))
	       :truss-length 1500
	       :center (translate-along-vector
			(translate-along-vector (the (edge-center :front :top))
						(the (face-normal-vector :top))
						(half (the-child height)))
			(the (face-normal-vector :rear))
			(nth (the-child index) (the truss-offsets)))
	       :orientation (alignment :rear (the (face-normal-vector :right))
				       :top (the (face-normal-vector :top))
				       )		
	       :pass-down (truss-angle
			   beam-width
			   beam-height
			   wall-thickness
			   material-density)))
  )


```

---

## building-hint-1.lisp - truss
Source: gornschool-training/t2/resources/source/building-hint-1.lisp
Type: tutorial

```
(define-object truss (box)
  :input-slots
  ((truss-length 2000)
   (truss-height nil)
   (truss-angle 30)

   (beam-width 30)
   (beam-height 50)
   (wall-thickness 3)

   (material-density 7800)
   (tonne-rate 450))

  :computed-slots
  ((length (the truss-length))
   (height (cond ((the truss-height)(the truss-height))
		 ((the truss-angle) (+ (* (half (the truss-length))
					  (tan (degrees-to-radians (the truss-angle))))
				       (the beam-height)))))
   (width (the beam-width))
  
   (truss-front-slope-vector (the (get-slope-vector! :front)))
   (truss-rear-slope-vector (the (get-slope-vector! :rear)))

   (front-slope-length (the (get-slope-length! :front)))
   (rear-slope-length (the (get-slope-length! :rear)))
   
   (front-slope-center (the (get-slope-center! :front)))
   (rear-slope-center (the (get-slope-center! :rear)))
   

   (beam-properties (mapsend (the children) :beam-properties))
   (total-mass (round-to-nearest
		(apply '+
		       (mapcar #'(lambda(a) (getf a :mass-kg))
			       (the beam-properties)))
		0.001))
   (total-cost (round-to-nearest
		(apply '+
		       (mapcar #'(lambda(a) (getf a :cost-gbp))
			       (the beam-properties)))
		0.01))
   )

  :functions
  ((get-slope-vector! (beam-side)
		      (let ((v-key (the (get-v-key! beam-side)))
			    (l-key (the (get-l-key! beam-side))))
		      (subtract-vectors (the vertical-beam (edge-center :rear v-key))
					(the lower-beam (edge-center l-key :top)))))
   (get-slope-length! (beam-side)
		      (let ((v-key (the (get-v-key! beam-side)))
			    (l-key (the (get-l-key! beam-side))))
			(3d-distance (the vertical-beam (edge-center :rear v-key))
				     (the lower-beam (edge-center l-key :top)))))
   (get-slope-center!
    (beam-side)
    (let ((pt (case beam-side
		(:front (the front-slope-construction-line center))
		(:rear  (the rear-slope-construction-line center))))
	  (norm-vector (case beam-side
			 (:front (the front-slope-beam (face-normal-vector :bottom)))
			 (:rear (the rear-slope-beam (face-normal-vector :bottom))))))
      (translate-along-vector pt
			      norm-vector
			      (half (the beam-height)))))
   (get-v-key! (beam-side)
	       (case beam-side
		 (:front :top)
		 (:rear :bottom)))
   (get-l-key! (beam-side)
	       (case beam-side
		 (:front :front)
		 (:rear :rear)))

   
   )
   
				       
  :objects
  ((lower-beam :type 'beam
	       :pass-down (:beam-height :beam-width :wall-thickness :material-density :tonne-rate)
	       :beam-length (the truss-length)
	       :center (translate-along-vector (the (face-center :bottom))
					       (the (face-normal-vector :top))
					       (half (the beam-height))))
   (vertical-beam :type 'beam
		  :pass-down (:beam-height :beam-width :wall-thickness :material-density :tonne-rate)
		  
		  :beam-length (- (the height) (the beam-height))
		  :orientation (alignment :rear (the (face-normal-vector :top))
					  :right (the (face-normal-vector :right)))  
		  :center (translate-along-vector (the lower-beam (face-center :top))
						  (the lower-beam (face-normal-vector :top))
						  (half (the-child beam-length))))
   (front-slope-beam :type 'beam
		     :pass-down (:beam-height :beam-width :wall-thickness :material-density :tonne-rate)
		     :beam-length (the front-slope-length)
		     :center (the front-slope-center)
		     :orientation (alignment :rear (the truss-front-slope-vector)
					     :right (the (face-normal-vector :right))))

   (rear-slope-beam :type 'beam
		     :pass-down (:beam-height :beam-width :wall-thickness :material-density :tonne-rate)
		     :beam-length (the rear-slope-length)
		     :center (the rear-slope-center)
		    :orientation (alignment :rear (the truss-rear-slope-vector)
					    :left (the (face-normal-vector :right))))
   
   )

  :hidden-objects
  ((front-slope-construction-line :type 'line
				  :start (the lower-beam (edge-center :front :top))
				  :end (the vertical-beam (edge-center :rear :top)))
   (rear-slope-construction-line :type 'line
				 :start (the lower-beam (edge-center :rear :top))
				 :end (the vertical-beam (edge-center :rear :bottom)))
   
   (front-mid-pt :type 'sphere
		 :display-controls (list :color :blue)
		 :radius 5
		 :center (the front-slope-construction-line center))
   (rear-mid-pt :type 'sphere
		:display-controls (list :color :blue)
		:center (the rear-slope-construction-line center)
		:radius 5)
   (pt-1 :type 'sphere
	 :radius 5
	 :display-controls (list :color :green)
	 :center (the lower-beam (edge-center :rear :top)))
   (pt-2 :type 'sphere
	 :radius 5
	 :display-controls (list :color :red)
	 :center (the vertical-beam (edge-center :rear :bottom)) )

   

   (vector-line :type 'vector-line
		:start-point (the rear-slope-construction-line center)
		:vector (the rear-slope-beam (face-normal-vector :bottom))
		:length 150)
   
   )))


```

---

## building-hint-1.lisp - vector-line
Source: gornschool-training/t2/resources/source/building-hint-1.lisp
Type: tutorial

```
(define-object vector-line (box)
  :input-slots
  ((start-point (make-point 0 0 0))
   (vector (make-vector 1 0 1))
   (length 50)
   (width 1))
  :computed-slots
  ((height (div (the length) 5)))
  :objects
  ((v-line :type 'line
	   :start (the start-point)
	   :display-controls (list :color :red)
	   :end (translate-along-vector (the start-point)
					(the vector)
					(the length)))
   (arrow :type 'cone
	  :radius-1 0
	  :radius-2 (div (the length) 50)
	  :length (div (the length) 5)
	  :display-controls (list :color :red)
	  :center (translate-along-vector (the v-line end)
					  (the vector)
					  (half (the-child length)))
	  :orientation (alignment :front (the vector)))))


```

---

## building-hint-1.lisp - beam
Source: gornschool-training/t2/resources/source/building-hint-1.lisp
Type: tutorial

```
(define-object beam (box)
  :input-slots
  ((beam-length 1000)
   (beam-width 40)
   (beam-height 50)
   (wall-thickness 2)
   (material-density 7800)
   (tonne-rate 500))

  :computed-slots
  ((length (the beam-length))
   (width (the beam-width))
   (height (the beam-height))

   (beam-volume (- (the outer volume) (the inner volume)))
   (beam-mass (* (div (the beam-volume) 1000000000) (the material-density)))
   (beam-cost (* (the tonne-rate) (div (the beam-mass) 1000)))
   (beam-properties (list :volume-mm3 (the beam-volume)
			  :mass-kg (round-to-nearest (the beam-mass) 0.01)
			  :cost-gbp (round-to-nearest (the beam-cost) 0.01)
			  :length-mm (the beam-length)
			  :width-mm (the beam-width)
			  :height-mm (the beam-height)
			  :thickness-mm (the wall-thickness))))
   
  :objects
  ((outer :type 'box
	  :length (the beam-length)
	  :width (the beam-width)
	  :height (the beam-height))

   (inner :type 'box
	  :length (the beam-length)
	  :width (- (the beam-width) (twice (the wall-thickness)))
	  :height (- (the beam-height) (twice (the wall-thickness)))))
  )








```

---

## truss-hint-1.lisp - header
Source: gornschool-training/t2/resources/source/truss-hint-1.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## truss-hint-1.lisp - truss
Source: gornschool-training/t2/resources/source/truss-hint-1.lisp
Type: tutorial

```
(define-object truss ()
  :objects
  ((lower-beam :type 'beam)
   (vertical-beam :type 'beam)
   (front-slope-beam :type 'beam)
   (rear-slope-beam :type 'beam)))


```

---

## truss-hint-1.lisp - beam
Source: gornschool-training/t2/resources/source/truss-hint-1.lisp
Type: tutorial

```
(define-object beam ()
  :objects
  ((outer :type 'box)
   (inner :type 'box)))

```

---

## using-lists.lisp - header
Source: gornschool-training/t2/resources/source/using-lists.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## using-lists.lisp - assembly-8
Source: gornschool-training/t2/resources/source/using-lists.lisp
Type: tutorial

```
(define-object assembly-8 (base-object)

  :computed-slots
  ((box-lengths (list 2 5 8))
   (box-centers (list (make-point 0 0 0)
		      (make-point 6 0 0)
		      (make-point 12 0 0)))
   (number-of-boxes (length (the box-lengths)))
   (box-volumes (list-elements (the my-box) (the-element volume)))
   (box-1-volume (nth 0 (the box-volumes))))

  :objects
  ((my-box :type 'box
	   :sequence (:size (the number-of-boxes))
	   :length (nth (the-child index) (the box-lengths))
	   :width 2
	   :height 1
	   :center (nth (the-child index) (the box-centers)))))

```

---

## truss-hint-7.lisp - header
Source: gornschool-training/t2/resources/source/truss-hint-7.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## truss-hint-7.lisp - degrees-to-radians
Source: gornschool-training/t2/resources/source/truss-hint-7.lisp
Type: tutorial

```
(defun degrees-to-radians (degrees)
  (div (* degrees pi) 180))


```

---

## truss-hint-7.lisp - truss
Source: gornschool-training/t2/resources/source/truss-hint-7.lisp
Type: tutorial

```
(define-object truss (box)
  :input-slots
  ((truss-length 2000)
   (truss-height 800)
   (truss-angle nil)

   (beam-width 50)
   (beam-height 50)
   (wall-thickness 3))

  :computed-slots
  ((length (the truss-length))
   (height (cond ((the truss-height)(the truss-height))
		 ((the truss-angle) (+ (* (half (the truss-length))
					  (tan (degrees-to-radians (the truss-angle))))
				       (the beam-height)))))
   (width (the beam-width))
  

   (truss-front-slope-vector (the (get-slope-vector! :front)))
   (truss-rear-slope-vector (the (get-slope-vector! :rear)))

   (front-slope-length (the (get-slope-length! :front)))
   (rear-slope-length (the (get-slope-length! :rear)))
   
   (front-slope-center (the (get-slope-center! :front)))
   (rear-slope-center (the (get-slope-center! :rear)))
   )

  :functions
  ((get-slope-vector! (beam-side)
		      (let ((v-key (the (get-v-key! beam-side)))
			    (l-key (the (get-l-key! beam-side))))
		      (subtract-vectors (the vertical-beam (edge-center :rear v-key))
					(the lower-beam (edge-center l-key :top)))))
   (get-slope-length! (beam-side)
		      (let ((v-key (the (get-v-key! beam-side)))
			    (l-key (the (get-l-key! beam-side))))
			(3d-distance (the vertical-beam (edge-center :rear v-key))
				     (the lower-beam (edge-center l-key :top)))))

   (get-slope-center! (beam-side)
		      (let ((pt (case beam-side
				  (:front (the front-slope-construction-line center))
				  (:rear  (the rear-slope-construction-line center))))
			    (norm-vector (case beam-side
					    (:front (the front-slope-beam (face-normal-vector :bottom)))
					    (:rear (the rear-slope-beam (face-normal-vector :bottom))))))
			(translate-along-vector pt
						norm-vector
						(half (the beam-height)))))
   (get-v-key! (beam-side)
	       (case beam-side
		 (:front :top)
		 (:rear :bottom)))
   (get-l-key! (beam-side)
	       (case beam-side
		 (:front :front)
		 (:rear :rear))))
   
				       
  :objects
  ((lower-beam :type 'beam
	       :beam-height (the beam-height)
	       :beam-width (the beam-width)
	       :beam-length (the truss-length)
	       :center (translate-along-vector (the (face-center :bottom))
					       (the (face-normal-vector :top))
					       (half (the beam-height))))
   (vertical-beam :type 'beam
		  :beam-length (- (the height) (the beam-height))
		  :beam-height (the beam-height)
		  :beam-width (the beam-width)
		  :orientation (alignment :rear (the (face-normal-vector :top))
					  :right (the (face-normal-vector :right)))
		  :center (translate-along-vector (the lower-beam (face-center :top))
						  (the lower-beam (face-normal-vector :top))
						  (half (the-child beam-length))))
   (front-slope-beam :type 'beam
		     :beam-length (the front-slope-length)
		     :beam-height (the beam-height)
		     :beam-width (the beam-width)
		     :center (the front-slope-center)
		     :orientation (alignment :rear (the truss-front-slope-vector)
					     :right (the (face-normal-vector :right))))

   (rear-slope-beam :type 'beam
		     :beam-length (the rear-slope-length)
		     :beam-height (the beam-height)
		     :beam-width (the beam-width)
		     :center (the rear-slope-center)
		    :orientation (alignment :rear (the truss-rear-slope-vector)
					    :left (the (face-normal-vector :right)))))

  :hidden-objects
  ((front-slope-construction-line :type 'line
				  :start (the lower-beam (edge-center :front :top))
				  :end (the vertical-beam (edge-center :rear :top)))
   (rear-slope-construction-line :type 'line
				 :start (the lower-beam (edge-center :rear :top))
				 :end (the vertical-beam (edge-center :rear :bottom)))
   (front-mid-pt :type 'sphere
		 :display-controls (list :color :blue)
		 :radius 5
		 :center (the front-slope-construction-line center))
   (rear-mid-pt :type 'sphere
		:display-controls (list :color :blue)
		:center (the rear-slope-construction-line center)
		:radius 5)
   (pt-1 :type 'sphere
	 :radius 5
	 :display-controls (list :color :green)
	 :center (the lower-beam (edge-center :rear :top)))
   (pt-2 :type 'sphere
	 :radius 5
	 :display-controls (list :color :red)
	 :center (the vertical-beam (edge-center :rear :top)) )
   (vector-line :type 'vector-line
		:start-point (the pt-1 center)
		:vector (the truss-rear-slope-vector)
		:length 150)))


```

---

## truss-hint-7.lisp - vector-line
Source: gornschool-training/t2/resources/source/truss-hint-7.lisp
Type: tutorial

```
(define-object vector-line (box)
  :input-slots
  ((start-point (make-point 0 0 0))
   (vector (make-vector 1 0 1))
   (length 50)
   (width 1))
  :computed-slots
  ((height (div (the length) 5)))
  :objects
  ((v-line :type 'line
	   :start (the start-point)
	   :display-controls (list :color :red)
	   :end (translate-along-vector (the start-point)
					(the vector)
					(the length)))
   (arrow :type 'cone
	  :radius-1 0
	  :radius-2 (div (the length) 50)
	  :length (div (the length) 5)
	  :display-controls (list :color :red)
	  :center (translate-along-vector (the v-line end)
					  (the vector)
					  (half (the-child length)))
	  :orientation (alignment :front (the vector)))))


```

---

## truss-hint-7.lisp - beam
Source: gornschool-training/t2/resources/source/truss-hint-7.lisp
Type: tutorial

```
(define-object beam (box)
  :input-slots
  ((beam-length 1000)
   (beam-width 40)
   (beam-height 50)
   (wall-thickness 2)
   (material-density 7800)
   (tonne-rate 500))

  :computed-slots
  ((length (the beam-length))
   (width (the beam-width))
   (height (the beam-height))

   (beam-volume (- (the outer volume) (the inner volume)))
   (beam-mass (* (div (the beam-volume) 1000000000) (the material-density)))
   (beam-cost (* (the tonne-rate) (div (the beam-mass) 1000)))
   (beam-properties (list :volume-mm3 (the beam-volume)
			  :mass-kg (round-to-nearest (the beam-mass) 0.01)
			  :cost-gbp (round-to-nearest (the beam-cost) 0.01)
			  :length-mm (the beam-length)
			  :width-mm (the beam-width)
			  :height-mm (the beam-height)
			  :thickness-mm (the wall-thickness))))
   
  :objects
  ((outer :type 'box
	  :length (the beam-length)
	  :width (the beam-width)
	  :height (the beam-height))

   (inner :type 'box
	  :length (the beam-length)
	  :width (- (the beam-width) (twice (the wall-thickness)))
	  :height (- (the beam-height) (twice (the wall-thickness)))))
  )

```

---

## building-hint-6.lisp - header
Source: gornschool-training/t2/resources/source/building-hint-6.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## building-hint-6.lisp - building-bom
Source: gornschool-training/t2/resources/source/building-hint-6.lisp
Type: tutorial

```
(defun building-bom (&key (nominal-height 3000)
		       (nominal-width 3000)
		       (nominal-length 3000)
		       (roof-angle 30))
  (let ((obj (make-object 'building
			  :nominal-height nominal-height
			  :nominal-width nominal-width
			  :nominal-length nominal-length
			  :truss-angle roof-angle)))
    (theo obj bom-formatted)))


```

---

## building-hint-6.lisp - building
Source: gornschool-training/t2/resources/source/building-hint-6.lisp
Type: tutorial

```
(define-object building (box)
  :input-slots
  ((nominal-height 3000)
   (nominal-width 3000)
   (nominal-length 4000)
   (brick-height 45)
   (brick-length 180)
   (brick-width 90)
   (mortar-joint-width 10)
   (truss-angle 30)
   (beam-width 40)
   (beam-height 50)
   (wall-thickness 3)
   (material-density 7800)
   (roof-overhang 50)
   (cladding-thickness 10)
   (max-beam-spacing 1500))

  :computed-slots
  ((length (the left-wall length))
   (width (the rear-wall length))
   (height (+ (the left-wall height) (the (roof-truss 0) height)))

   (number-of-roof-trusses (let ((trusses (ceiling (the left-wall length) 1500)))
			     (max trusses 2)))

   (truss-spacing (div (- (the left-wall length) (the beam-width))
		       (- (the number-of-roof-trusses) 1)))
   (truss-offsets (let ((res nil))
		    (dotimes (n (the number-of-roof-trusses) (nreverse res))
		      (push (+ (half (the beam-width))
			       (* n (the truss-spacing))) res))))

   (roof-length (+ (the left-wall length) (twice (the roof-overhang))))
   (roof-width (the cladding-thickness))
   (roof-height (let ((apex (the (roof-truss 0) apex-point))
		      (gutter (the (roof-truss 0) front-gutter-point)))
		  (+ (3d-distance apex gutter) (the roof-overhang))))

   ;; building properties
   (walls (remove nil (mapcar #'(lambda(a) (when (typep a 'wall) a)) (the children))))
   (full-bricks (apply '+ (mapsend (the walls) :full-bricks)))
   (half-bricks (apply '+ (mapsend (the walls) :half-bricks)))
   (mortar-volume (apply '+ (mapsend (the walls) :mortar-volume)))
   (cladding-dimensions (list :length (the roof-length)
			      :width (the roof-height)))
   (beam-properties (the (roof-truss 0) beam-properties))
   (beam-qty-by-size (let ((res nil))
		       (dolist (plis (the beam-properties) )
			 (let* ((trusses (the number-of-roof-trusses))
				(l (getf plis :length-mm))
				(p (position l res :key #'(lambda(a) (getf a :length-mm))))
				(qty (when p (getf (nth p res) :qty))))
			   (if p (setf (getf (nth p res) :qty) (+ qty trusses))
			       (setq res (append (list (list :length-mm l :qty trusses)) res)))))
		       (safe-sort res '< :key #'(lambda(a) (getf a :length-mm)))))
		       

   (roof-truss-mass (* (apply '+ (mapcar #'(lambda(a) (getf a :mass-kg))
					 (the beam-properties)))
		       (the number-of-roof-trusses)))

   (building-materials (list :full-bricks (the full-bricks)
			     :half-bricks (the half-bricks)
			     :mortar-volume-m3 (div (the mortar-volume) 1000000000)
			     :beams (the beam-qty-by-size)
			     :roof-cladding (append (the cladding-dimensions) (list :qty 2))))

  (bom-formatted (let* ((bom (the building-materials))
			(cladding (getf bom :roof-cladding))
			(bricks (format nil "Bricks~%======~%  Full Bricks ~a~%  Half Bricks ~a~%" 
					(getf bom :full-bricks) 
					(getf bom :half-bricks)))
			(mortar (format nil "Mortar~%======~%  Volume ~,3f m^3~%" 
					(getf bom :mortar-volume-m3)))
			(l (round-to-nearest (getf cladding :length) 1))
			(w (round-to-nearest (getf cladding :width) 1))
			(roof (format nil "Roof Cladding~%======~%  Qty ~a~%  Dimensions (L x W x T) ~d x ~d x ~d~%" 
				      (getf cladding :qty)
				      l w (the cladding-thickness)))
			(beams (getf (the building-materials) :beams))
			(beams-list (flatten
				     (mapcar #'(lambda(a)
						 (list (getf a :qty) (round-to-nearest (getf a :length-mm) 1)))
					     beams)))
			
			(beams-header (format nil "Beams~%=====~%  Section (H x W x T) ~a x ~a x ~a~%"
						 (the beam-height) (the beam-width) (the wall-thickness)))
			(beam-lengths (format nil "~{  Qty ~a Length ~a~%~}" beams-list)))
		   (format nil "~@{~a~}" bricks mortar roof beams-header beam-lengths))) 
   
		
   )

  :functions
  ((get-roof-mid-point! (first-gutter last-gutter last-index)
		       (let*((mid-gutter (midpoint first-gutter last-gutter))
			     (first-apex (the (roof-truss 0) apex-point))
			     (last-apex (the (roof-truss last-index) apex-point))
			     (mid-apex (midpoint first-apex last-apex))
			     (vec (subtract-vectors mid-gutter mid-apex))
			     (mid-edge (translate-along-vector mid-gutter vec (the roof-overhang))))
			 (midpoint mid-apex mid-edge))) )
  
  :objects
  ((left-wall :type 'wall
	      :pass-down (brick-height
			  brick-length
			  brick-width
			  mortar-joint-width)
	      :center (translate-along-vector
		       (translate-along-vector (the (edge-center :bottom :left))
					       (the (face-normal-vector :right))
					       (half (the-child width)))
		       (the (face-normal-vector :top))
		       (half (the-child height)))
	      :wall-length (the nominal-length)
	      :wall-height (the nominal-height))

   (right-wall :type 'wall
	       :pass-down (brick-height
			   brick-length
			   brick-width
			   mortar-joint-width)
	       :center (translate-along-vector
			(translate-along-vector (the (edge-center :bottom :right))
						(the (face-normal-vector :left))
						(half (the-child width)))
			(the (face-normal-vector :top))
			(half (the-child height)))
	       :wall-length (the nominal-length)
	       :wall-height (the nominal-height))

   (rear-wall :type 'wall
	      :pass-down (brick-height
			  brick-length
			  brick-width
			  mortar-joint-width)
	      :center (translate-along-vector
			(translate-along-vector (the (edge-center :bottom :rear))
						(the (face-normal-vector :front))
						(half (the-child width)))
			(the (face-normal-vector :top))
			(half (the-child height)))
	      :orientation (alignment :rear (the (face-normal-vector :right)))
	      :wall-length (the nominal-width)
	      :wall-height (the nominal-height))

   (roof-truss :type 'truss
	       :sequence (:size (the number-of-roof-trusses))
	       :truss-length (the rear-wall length)
	       :center (translate-along-vector
			(translate-along-vector (the (edge-center :front :top))
						(the (face-normal-vector :bottom))
						(half (the-child height)))
			(the (face-normal-vector :rear))
			(nth (the-child index) (the truss-offsets)))
	       :orientation (alignment :rear (the (face-normal-vector :right))
				       )		
	       :pass-down (truss-angle
			   beam-width
			   beam-height
			   wall-thickness
			   material-density))
   
   (roof-cladding-left
    :type 'box
    :length (the roof-length)
    :height (the roof-height)
    :width (the cladding-thickness)
    :orientation (alignment :left (the (roof-truss 0) front-slope-normal))
    :center (let* ((last-index (- (the number-of-roof-trusses) 1))
		   (first-gutter (the (roof-truss 0) front-gutter-point))
		   (last-gutter (the (roof-truss last-index) front-gutter-point))
		   (mid-ctr (the (get-roof-mid-point! first-gutter last-gutter last-index))))
	      (translate-along-vector mid-ctr
				      (the (roof-truss 0) front-slope-normal)
				      (half (the cladding-thickness)))))
   
   (roof-cladding-right :type 'box
			:length (the roof-length)
			:height (the roof-height)
			:width (the cladding-thickness)
			:orientation (alignment :left (the (roof-truss 0) rear-slope-normal))
			 :center (let* ((last-index (- (the number-of-roof-trusses) 1))
					(first-gutter (the (roof-truss 0) rear-gutter-point))
					(last-gutter (the (roof-truss last-index) rear-gutter-point))
					(mid-ctr (the (get-roof-mid-point! first-gutter last-gutter last-index))))
				 (translate-along-vector mid-ctr
							 (the (roof-truss 0) rear-slope-normal)
							 (half (the cladding-thickness)))))
   )

  
		       
  )



```

---

## building-hint-6.lisp - wall
Source: gornschool-training/t2/resources/source/building-hint-6.lisp
Type: tutorial

```
(define-object wall(box)
  :input-slots
  ((brick-height 45)
   (brick-length 180)
   (brick-width 90)
   (mortar-joint-width 10)
   (wall-length 3700)
   (wall-height 900)
   (first-row :start-full)
   (front-edge :full)
   (rear-edge :full))

  :computed-slots
  ((row-height (+ (the brick-height) (the mortar-joint-width)))
   (number-of-rows (round-to-nearest (div (the wall-height) (the row-height)) 1))
   (actual-wall-height (* (the row-height) (the number-of-rows)))
   
   ;; for the wall-length we need the number of full bricks
   ;; if there are n full bricks then there will be (n-1) mortar joints
   ;; so n*brick-length + n-1*mortar-joint-width = overall-length
   ;; or n(brick-length + mortar-join-width) - mortar-joint-width = overall-length
   ;; or n = (overall-length - mortar-joint-width)/(brick-length + mortar-joint-width)
   (number-of-bricks (round-to-nearest (div (- (the wall-length) (the mortar-joint-width))
					    (+ (the brick-length) (the mortar-joint-width)))
				       1))
   (actual-wall-length (+ (* (the number-of-bricks) (the brick-length))
			  (* (- (the number-of-bricks) 1) (the mortar-joint-width))))

   ;; box inputs - gives the wall bounding box
   (height (the actual-wall-height))
   (width (the brick-width))
   (length (the actual-wall-length))

   (full-bricks (apply '+ (list-elements (the row) (the-element full-bricks))))
   (half-bricks (apply '+ (list-elements (the row) (the-element half-bricks))))
   (mortar-volume (apply '+ (list-elements (the row) (the-element mortar-volume))))
   (mortar-density 2162)
   (mortar-mass (* (the mortar-density) (div (the mortar-volume) 1000000000)))
   )
   
     
  :objects
  ((row :type 'row
     :sequence (:size (the number-of-rows))
     :center (translate-along-vector (the (face-center :bottom))
				     (the (face-normal-vector :top))
				     (+ (half (the-child height))
					(* (the-child index) (the-child height))))
     :height (+ (the brick-height) (the mortar-joint-width))
     :full-bricks-per-row (the number-of-bricks)
     :pass-down (brick-height
		 brick-length
		 brick-width
		 mortar-joint-width
		 length
		 width
		 first-row
		 front-edge
		 rear-edge))))


```

---

## building-hint-6.lisp - row
Source: gornschool-training/t2/resources/source/building-hint-6.lisp
Type: tutorial

```
(define-object row (box)
  :input-slots
  (full-bricks-per-row
   brick-height
   brick-length
   brick-width
   mortar-joint-width
   first-row
   front-edge
   rear-edge )

  :computed-slots
  ((full-brick-row? (cond ((eq (the first-row) :start-full)
			   (or (zerop (the index)) (evenp (the index))))
			  ((eq (the first-row) :start-half)
			   (not (or (zerop (the index)) (evenp (the index)))))))
		    
   (full-bricks (the bricks-and-mortar full-bricks))
   (half-bricks (the bricks-and-mortar half-bricks))
   (mortar-volume (+ (the bricks-and-mortar mortar-joint-volume)
		     (the mortar-bed volume))))

  :objects
  ((bricks-and-mortar :type 'bricks-and-mortar
		      :height (the brick-height)
		      :center (translate-along-vector (the mortar-bed (face-center :top))
						      (the mortar-bed (face-normal-vector :top))
						      (half (the-child height)))
		      :pass-down (width
				  length
				  full-brick-row?
				  brick-height
				  brick-length
				  brick-width
				  mortar-joint-width
				  full-bricks-per-row
				  front-edge
				  rear-edge))
   (mortar-bed :type 'box
	       :height (the mortar-joint-width)
	       :center (translate-along-vector (the (face-center :bottom))
					       (the (face-normal-vector :top))
					       (half (the-child height)))
	       :pass-down (width
			   length))))


```

---

## building-hint-6.lisp - bricks-and-mortar
Source: gornschool-training/t2/resources/source/building-hint-6.lisp
Type: tutorial

```
(define-object bricks-and-mortar (box)
  :input-slots
  (full-brick-row?
   full-bricks-per-row
   brick-height
   brick-length
   brick-width
   mortar-joint-width
   front-edge
   rear-edge)

  :computed-slots
  ((first-full-brick-start-point (if (the full-brick-row?)
				     (the (face-center :front))
				     (the (mortar-joint 0) (face-center :rear))))
   
   (first-mortar-joint-start-point (cond ((the full-brick-row?) (the (full-brick 0) (face-center :rear)))
					 ((eq (the front-edge) :full) (the (half-brick 0) (face-center :rear)))
					 ((eq (the front-edge) :keyed) (translate-along-vector (the (face-center :front))
											       (the (face-normal-vector :rear))
											       (half (the brick-length))))))

   (number-of-full-bricks (if (the full-brick-row?)
			      (the full-bricks-per-row)
			      (- (the full-bricks-per-row) 1)))

   (number-of-half-bricks (cond ((the full-brick-row?) 0)
				((and (eq (the front-edge) :full)(eq (the rear-edge) :full)) 2)
				((and (eq (the front-edge) :keyed) (eq (the rear-edge) :full)) 1)
				((and (eq (the front-edge) :full) (eq (the rear-edge) :keyed)) 1)
				((and (eq (the front-edge) :keyed) (eq (the rear-edge) :keyed)) 0)))

   ;; whether or not the ends are :full or :keyed, the number of mortar joints remains the same since the mortar joint
   ;; when it is :keyed is used to connect to the full brick of the other wall
   (number-of-mortar-joints (if (the full-brick-row?)
				(- (the number-of-full-bricks) 1)
				(+ (the number-of-full-bricks) 1)))

   ;; if it isn't a full brick row then there will be an extra joint because one
   ;; full brick is replaced with 2 half bricks so without correcting the
   ;; mortar-joint-width the ends of a full brick row and one starting and
   ;; finishing with half bricks won't align. So we need to correct
   ;; the mortar-joint-width
   (corrected-joint-width (if (the full-brick-row?)
				     (the mortar-joint-width)
				     (let ((total-gap (* (- (the number-of-mortar-joints) 1)
									(the mortar-joint-width))))
				       (div total-gap (the number-of-mortar-joints)))))
   ;; collating the output. We could do this analytically, but for this example we'll use the geometry
   (full-bricks (length (list-elements (the full-brick))))
   (half-bricks (length (list-elements (the half-brick))))
   (mortar-joint-volume (apply '+ (list-elements (the mortar-joint)
						 (the-element volume))))
   )
  
  :functions
  ((first-full-brick-center!
    ()
    (translate-along-vector (the first-full-brick-start-point)
			    (the (face-normal-vector :rear))
			    (half (the brick-length))))
   
   (other-full-brick-center!
    (index)
    ;; if its a full brick row, (full-brick 1) is positioned relative to (joint 0)
    ;; if its a half brick row, (full-brick 1) is positioned relative to (joint 1)
    (let ((ind (if (the full-brick-row?) (- index 1) index)))
	  (translate-along-vector (the (mortar-joint ind) (face-center :rear))
				  (the (face-normal-vector :rear))
				  (half (the brick-length)))))
   
    (first-joint-center!
     ()
     (translate-along-vector (the first-mortar-joint-start-point)
			     (the (face-normal-vector :rear))
			     (half (the corrected-joint-width))))
   (other-joint-center!
    (index)
    ;; if its a full brick row, (joint 1) is positioned relative to (full-brick 1)
    ;; if its a half brick row, (joint 1) is positioned relative to (full-brick 0)
    (let ((ind (if (the full-brick-row?) index (- index 1))))
      (translate-along-vector (the (full-brick ind) (face-center :rear))
			      (the (face-normal-vector :rear))
			      (half (the corrected-joint-width)))))

    (first-half-brick-center!
     ()
     (translate-along-vector (the (face-center :front))
			     (the (face-normal-vector :rear))
			     (half (half (the brick-length)))))
   
   (last-half-brick-center!
     ()
     (translate-along-vector (theo (the mortar-joint last) (face-center :rear))
			     (the (face-normal-vector :rear))
			     (half (half (the brick-length))))))
 			     
  
  :objects
  ((full-brick :type 'box
	       :sequence (:size (the number-of-full-bricks))
	       :center (if (= (the-child index) 0)
			   (the first-full-brick-center!)
			   (the (other-full-brick-center! (the-child index))))
	       :length (the brick-length)
	       :height (the brick-height)
	       :width (the brick-width))

   (half-brick :type 'box
	       :sequence (:size (the number-of-half-bricks))
	       :center (cond ((and (= (the-child index) 0)
				   (eq (the front-edge) :full)) (the first-half-brick-center!))
			     ((and (= (the-child index) 0)
				   (eq (the front-edge) :keyed)
				   (eq (the rear-edge) :full)) (the last-half-brick-center!))
			     ((eq (the rear-edge) :full) (the last-half-brick-center!)))
	       :length (half (the brick-length))
	       :height (the brick-height)
	       :width (the brick-width))

   (mortar-joint :type 'box
		 :sequence (:size (the number-of-mortar-joints))
		 :center (if (= (the-child index) 0)
			     (the first-joint-center!)
			  (the (other-joint-center! (the-child index))))   
		 :height (the brick-height)
		 :width (the brick-width)
		 :length (the corrected-joint-width))))



```

---

## building-hint-6.lisp - degrees-to-radians
Source: gornschool-training/t2/resources/source/building-hint-6.lisp
Type: tutorial

```
(defun degrees-to-radians (degrees)
  (div (* degrees pi) 180))



```

---

## building-hint-6.lisp - truss
Source: gornschool-training/t2/resources/source/building-hint-6.lisp
Type: tutorial

```
(define-object truss (box)
  :input-slots
  ((truss-length 2000)
   (truss-height nil)
   (truss-angle 30)

   (beam-width 30)
   (beam-height 50)
   (wall-thickness 3)

   (material-density 7800)
   (tonne-rate 450))

  :computed-slots
  ((length (the truss-length))
   (height (cond ((the truss-height)(the truss-height))
		 ((the truss-angle) (+ (* (half (the truss-length))
					  (tan (degrees-to-radians (the truss-angle))))
				       (the beam-height)))))
   (width (the beam-width))
  
   (truss-front-slope-vector (the (get-slope-vector! :front)))
   (truss-rear-slope-vector (the (get-slope-vector! :rear)))

   (front-slope-length (the (get-slope-length! :front)))
   (rear-slope-length (the (get-slope-length! :rear)))
   
   (front-slope-center (the (get-slope-center! :front)))
   (rear-slope-center (the (get-slope-center! :rear)))
   

   (beam-properties (mapsend (the children) :beam-properties))
   (total-mass (round-to-nearest
		(apply '+
		       (mapcar #'(lambda(a) (getf a :mass-kg))
			       (the beam-properties)))
		0.001))
   (total-cost (round-to-nearest
		(apply '+
		       (mapcar #'(lambda(a) (getf a :cost-gbp))
			       (the beam-properties)))
		0.01))

   ;; messages to support roof cladding sizing and positioning
   (apex-point (inter-line-plane (the rear-slope-construction-line end)
			   (the truss-rear-slope-vector)
			   (the lower-beam center)
				 (the (face-normal-vector :rear))))
   (front-gutter-point (the front-slope-construction-line start))
   (rear-gutter-point (the rear-slope-construction-line start))
   (front-slope-normal (the front-slope-beam (face-normal-vector :top)))
   (rear-slope-normal (the rear-slope-beam (face-normal-vector :top)))
   )

  :functions
  ((get-slope-vector! (beam-side)
		      (let ((v-key (the (get-v-key! beam-side)))
			    (l-key (the (get-l-key! beam-side))))
		      (subtract-vectors (the vertical-beam (edge-center :rear v-key))
					(the lower-beam (edge-center l-key :top)))))
   (get-slope-length! (beam-side)
		      (let ((v-key (the (get-v-key! beam-side)))
			    (l-key (the (get-l-key! beam-side))))
			(3d-distance (the vertical-beam (edge-center :rear v-key))
				     (the lower-beam (edge-center l-key :top)))))
   (get-slope-center!
    (beam-side)
    (let ((pt (case beam-side
		(:front (the front-slope-construction-line center))
		(:rear  (the rear-slope-construction-line center))))
	  (norm-vector (case beam-side
			 (:front (the front-slope-beam (face-normal-vector :bottom)))
			 (:rear (the rear-slope-beam (face-normal-vector :bottom))))))
      (translate-along-vector pt
			      norm-vector
			      (half (the beam-height)))))
   (get-v-key! (beam-side)
	       (case beam-side
		 (:front :top)
		 (:rear :bottom)))
   (get-l-key! (beam-side)
	       (case beam-side
		 (:front :front)
		 (:rear :rear)))

   
   )
   
  
  
  :objects
  ((lower-beam :type 'beam
	       :pass-down (:beam-height :beam-width :wall-thickness :material-density :tonne-rate)
	       :beam-length (the truss-length)
	       :center (translate-along-vector (the (face-center :bottom))
					       (the (face-normal-vector :top))
					       (half (the beam-height))))
   (vertical-beam :type 'beam
		  :pass-down (:beam-height :beam-width :wall-thickness :material-density :tonne-rate)
		  
		  :beam-length (- (the height) (the beam-height))
		  :orientation (alignment :rear (the (face-normal-vector :top))
					  :right (the (face-normal-vector :right)))  
		  :center (translate-along-vector (the lower-beam (face-center :top))
						  (the lower-beam (face-normal-vector :top))
						  (half (the-child beam-length))))
   (front-slope-beam :type 'beam
		     :pass-down (:beam-height :beam-width :wall-thickness :material-density :tonne-rate)
		     :beam-length (the front-slope-length)
		     :center (the front-slope-center)
		     :orientation (alignment :rear (the truss-front-slope-vector)
					     :right (the (face-normal-vector :right))))

   (rear-slope-beam :type 'beam
		    :pass-down (:beam-height :beam-width :wall-thickness :material-density :tonne-rate)
		    :beam-length (the rear-slope-length)
		    :center (the rear-slope-center)
		    :orientation (alignment :rear (the truss-rear-slope-vector)
					    :left (the (face-normal-vector :right))))
   
   )

  :hidden-objects
  ((apex-pt :type 'sphere
	    :radius 5
	    :display-controls (list :color :green)
	    :center (the apex-point))
   (front-slope-construction-line :type 'line
				  :start (the lower-beam (edge-center :front :top))
				  :end (the vertical-beam (edge-center :rear :top)))
   (rear-slope-construction-line :type 'line
				 :start (the lower-beam (edge-center :rear :top))
				 :end (the vertical-beam (edge-center :rear :bottom)))
   
   (front-mid-pt :type 'sphere
		 :display-controls (list :color :blue)
		 :radius 5
		 :center (the front-slope-construction-line center))
   (rear-mid-pt :type 'sphere
		:display-controls (list :color :blue)
		:center (the rear-slope-construction-line center)
		:radius 5)
   (pt-1 :type 'sphere
	 :radius 5
	 :display-controls (list :color :green)
	 :center (the lower-beam (edge-center :rear :top)))
   (pt-2 :type 'sphere
	 :radius 5
	 :display-controls (list :color :red)
	 :center (the vertical-beam (edge-center :rear :bottom)) )

   

   (vector-line :type 'vector-line
		:start-point (the rear-slope-construction-line center)
		:vector (the rear-slope-beam (face-normal-vector :bottom))
		:length 150)
   
   ))


```

---

## building-hint-6.lisp - vector-line
Source: gornschool-training/t2/resources/source/building-hint-6.lisp
Type: tutorial

```
(define-object vector-line (box)
  :input-slots
  ((start-point (make-point 0 0 0))
   (vector (make-vector 1 0 1))
   (length 50)
   (width 1))
  :computed-slots
  ((height (div (the length) 5)))
  :objects
  ((v-line :type 'line
	   :start (the start-point)
	   :display-controls (list :color :red)
	   :end (translate-along-vector (the start-point)
					(the vector)
					(the length)))
   (arrow :type 'cone
	  :radius-1 0
	  :radius-2 (div (the length) 50)
	  :length (div (the length) 5)
	  :display-controls (list :color :red)
	  :center (translate-along-vector (the v-line end)
					  (the vector)
					  (half (the-child length)))
	  :orientation (alignment :front (the vector)))))

			 
		  

```

---

## building-hint-6.lisp - beam
Source: gornschool-training/t2/resources/source/building-hint-6.lisp
Type: tutorial

```
(define-object beam (box)
  :input-slots
  ((beam-length 1000)
   (beam-width 40)
   (beam-height 50)
   (wall-thickness 2)
   (material-density 7800)
   (tonne-rate 500))

  :computed-slots
  ((length (the beam-length))
   (width (the beam-width))
   (height (the beam-height))

   (beam-volume (- (the outer volume) (the inner volume)))
   (beam-mass (* (div (the beam-volume) 1000000000) (the material-density)))
   (beam-cost (* (the tonne-rate) (div (the beam-mass) 1000)))
   (beam-properties (list :volume-mm3 (the beam-volume)
			  :mass-kg (round-to-nearest (the beam-mass) 0.01)
			  :cost-gbp (round-to-nearest (the beam-cost) 0.01)
			  :length-mm (the beam-length)
			  :width-mm (the beam-width)
			  :height-mm (the beam-height)
			  :thickness-mm (the wall-thickness))))
   
  :objects
  ((outer :type 'box
	  :length (the beam-length)
	  :width (the beam-width)
	  :height (the beam-height))

   (inner :type 'box
	  :length (the beam-length)
	  :width (- (the beam-width) (twice (the wall-thickness)))
	  :height (- (the beam-height) (twice (the wall-thickness)))))
  )










```

---

## more-on-lists.lisp - header
Source: gornschool-training/t2/resources/source/more-on-lists.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## more-on-lists.lisp - subseq-safe
Source: gornschool-training/t2/resources/source/more-on-lists.lisp
Type: tutorial

```
(defun subseq-safe (sequence start &optional end)
  ;; if either the start or end inputs to subseq are more than the length of the list, then subseq will error
  ;; also subseq is only valid for sequences, so strings and lists
  ;; if any of the cases that would cause subseq to error are encountered, subseq-safe returns nil, otherwise it
  ;; returns the value subseq would normally return
  (when (or (stringp sequence) (listp sequence))
    (cond ((>= start (length sequence)) nil)
	  ((and end (> end (length sequence))) (subseq sequence start))
	  (t (subseq sequence start end)))))


```

---

## more-on-lists.lisp - more-on-lists
Source: gornschool-training/t2/resources/source/more-on-lists.lisp
Type: tutorial

```
(define-object more-on-lists (base-object)

  :computed-slots
  ((my-number-list (list 1 2 3 4))
   (my-number-list-1 (list 1 2 3 4))
   (my-string-list (list "peter" "paul" "mike" "john"))
   (my-plist (list :uk "London" :france "Paris" :belgium "Brussels"))
   
   ;; Length of a list
   (length-1 (length (the my-number-list)))    ;;; 4
   (length-2 (length nil))                     ;;; 0
   


   ;; member of a list
   (member-1 (member 2 (the my-number-list)))                           ;;; (2 3 4)
   (member-2 (member "paul" (the my-string-list)))                      ;;; returns nil because the default test for equality is eql
                                                                        ;;; use the :test input to specify what test for equality is to be used
   (member-2a (member "paul" (the my-string-list) :test 'string-equal)) ;; ("paul" "mike" "john") (or 'string=)

   ;; parts of lists
   (part-1 (subseq (the my-number-list) 1))    ;;; (2 3 4) anything from the first element onwards is retained
   (part-2 (subseq (the my-number-list) 0 3))  ;;; (1 2 3) anything beyond the 3rd element is removed
   ;; if you trim beyond the end of the list an error is generated
   (part-3 (ignore-errors (subseq (the my-number-list) 0 10))) ;;; NIL
					                       ;;; #<SIMPLE-ERROR #x210376DFFD>



   ;; removing elements from lists
   (remove-1 (remove 3 (the my-number-list)))                           ;;; (1 2 4)
   (remove-2 (remove "mike" (the my-string-list)))                      ;;; ("peter" "paul" "mike" "john") same as member, we have to specify the correct :test
   (remove-3 (remove "mike" (the my-string-list) :test 'string-equal))  ;;; ("peter" "paul" "john")

   ;; removing duplicates
   (remove-duplicates-1 (remove-duplicates (list 1 2 3 4 2 3)))                              ;;; (1 4 2 3)

   ;; we can also define what we mean by a duplicate, for example when working on float numbers
   (remove-duplicates-2 (remove-duplicates (list 15.0 112.0 5.1 77.8 15.1)
					       :test #'(lambda (a b) (near-to? a b 0.5))))  ;;; (112.0 5.1 77.8 15.1)
   
   (list-remove-duplicates-3 (remove-duplicates (list "string-1" "string-2" "string-3" "string-1" "string-3")))
                                                                                            ;;; ("string-1" "string-2" "string-3" "string-1" "string-3")
   
   (list-remove-duplicates-4 (remove-duplicates (list "string-1" "string-2" "string-3" "string-1" "string-3")
						:test #'(lambda (a b) (string-equal a b)))) ;; ("string-2" "string-1" "string-3")

   ;; sorting
   (sort-1 (sort  (the my-number-list-1) #'>)) ;;; (4 3 2 1) BUT if we then evaluate (the my-number-list-1) is returns (1).
	                                      ;;; This is because sort is DESTRUCTIVE - it modifies the supplied list
	  
   (sort-2 (safe-sort (the my-number-list) #'>))              ;;; (4 3 2 1) and (the my-number-list) is unchanged	   
   (sort-3 (safe-sort (the my-string-list) #'string-lessp))   ;;; ("john" "mike" "paul" "peter")	   
   (sort-4 (safe-sort (the my-string-list) #'string<))	      ;;; ("john" "mike" "paul" "peter")

   
   ;; flattening lists
   (flatten-1 (flatten (list 1 2 (list 3 4 (list 5 6))))) ;;; (1 2 3 4 5 6) - conceptually it just removes all of the parens, no matter how deeply nested a list is
   (flatten-2 (flatten (list 1 2 3 nil 4 5)))             ;;; (1 2 3 4 5) - flatten removes nil because nil is an empty list

  

   
   
   )

  
   
)



```

---

