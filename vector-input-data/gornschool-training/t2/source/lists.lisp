(in-package :training-2)

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


   (code-1 (list "(define-object assembly-8 (base-object)"
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
  

