(in-package :training-2)

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
