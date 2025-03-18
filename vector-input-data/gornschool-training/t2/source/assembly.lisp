(in-package :training-2)

(defparameter *publish-prefix* "t2")

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




