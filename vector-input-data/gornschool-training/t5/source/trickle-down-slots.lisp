(in-package :training-5)

(define-object trickle-down-slots (base-training-sheet)
  :input-slots
  (getting-start-url
   define-object-url
   wall-example-url)
  
  :computed-slots
  ((index-words (list ":trickle-down-slots" ":defaulting"))
     (code-1 (list "(define-object top-level-1 (base-object)"
		 "  :computed-slots"
		 "  ((my-slot-1 5))"
		 ""
		 "  :objects"
		 "  ((child-1 :type 'child-1"
		 "            :my-slot-1 (the my-slot-1))"
		 ""
		 "   (child-2 :type 'child-2)))"
		 ""
		 "(define-object child-1 (base-object)"
		 "  :input-slots"
		 "  (my-slot-1))"
		 ""  
		 "(define-object child-2 (base-object)"
		 "  :input-slots"
		 "  ((my-slot-1 3)))"))
   (repl-1 (list (list :command "(make-self 'top-level-1)"
		       :output "#<TOP-LEVEL-1 #x210522168D>")
		 (list :command "(the child-1 my-slot-1)"
		       :output 5)
		 (list :command "(the child-2 my-slot-1)"
		       :output 3)))
   (code-2 (list "(define-object top-level-2 (base-object)"
		 "  :computed-slots"
		 "  ((my-slot-1 5))"
		 ""
		 "  :objects"
		 "  ((child-1 :type 'child-1"
		 "            :pass-down (my-slot-1))))"))
   
   (repl-2 (list (list :command "(make-self 'top-level-2)"
		       :output "#<TOP-LEVEL-1 #x210561B13D>")
		 (list :command "(the child-1 my-slot-1)"
		       :output 5)))

   (code-3 (list "(define-object top-level-3 (base-object)"
		 "  :computed-slots"
		 "  ((my-slot-2 2))"
		 ""
		 "  :trickle-down-slots"
		 "  (my-slot-2)"
		 ""
		 "  :objects"
		 "  ((child-3 :type 'child-3)"
		 "  (child-4 :type 'child-4)))"
		 ""
		 "(define-object child-3 (base-object)"
		 "  :input-slots "
		 "  ((my-slot-2 3 :defaulting)))"
		 ""
		 "(define-object child-4 (base-object)"
		 "  :input-slots "
		 "  ((my-slot-2 5)))"))
   (repl-3 (list (list :command "(make-self 'top-level-3)"
		       :output "#<TOP-LEVEL-3 #x2104A23E2D>")
		 (list :command "(the my-slot-2)"
		       :output 2)
		 (list :command "(the child-3 my-slot-2)"
		       :output 2)
		 (list :command "(the child-4 my-slot-2)"
		       :output 5)))
   (code-4 (list "(define-object top-level-4 (base-object)"
		 "  :computed-slots"
		 "  ((my-slot-3 2))"
		 ""
		 "  :trickle-down-slots"
		 "  (my-slot-3)"
		 ""
		 "  :objects"
		 "  ((child-5 :type 'child-5"
		 "            :my-slot-3 1)))"
		 ""
		 "(define-object child-5 (base-object)"
		 "  :input-slots"
		 "  ((my-slot-3 5 :defaulting)))"))
   (repl-4 (list (list :command "(make-self 'top-level-4)"
		       :output "#<TOP-LEVEL-4 #x2104A3B93D>")
		 (list :command "(the child-5 my-slot-3)"
		       :output 1)))

		 
		 
   (body-content (with-cl-who-string ()
		   (:div :class "grid-container-2-650px"
			 (:div :class "grid-item"
			       (:p "Recall in the "
				   (if (the getting-started-url)
				       (htm (:a :href (the getting-started-url) "Getting Started with GendL"))
				       (htm "Getting Started with GendL "))" tutorial, in the "
				   (if (the define-object-url)
				       (htm (:a :href (the define-object-url)"Defining Objects"))
				       (htm "Defining Objects")) " topic we discussed how values may be passed from a parent object to a child. The following code summarises the process discussed"
				   (str (code-example (the code-1)))
				   "the object "
				   (:span :class "object" "child-1")" defines a required "
				   (:span :class "object-keyword" ":input-slot") " "
				   (:span :class "slot" "my-slot-1")" and this is passed in from the parent in the parents "
				   (:span :class "object-keyword"":objects")" section. The object "(:span :class "object" "child-2")" defines an optional "
				   (:span :class "object-keyword" ":input-slot")" "
				   (:span :class "slot" "my-slot-1")", in this example as no value is passed in from the parent it uses the default value"
				   (str (repl-example (the repl-1))))
			       (:p "Furthermore, we covered the use of the "
				   (:span :class "object-keyword" ":pass-down")" input to a child object in the "
				   (if (the wall-example-url)
				       (htm (:a :href (the wall-example-url)"Wall Example"))
				       (htm "Wall Example")) " topic"
				   (str (code-example (the code-2)))
				   (str (repl-example (the repl-2))))
			       (:p "There is, however, a further way to ensure values flow through from a parent to a child - by using "(:span :class "object-keyword" ":trickle-down-slots")
				   (:p "When a slot is defined as a "
				       (:span :class "object-keyword"":trickle-down-slot")", it will automatically flow through to a child object providing both of the following conditions are satisfied"
				       (:ul (:li "The child object has an "
						 (:span :class "object-keyword"":input-slot")" of the same name")
					    (:li "And that "
						 (:span :class "object-keyword"":input-slot")" in the child object is tagged as "
						 (:span :class "object-keyword"":defaulting")" and given a default value"))
				       (str (code-example (the code-3)))
				       (str (repl-example (the repl-3))))
				   (:p "Even if the slot is defined in the parent as a "
				       (:span :class "object-keyword"":trickle-down-slot")", if an input is given explicitly it will be used, over-riding the "
				       (:span :class "object-keyword"":trickle-down-slot")" value"
				       (str (code-example (the code-4)))
				       (str (repl-example (the repl-4))))
				   (:p (:span :class "object-keyword"":trickle-down-slots")" are a very convenient and semi-automatic way to pass values form parent to child. An example use is with the "
				       (:span :class "object""box")" object, where "
				       (:span :class "slot" "length")", "
				       (:span :class "slot" "width")" and "
				       (:span :class "slot" "height")" are all "
				       (:span :class "object-keyword" ":trickle-down-slots")". In previous examples, we have explicitly passed the values through, however strictly speaking this has not been required. One word of caution; it can sometimes be difficult to see where values are coming from if "
				       (:span :class "object-keyword"":trickle-down-slots")" are used extensively and particularly if they are incorporated into mixins, but as with all things used appropriately they are a useful addition to the developer toolbox")))
			 (:div :class "grid-item"
			       (:h2 "Resources")
			       (str (the resource-links))))))))
