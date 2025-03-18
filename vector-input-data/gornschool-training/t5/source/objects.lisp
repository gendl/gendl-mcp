(in-package :training-5)

(define-object objects (base-training-sheet)
:input-slots
  (getting-start-url
   define-object-url
   truss-example-url)
  
  :computed-slots
  ((index-words (list ":pseudo-inputs" ":hidden-objects" "hidden-children"))
   (code-1 (list "(define-object hidden-objects (base-object)"
		 "  :computed-slots"
		 "  ((box-volume (the (my-box 0) volume))"
		 "   (hidden-box-volume (the (my-hidden-box 0) volume))"
		 "   (object-list (list-elements (the)))"
		 "   (all-objects (append (the children)"
		 "	                  (the hidden-children))))"
		 ""
		 "  :objects"
		 "  ((my-box :type 'box"
		 "           :length 2"
		 "           :width 2"
		 "           :height 2"
		 "           :sequence (:size 3)))"
		 "  :hidden-objects"
		 "  ((my-hidden-box :type 'box"
		 "                  :length 3"
		 "                  :width 3"
		 "                  :height 3"
		 "                  :sequence (:size 2))))"))
   (repl-1 (list (list :command "(make-self 'hidden-objects)"
		       :output "#<HIDDEN-OBJECTS #x21043FB8BD>")
		 (list :command "(the box-volume)"
		       :output 8)
		 (list :command "(the hidden-box-volume)"
		       :output 27)
		 (list :command "(the children)"
		       :output "(#<BOX #x21044894DD> #<BOX #x210448B6ED> #<BOX #x210448B2CD>)")
		 (list :command "(object-list)"
		       :output "(#<BOX #x21044894DD> #<BOX #x210448B6ED> #<BOX #x210448B2CD>)")
		 (list :command "(the hidden-children)"
		       :output "(#<BOX #x21044876DD> #<BOX #x2104494BAD>)")
		 (list :command "(the all-objects)"
		       :output "(#<BOX #x21044894DD> #<BOX #x210448B6ED> #<BOX #x210448B2CD>)")
		 (list :output " #<BOX #x21044876DD> #<BOX #x2104494BAD>)")))

   (code-2 (list "(define-object pseudo-inputs (base-object)"
		 "  :computed-slots"
		 "  ((volumes (mapcar #'(lambda(a) (theo a volume)) (the children))))"
		 ""
		 "  :objects"
		 "  ((my-box :type 'box"
		 "           :length 2"
		 "           :width 3"
		 "           :height 4"
		 "           :sequence (:size 3))"
		 "  (my-cylinder :type 'cylinder"
		 "               :radius 2"
		 "               :height 6"
		 "               :pseudo-inputs (:volume)"
		 "               :volume (div (* (the-child height)"
		 "                               (expt (the-child radius) 2)"
		 "                               pi) 2))))"))

   (repl-2 (list (list :command "(make-self 'pseudo-inputs)"
		       :output "#<PSEUDO-INPUTS #x210485A1BD>")
		 (list :command "(the volumes)"
		       :output "(37.69911184307752 24 24 24)")))
   
   (body-content (with-cl-who-string ()
		   (:div :class "grid-container-2-650px"
			 (:div :class "grid-item"
			       (:p "In the "
				   (if (the getting-started-url)
				       (htm (:a :href (the getting-started-url) "Getting Started with GendL"))
				       (htm "Getting Started with GendL "))" tutorial, the "
				   (if (the define-object-url)
				       (htm (:a :href (the define-object-url)"Defining Objects"))
				       (htm "Defining Objects")) " topic covered the general syntax for defining objects. In that topic we covered "
				   (:span :class "object-keyword" ":input-slots")", "
				   (:span :class "object-keyword" ":computed-slots")", "
				   (:span :class "object-keyword" ":functions")" and "
				   (:span :class "object-keyword" ":objects")". In the previous topic we also covered "
				   (:span :class "object-keyword" ":trickle-down-slots")". In this topic we will look at "
				   (:span :class "object-keyword" ":hidden-objects")" and the "(:span :class "general-keyword" ":pseudo-inputs")" input to both "
				   (:span :class "object-keyword" ":objects")" and "
				   (:span :class "object-keyword" ":hidden-objects"))
			       (:h3 ":hidden-objects")
			       (:p "We actually introduced "
				   (:span :class "object-keyword" ":hidden-objects")" in the "
				   (if (the define-object-url)
				       (htm (:a :href (the truss-example-url)"Truss Example"))
				       (htm "Truss Example"))" topic of the "
				   (if (the getting-started-url)
				       (htm (:a :href (the getting-started-url) "Getting Started with GendL"))
				       (htm "Getting Started with GendL "))" tutorial")
			       (:p "The most obvious outcome of using "(:span :class "object-keyword" ":hidden-objects")" is that child objects defined in this way don't show up in the Geysr tree structure. However there are other effects"
				   (:ul (:li (:span :class "object-keyword" ":hidden-objects")" are omitted from the list returned by (list-elements (the))")
					(:li (:span :class "object-keyword" ":hidden-objects")" are not included in the messages (the children) or (the safe-children)"))
				  
				   (:span :class "object-keyword" ":hidden-objects")", however, can be accessed using referencing chains in exactly the same way that a normal child obejct is referenced. To list all "
				   (:span :class "object-keyword" "hidden-objects")" we need to use the message "
				   (:span :class "slot" "(the hidden-children)")
				   (str (code-example (the code-1)))
				   (:image :src (format nil "/~a-images/hidden-objects.png" (the publish-prefix)) :style "margin: 5px 0 0 3% ;width: auto; height: 200px;" )
				   (str (repl-example (the repl-1))))
			       (:h3 ":pseudo-inputs")
			       (:p (:span :class "general-keyword" ":pseudo-inputs")" enable us to pass a value into an object without the slot being defined as an "
				   (:span :class "object-keyword" ":input-slot")". This may seem a curious requirement, as if its not defined as an "
				   (:span :class "object-keyword" ":input-slot")" we won't be able to use it in "
				   (:span :class "object-keyword" ":computed-slots")" or pass forn to further child "
				   (:span :class "object-keyword" ":objects")". But it actually turns out to be really usefl if we want to make a slight customisation to an object without needing to define a new object. It may be that we wish to perform a calculation and attach the value to that object, or simply we want to 'tag' an object with some value. For example, the GendL "
				   (:span :class "object" "cylinder")" wifeframe object doesn't support a message "
				   (:span :class "slot" "volume")", but say we have a number of objects that do, and a cylinder, and we want to process the volumes using "
				   (:span :class "function" "mapcar")"."
				   (str (code-example (the code-2)))
				   "In this code, we have an input "
				   (:span :class "slot" "volume")" defined for "
				   (:span :class "object" "cylinder")", but "
				   (:span :class "object" "cylinder")" doesb't have a corresponding "
				   (:span :class "object-keyword" ":input-slot")", so we have identified this input as a "
				   (:span :class "general-keyword" ":pseudo-input")". Note that, if we don't define"
				   (:span :class "slot" "volume")" as a "
				   (:span :class "general-keyword" "pseudo-input")", the code will still work but we will get a compiler warning. Whilst this is the case, its good practice from a coding perspective to identify such slots correctly"
				   (str (repl-example (the repl-2)))))
			 (:div :class "grid-item"
			       (:h2 "Resources")
			       (str (the resource-links))))))))
					   
				  
