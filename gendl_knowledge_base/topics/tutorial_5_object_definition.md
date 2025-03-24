# Gendl Documentation - tutorial_5_object_definition

## strings-for-display.lisp - header
Source: gornschool-training/t5/resources/source/strings-for-display.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## strings-for-display.lisp - name-display
Source: gornschool-training/t5/resources/source/strings-for-display.lisp
Type: tutorial

```
(define-object  name-display (base-object)
  
  :objects
  ((my-named-box :type 'my-box
		 :strings-for-display "My Custom Box"
		 :length 3
		 :width 4
		 :height 5)))
   


```

---

## pseudo-objects.lisp - header
Source: gornschool-training/t5/resources/source/pseudo-objects.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## pseudo-objects.lisp - pseudo-inputs
Source: gornschool-training/t5/resources/source/pseudo-objects.lisp
Type: tutorial

```
(define-object pseudo-inputs (base-object)
  :computed-slots
  ((volumes (mapcar #'(lambda(a) (theo a volume)) (the children))))
  
  :objects
  ((my-box :type 'box
	   :length 2
	   :width 3
	   :height 4
	   :sequence (:size 3))
   (my-cylinder :type 'cylinder
		:radius 2
		:height 6
		:pseudo-inputs (:volume)
		:volume (div (* (the-child height)
				(expt (the-child radius) 2)
				pi) 2))))

```

---

## object-tagging.lisp - header
Source: gornschool-training/t5/resources/source/object-tagging.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## object-tagging.lisp - object-tagging
Source: gornschool-training/t5/resources/source/object-tagging.lisp
Type: tutorial

```
(define-object object-tagging (base-object)
  :computed-slots
  ((all-geometry (remove nil
			 (mapcar #'(lambda(a) (when (typep a 'all-geometry-mixin) a))
				 (the children))))
   (3d-shapes (remove nil
		      (mapcar #'(lambda(a) (when (typep a '3d-shape-mixin) a))
			      (the children))))
   )
  :objects
  ((my-box :type 'my-box
	   :length 3
	   :width 4
	   :height 5)
   (my-sphere :type 'my-sphere
	      :radius 4)

   (my-line :type 'my-line
	    :start (make-point 0 0 0)
	    :end (make-point 10 0 0))))


```

---

## object-tagging.lisp - my-box
Source: gornschool-training/t5/resources/source/object-tagging.lisp
Type: tutorial

```
(define-object my-box (box
		       3d-shape-mixin
		       all-geometry-mixin))


```

---

## object-tagging.lisp - my-sphere
Source: gornschool-training/t5/resources/source/object-tagging.lisp
Type: tutorial

```
(define-object my-sphere (sphere
			  3d-shape-mixin
			  all-geometry-mixin))


```

---

## object-tagging.lisp - my-line
Source: gornschool-training/t5/resources/source/object-tagging.lisp
Type: tutorial

```
(define-object my-line (line
			all-geometry-mixin))


```

---

## object-tagging.lisp - all-geometry-mixin
Source: gornschool-training/t5/resources/source/object-tagging.lisp
Type: tutorial

```
(define-object all-geometry-mixin())


```

---

## object-tagging.lisp - 3d-shape-mixin
Source: gornschool-training/t5/resources/source/object-tagging.lisp
Type: tutorial

```
(define-object 3d-shape-mixin())


```

---

## trickle-down-slots.lisp - header
Source: gornschool-training/t5/resources/source/trickle-down-slots.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## trickle-down-slots.lisp - top-level-1
Source: gornschool-training/t5/resources/source/trickle-down-slots.lisp
Type: tutorial

```
(define-object top-level-1 (base-object)
   :computed-slots
  ((my-slot-1 5))
  
  :objects
  ((child-1 :type 'child-1
	    :my-slot-1 (the my-slot-1))

   (child-2 :type 'child-2)))


```

---

## trickle-down-slots.lisp - child-1
Source: gornschool-training/t5/resources/source/trickle-down-slots.lisp
Type: tutorial

```
(define-object child-1 (base-object)
  :input-slots
  (my-slot-1))
  

```

---

## trickle-down-slots.lisp - child-2
Source: gornschool-training/t5/resources/source/trickle-down-slots.lisp
Type: tutorial

```
(define-object child-2 (base-object)
  :input-slots
  ((my-slot-1 3)))

;;;;;;;;;;;;;;;;;;;;


```

---

## trickle-down-slots.lisp - top-level-2
Source: gornschool-training/t5/resources/source/trickle-down-slots.lisp
Type: tutorial

```
(define-object top-level-2 (base-object)
   :computed-slots
  ((my-slot-1 5))
  
  :objects
  ((child-1 :type 'child-1
	    :pass-down (my-slot-1))))

   
;;;;;;;;;;;;;;;;;;;;;;


```

---

## trickle-down-slots.lisp - top-level-3
Source: gornschool-training/t5/resources/source/trickle-down-slots.lisp
Type: tutorial

```
(define-object top-level-3 (base-object)
  :computed-slots
  ((my-slot-2 2))
  
  :trickle-down-slots
  (my-slot-2)
  
  :objects
  ((child-3 :type 'child-3)
   (child-4 :type 'child-4)))


```

---

## trickle-down-slots.lisp - child-3
Source: gornschool-training/t5/resources/source/trickle-down-slots.lisp
Type: tutorial

```
(define-object child-3 (base-object)
  :input-slots ((my-slot-2 3 :defaulting)))


```

---

## trickle-down-slots.lisp - child-4
Source: gornschool-training/t5/resources/source/trickle-down-slots.lisp
Type: tutorial

```
(define-object child-4 (base-object)
  :input-slots ((my-slot-2 5)))

;;;;;;;;;;;;;;;;;;;;;;


```

---

## trickle-down-slots.lisp - top-level-4
Source: gornschool-training/t5/resources/source/trickle-down-slots.lisp
Type: tutorial

```
(define-object top-level-4 (base-object)
  :computed-slots
  ((my-slot-3 2))
  
  :trickle-down-slots
  (my-slot-3)
  
  :objects
  ((child-5 :type 'child-5
	    :my-slot-3 1)))


```

---

## trickle-down-slots.lisp - child-5
Source: gornschool-training/t5/resources/source/trickle-down-slots.lisp
Type: tutorial

```
(define-object child-5 (base-object)
  :input-slots
  ((my-slot-3 5 :defaulting)))

```

---

## settable-slots.lisp - header
Source: gornschool-training/t5/resources/source/settable-slots.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## settable-slots.lisp - settable-slots
Source: gornschool-training/t5/resources/source/settable-slots.lisp
Type: tutorial

```
(define-object settable-slots (base-object)

  :computed-slots
  ((speed 25 :settable)
   (time 15 :settable)
   (distance (* (the speed) (the time)) :settable))

  :functions
  ((set-speed! (&key (value 20)) (the (set-slot! :speed value)))
   (set-time! (&key (value 10)) (the (set-slot! :time value)))
   (set-distance! () (the (set-slot! :distance 100)))
   (reset-distance! () (the (restore-slot-default! :distance)))
   (reset-all! () (the (restore-slot-defaults! (list :speed :time :distance))))))


```

---

## hidden-objects.lisp - header
Source: gornschool-training/t5/resources/source/hidden-objects.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## hidden-objects.lisp - hidden-objects
Source: gornschool-training/t5/resources/source/hidden-objects.lisp
Type: tutorial

```
(define-object hidden-objects (base-object)
  :computed-slots
  ((box-volume (the (my-box 0) volume))
   (hidden-box-volume (the (my-hidden-box 0) volume))
   (object-list (list-elements (the)))
   (all-objects (append (the children)
			(the hidden-children))))
  
  :objects
  ((my-box :type 'box
	   :length 2
	   :width 2
	   :height 2
	   :sequence (:size 3)))
  :hidden-objects
  ((my-hidden-box :type 'box
		  :length 3
		  :width 3
		  :height 3
		  :sequence (:size 2))))





```

---

## objects.lisp - header
Source: gornschool-training/t5/source/objects.lisp
Type: tutorial

```
(in-package :training-5)


```

---

## objects.lisp - objects
Source: gornschool-training/t5/source/objects.lisp
Type: tutorial

```
(define-object objects (base-training-sheet)
:input-slots
  (getting-start-url
   define-object-url
   truss-example-url)
  
  :computed-slots
  ((index-words (list ":pseudo-inputs" ":hidden-objects" "hidden-children"))
   (code-1 (list "
```

---

## objects.lisp - hidden-objects
Source: gornschool-training/t5/source/objects.lisp
Type: tutorial

```
(define-object hidden-objects (base-object)"
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

   (code-2 (list "
```

---

## objects.lisp - pseudo-inputs
Source: gornschool-training/t5/source/objects.lisp
Type: tutorial

```
(define-object pseudo-inputs (base-object)"
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
					   
				  

```

---

## assembly.lisp - header
Source: gornschool-training/t5/source/assembly.lisp
Type: tutorial

```
(in-package :training-5)

(defparameter *publish-prefix* "t5")


```

---

## assembly.lisp - assembly
Source: gornschool-training/t5/source/assembly.lisp
Type: tutorial

```
(define-object assembly(base-tutorial-sheet)
  :input-slots
  ((getting-started-url nil)
   
```

---

## assembly.lisp - nil
Source: gornschool-training/t5/source/assembly.lisp
Type: tutorial

```
(define-object-url nil)
   (wall-example-url nil)
   (truss-example-url nil)
   (tutorial-name "More on the GendL object"))

  :computed-slots
  ((introduction (with-cl-who-string ()
		   (:p "In the " (if (the getting-started-url)
				     (htm (:a :href (the getting-started-url) "Getting started with GendL"))
				     "Getting started with GendL") "tutorial, we covered the most common options for the GendL object. Some of the examples we used in that tutorial introduced some extra features. This tutorial recaps on ths additional features and additionally covers others")))
  )
		       

  :objects
  ((useful-slots :type 'useful-slots
		 :pass-down (page-objects)
		 :publish-prefix *publish-prefix*
		 :page 1
		 :page-title "Useful Slots"
		 :resources (list "strings-for-display.lisp" "object-tagging.lisp"))
    
    (trickle-down-slots :type 'trickle-down-slots
			:pass-down (page-objects
				    getting-started-url
				    define-object-url
				    wall-example-url )
                        :pseudo-inputs (getting-started-url)
			:publish-prefix *publish-prefix*
			:page 2
			:page-title ":trickle-down-slots"
			:resources (list "trickle-down-slots.lisp"))
    
   (objects :type 'objects
	    :pass-down (page-objects
			getting-started-url
			 define-object-url
			truss-example-url)
            :pseudo-inputs (getting-started-url)
	    :publish-prefix *publish-prefix*
	    :page 3
	    :page-title ":objects"
	    :resources (list "hidden-objects.lisp" "pseudo-inputs.lisp"))
    
   (settable-slots :type 'settable-slots
		   :pass-down (page-objects
			       getting-started-url)
                   :pseudo-inputs (getting-started-url)
		   :publish-prefix *publish-prefix*
		   :page 4
		   :page-title ":settable slots"
		   :resources (list "settable-slots.lisp"))
   )
  )







```

---

## trickle-down-slots.lisp - header
Source: gornschool-training/t5/source/trickle-down-slots.lisp
Type: tutorial

```
(in-package :training-5)


```

---

## trickle-down-slots.lisp - trickle-down-slots
Source: gornschool-training/t5/source/trickle-down-slots.lisp
Type: tutorial

```
(define-object trickle-down-slots (base-training-sheet)
  :input-slots
  (getting-start-url
   define-object-url
   wall-example-url)
  
  :computed-slots
  ((index-words (list ":trickle-down-slots" ":defaulting"))
     (code-1 (list "
```

---

## trickle-down-slots.lisp - top-level-1
Source: gornschool-training/t5/source/trickle-down-slots.lisp
Type: tutorial

```
(define-object top-level-1 (base-object)"
		 "  :computed-slots"
		 "  ((my-slot-1 5))"
		 ""
		 "  :objects"
		 "  ((child-1 :type 'child-1"
		 "            :my-slot-1 (the my-slot-1))"
		 ""
		 "   (child-2 :type 'child-2)))"
		 ""
		 "
```

---

## trickle-down-slots.lisp - child-1
Source: gornschool-training/t5/source/trickle-down-slots.lisp
Type: tutorial

```
(define-object child-1 (base-object)"
		 "  :input-slots"
		 "  (my-slot-1))"
		 ""  
		 "
```

---

## trickle-down-slots.lisp - child-2
Source: gornschool-training/t5/source/trickle-down-slots.lisp
Type: tutorial

```
(define-object child-2 (base-object)"
		 "  :input-slots"
		 "  ((my-slot-1 3)))"))
   (repl-1 (list (list :command "(make-self 'top-level-1)"
		       :output "#<TOP-LEVEL-1 #x210522168D>")
		 (list :command "(the child-1 my-slot-1)"
		       :output 5)
		 (list :command "(the child-2 my-slot-1)"
		       :output 3)))
   (code-2 (list "
```

---

## trickle-down-slots.lisp - top-level-2
Source: gornschool-training/t5/source/trickle-down-slots.lisp
Type: tutorial

```
(define-object top-level-2 (base-object)"
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

   (code-3 (list "
```

---

## trickle-down-slots.lisp - top-level-3
Source: gornschool-training/t5/source/trickle-down-slots.lisp
Type: tutorial

```
(define-object top-level-3 (base-object)"
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
		 "
```

---

## trickle-down-slots.lisp - child-3
Source: gornschool-training/t5/source/trickle-down-slots.lisp
Type: tutorial

```
(define-object child-3 (base-object)"
		 "  :input-slots "
		 "  ((my-slot-2 3 :defaulting)))"
		 ""
		 "
```

---

## trickle-down-slots.lisp - child-4
Source: gornschool-training/t5/source/trickle-down-slots.lisp
Type: tutorial

```
(define-object child-4 (base-object)"
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
   (code-4 (list "
```

---

## trickle-down-slots.lisp - top-level-4
Source: gornschool-training/t5/source/trickle-down-slots.lisp
Type: tutorial

```
(define-object top-level-4 (base-object)"
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
		 "
```

---

## trickle-down-slots.lisp - child-5
Source: gornschool-training/t5/source/trickle-down-slots.lisp
Type: tutorial

```
(define-object child-5 (base-object)"
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

```

---

## useful-slots.lisp - header
Source: gornschool-training/t5/source/useful-slots.lisp
Type: tutorial

```
(in-package :training-5)


```

---

## useful-slots.lisp - useful-slots
Source: gornschool-training/t5/source/useful-slots.lisp
Type: tutorial

```
(define-object useful-slots (base-training-sheet)

  :computed-slots
  ((index-words (list "strings-for-display" "root" "children" "safe-children" "all-mixins" "typep" "type" "parent"))
   (code-1 (list "
```

---

## useful-slots.lisp - name-display
Source: gornschool-training/t5/source/useful-slots.lisp
Type: tutorial

```
(define-object name-display (base-object)"
		 " :objects"
		 "  ((my-named-object :type 'box"
		 "                    :strings-for-display \"My Custom Box\""
		 "                    :length 3"
		 "                    :width 4"
		 "                    :height 5)))"))
   (repl-1 (list (list :command "(setq self (make-object 'name-display))"
		       :output "#NAME-DISPLAY #x210408140D>")
		 (list :command "(setq self (the my-named-object))"
		       :output "#<BOX #x2104080FFD>")
		 (list :command "(the root)"
		       :output "#NAME-DISPLAY #x210408140D>")))

   (repl-2 (list (list :command "(setq self (make-object 'name-display))"
		       :output "#NAME-DISPLAY #x210408140D>")
		 (list :command "(the children)"
		       :output "(#<BOX #x2104080FFD>)")
		 (list :command "(the safe-children"
		       :output "(#<BOX #x2104080FFD>)")
		 (list :command "(list-elements (the))"
		       :output "(#<BOX #x2104080FFD>)")))

   (repl-3 (list (list :command "(setq self (make-object 'name-display))"
		       :output "#NAME-DISPLAY #x210408140D>")
		 (list :command "(setq self (the my-named-object))"
		       :output "#<BOX #x2104080FFD>")
		 (list :command "(the parent)"
		       :output "#NAME-DISPLAY #x210408140D>")))

   (repl-4 (list (list :command "(setq self (make-object 'name-display))"
		       :output "#NAME-DISPLAY #x210408140D>")
		 (list :command "(the type)"
		       :output "#NAME-DISPLAY #x210408140D>")
		 (list :command "(setq self (the my-named-object))"
		       :output "#<BOX #x2104080FFD>")
		 (list :command "(the type)"
		       :output "#<BOX #x2104080FFD>")))
   (repl-5 (list (list :command "(setq self (make-object 'name-display))"
		       :output "#NAME-DISPLAY #x210408140D>")
		 (list :command "(the all-mixins)"
		       :output "(BASE-OBJECT VANILLA-MIXIN VANILLA-MIXIN* STANDARD-OBJECT T GENDL::GDL-BASIS)")))

   (code-2 (list "
```

---

## useful-slots.lisp - object-tagging
Source: gornschool-training/t5/source/useful-slots.lisp
Type: tutorial

```
(define-object object-tagging (base-object)"
		 "  :computed-slots"
		 "  ((all-geometry (remove nil"
		 "                         (mapcar #'(lambda(a) (when (typep a 'all-geometry-mixin) a))"
		 "                                   (the children))))"
		 "   (3d-shapes (remove nil"
		 "                      (mapcar #'(lambda(a) (when (typep a '3d-shape-mixin) a))"
		 "                                (the children)))))"
		 ""
		 "  :objects"
		 "  ((my-box :type 'my-box"
		 "           :length 3"
		 "           :width 4"
		 "           :height 5)"
		 "   (my-sphere :type 'my-sphere"
		 "              :radius 4)"
		 "   (my-line :type 'my-line"
		 "            :start (make-point 0 0 0)"
		 "            :end (make-point 10 0 0))))"
		 ""
		 "
```

---

## useful-slots.lisp - my-box
Source: gornschool-training/t5/source/useful-slots.lisp
Type: tutorial

```
(define-object my-box (box"
		 "                       3d-shape-mixin"
		 "                       all-geometry-mixin))"
		 ""
		 "
```

---

## useful-slots.lisp - my-sphere
Source: gornschool-training/t5/source/useful-slots.lisp
Type: tutorial

```
(define-object my-sphere (sphere"
		 "                          3d-shape-mixin"
		 "                          all-geometry-mixin))"
		 ""
		 "
```

---

## useful-slots.lisp - my-line
Source: gornschool-training/t5/source/useful-slots.lisp
Type: tutorial

```
(define-object my-line (line"
		 "                        all-geometry-mixin))"
		 ""
		 "
```

---

## useful-slots.lisp - all-geometry-mixin
Source: gornschool-training/t5/source/useful-slots.lisp
Type: tutorial

```
(define-object all-geometry-mixin())"
		 ""
		 "
```

---

## useful-slots.lisp - 3d-shape-mixin
Source: gornschool-training/t5/source/useful-slots.lisp
Type: tutorial

```
(define-object 3d-shape-mixin())"))
   (repl-6 (list (list :command "(make-self 'object-tagging)"
		       :output "#<OBJECT-TAGGING #x210500014D>")
		 (list :command "(the all-geometry)"
		       :output "(#<MY-BOX #x210500566D> #<MY-SPHERE #x2105004DCD> #<MY-LINE #x21050045FD>)")
		 (list :command "(the 3d-shapes)"
		       :output "(#<MY-BOX #x210500566D> #<MY-SPHERE #x2105004DCD>)")))
   
   (body-content (with-cl-who-string ()
		   (:div :class "grid-container-2-650px"
			 (:div :class "grid-item"
			       (:p "All GendL objetcs use, or mix in, the "
				   (:span :class "object" "Vanilla-Mixin*") " object. The full set of messages supported by this object are documented in YADD, but its worth highlighting a few in particular that are useful for application development and application debugging/inspection")
		               (:h3 "Application Development")
		               (:ul (:li (:span :class "slot" "strings-for-display") " - changes the display name used, for example, by Geysr to the specified string. It is specified as an inut to the object, and over-rides the default name"
				         (str (code-example (the code-1)))
				         (:image :src (format nil "/~a-images/strings-for-display.png" (the publish-prefix)) :style "margin: 5px 0 0 3% ;width: auto; height: 200px;" ))
			            (:li (:span :class "slot" "root") " - from anywhere in the objet tree this will return the object corrssponding to the root node"
				         (str (repl-example (the repl-1))))
			            (:li (:span :class "slot" "children") " - returns a list of child objects. This is equivalent to "
				         (:em (:b "(list-elements (the))"))". A variant is "
				         (:span :class "slot" "safe-children")" which will return a plist with error information for an child objects which throws an error"
				         (str (repl-example (the repl-2))))
			            (:li (:span :class "slot" "parent") " - returns the parent object. Care should be taken when using this message since it may impact reusability of an object if the object is used in multiple places and this is used to reference a particular slot/message in the parent object; if the object can be used by mutiple objects then it implies that slot/message must be supported wherever this is used as a child object"
				         (str (repl-example (the repl-3)))))
		               (:h3 "Application Debugging/Inspection")
		               (:ul (:li (:span :class "slot" "type") " - returns the type of the current object"
				         (str (repl-example (the repl-4))))
			            (:li (:span :class "slot" "all-mixins") " - returns a list of the mixins used by the current-object. Note that this is generated recursively, so if the mixin specified in the define-object specification itself has one or more mixins, these will be included. Any duplicates are removed"
				         (str (repl-example (the repl-5)))
				         "Note that the typep predicate examines the all-mixins list and will return t if the specified object being tested has a miixin of the type tested against. This can be particularly useful for 'tagging' objects of different types, by just mixing in an empty object and using typep against that empty object"
				         (str (code-example (the code-2)))
				         (str (repl-example (the repl-6))))))
			 (:div :class "grid-item"
			       (:h2 "Resources")
			       (str (the resource-links))))))))





```

---

## settable-slots.lisp - header
Source: gornschool-training/t5/source/settable-slots.lisp
Type: tutorial

```
(in-package :training-5)


```

---

## settable-slots.lisp - settable-slots
Source: gornschool-training/t5/source/settable-slots.lisp
Type: tutorial

```
(define-object settable-slots (base-training-sheet)
  :input-slots
  (getting-started-url)
  
  :computed-slots
  ((index-words (list ":settable" "set-slot!" "restore-slot-default" "restore-slot-defaults!" "dependancy tracker" "demand driven"))
		
   (code-1 (list "
```

---

## settable-slots.lisp - settable-slots
Source: gornschool-training/t5/source/settable-slots.lisp
Type: tutorial

```
(define-object settable-slots (base-object)"
		 ""
		 "  :computed-slots"
		 "  ((speed 25 :settable)"
		 "   (time 15 :settable)"
		 "   (distance (* (the speed) (the time)) :settable))"
		 ""
		 "  :functions"
		 "  ((set-speed! (&key (value 20)) (the (set-slot! :speed value)))"
		 "   (set-time! (&key (value 10)) (the (set-slot! :time value)))"
		 "   (set-distance! () (the (set-slot! :distance 100)))"
		 "   (reset-distance! () (the (restore-slot-default! :distance)))"
		 "   (reset-all! () (the (restore-slot-defaults! (list :speed :time :distance))))))"))

   (repl-1 (list (list :command "(make-self 'settable-slots)"
		       :output "#<SETTABLE-SLOTS #x2104DC5DFD>")
		 (list :command "(the distance)"
		       :output 375)))
   (repl-2 (list (list :command "(the set-speed!)"
		       :output "NIL")
		 (list :command "(the distance)"
		       :output 300)))
   (repl-3 (list (list :command "(the set-time!)"
		       :output "NIL")
		 (list :command "(the distance)"
		       :output 200)))
   
   (repl-4 (list (list :command "(the set-distance!)"
		       :output "NIL")
		 (list :command "(the distance)"
		       :output 100)
		 (list :command "(the (set-speed! :value 30))"
		       :output "NIL")
		 (list :command "(the distance)"
		       :output 100)))
   (repl-5 (list (list :command "(the reset-distance!)"
		       :output ":DISTANCE")
		  (list :command "(the distance)"
		       :output 300)))
   (repl-6 (list (list :command "(the reset-all!)"
		       :output "(:SPEED :TIME :DISTANCE)")
		 (list :command "(the speed)"
		       :output 25)
		 (list :command "(the time)"
		       :output 15)
		 (list :command "(the distance)"
		       :output 375)))
   

   (body-content (with-cl-who-string ()
		   (:div :class "grid-container-2-650px"
			 (:div :class "grid-item"
			       (:p "In the "
				   (if (the getting-started-url)
				       (htm (:a :href (the getting-started-url) "Getting Started with GendL"))
				       (htm "Getting Started with GendL "))
				   " tutorial, we identified on of the biggest differences between GendL and other programming languages was that slot evaluation was always demand driven and the inbuilt dependency tracker will always ensure slot values are current whenever any input changes. This removes a big burdon from the developer and also ensures run times are fast, as we are never calculating any values we don't need to use.")
		               (:p "However, there may be times when we need to make programatic changes to values an over-ride this default behaviour. To do this, we identify either slots as "(:span :class "general-keyword" ":settable")" and then we use the "(:span :class "function" "set-slot!")" function to programaically alter the value of the selected slot.")
		               (:p "When we make a change to a slot value by programatically setting it, the dependency tracker is aware of this and any changes which would ordinarily cause the value to be updated are suspended. If we want to reverse this behaviour (back to default) we can use either of the "(:span :class "function" "restore-slot-default!")" or "(:span :class "function" "restore-slot-defaults!")" functions")
		               (:p "As with a lot of different techniques, programatically altering slot values and over-riding the default behaviour has its place and is a useful additoon to the tools and techniques available to the programmer. However, we need to be aware that at the point we do this, and until we revert to default behaviour, it becomes to responsibility of the developer to manage dependencies. For this reason it is recommended to use this technique sparingly and only when a solution using the default behaviour will not give the desired results")
		               (:p "Consider the following code")
		               (str (code-example (the code-1)))
		               (:p "3 "(:span :class "object-keyword" ":computed slots")", "(:span :class "slot" "speed")", "
			           (:span :class "slot" "time")" and "
			           (:span :class "slot" "distance")" have been tagged as "
			           (:span :class "general-keyword" ":settable")". 5 functions have been defined, 3 which set new values (using "
			           (:span :class "function" "set-slot!")") for the respective slots and 2 which will restore default values and behaviours (using "
			           (:span :class "function" ":restore-slot-default!")" and "
			           (:span :class "function" ":restore-slot-defaults!")"). If we make the object and evaluate distance we get the following"
			           (str (repl-example (the repl-1)))
			           "We can now programatically change the value of speed by running the "(:span :class "function" "set-speed!")" function. If we evaluate "
			           (:span :class "slot" "distance")", we can see that"
			           (:ul (:li "The value has changed")
			                (:li "But the underlying calculation is being performed as specified"))
			           (str (repl-example (the repl-2)))
			           "We can do the same with "(:span :class "slot" "time")", getting the same results"
			           (str (repl-example (the repl-3)))
			           "If we now modify the value of "(:span :class "slot" "distance")", we can see the change in that value, but then going back and changing the value of "
			           (:span :class "slot" "speed")" again, we can see that "(:span :class "slot" "distance")" has not been changed. This is because "(:span :class "slot" "distance")" has had its value set and any dependency tracking associated with it, which would ordinarily cause a change to its value, has been disabled"
			           (str (repl-example (the repl-4)))
			           "However, if we now reset the value of "(:span :class "slot" "distance")" only, we can see that dependency tracking has picked up the values of "(:span :class "slot" "speed")" and "(:span :class "slot" "time")" and has updated the value for "(:span :class "slot" "distance")
			           (str (repl-example (the repl-5)))
			           "Finally, if we reset all slots, all are set back to default"
			           (str (repl-example (the repl-6)))))
			 (:div :class "grid-item"
			       (:h2 "Resources")
			       (str (the resource-links))))))))


```

---

