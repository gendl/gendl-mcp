(in-package :training-5)

(defparameter *publish-prefix* "t5")

(define-object assembly(base-tutorial-sheet)
  :input-slots
  ((getting-started-url nil)
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






