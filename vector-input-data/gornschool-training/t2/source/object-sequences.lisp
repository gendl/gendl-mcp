(in-package :training-2)

(define-object object-sequences (base-training-sheet)

  :computed-slots
  ((index-words (list ":sequence" "standard sequence" "radial sequence"
                      "variable sequence" "matrix sequence" ":lateral"
                      ":longitudinal" ":vertical" "index" "the-child" ":size" ":radial"))

   (code-1 (list "(define-object assembly-2 (base-object)"
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
   
   (code-2 (list "(define-object assembly-6 (base-object)"
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

   
   (code-3 (list "(define-object assembly-7 (base-object)"
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
  
  
