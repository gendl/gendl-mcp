(in-package :training-2)

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
   
   (code-1 (list "(define-object truss ()"
		 "  :objects"
		 "  ((lower-beam :type 'beam)"
		 "   (vertical-beam :type 'beam)"
		 "   (front-slope-beam :type 'beam)"
		 "   (rear-slope-beam :type 'beam)))"
		 ""
		 "(define-object beam ()"
		 "  :objects"
		 "  ((outer :type 'box)"
		 "   (inner :type 'box)))"))

   (code-2 (list "(define-object beam (box)"
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


  (code-3 (list "(defun degrees-to-radians (degrees)"
		"   (div (* degrees pi) 180))"
		""
		"(define-object truss (box)"
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

   (code-4 (list "(define-object truss (box)"
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

   (code-5 (list "(define-object truss (box)"
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
		 "(define-object vector-line (box)"
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

   (code-6 (list "(define-object truss (box)"
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


    (code-7 (list "(define-object truss (box)"
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

   (code-8 (list "(define-object truss (box)"
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

   (code-9 (list "(define-object truss (box)"
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
				    
   
