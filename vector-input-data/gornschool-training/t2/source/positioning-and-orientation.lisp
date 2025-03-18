(in-package :training-2)

(define-object positioning-and-orientation (base-training-sheet)
  :computed-slots
  ((index-words (list "center" ":orientation" "alignment" "make-point" "translate-along-vector" "make-vector" "face-center" "face-normal-vector" "edge-center"
		      "vertex" ":top"":bottom"":front" ":rear" ":left" ":right" "the-child" "co-ordinate system" "axis system"))

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
		 "         :center (translate-along-vector"
                 "                   (the box-2 center)"
		 "                   (make-vector 1 1 0)"
		 "                   5))))"))


   (code-2 (list "(define-object assembly-3 (base-object)"
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

   (code-3 (list "(define-object assembly-4 (base-object)"
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

   (code-4 (list "(define-object assembly-5 (base-object)"
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



