(in-package :training-5)

(define-object useful-slots (base-training-sheet)

  :computed-slots
  ((index-words (list "strings-for-display" "root" "children" "safe-children" "all-mixins" "typep" "type" "parent"))
   (code-1 (list "(define-object name-display (base-object)"
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

   (code-2 (list "(define-object object-tagging (base-object)"
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
		 "(define-object my-box (box"
		 "                       3d-shape-mixin"
		 "                       all-geometry-mixin))"
		 ""
		 "(define-object my-sphere (sphere"
		 "                          3d-shape-mixin"
		 "                          all-geometry-mixin))"
		 ""
		 "(define-object my-line (line"
		 "                        all-geometry-mixin))"
		 ""
		 "(define-object all-geometry-mixin())"
		 ""
		 "(define-object 3d-shape-mixin())"))
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




