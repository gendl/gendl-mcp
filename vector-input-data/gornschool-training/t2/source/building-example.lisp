(in-package :training-2)

(define-object building-example (base-training-sheet)

  :functions
  ((hint-1! () (the (set-slot! :hint-1 (not (the hint-1)))))
   (hint-2! () (the (set-slot! :hint-2 (not (the hint-2)))))
   (hint-3! () (the (set-slot! :hint-3 (not (the hint-3)))))
   (hint-4! () (the (set-slot! :hint-4 (not (the hint-4)))))
   (hint-5! () (the (set-slot! :hint-5 (not (the hint-5))))))

  :computed-slots
  ((hint-1 nil :settable)
   (hint-2 nil :settable)
   (hint-3 nil :settable)
   (hint-4 nil :settable)
   (hint-5 nil :settable)
   
   
   (code-1 (list "(define-object full-start-wall (wall)"
		 "  :input-slots"
		 "  ((first-row :start-full)))"
		 ""
		 "(define-object half-start-wall (wall)"
		 "  :input-slots"
		 "  ((first-row :start-half)))"
		 ""
		 "(define-object wall(box)"
		 "  :input-slots"
		 "  (..."
		 "   ..."
		 "   (first-row :start-full))"
		 "  ..."
		 "  ..."
		 "  :objects"
		 "  ((row :type 'row"
		 "    ..."
		 "    ..."
		 "    :pass-down (..."
		 "                ..."
		 "                first-row))))"
		 ""
		 "(define-object row (box)"
		 "  :input-slots"
		 "  (..."
		 "   ..."
		 "   first-row)"
		 ""
		 "  :computed-slots"
		 "  ((full-brick-row? (cond ((eq (the first-row) :start-full)"
		 "                           (or (zerop (the index)) (evenp (the index))))"
		 "                          ((eq (the first-row) :start-half)"
		 "                           (not (or (zerop (the index)) (evenp (the index)))))))"
		 "    ..."		 
		 "    ..."
		 "  )"
		 ")"))

   (code-2 (list "(define-object half-start-wall-front-key (wall)"
		 "  :input-slots"
		 "  ((first-row :start-half)"
		 "   (front-edge :keyed)))"
		 ""
		 "(define-object half-start-wall-rear-key (wall)"
		 "  :input-slots"
		 "  ((first-row :start-half)"
		 "   (rear-edge :keyed)))"
		 ""
		 "(define-object half-start-wall-both-key (wall)"
		 "  :input-slots"
		 "  ((first-row :start-half)"
		 "   (front-edge :keyed)"
		 "   (rear-edge :keyed)))"
		 ""
		 "(define-object wall(box)"
		 "  :input-slots"
		 "  (..."
		 "   ..."
		 "   (front-edge :full)"
		 "   (rear-edge :full))"
		 "  :objects"
		 "  ((row :type 'row"
		 "        ..."
		 "        :pass-down (..."
		 "                    ..."
		 "                    front-edge"
		 "                    rear-edge))))"
		 ""
		 "(define-object row (box)"
		 "  :input-slots"
		 "  (..."
		 "   ..."		 
		 "   front-edge"
		 "   rear-edge )"
		 "  :objects"
		 "  ((bricks-and-mortar :type 'bricks-and-mortar"
		 "                     ..."
		 "                     :pass-down (..."
		 "                                ..."
		 "                                front-edge"
		 "                                rear-edge)))"
		 ""
		 "(define-object bricks-and-mortar (box)"
		 "  :input-slots"
		 "  (..."
		 "   ..."
		 "   front-edge"
		 "   rear-edge)"
		 ""
		 "  :computed-slots"
		 "  (..."
		 "   ..."	   
		 "   (first-mortar-joint-start-point (cond ((the full-brick-row?)"
		 "                                        (the (full-brick 0) (face-center :rear)))"
		 "                                       ((eq (the front-edge) :full)"
		 "                                        (the (half-brick 0) (face-center :rear)))"
		 "                                       ((eq (the front-edge) :keyed)"
		 "                                        (translate-along-vector "
		 "                                           (the (face-center :front))"
		 "	                                      (the (face-normal-vector :rear))"
		 "	                                      (half (the brick-length))))))"
		 "   (number-of-half-bricks (cond ((the full-brick-row?) 0)"
		 "                              ((and (eq (the front-edge) :full)"
		 "                                    (eq (the rear-edge) :full)) 2)"
		 "                              ((and (eq (the front-edge) :keyed)"
		 "                                    (eq (the rear-edge) :full)) 1)"
		 "                              ((and (eq (the front-edge) :full)"
		 "                                    (eq (the rear-edge) :keyed)) 1)"
		 "                              ((and (eq (the front-edge) :keyed)"
		 "                                    (eq (the rear-edge) :keyed)) 0)))"
		 " :objects"
		 " (..."
		 "  ..."
		 "  (half-brick :type 'box"
		 "              :sequence (:size (the number-of-half-bricks))"
		 "              :center (cond ((and (= (the-child index) 0)"
		 "			           (eq (the front-edge) :full)) "
		 "                            (the first-half-brick-center!))"
		 "	                     ((and (= (the-child index) 0)"
		 "	                           (eq (the front-edge) :keyed)"
		 "	                           (eq (the rear-edge) :full)) "
		 "                            (the last-half-brick-center!))"
		 "                           ((eq (the rear-edge) :full) "
		 "                            (the last-half-brick-center!)))"
	       	 "               ...)))"))

   (code-3 (list "(define-object building (box)"
		 "  :input-slots"
		 "  (..."
		 "   ..."
		 "   (max-beam-spacing 1500))"
		 "  :computed-slots"
		 "  (..."
		 "   ..."
		 "   (number-of-roof-trusses (let ((trusses (ceiling (the left-wall-length) 1500)))"
		 "			      (max trusses 2)))"
		 "  ))"))

   (code-4 (list "(define-object truss (box)"
		 " ..."
		 " ..."
		 " :computed-slots"
		 "  (..."
		 "   ..."
		 "   ;;messages to support roof cladding sizing and positioning "
		 "   (apex-point (inter-line-plane (the rear-slope-construction-line end)"
		 "                                 (the truss-rear-slope-vector)"
		 "                                 (the lower-beam center)"
		 "                                 (the (face-normal-vector :rear))))"
		 "   (front-gutter-point (the front-slope-construction-line start))"
		 "   (rear-gutter-point (the rear-slope-construction-line start))"
		 "   (front-slope-normal (the front-slope-beam (face-normal-vector :top)))"
		 "   (rear-slope-normal (the rear-slope-beam (face-normal-vector :top))))"
		 " )"))
   (code-5 (list "(define-object building (box)"
		 "  :input-slots"
		 "   (..."
		 "    ..."
		 "    (roof-overhang 50)"
		 "    (cladding-thickness 10))"
		 ""
		 "  :computed-slots"
		 "   (..."
		 "    ..."
		 "    (roof-length (+ (the left-wall length) (twice (the roof-overhang))))"
		 "    (roof-width (the cladding-thickness))"
		 "    (roof-height (let ((apex (the (roof-truss 0) apex-point))"
		 "                       (gutter (the (roof-truss 0) front-gutter-point)))"
		 "                  (+ (3d-distance apex gutter) (the roof-overhang))))"))

   (code-6 (list "(define-object building (box)"
		 " ..."
		 " ..."
		 " :functions"
		 "  ((get-roof-mid-point! (first-gutter last-gutter last-index)"
		 "                          (let*((mid-gutter (midpoint first-gutter last-gutter))"
		 "                                (first-apex (the (roof-truss 0) apex-point))"
		 "                                (last-apex (the (roof-truss last-index) apex-point))"
		 "                                (mid-apex (midpoint first-apex last-apex))"
		 "                                (vec (subtract-vectors mid-gutter mid-apex))"
		 "                                (mid-edge (translate-along-vector "
		 "                                                   mid-gutter "
		 "                                                   vec "
		 "                                                   (the roof-overhang))))"
		 "                         (midpoint mid-apex mid-edge))))"
		 " :objects"
		 "  (..."
		 "   ..."
		 "   (roof-cladding-left "
		 "     :type 'box"
		 "     :length (the roof-length)"
		 "     :height (the roof-height)"
		 "     :width (the cladding-thickness)"
		 "     :orientation (alignment :left (the (roof-truss 0) front-slope-normal))"
		 "     :center (let* ((last-index (- (the number-of-roof-trusses) 1))"
		 "		      (first-gutter (the (roof-truss 0) front-gutter-point))"
		 "		      (last-gutter (the (roof-truss last-index) front-gutter-point))"
		 "		      (mid-ctr (the (get-roof-mid-point! "
		 "                                            first-gutter "
		 "                                            last-gutter "
		 "                                            last-index))))"
		 "		  (translate-along-vector "
		 "                             mid-ctr"
		 "			       (the (roof-truss 0) front-slope-normal)"
		 "			       (half (the cladding-thickness)))))"))

   (code-7 (list "(define-object building (box)"
		 " ..."
		 
		 ":computed-slots"
		 "(..."
		 " ..."
		 " ;; building properties"
		 " (walls (remove nil (mapcar #'(lambda(a) (when (typep a 'wall) a)) (the children))))"
		 " (full-bricks (apply '+ (mapsend (the walls) :full-bricks)))"
		 " (half-bricks (apply '+ (mapsend (the walls) :half-bricks)))"
		 " (mortar-volume (apply '+ (mapsend (the walls) :mortar-volume)))"
		 " (cladding-dimensions (list :length (the roof-length)"
		 "			    :width (the roof-height)))"
		 " (beam-properties (the (roof-truss 0) beam-properties))"
		 " (beam-qty-by-size "
		 "    (let ((res nil))"
		 "	(dolist (plis (the beam-properties) )"
		 "	  (let* ((trusses (the number-of-roof-trusses))"
		 "		 (l (getf plis :length-mm))"
		 "                (p (position l res :key #'(lambda(a) (getf a :length-mm))))"
		 "                (qty (when p (getf (nth p res) :qty))))"
		 "          (if p (setf (getf (nth p res) :qty) (+ qty trusses))"
		 "              (setq res (append (list (list :length-mm l :qty trusses)) res)))))"
		 "      (safe-sort '< res :key #'(lambda(a) (getf a :length-mm)))))"
		 ""
		 " (roof-truss-mass (* (apply '+ (mapcar #'(lambda(a) (getf a :mass-kg)) "
		 "                                       (the beam-properties)))"
		 "		       (the number-of-roof-trusses)))"
		 ""
		 " (building-materials "
		 "   (list :full-bricks (the full-bricks)"
		 "	 :half-bricks (the half-bricks)"
		 "	 :mortar-volume-m3 (div (the mortar-volume) 1000000000)"
		 "	 :beams (the beam-qty-by-size)"
		 "	 :roof-cladding (append (the cladding-dimensions) (list :qty 2))))"))
   (repl-1 (list (list :command "(setq self (make-object 'building)"
		       :output "#<BUILDING #x21045DF6AD>")
		  (list :command "(the building-materials)"
			:output (list "(:FULL-BRICKS 3109"
				      " :HALF-BRICKS 162"
				      " :MORTAR-VOLUME-M3 0.6665175"
				      " :BEAMS ((:LENGTH-MM 874.685657822283"
				      "          :QTY 3)"
				      "         (:LENGTH-MM 1727.7658984943535 "
				      "          :QTY 6)"
				      "         (:LENGTH-MM 3030 "
				      "          :QTY 3))"
				      " :ROOF-CLADDING (:LENGTH 4080 "
				      "                 :WIDTH 1806.755259207346 "
				      "                 :QTY 2))"))))
   
   (body-content (with-cl-who-string()
		   ((:div :class "main-page-container")
		    ((:div :class "main-page-item")
		     (str (the start-section main-div))
		     (str (the hint-1-section main-div))
		     (str (the hint-2-section main-div))
		     (str (the hint-3-section main-div))
		     (str (the hint-4-section main-div))
		     (str (the hint-5-section main-div)))
		    ((:div :class "main-page-item")
		     (:h2 "Resources")
		     (str (the resource-links-section main-div)))))))

  :objects
  ((resource-links-section :type 'sheet-section
			   :inner-html (with-cl-who-string()
					 (:table
					     (let ((icon "/common-images/lisp-file.png")
						   (lis (list (list :available (the hint-1) :file "building-hint-1.lisp")
							      (list :available (the hint-2) :file "building-hint-2.lisp")
							      (list :available (the hint-3) :file "building-hint-3.lisp")
							      (list :available (the hint-4) :file "building-hint-4.lisp")
							      (list :available (the hint-5) :file "building-hint-5.lisp"))))
					       (dolist (l lis)
						 (let* ((f (getf l :file))
						       (link (format nil "/t2-resources/~a" f)))
						   (htm (:tr (when (getf l :available)
							     (htm (:td ((:a :href link) ((:img :src icon :style "width: 40px; height: auto;"))))
								  (:td ((:a :href link) (str f)))))))))))))

   
   (start-section :type 'sheet-section
		  :inner-html (with-cl-who-string()

				(:div :class "grid-container-2-650px"
				      (:div :class "grid-item"
					    (:p "In our two worked examples we have made a wall and a roof truss.

In this worked example, using these objects we now want to create a building")
				(:h2 "Brief")
				(:p "Using the wall and truss objects, and modifying where necessary,
generate a model of a three-sided structure (4th side is open) with a
pitched roof. The 3 sides of the structure are brick walls and should
be keyed together where they join and have a flush face at the open
edge. The brick structure should be 3m wide, 4m long and 3m
high (nominal dimension), the roof slope should be 30 degrees. The
roof truss beams are 40mm wide and 50mm tall, with a wall thickness of
3mm and a density of 7800 kg/m" (:sup "3")". They should be equally
spaced, no more than 1.5m apart and the minimum number of roof trusses
should be used subject to there being at least 2. Cladding should be
added to the roof, such that it touches on the apex and overhangs at
the front, rear and sides by 50mm. The cladding is 10mm
thick. Determine the size of the cladding panels, the mass of the roof
trusses and the lengths of the individual beams, the number of bricks
and half-bricks required and the volume of morter. Bricks are the
standard size - 180mm long, 45mm high and 90mm wide") (str (the (hint-button :function-key :hint-1!)))))))


   (hint-1-section :type 'sheet-section
		   :inner-html (with-cl-who-string()
				 (when (the hint-1)
				   (htm
				    (:div :class "grid-container-2-650px"
					(:div :class "grid-item"
				    
					  (:p "The first stage in the process is to define the object structure of the model. We will have"
					      (:ul (:li "A left wall")
						   (:li "A right wall")
						   (:li "A rear wall")
						   (:li "A number of roof trusses")
						   (:li "A left side roof cladding")
						   (:li "A right side roof cladding")))
					  (:p "Then we pass the nominal structure dimensions into the wall objects"
					      (:ul (:li "Right and Left wall "
							((:span :class "slot")":wall-length")" and "
							((:span :class "slot")":wall-height")" are nominal height and length respectively")
						   (:li "Rear wall "
							((:span :class "slot")":wall-length")" is the nominal width and "
						((:span :class "slot")":wall-height")" is the nominal height"))
					      "The wall objects provide "
					      ((:span :class "slot")":length")" and "
					      ((:span :class "slot")":height")" messages which are the actual dimensions, based on the supplied nominal dimensions. We use these values to specify"
					      (:ul (:li "The Truss "
							((:span :class "slot")":truss-length")" (equal to the rear wall "
							((:span :class "slot")":length")")")
						   (:li "The building length (equal to the left/right wall "
							((:span :class "slot")":length")"")
						   (:li "The building width (equal to the rear wall "
							((:span :class "slot")":length")")")
						   (:li "The building height (equal to the Truss "
							((:span :class "slot")":height")" plus any of the walls "
							((:span :class "slot")":height")""))
					      "We have now defined the bounding boxes of the 3 walls, the trusses and the overall building")
					  (:p "Next we need to position the objects. First the positioning of the walls:"
					      (:ul (:li "The left and right walls are positioned relative to the "
							((:span :class "function")"(edge-center :left :bottom)")" and the "
							((:span :class "function")"(edge-center :right :bottom)")" points of the main bounding box")
						   (:li "The rear wall is positioned relative to the "
							((:span :class "function")"(edge-center :rear :bottom)")" of the main bounding box"))
					      "Positioning of the trusses will be relative to the "
					      ((:span :class "function")"(edge-center :front :top)")" of the main bounding box, but we need to calculate the
offsets of the trusses from this point based on the overall length and the number of trusses. In the first instance we will hard-code the number of trusses,
calculate these points and then apply them t the sequence of trusses based on the index number of the truss")
					  (:p "Orientation is relatively straight forward"
					      (:ul (:li "The right and left walls do not need any change to orientation")
						   (:li "The rear wall has its "((:span :class "object-keyword")":rear")" axis aligned with the main bounding box "
							((:span :class "function")"(face-normal-vector :right)")"")
						   (:li "Each of the trusses has their "((:span :class "object-keyword")":rear")" axis aligned with the main bounding box "
							((:span :class "function")"(face-normal-vector :right)")"")))
					  (:p "With all of these calculations implemented, the bounding boxes of each object may now be viewed in Geysr to ensure everything aligns correctly")
					  (:p "The next step is to modify the wall object to enable the left to rear and right to rear joints to key together")
					  (str (the (hint-button :function-key :hint-2!)))))
				    (:div :class "grid-item"
					  (:img :src (format nil "/~a-images/geysr-building-1.png" (the publish-prefix))
                                                :style "width: auto; height: 300px; margin: 1em 0 1em 3% ;" ))))))

   (hint-2-section :type 'sheet-section
		   :inner-html (with-cl-who-string()
				 (when (the hint-2)
				   (htm ((:div :class "main-page-container")
					 ((:div :class "main-page-item")
					  (:p "To key 2 walls together we need 2 conditions"
					      (:ul (:li "For 2 adjoining walls, the first row in one must start with full bricks and the first row in the other must start with half bricks")
						   (:li "On the joining edge, the half bricks must be left out, so the gap can be filled by the full brick of the adjoining wall"))
					      "We will deal with these seperately")
					  (:h2 "Full or Half brick starting row")
					  (:ul (:li "Add an "
						    ((:span :class "object-keyword")":input-slot")" "
						    ((:span :class "slot")"first-row")" to the "
						    ((:span :class "object")"wall")" object")
					       (:li "Its values are "
						    ((:span :class "object-keyword")":full-start")" for the first row to be started with full bricks, and "
						    ((:span :class "object-keyword")":half-start")" for the first row to be started with half bricks")
					       (:li "Pass this variable into the "
						    ((:span :class "object")"row")" object")
					  (:li "Modify the "
					       ((:span :class "object-keyword")":computed-slot")" "
					       ((:span :class "slot")"full-brick-row?")" in the "
					       ((:span :class "object")"row")" object to include the value of this slot in its logic"))
					 (:p "A simple way to test this is to create 2 test objects, say "
					     ((:span :class "object")"full-start-wall")" and "
					     ((:span :class "object")"half-start-wall")". Both mixin the "
					     ((:span :class "object")"wall")" object and whilst "
					     ((:span :class "object")"full-start-wall")" sets the "
					     ((:span :class "object-keyword")":input-slot")" "
					     ((:span :class "slot")"first-row")" to "
					     ((:span :class "object-keyword")":full-start")", "
					     ((:span :class "object")"half-start-wall")" sets the "
					     ((:span :class "object-keyword")":input-slot")" "
					     ((:span :class "slot")"first-row")" to "
					     ((:span :class "object-keyword")":half-start")". (Note that strictly, because wall defaults "
					     ((:span :class "slot")"first-row")" to "
					     ((:span :class "object-keyword")":full-start")" we don't need to provide any "
					     ((:span :class "object-keyword")":input-slot")" values for "
					     ((:span :class "object")"full-start-wall")", its just simpler to see what the 2 different values are) We can then instantiate both objects in Geysr and test that one starts with full bricks on the first row whilst the other starts with half bricks on the first row"))
					 ((:div :class "main-page-item"))
					 ((:div :class "main-page-item")
					  (str (code-example (the code-1))))
					 ((:div :class "main-page-item")
					  (:img :src (format nil "/~a-images/geysr-building-2.png" (the publish-prefix)) :style "width: auto; height: 300px; margin: 1em 0 1em 3% ;" )
					  (:img :src (format nil "/~a-images/geysr-building-3.png" (the publish-prefix)) :style "width: auto; height: 290px; margin: 1em 0 1em 3% ;" ))
					 ((:div :class "main-page-item")
					  (:h2 "Full or Keyed edges")
					  (:ul (:li "Add 2 "
						    ((:span :class "object-keyword")":input-slots")", "
						    ((:span :class "slot")"front-edge")" and "
						    ((:span :class "slot")"rear-edge")" to the wall object. Keyword values will be "
						    ((:span :class "object-keyword")":full")" for a flush edge, "
						    ((:span :class "object-keyword")":keyed")" for a keyed edge")
					       (:li "Pass these values down through the "
						    ((:span :class "object")"row")" object into the "
						    ((:span :class "object")"bricks-and-mortar")" object")
					       (:li "Update the "
						    ((:span :class "object-keyword")":computed-slot")" "
						    ((:span :class "slot")"first-mortar-joint-start-point")". The original definition located the first mortar joint off the back face of the firt half brick when it was a half-brick row. An extra condition is added to calculate the location point when the front edge of the row is "
						    ((:span :class "object-keyword")":keyed")"")
					       (:li "Add a new "
						    ((:span :class "object-keyword")":computed-slot")" "
						    ((:span :class "slot")"number-of-half-bricks")" which defines how many half bricks are present depending on whether the row is a full brick row or whether the front, rear or both edges are "
						    ((:span :class "object-keyword")":keyed"))
					       (:li "Update the definition of the "((:span :class "object")"half-brick")" child object, such that the "
						    ((:span :class "object-keyword")":sequence (:size)")" refers to "
						    ((:span :class "slot")" (the number-of-half-bricks)")" and the "
						    ((:span :class "object-keyword")":center")" position depends on whether it is the front or rear half brick and whether or not this is "
						    ((:span :class "object-keyword")":full")" or "
						    ((:span :class "object-keyword")":keyed")))
					  (:p "Again, we can create some simple test objects to test the different "
					      ((:span :class "object-keyword")":keyed")" or "
					      ((:span :class "object-keyword")":full")" combinations"))
					 ((:div :class "main-page-item"))
					 ((:div :class "main-page-item")
					  (str (code-example (the code-2))))
					 ((:div :class "main-page-item")
					  (:img :src (format nil "/~a-images/geysr-building-4.png" (the publish-prefix)) :style "width: auto; height: 300px; margin: 1em 0 1em 3% ;" )
					  (:img :src (format nil "/~a-images/geysr-building-5.png" (the publish-prefix)) :style "width: auto; height: 270px; margin: 1em 0 1em 3% ;" )
					  (:img :src (format nil "/~a-images/geysr-building-6.png" (the publish-prefix)) :style "width: auto; height: 285px; margin: 1em 0 1em 3% ;" )))
					(:p "Up to now, the number of trusses has been hard coded, we now need to make that dependant on the structure length")
					(str (the (hint-button :function-key :hint-3!))) ))))

   (hint-3-section :type 'sheet-section
		   :inner-html (with-cl-who-string()
				 (when (the hint-3)
				   (htm (:div :class "grid-container-2-650px"
					(:div :class "grid-item"
					  (:p "The requirements says there should be a minimum of 2 roof trusses, they should be no more than 1500mm apart and the minimum number of trusses should be used. The trusses are spaced along the side wall length side, so if we divide that length "
					      ((:span :class "slot")"(the left-wall length)")" by 1500 we get the exact pitch. We need an integer and we need to round up so the "
					      ((:span :class "function")"ceiling")" function is used which round up to the nearest whole number. Finally should return this number or 2, whichever is greatest")
					(str (code-example (the code-3)))
					(:p "Our final task in the design is to calculate the size of the roof panels and position and orient them")

                                        (str (the (hint-button :function-key :hint-4!)))                                        
					))))))

   
   (hint-4-section :type 'sheet-section
		   :inner-html (with-cl-who-string()
				 (when (the hint-4)
				   (htm
				    (:div :class "grid-container-2-650px"
					  (:div :class "grid-item"
						(:p "First we need to calculate the apex point for the roof. There are a number of approaches for this; here we choose to augment the "
					    ((:span :class "object")"truss")" object to add in an "
					    ((:span :class "slot")"apex-point")" message, using the GendL "
					    ((:span :class "function")"inter-line-plane")" function. The line is defined as a point and vector (in our case the start of the foor slope construction line and the roof slope vector) and the plane is defines as a point and a vector which is normal to the plane (in out case the center point of the lower beam and the "
					    ((:span :class "function")"(face-normal-vector :rear)")" of the "
					    ((:span :class "object")"truss")" bounding box). We also add some further messages which we need to compute the roof size and orientation"
					    (:ul (:li ((:span :class "slot")"front-gutter-point")" - the bottom edge of the front-slope-beam")
						 (:li ((:span :class "slot")"rear-gutter-point")" - the bottom edge of the rear-slope-beam")
						 (:li ((:span :class "slot")"front-slope-normal")" - the (face-normal-vector :top) of the front-slope-beam")
						 (:li ((:span :class "slot")"rear-slope-normal")" - the (face-normal-vector :top) of the rear-slope-beam")))
					(str (code-example (the code-4)))
					(:p "Next we need to add the remaining "
					    ((:span :class "object-keyword")":input-slots")" which enable the dimensions of the roof cladding to be calculated and add the "
					    ((:span :class "object-keyword")":computed-slots")" which will evaluate to the roof cladding dimensions"
					    (str (code-example (the code-5)))
					    (:p "And finally we set the "
						((:span :class "slot")":length")", "
						((:span :class "slot")":width")" and "
						((:span :class "slot")":height")" of the "
						((:span :class "object")"roof-cladding-right")" and "
						((:span :class "object")"roof-cladding-left")" objects, align their orientation such that their "
						((:span :class "object-keyword")":left")" axis is aligned twith the"
						((:span :class "function")"(face-normal-vector :top)")" of the sloping beams, and define their center points. The "
						((:span :class "slot")":center")" calculation is done using a "
						((:span :class "special-operator")"let")" binding, but since some of the local bindings need previously defined symbols, we use the "
						((:span :class "special-operator")"let*")" special operator rather than "
						((:span :class "special-operator")"let")". Also note the use of a "
						((:span :class "object-keyword")":function")" "
						((:span :class "function")":get-roof-mid-point!")" as the processing of this is common to both the left and right cladding")))
					(:div :class "grid-item")
					(:div :class "grid-item"
					      (str (code-example (the code-6))))
					(:div :class "grid-item"
					      (:img :src (format nil "/~a-images/geysr-building-7.png" (the publish-prefix)) :style "width: auto; height: 400px; margin: 1em 0 1em 3% ;" ))
						(:div :class "grid-item"
					              (:p "To complete the brief we now need to assemble the building properties")

                                                      (str (the (hint-button :function-key :hint-5!)))))))))


   (hint-5-section :type 'sheet-section
		   :inner-html (with-cl-who-string()
				 (when (the hint-5)
				   (htm
				    (:div :class "grid-container-2-650px"
					(:div :class "grid-item"
				    (:p "Each of the "
					    ((:span :class "object")"wall")" objects supports the messages "
					    ((:span :class "slot")"full-bricks")", "
					    ((:span :class "slot")"half-bricks")" and "
					    ((:span :class "slot")"mortar-volume")". We need to retrieve and sum these")
					(:p "The roof cladding dimensions can be taken from the inputs to the "
					    ((:span :class "object")"roof-cladding-right")" and "
					    ((:span :class "object")"roof-cladding-left")" objects")
					(:p "Each "
					    ((:span :class "object")"roof-truss")" supports the message "
					    ((:span :class "slot")"beam-properties")". As all the "
					    ((:span :class "object")"roof-truss")" objects are identical, we can just work with "
					    ((:span :class "slot")"beam-properties")" from one "
					    ((:span :class "object")"roof-truss")" and multiply by "
					    ((:span :class "slot")"(the number-of-roof-trusses)")". We need to process this list to determine quantities of beams by size and the total truss mass. Note that when processing the "
					    ((:span :class "slot")"beam-properties")" to obtain "
					    ((:span :class "slot")"(the beam-qty-by-size)")" the "
					    ((:span :class "macro")"dolist")" iteration doesn't return anything , its just side-effecting on the locally bound variable "
					    ((:span :class "slot")"res")" and then returns the value of "
					    ((:span :class "slot")"res")" sorted by "
					    ((:span :class "object-keyword")":length-mm")". The final return value is a plist,  "
					    ((:span :class "slot")"building-materials")))
					(:div :class "grid-item")
					(:div :class "grid-item"
					  (str (code-example (the code-7))))
					 (:div :class "grid-item"
					 (str (repl-example (the repl-1)))) )))))))

