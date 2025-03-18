(in-package :training-2)

(define-object wall-example (base-training-sheet)
  :computed-slots
  ((index-words (list ":pass-down" "list-elements" "the-element" "the-object" "theo" "apply" "+" "apply '+" "zerop" "evenp" "last (sequence)"
		      "let" "local binding" "translate-along-vector" "face-center" "face-normal-vector" ":functions" "debugging" "code maintenance" "code readability"))

   (hint-1 nil :settable)
   (hint-2 nil :settable)
   (hint-3 nil :settable)
   (hint-4 nil :settable)
   (hint-5 nil :settable)
   (hint-6 nil :settable)
   (hint-7 nil :settable)

   
   
   (code-1 (list "(define-object wall()"
		 "  :objects"
		 "  ((row :type 'row))"
		 " )"
		 ""
		 "(define-object row ()"
		 "  :objects"
		 "  ((bricks-and-mortar :type 'bricks-and-mortar)"
		 "   (mortar-bed :type 'box)))"
		 ""
		 "(define-object bricks-and-mortar ()"
		 "  :objects"
		 "  ((full-brick :type 'box)"
		 "   (half-brick :type 'box)"
		 "   (mortar-joint :type 'box)))"))

   (code-2 (list "(define-object bricks-and-mortar ()"
		 "  :input-slots"
		 "  (brick-height"
		 "   brick-length"
		 "   brick-width"
		 "   mortar-joint-width)"
		 ""
		 "  :objects"
		 "  ((full-brick :type 'box"
		 "	           :length (the brick-length)"
		 "	           :height (the brick-height)"
		 "	           :width (the brick-width))"
		 "   (half-brick :type 'box"
		 "	           :length (half (the brick-length))"
		 "	           :height (the brick-height)"
		 "	           :width (the brick-width))"
		 "   (mortar-joint :type 'box"
		 "		     :height (the brick-height)"
		 "		     :width (the brick-width)"
		 "		     :length (the mortar-joint-width))))"))

   (code-3 (list "(define-object row ()"
		 "  :input-slots"
		 "  (brick-height"
		 "   brick-length"
		 "   brick-width"
		 "   mortar-joint-width)"
		 ""
		 "  :objects"
		 "  ((bricks-and-mortar :type 'bricks-and-mortar"
		 "  		        :brick-height (the brick-height)"
		 "  		        :brick-length (the brick-length)"
		 "  		        :brick-width (the brick-width)"
		 "  		        :mortar-joint-width (the mortar-joint-width))"
		 "   (mortar-bed :type 'box"
		 "	         :height (the mortar-joint-width))))"))

   (code-4 (list "(define-object wall()"
		 "  :input-slots"
		 "  ((brick-height 45)"
		 "   (brick-length 180)"
		 "   (brick-width 90)"
		 "   (mortar-joint-width 10)"
		 "   (wall-length 3700)"
		 "   (wall-height 900))"
		 "   "
		 "  :objects"
		 "  ((row :type 'row"
		 "        :brick-height(the brick-height)"
		 "        :brick-length (the brick-length)"
		 "        :brick-width (the brick-width)"
		 "        :mortar-joint-width (the mortar-joint-width))))"))

   (code-5 (list "(define-object wall(box)"
		 ":input-slots"
		 "((brick-height 45)"
		 " (brick-length 180)"
		 " (brick-width 90)"
		 " (mortar-joint-width 10)"
		 " (wall-length 3700)"
		 " (wall-height 900))"
		 ""
		 ":computed-slots"
		 "((row-height (+ (the brick-height) (the mortar-joint-width)))"
		 " (number-of-rows (round-to-nearest (div (the wall-height) (the row-height)) 1))"
		 " (actual-wall-height (* (the row-height) (the number-of-rows)))"
		 ""
		 " ;; for the wall-length we need the number of full bricks"
		 " ;; if there are n full bricks then there will be (n-1) mortar joints"
		 " ;; so n*brick-length + n-1*mortar-joint-width = overall-length"
		 " ;; or n(brick-length + mortar-join-width) - mortar-joint-width = overall-length"
		 " ;; or n = (overall-length - mortar-joint-width)/(brick-length + mortar-joint-width)"
		 " (number-of-bricks (round-to-nearest "
		 "                       (div (- (the wall-length) (the mortar-joint-width))"
		 "			      (+ (the brick-length) (the mortar-joint-width)))"
		 "			 1))"
		 " (actual-wall-length (+ (* (the number-of-bricks) (the brick-length))"
		 "			  (* (- (the number-of-bricks) 1) (the mortar-joint-width))))"
		 ""
		 " ;; box inputs - gives the wall bounding box"
		 " (height (the actual-wall-height))"
		 " (width (the brick-width))"
		 " (length (the actual-wall-length)))"
		 " "
		 ":objects"
		 "((row :type 'row"
		 "      :sequence (:size (the number-of-rows))"
		 "      :bricks-per-row (the number-of-bricks)"
		 "      :length (the length)"
		 "      :width (the width)"
		 "      :brick-height(the brick-height)"
		 "      :brick-length (the brick-length)"
		 "      :brick-width (the brick-width)"
		 "      :mortar-joint (the mortar-joint-width))))"))

   (code-6 (list "(define-object wall(box)"
		 "..."
		 "..."
		 "..."
		 ":objects"
		 "((row :type 'row"
		 "      :sequence (:size (the number-of-rows))"
		 "      :center (translate-along-vector (the (face-center :bottom))"
		 "				       (the (face-normal-vector :top))"
		 "				       (+ (half (the-child height))"
		 "					  (* (the-child index) (the-child height))))"
		 "      :length (the length)"
		 "      :width (the width)"
		 "      :height (+ (the brick-height) (the mortar-joint-width)))"
		 "      :bricks-per-row (the number-of-bricks)"
		 "      :brick-height(the brick-height)"
		 "      :brick-length (the brick-length)"
		 "      :brick-width (the brick-width)"
		 "      :mortar-joint-width (the mortar-joint-width))"))

   (code-7 (list "(define-object row (box)"
		 ":input-slots"
		 "(brick-height"
		 " brick-length"
		 " brick-width"
		 " mortar-joint-width)"
		 ""
		 ":objects"
		 "((bricks-and-mortar :type 'bricks-and-mortar"
		 "		      :width (the width)"
		 "		      :length (the length)"
		 "		      :height (the brick-height)"
		 "		      :center (translate-along-vector "
		 "                               (the mortar-bed (face-center :top))"
		 "				 (the mortar-bed (face-normal-vector :top))"
		 "				 (half (the-child height)))"
		 "		      :brick-height (the brick-height)"
		 "		      :brick-length (the brick-length)"
		 "		      :brick-width (the brick-width)"
		 "		      :mortar-joint-width (the mortar-joint-width))"
		 " (mortar-bed :type 'box"
		 "	       :height (the mortar-joint-width)"
		 "	       :width (the width)"
		 "	       :length (the length)"
		 "	       :center (translate-along-vector (the (face-center :bottom))"
		 "					       (the (face-normal-vector :top))"
		 "					       (half (the-child height))))))"))


   (code-8 (list "(define-object row (box)"
		 ":input-slots"
		 "(full-bricks-per-row"
		 " brick-height"
		 " brick-length"
		 " brick-width"
		 " mortar-joint-width)"
		 ""
		 ":computed-slots"
		 " ((full-brick-row? (or (zerop (the index)) (evenp (the index)))))"
		 ""
		 ":objects"
		 "((bricks-and-mortar :type 'bricks-and-mortar"
		 "		      :height (the brick-height)"
		 "		      :center (translate-along-vector "
		 "                                     (the mortar-bed (face-center :top))"
		 "				       (the mortar-bed (face-normal-vector :top))"
		 "				       (half (the-child height)))"
		 "		      :pass-down (width"
		 "				  length"
		 "				  full-brick-row?"
		 "				  brick-height"
		 "				  brick-length"
		 "				  brick-width"
		 "				  mortar-joint-width"
		 "				  full-bricks-per-row))"
		 "  " 
		 " (mortar-bed :type 'box"
		 "	       :height (the mortar-joint-width)"
		 "	       :center (translate-along-vector (the (face-center :bottom))"
		 "					       (the (face-normal-vector :top))"
		 "					       (half (the-child height)))"
		 "	       :pass-down (width"
		 "			   length))))"))

   (code-9 (list "(define-object bricks-and-mortar (box)"
		 "..."
		 "..."
		 "..."
		 ":computed-slots"
		 "((first-full-brick-start-point (if (the full-brick-row?)"
		 "				     (the (face-center :front))"
		 "				     (the (mortar-joint 0) (face-center :rear))))"
		 ""
		 " (first-mortar-joint-start-point (if (the full-brick-row?)"
		 "				       (the (full-brick 0) (face-center :rear))"
		 "				       (the (half-brick 0) (face-center :rear))))"
		 ""
		 " (number-of-full-bricks (if (the full-brick-row?)"
		 "			      (the full-bricks-per-row)"
		 "			      (- (the full-bricks-per-row) 1)))"
		 ""
		 " (number-of-mortar-joints (if (the full-brick-row?)"
		 "				(- (the number-of-full-bricks) 1)"
		 "				(+ (the number-of-full-bricks) 1)))"
		 ""
		 "   ;; if it isn't a full brick row then there will be an extra joint because one "
		 "   ;; full brick is replaced with 2 half bricks so without correcting the "
		 "   ;; mortar-joint-width the ends of a full brick rowand one startng and"
		 "   ;;  finishing with half bricks won't align. So we need to correct "
		 "   ;; the mortar-joint-width"
		 " (corrected-joint-width (if (the full-brick-row?)"
		 "		              (the mortar-joint-width)"
		 "		              (let ((total-gap (* (- (the number-of-mortar-joints) 1)"
		 "					          (the mortar-joint-width))))"
		 "                                 (div total-gap (the number-of-mortar-joints))))))"
		 "..."
		 "..."
		 ")"))

   (code-10 (list "(define-object bricks-and-mortar (box)"
		  "..."
		  "..."
		  "..."
		  ":functions"
		  "((first-full-brick-center!"
		  "   ()"
		  "   (translate-along-vector (the first-full-brick-start-point)"
		  "			   (the (face-normal-vector :rear))"
		  "			   (half (the brick-length))))"
		  ""
		  " (other-full-brick-center!"
		  "   (index)"
		  "   ;; if its a full brick row, (full-brick 1) is positioned relative to (joint 0)"
		  "   ;; if its a half brick row, (full-brick 1) is positioned relative to (joint 1)"
		  "   (let ((ind (if (the full-brick-row?) (- index 1) index)))"
		  "      (translate-along-vector (the (mortar-joint ind) (face-center :rear))"
		  "			      (the (face-normal-vector :rear))"
		  "			      (half (the brick-length)))))"
		  ""
		  " (first-joint-center!"
		  "   ()"
		  "   (translate-along-vector (the first-mortar-joint-start-point)"
		  "			    (the (face-normal-vector :rear))"
		  "			    (half (the corrected-joint-width))))"
		  ""
		  " (other-joint-center!"
		  "   (index)"
		  "   ;; if its a full brick row, (joint 1) is positioned relative to (full-brick 1)"
		  "   ;; if its a half brick row, (joint 1) is positioned relative to (full-brick 0)"
		  "   (let ((ind (if (the full-brick-row?) index (- index 1))))"
		  "      (translate-along-vector (the (full-brick ind) (face-center :rear))"
		  "			      (the (face-normal-vector :rear))"
		  "			      (half (the corrected-joint-width)))))"
		  ""
		  " (first-half-brick-center!"
		  "   ()"
		  "   (translate-along-vector (the (face-center :front))"
		  "			   (the (face-normal-vector :rear))"
		  "			   (half (half (the brick-length)))))"
		  ""
		  "  (last-half-brick-center!"
		  "    ()"
		  "    (translate-along-vector (theo (the mortar-joint last) (face-center :rear))"
		  "			    (the (face-normal-vector :rear))"
		  "			    (half (half (the brick-length))))))"
		  "..."
		  "..."
		  ")" ))
   
   (code-11 (list "(define-object bricks-and-mortar (box)"
		  "..."
		  "..."
		  "..."
		  " :objects"
		  " ((full-brick :type 'box"
		  "	         :sequence (:size (the number-of-full-bricks))"
		  "	         :center (if (= (the-child index) 0)"
		  "			   (the first-full-brick-center!)"
		  "			   (the (other-full-brick-center! (the-child index))))"
		  "              :length (the brick-length)"
		  "              :height (the brick-height)"
		  "	         :width (the brick-width))"
		  ""
		  "  (half-brick :type 'box"
		  "	         :sequence (:size (if (the full-brick-row?) 0 2))"
		  "	         :center (if (= (the-child index) 0)"
		  "		   	     (the first-half-brick-center!)"
		  "			     (the last-half-brick-center!))"
		  "	         :length (half (the brick-length))"
		  "	         :height (the brick-height)"
		  "	         :width (the brick-width))"
		  ""
		  "  (mortar-joint :type 'box"
		  "		   :sequence (:size (the number-of-mortar-joints))"
		  "		   :center (if (= (the-child index) 0)"
		  "			       (the first-joint-center!)"
		  "			       (the (other-joint-center! (the-child index))))  " 
		  "		   :height (the brick-height)"
		  "		   :width (the brick-width)"
		  "		   :length (the corrected-joint-width))))"))

   (code-12 (list "(define-object wall(box)"
		  "  :computed-slots"
		  "  ((full-bricks (apply '+ (list-elements (the row) (the-element full-bricks))))"
		  "   (half-bricks (apply '+ (list-elements (the row) (the-element half-bricks))))"
		  "   (mortar-volume (apply '+ (list-elements (the row) (the-element mortar-volume))))"
		  "   (mortar-density 2162)"
		  "   (mortar-mass (* (the mortar-density) (div (the mortar-volume) 1000000000)))))"
		  ""
		  "(define-object row (box)"
		  "  :computed-slots"
		  "  ((full-bricks (the bricks-and-mortar full-bricks))"
		  "   (half-bricks (the bricks-and-mortar half-bricks))"
		  "   (mortar-volume (+ (the bricks-and-mortar mortar-joint-volume)"
		  "		     (the mortar-bed volume)))))"
		  ""
		  "(define-object bricks-and-mortar (box)"
		  "  :computed-slots"
		  "  ("
		  "   ;; collating the output. We could do this analytically,"
		  "   ;; but for this example we'll use the geometry"
		  "   (full-bricks (length (list-elements (the full-brick))))"
		  "   (half-bricks (length (list-elements (the half-brick))))"
		  "   (mortar-joint-volume (apply '+ (list-elements (the mortar-joint)"
		  "                                              (the-element volume))))))"))

   (repl-1 (list (list :command "(setq self (make-object 'wall))"
		       :output "#<GDL-USER::WALL #x210582B91D>")
		 
		 (list :command "(the full-bricks)"
		       :output 296)
		 (list :command "(the half-bricks)"
		       :output 16)
		 (list :command "(the mortar-volume)"
		       :output 6.3504E+7)
		 (list :command "(the mortar-mass)"
		       :output 137.295648)))


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
		     (str (the hint-7-section main-div)))
		    
		    ((:div :class "main-page-item")
		     (:h2 "Resources")
		     (str (the resource-links-section main-div)))))))

  :functions
  (
   ;;
   ;; FLAG - reduce these repetitive slots somehow.
   ;;
   (hint-1! () (the (set-slot! :hint-1 (not (the hint-1)))))
   (hint-2! () (the (set-slot! :hint-2 (not (the hint-2)))))
   (hint-3! () (the (set-slot! :hint-3 (not (the hint-3)))))
   (hint-4! () (the (set-slot! :hint-4 (not (the hint-4)))))
   (hint-5! () (the (set-slot! :hint-5 (not (the hint-5)))))
   (hint-6! () (the (set-slot! :hint-6 (not (the hint-6)))))
   (hint-7! () (the (set-slot! :hint-7 (not (the hint-7)))))


   )

  :objects
  ((start-section
    :type 'sheet-section
    :inner-html (with-cl-who-string ()
		  (:div :class "grid-container-2-650px"
			(:div :class "grid-item"
			      (:p "Up to now we have covered the basics of "
			          (:ul
                                   (:li "Defining & Instantiating GendL objects")
				   (:li "Positioning and orienting geometry")
				   (:li "Defining child objects and sequences of child objects")
				   (:li "Dealing with numbers and lists")
				   (:li "Defining standard CL functions and GendL object functions"))
			          "This tutorial aims to bring the above concepts together with a
tangible example. Starting with the example briefing below, you can
either try to develop your own solution or build up a solution in a
guided step-by-step manner by clicking the Hint button")
			      (:h3 "Example Briefing")
			      (:p "Imagine a wall is built from bricks which
are 180mm long, 45mm high and 90mm wide. Each course (i.e. row) of
bricks sits on a bed of mortar which is 10mm thick, and each brick is
joined to its lateral neighbor(s) with mortar having a nominal joint
thickness of 10mm. While a wall instance can be specified using
nominal dimensions for length and height, the actual dimensions may
vary slightly in order to use a whole number of bricks per
row. Vertically adjacent rows of bricks are offset by half a
brick with respect to the row below and so will consist of a
half-brick at the beginning and a half-brick at the end of the
row (i.e. row).")

                              (:p "Define and instantiate a GendL model of a wall which has a default
nominal height of 900mm and a nominal length of 3700mm, and report
the number of full bricks and number of half bricks used. Assuming the
density of mortar is 2182kg/m"(:sup "3") " determine also the mass of
mortar used")))
                  
		  (:p (str (the (hint-button :function-key :hint-1!))))))
                  
   (hint-1-section
    :type 'sheet-section
    :inner-html (with-cl-who-string()
		  (when (the hint-1)
		    (htm
		     (:div :class "grid-container-2-650px"
			   (:div :class "grid-item"
				 (:p "Start by thinking of the structure"
				     (:ul (:li "A wall has rows of bricks and mortar.")
					  (:li "A row of bricks and morter has a row of bricks seperated by mortar joints, sat on a bed of mortar")
					  (:li "A row of bricks seperated by mortar joints is made of full bricks and half bricks and mortar")
					  (:li "A bed of mortar sits below every row of bricks separated by mortar joints"))
				     "And from that conceptual description, lay down a definition for an
outline object structure. Note that while as yet it contains neither dimensions
nor mixins, at its leaf level it is already using GendL wireframe geometry primitives")))
                     
		     (str (code-example (the code-1)))
                     
		     (str (the (hint-button :function-key :hint-2!)))))))
                                            
   (hint-2-section
    :type 'sheet-section
    :inner-html (with-cl-who-string()
		  (when (the hint-2)
		    (htm
		     (:div :class "grid-container-2-650px"
			   (:div :class "grid-item"
				 (:p "Working from the bottom with the GendL leaf instances of "
				     ((:span :class "object")"brick")", "
				     ((:span :class "object")"half-brick")" and "
				     ((:span :class "object")"mortar-joint")" (all standard "
                                     (:span :class "object" "box") "es), determine appropriate referencing expressions for their
dimensional inputs. This will also inform as to what "
                                     (:span :class "object-keyword" ":input-slots")
                                     " you will need coming into the "
                                     ((:span :class "object") "bricks-and-mortar")" parent object.")
				 (str (code-example (the code-2)))
				 (:p "Then move up to the "((:span :class "object") "row")" object and repeat")
				 (str (code-example (the code-3)))
				 (:p "Then move up to the "
				     ((:span :class "object")"wall")" object and repeat, but with this object add the default values for "
				     ((:span :class "object-keyword")":input-slots")", and the other 2 known inputs "
				     ((:span :class "slot")"wall-height")" and "
				     ((:span :class "slot")"wall-length"))
				 (str (code-example (the code-4)))))
                     
		     (:p (str (the (hint-button :function-key :hint-3!))))))))

   (hint-3-section :type 'sheet-section
		   :inner-html (with-cl-who-string()
				 (when (the hint-3)
				   (htm
				    (:div :class "grid-container-2-650px"
					  (:div :class "grid-item"
						(:p "Now working from the top down you need to determine the actual overall dimensions of the "
					    ((:span :class "object")"wall")" and from the the number of "
					    ((:span :class "object")"rows")". If you use a "
					    ((:span :class "object")"box")" mixin for the "
					    ((:span :class "object")"wall")
                                            " object and specify its "
                                            (:span :class "slot" "width") ", "
                                            (:span :class "slot" "length") ", and "
                                            (:span :class "slot" "height")                                             
                                            " appropriately (and leaving "
                                            (:span :class "slot" "center") " and "
                                            (:span :class "slot" "orientation")
                                            " to their default values for now), then you can probe and display its reference box i.e. bounding-box. Having calculated the "
					    ((:span :class "slot")"number-of-rows")", you can also set the "
					    ((:span :class "object-keyword")":sequence (:size )")" for the "
					    ((:span :class "object")"row")" object and additionally pass in the "
					    ((:span :class "slot")"bricks-per-row")", "
					    ((:span :class "slot")"width")" and "
					    ((:span :class "slot")"length")". Compiling "
					    ((:span :class "object")"wall")" and displaying in Geysr, you can show the overall bounding box and see the sequence of "
					    ((:span :class "object")"row")" objects"))
					 (:div :class "grid-item") 
					(:div :class "grid-item"
					  (str (code-example (the code-5))))
					(:div :class "grid-item"
					  (:img :src (format nil "/~a-images/geysr-wall-1.png" (the publish-prefix)) :style "width: auto; height: 300px; margin: 1em 0 1em 3% ;" )))
				    (:p (str (the (hint-button :function-key :hint-4!))))))))
                                                               
   (hint-4-section :type 'sheet-section
		   :inner-html (with-cl-who-string()
				 (when (the hint-4)
				   (htm
				    (:div :class "grid-container-2-650px"
					  (:div :class "grid-item"
						(:p "The next task is to position each row of bricks, first by positioning the bounding box for each "
					    ((:span :class "object")"row")". Each "
					    ((:span :class "object")"row")" is "
					    ((:span :class "slot")"(the brick-height)")" + "
					    ((:span :class "slot")"(the mortar-jpint-width)")" high. The center of the first "
					    ((:span :class "object")"row")" is half of this dimension above the bottom of the "
					    ((:span :class "object")"wall")", and the center of each subsequent "
					    ((:span :class "object")"row")" is the "
					    ((:span :class "object")"row")" height above the center of the previous "
					    ((:span :class "object")"row")". Updating the definition of "
					    ((:span :class "object")"row")" to use "
					    ((:span :class "object")"box")" as its mixin and then specifying "
					    ((:span :class "object-keyword")":center")" and "
					    ((:span :class "object-keyword")":height")" is sufficient to define and position each "
					    ((:span :class "object")"row")" of the "
					    ((:span :class "object")"wall")". The code below shows just the update to the definition of the "
					    ((:span :class "object")"row")" child object of "
					    ((:span :class "object")"wall")))
					 (:div :class "grid-item") 
					(:div :class "grid-item"
					  (str (code-example (the code-6))))
					 (:div :class "grid-item"
					  (:img :src (format nil "/~a-images/geysr-wall-2.png"  (the publish-prefix)):style "width: auto; height: 300px; margin: 1em 0 1em 3% ;" )))
					(:p (str (the (hint-button :function-key :hint-5!))))))))

   (hint-5-section :type 'sheet-section
		   :inner-html (with-cl-who-string()
				 (when (the hint-5)
				   (htm
				    (:div :class "grid-container-2-650px"
					  (:div :class "grid-item"
						(:p "Now you can move down to the "
					    ((:span :class "object")"row")" level and position the "
					    ((:span :class "object")"bricks-and-mortar")" object plus the "
					    ((:span :class "object")"mortar-bed")" object. The "
					    ((:span :class "object")"mortar-bed")" is positioned relative to the "
					    ((:span :class "object-keyword")":bottom")" face of the "
					    ((:span :class "object")"row")" and the "
					    ((:span :class "object")"bricks-and-mortar")" is positioned relative to the "
					    ((:span :class "object-keyword")"top")" face of the "
					    ((:span :class "object")"mortar-bed")))
					  (:div :class "grid-item")
					(:div :class "grid-item"
					  (str (code-example (the code-7))))
					 (:div :class "grid-item"
					  (:img :src (format nil "/~a-images/geysr-wall-3.png" (the publish-prefix)) :style "width: auto; height: 200px; margin: 1em 0 1em 3% ;" )))
				    (:p (str (the (hint-button :function-key :hint-6!))))))))
   
   (hint-6-section :type 'sheet-section
		   :inner-html (with-cl-who-string()
				 (when (the hint-6)
				   (htm
				    (:div :class "grid-container-2-650px"
					  (:div :class "grid-item"
						(:p "Moving on to the actual "
					    ((:span :class "object") "bricks-and-mortar")
                                            ", you first need to determine if the row of bricks is made of full bricks only or starts and ends
with half bricks. You can do this using the Lisp predicates "
					    ((:span :class "function")"zerop")" and "
					    ((:span :class "function")"evenp")" to test the "
					    ((:span :class "object")"row")" object index number. If it is zero or an even number then the "
					    ((:span :class "object")"row")" will be made of full bricks only. One other change made to the "
					    ((:span :class "object")"row")" definition is to use "
					    ((:span :class "object-keyword")":pass-down")" in the object definitions for "
					    ((:span :class "object")"bricks-and-morter")" and "
					    ((:span :class "object")"morter-bed")". When a parent slot has the same name as a child objects "
					    ((:span :class "object-keyword")":input-slot")" you can use "
					    ((:span :class "object-keyword")":pass-down")" and just provide a list of the slot names avoiding having to map the "
					    ((:span :class "object-keyword")":input-slot")" to the parents slot with the "
					    ((:span :class "macro")"the")" macro")
					(str (code-example (the code-8)))
					(:p "you need to add the input-slot full-brick-row? to the bricks-and-morter object and start to thnk about positioning the full-brick, half-brick and mortar-joint objects. If you consider a row of full bricks"))
					  (:div :class "grid-item")
					  (:div :class "grid-item"
					  (str (code-example (the code-9))))
					 (:div :class "grid-item"
					  (:p "This section of code for "
					      ((:span :class "object")"bricks-and-mortar")" covers just the "
					      ((:span :class "object-keyword")":computed-slots")". They could quite easily be defined as part of the child object definitions but defining them as "
					      ((:span :class "object-keyword")":computed-slots")" helps with debugging and makes the code more maintainable. There may be a slight performance hit in doing this but that can be dealt with later if required. Its one of the great things about GendL and Lisp, start of by writing code fast, then if theres and issue transform it to fast code. A detail to consider is the width of the mortar joint. In a full brick row, with N full bricks there are N-1 joints, but if the row starts and ends with half bricks an extra joint will be included. If you maintain the joint width, the end of a row starting and ending with half bricks will not be alligned with the end of a full brick row, so you have to reduce the mortar joint for rows starting and ending with half bricks. In the "
					      ((:span :class "object-keyword")":computed-slot")" "
					      ((:span :class "slot")"corrected-joint-width")" the Lisp special operator "
					      ((:span :class "special-operator")"let")" has been used to enable a local binding to be created. We'll cover local bindings in a later tutorial, but for now its just like a temporary variable only available to the slot its defined in"))
					(:div :class "grid-item"
					  (str (code-example (the code-10))))
					(:div :class "grid-item"
					  (:p "This section of code for "
					      ((:span :class "object")"bricks-and-mortar")" covers just the "
					      ((:span :class "object-keyword")":functions")". These "
					      ((:span :class "object-keyword")":functions")" are used in the child objects to define the "
					      ((:span :class "object-keyword")":center")" of "
					      ((:span :class "object")"full-brick")", "
					      ((:span :class "object")"half-brick")" and "
					      ((:span :class "object")"mortar-joint")". As above, this is implemented more for convenience and code managability than technical requirement"
					      (:ul (:li "The first "
							((:span :class "object")"full-brick")" (index = 0) will be positioned relative to the front face of the parents ("
							((:span :class "object")"bricks-and-mortar")") bounding box.")
						   (:li "The first "
							((:span :class "object")"mortar-joint")" (index = 0) will be positioned relative to the rear face of the first (nth = 0) "
							((:span :class "object")"full-brick")".")
						   (:li "The second "
							((:span :class "object")"full-brick")" (index = 1) will be positioned relative to the rear face of the first "
							((:span :class "object")"mortar-joint")" (index=0).")
						   (:li "The second "
							((:span :class "object")"mortar-joint")" (index = 1) will be positioned relative to the rear face of the second "
							((:span :class "object")"full-brick")" (nth = 1).")
						   (:li "The third "
							((:span :class "object")"full-brick")" (index = 2) will be positioned relative to the rear face of the second "
							((:span :class "object")"mortar-joint")" (index = 1).")
						   (:li "And so on... You can see a pattern emerging that you can use to code the positioning"))
					      "When the row starts with a "
					      ((:span :class "object")"half-brick")" its slightly different"
					      (:ul (:li "The first "
						        ((:span :class "object")"half-brick")" (index = 0) will be positioned relative to the front face of the parents ("
						        ((:span :class "object")"bricks-and-mortar")") bounding box.")
						   (:li "The first "
						        ((:span :class "object")"mortar-joint")" (index = 0) will be positioned relative to the rear face of the first (nth = 0) "
						        ((:span :class "object")"half-brick")".")
						   (:li "The first "
						        ((:span :class "object")"full-brick")" (index = 0) will be positioned relative to the rear face of the first  "
						        ((:span :class "object")"mortar-joint")" (index = 0).")
						   (:li "The second "
						        ((:span :class "object")"mortar-joint")" (index = 1) will be positioned relative to the rear face of the first "
						        ((:span :class "object")"full-brick")" (nth = 0).")
						   (:li "The second "
						        ((:span :class "object")"full-brick")" (index = 1) will be positioned relative to the rear face of the second "
						        ((:span :class "object")"mortar-joint")" (index = 1).")
						   (:li "And so on.. Again you can see a pattern emerging that you can use to code the positioning")))
					  (:p "Some points to note"
					      (:ul (:li "The "
							((:span :class "object-keyword")":functions")" "
							((:span :class "function")"other-brick-center!")" and "
							((:span :class "function")"other-joint-center!")" use the special operator "
							((:span :class "special-operator")"let")" to create a local binding again, just to make the calculation easier to understand")
						   (:li "When calculating the reference point for the second "
							((:span :class "object")"half-brick")" ("
							((:span :class "function")"last-half-brick-center!")"), you send the message "
							((:span :class "slot")"last")" to the "
							((:span :class "object")"mortar-joint")" sequence to return the last "
							((:span :class "object")"mortar-joint")" in the sequence. As this returns an object, to get the "
							((:span :class "function")"(face-center :rear)")" value you need to use the "
							((:span :class "macro")"the-object")" or "
							((:span :class "macro")"theo")" macro instead of "
							((:span :class "macro")"the")
							(:li "As discussed in the Functions and :functions tutorial, the "
							     ((:span :class "object-keyword")":function")" names all end with the ! character. This is out of convention for readability rather than and technical need")))))
					(:div :class "grid-item"
					  (str (code-example (the code-11))))
					(:div :class "grid-item"
					  (:p "Finally, if you look at the "((:span :class "object")":objects")" definition, you can see that the use of the "
					      ((:span :class "function")":functions")" has enabled the readability of this section to be maintained. An interesting point to note is the definition of "
					      ((:span :class "object")"half-brick")", particularly the "
					      ((:span :class "general-keyword")":sequence (:size )")" value. When the row is a full brick row (ie no half bricks), the size is zero which means that this sequence contains no objects. This will need to be borne in mind when you come to collecting information about the number of "
					      ((:span :class "object")"full-bricks")" and "
					      ((:span :class "object")"half-bricks")" in the "
					      ((:span :class "object")"wall")))
					(:div :class "grid-item"
					(:p "You can now inspect the model in Geysr, first by drawing the first 2 "
					    ((:span :class "object")"full-bricks")" and "
					    ((:span :class "object")"mortar-joints")" from the bottom "
					    ((:span :class "object")"row")", then the whole bottom "
					    ((:span :class "object")"row")" of bricks, then the bottom "
					    ((:span :class "object")"row")" with the "
					    ((:span :class "object")"morter-bed")", then add the second "
					    ((:span :class "object")"row")" (showing the "
					    ((:span :class "object")"half-bricks")" to get the brick offset) and then finally the full "
					    ((:span :class "object")"wall"))
					(:img :src (format nil "/~a-images/geysr-wall-4.png" (the publish-prefix)) :style "width: auto; height: 200px; margin: 1em 0 1em 3% ;" ) (:br)
					(:img :src (format nil "/~a-images/geysr-wall-5.png" (the publish-prefix)) :style "width: auto; height: 200px; margin: 1em 0 1em 3% ;" )(:br)
					(:img :src (format nil "/~a-images/geysr-wall-6.png" (the publish-prefix)) :style "width: auto; height: 200px; margin: 1em 0 1em 3% ;" )(:br)
					(:img :src (format nil "/~a-images/geysr-wall-7.png" (the publish-prefix)) :style "width: auto; height: 200px; margin: 1em 0 1em 3% ;" )
					(:p "The final task is to use the geometry to determine how many full bricks and half bricks and what volume of morter are required ")))
					
					(:p (str (the (hint-button :function-key :hint-7!))))))))

   (hint-7-section :type 'sheet-section
		   :inner-html (with-cl-who-string()
				 (when (the hint-7)
				   (htm
				    (:div :class "grid-container-2-650px"
					  (:div :class "grid-item"
						(:p "You now need to gather the outputs. You can do this at the lowest ("
					    ((:span :class "object")"bricks-and-morter")") level and then collate them from the object sequences. Whilst
you could re-do the calculation explicitly, the model is already calculating these values implicitly anyway and you can use this fact to your advantage
by probing the model directly for the number of objects (for "
					    ((:span :class "object")"full-bricks")" and "
					    ((:span :class "object")"half-bricks")") in each "
					    ((:span :class "object")"row")" and getting a total "
					    ((:span :class "slot")"volume")" for the "
					    ((:span :class "object")"mortar-joint")" and "
					    ((:span :class "object")"mortar-bed")". The code extract below just shows the additional "
					    ((:span :class "object-keywords")":computed-slots")" required in each object to achieve this.")
					(:p "Note the use of the "
					    ((:span :class "function")"apply")" function to sum the "
					    ((:span :class "slot")"mortar-joint-volume")" in "
					    ((:span :class "object")"bricks-and-mortar")" and to sum "
					    ((:span :class "slot")"full-bricks")", "
					    ((:span :class "slot")"half-bricks")" and "
					    ((:span :class "slot")"mortar-volume")" in "
					    ((:span :class "object")"wall")". We use "
					    ((:span :class "function")"apply")" in conjunction with the "
					    ((:span :class "function")"+")" function to sum the elements of the list returned by ("
					    ((:span :class "macro") "list-elements")" [object] ("((:span :class "macro")"the-element")
                                            " [slot])). There are more efficient ways of summing and otherwise summarizing values from sequences and trees of GendL
objects, but the "
                                            (:span :class "function" "apply")
                                            " technique shown here has the advantage of being simple and generally applicable."))
					  (:div :class "grid-item")
					  (:div :class "grid-item"
				          (str (code-example (the code-12))))
				        (:div :class "grid-item"
				          (str (repl-example (the repl-1)))))))))

   (resource-links-section :type 'sheet-section
			   :inner-html (with-cl-who-string()
					 (:table
					     (let ((icon "/common-images/lisp-file.png"))
				
					       (htm (:tr (when (the hint-1)
							   (htm (:td (:a :href (format nil "/~a-resources/wall-hint-1.lisp" *publish-prefix*)
									 (:img :src icon :style "width: 40px; height: auto;")))
								(:td (:a :href (format nil "/~a-resources/wall-hint-1.lisp" *publish-prefix*)
									 "wall-hint-1.lisp")))))
						    (:tr (when (the hint-2)
							   (htm (:td (:a :href (format nil "/~a-resources/wall-hint-2.lisp" *publish-prefix*) 
									 (:img :src icon :style "width: 40px; height: auto;")))
								(:td (:a :href (format nil "/~a-resources/wall-hint-2.lisp" *publish-prefix*)
									 "wall-hint-2.lisp")))))
						    (:tr (when (the hint-3)
							   (htm (:td (:a :href (format nil "/~a-resources/wall-hint-3.lisp" *publish-prefix*)
									 (:img :src icon :style "width: 40px; height: auto;")))
								(:td (:a :href (format nil "/~a-resources/wall-hint-3.lisp" *publish-prefix*)
									 "wall-hint-3.lisp")))))
						    (:tr (when (the hint-4)
							   (htm (:td (:a :href (format nil "/~a-resources/wall-hint-4.lisp" *publish-prefix*)
									 (:img :src icon :style "width: 40px; height: auto;")))
								(:td (:a :href (format nil "/~a-resources/wall-hint-4.lisp" *publish-prefix*)
									 "wall-hint-4.lisp")))))
						    (:tr (when (the hint-5)
							   (htm (:td (:a :href (format nil "/~a-resources/wall-hint-5.lisp" *publish-prefix*)
									 (:img :src icon :style "width: 40px; height: auto;")))
								(:td (:a :href (format nil "/~a-resources/wall-hint-5.lisp" *publish-prefix*)
									 "wall-hint-5.lisp")))))
						    (:tr (when (the hint-6)
							   (htm (:td (:a :href (format nil "/~a-resources/wall-hint-6.lisp" *publish-prefix*)
									 (:img :src icon :style "width: 40px; height: auto;")))
								(:td (:a :href (format nil "/~a-resources/wall-hint-6.lisp" *publish-prefix*)
									 "wall-hint-6.lisp")))))
						    (:tr (when (the hint-7)
							   (htm (:td (:a :href (format nil "/~a-resources/wall-hint-7.lisp" *publish-prefix*)
									 (:img :src icon :style "width: 40px; height: auto;")))
								(:td (:a :href (format nil "/~a-resources/wall-hint-7.lisp" *publish-prefix*)
									 "wall-hint-7.lisp"))))))))))

   )
				   
				       
  )
