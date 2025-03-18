
(in-package :gwl-user)

(define-object single-viewport-sheet (geysr:assembly)

  :input-slots
  ((background-color :cyan))
  
  :computed-slots
  ((body-onpageshow nil )
   (body-onresize nil)
   (body-onpageload nil)



   (x3dom? (eql (the viewport image-format-selector value) :x3dom))
   (viewport-dimensions (list :length 500 :width 500) :settable)

   (main-sheet-body
    (with-cl-who-string ()
      (when gwl:*developing?* (str (the development-links)))
      (:p "Sample Graphics Viewport")
      (:p
       (str (the menu main-div))
       (str (the menu-script main-div))
       (str (the viewport main-div))
       ;;(str (the viewport viewport-script))
       ))))

  
  :objects
  ((wall :type 'gdl-user::wall)

   (viewport :type 'geysr:viewport
             :view-direction-default :trimetric
	     :image-format-default :svg
	     :image-format-plist (list :x3dom "Shaded"
				       :svg "Wireframe"
                                       ;;:a-frame "VR"
                                       )
	     :empty-display-list-message (with-cl-who-string ()
					   (:p (:h2 (fmt "Viewport Display-List is currently empty."))))
	     :div-style "background: green; height: 100%; width: 100%"	     
	     :geysr self
	     :div-class "viewport-wrapper"
	     :root-object self
	     :length (if (the x3dom?) "100%" (getf (the viewport-dimensions) :length))
	     :width (if (the x3dom?) "100%" (getf (the viewport-dimensions) :width))
	     :onclick-function nil
             :display-list-object-roots (list (the wall))
	     :background-color (the background-color)  ;;was  "#d3d3d3"
             )))


(publish-gwl-app "/wall" 'single-viewport-sheet)


(in-package :gdl-user)

(define-object wall (box)
  :input-slots
  ((brick-height 45)
   (brick-length 180)
   (brick-width 90)
   (mortar-joint-width 10)
   (wall-length 3700)
   (wall-height 900))

  :computed-slots
  ((row-height (+ (the brick-height) (the mortar-joint-width)))
   (number-of-rows (round-to-nearest (div (the wall-height) (the row-height)) 1))
   (actual-wall-height (* (the row-height) (the number-of-rows)))
   
   ;; for the wall-length we need the number of full bricks
   ;; if there are n full bricks then there will be (n-1) mortar joints
   ;; so n*brick-length + n-1*mortar-joint-width = overall-length
   ;; or n(brick-length + mortar-join-width) - mortar-joint-width = overall-length
   ;; or n = (overall-length - mortar-joint-width)/(brick-length + mortar-joint-width)
   (number-of-bricks (round-to-nearest (div (- (the wall-length) (the mortar-joint-width))
					    (+ (the brick-length) (the mortar-joint-width)))
				       1))
   (actual-wall-length (+ (* (the number-of-bricks) (the brick-length))
			  (* (- (the number-of-bricks) 1) (the mortar-joint-width))))

   ;; box inputs - gives the wall bounding box
   (height (the actual-wall-height))
   (width (the brick-width))
   (length (the actual-wall-length))

   (full-bricks (apply '+ (list-elements (the row) (the-element full-bricks))))
   (half-bricks (apply '+ (list-elements (the row) (the-element half-bricks))))
   (mortar-volume (apply '+ (list-elements (the row) (the-element mortar-volume))))
   (mortar-density 2162)
   (mortar-mass (* (the mortar-density) (div (the mortar-volume) 1000000000)))
   )
   
     
  :objects
  ((row :type 'row
     :sequence (:size (the number-of-rows))
     :center (translate-along-vector (the (face-center :bottom))
				     (the (face-normal-vector :top))
				     (+ (half (the-child height))
					(* (the-child index) (the-child height))))
     :height (+ (the brick-height) (the mortar-joint-width))
     :full-bricks-per-row (the number-of-bricks)
     :pass-down (brick-height
		 brick-length
		 brick-width
		 mortar-joint-width
		 length
		 width))))

(define-object row (box)
  :input-slots
  (full-bricks-per-row
   brick-height
   brick-length
   brick-width
   mortar-joint-width)

  :computed-slots
  ((full-brick-row? (or (zerop (the index)) (evenp (the index))))
   (full-bricks (the bricks-and-mortar full-bricks))
   (half-bricks (the bricks-and-mortar half-bricks))
   (mortar-volume (+ (the bricks-and-mortar mortar-joint-volume)
		     (the mortar-bed volume))))

  :objects
  ((bricks-and-mortar :type 'bricks-and-mortar
		      :height (the brick-height)
		      :center (translate-along-vector (the mortar-bed (face-center :top))
						      (the mortar-bed (face-normal-vector :top))
						      (half (the-child height)))
		      :pass-down (width
				  length
				  full-brick-row?
				  brick-height
				  brick-length
				  brick-width
				  mortar-joint-width
				  full-bricks-per-row))
   (mortar-bed :type 'box
	       :height (the mortar-joint-width)
	       :center (translate-along-vector (the (face-center :bottom))
					       (the (face-normal-vector :top))
					       (half (the-child height)))
	       :pass-down (width
			   length))))

(define-object bricks-and-mortar (box)
  :input-slots
  (full-brick-row?
   full-bricks-per-row
   brick-height
   brick-length
   brick-width
   mortar-joint-width)

  :computed-slots
  ((first-full-brick-start-point (if (the full-brick-row?)
				     (the (face-center :front))
				     (the (mortar-joint 0) (face-center :rear))))
   
   (first-mortar-joint-start-point (if (the full-brick-row?)
				       (the (full-brick 0) (face-center :rear))
				       (the (half-brick 0) (face-center :rear))))

   (number-of-full-bricks (if (the full-brick-row?)
			      (the full-bricks-per-row)
			      (- (the full-bricks-per-row) 1)))

   (number-of-mortar-joints (if (the full-brick-row?)
				(- (the number-of-full-bricks) 1)
				(+ (the number-of-full-bricks) 1)))

   ;; if it isn't a full brick row then there will be an extra joint because one
   ;; full brick is replaced with 2 half bricks so without correcting the
   ;; mortar-joint-width the ends of a full brick row and one starting and
   ;; finishing with half bricks won't align. So we need to correct
   ;; the mortar-joint-width
   (corrected-joint-width (if (the full-brick-row?)
				     (the mortar-joint-width)
				     (let ((total-gap (* (- (the number-of-mortar-joints) 1)
									(the mortar-joint-width))))
				       (div total-gap (the number-of-mortar-joints)))))
   ;; collating the output. We could do this analytically, but for this example we'll use the geometry
   (full-bricks (length (list-elements (the full-brick))))
   (half-bricks (length (list-elements (the half-brick))))
   (mortar-joint-volume (apply '+ (list-elements (the mortar-joint)
						 (the-element volume))))
   )
  
  :functions
  ((first-full-brick-center!
    ()
    (translate-along-vector (the first-full-brick-start-point)
			    (the (face-normal-vector :rear))
			    (half (the brick-length))))
   
   (other-full-brick-center!
    (index)
    ;; if its a full brick row, (full-brick 1) is positioned relative to (joint 0)
    ;; if its a half brick row, (full-brick 1) is positioned relative to (joint 1)
    (let ((ind (if (the full-brick-row?) (- index 1) index)))
	  (translate-along-vector (the (mortar-joint ind) (face-center :rear))
				  (the (face-normal-vector :rear))
				  (half (the brick-length)))))
   
    (first-joint-center!
     ()
     (translate-along-vector (the first-mortar-joint-start-point)
			     (the (face-normal-vector :rear))
			     (half (the corrected-joint-width))))
   (other-joint-center!
    (index)
    ;; if its a full brick row, (joint 1) is positioned relative to (full-brick 1)
    ;; if its a half brick row, (joint 1) is positioned relative to (full-brick 0)
    (let ((ind (if (the full-brick-row?) index (- index 1))))
      (translate-along-vector (the (full-brick ind) (face-center :rear))
			      (the (face-normal-vector :rear))
			      (half (the corrected-joint-width)))))

    (first-half-brick-center!
     ()
     (translate-along-vector (the (face-center :front))
			     (the (face-normal-vector :rear))
			     (half (half (the brick-length)))))
   
   (last-half-brick-center!
     ()
     (translate-along-vector (theo (the mortar-joint last) (face-center :rear))
			     (the (face-normal-vector :rear))
			     (half (half (the brick-length))))))
 			     
  
  :objects
  ((full-brick :type 'box
	       :sequence (:size (the number-of-full-bricks))
	       :center (if (= (the-child index) 0)
			   (the first-full-brick-center!)
			   (the (other-full-brick-center! (the-child index))))
	       :length (the brick-length)
	       :height (the brick-height)
	       :width (the brick-width))

   (half-brick :type 'box
	       :sequence (:size (if (the full-brick-row?) 0 2))
	       :center (if (= (the-child index) 0)
			   (the first-half-brick-center!)
			   (the last-half-brick-center!))
	       :length (half (the brick-length))
	       :height (the brick-height)
	       :width (the brick-width))

   (mortar-joint :type 'box
		 :sequence (:size (the number-of-mortar-joints))
		 :center (if (= (the-child index) 0)
			     (the first-joint-center!)
			  (the (other-joint-center! (the-child index))))   
		 :height (the brick-height)
		 :width (the brick-width)
		 :length (the corrected-joint-width))))






