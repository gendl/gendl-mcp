(in-package :gdl-user)

(define-object wall(box)
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
   (length (the actual-wall-length)))
   
     
  :objects
  ((row :type 'row
     :sequence (:size (the number-of-rows))
     :center (translate-along-vector (the (face-center :bottom))
				     (the (face-normal-vector :top))
				     (+ (half (the-child height))
					(* (the-child index) (the-child height))))
     :length (the length)
     :width (the width)
     :height (+ (the brick-height) (the mortar-joint-width))
     :bricks-per-row (the number-of-bricks)
     :brick-height(the brick-height)
     :brick-length (the brick-length)
     :brick-width (the brick-width)
     :mortar-joint-width (the mortar-joint-width))))

(define-object row (box)
  :input-slots
  (brick-height
   brick-length
   brick-width
   mortar-joint-width)

  

  :objects
  ((bricks-and-mortar :type 'bricks-and-mortar
		      :brick-height (the brick-height)
		      :brick-length (the brick-length)
		      :brick-width (the brick-width)
		      :mortar-joint-width (the mortar-joint-width))
   (mortar-bed :type 'box
	       :height (the mortar-joint-width))
   )
  )

(define-object bricks-and-mortar ()
  :input-slots
  (brick-height
   brick-length
   brick-width
   mortar-joint-width)
  
  :objects
  ((full-brick :type 'box
	       :length (the brick-length)
	       :height (the brick-height)
	       :width (the brick-width))
   (half-brick :type 'box
	       :length (half (the brick-length))
	       :height (the brick-height)
	       :width (the brick-width))
   (mortar-joint :type 'box
		 :height (the brick-height)
		 :width (the brick-width)
		 :length (the mortar-joint-width))))
