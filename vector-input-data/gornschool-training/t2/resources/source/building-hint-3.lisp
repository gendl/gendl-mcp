(in-package :gdl-user)

(define-object building (box)
  :input-slots
  ((nominal-height 3000)
   (nominal-width 3000)
   (nominal-length 4000)
   (brick-height 45)
   (brick-length 180)
   (brick-width 90)
   (mortar-joint-width 10)
   (truss-angle 30)
   (beam-width 40)
   (beam-height 50)
   (wall-thickness 3)
   (material-density 7800)

   (max-beam-spacing 1500))

  :computed-slots
  ((length (the left-wall length))
   (width (the rear-wall length))
   (height (+ (the left-wall height) (the (roof-truss 0) height)))

   (number-of-roof-trusses (let ((trusses (ceiling (the left-wall length) 1500)))
			     (max trusses 2)))

   (truss-spacing (div (- (the left-wall length) (the beam-width))
		       (- (the number-of-roof-trusses) 1)))
   (truss-offsets (let ((res nil))
		    (dotimes (n (the number-of-roof-trusses) (nreverse res))
		      (push (+ (half (the beam-width))
			       (* n (the truss-spacing))) res))))
   )

   
  
  :objects
  ((left-wall :type 'wall
	      :pass-down (brick-height
			  brick-length
			  brick-width
			  mortar-joint-width)
	      :center (translate-along-vector
		       (translate-along-vector (the (edge-center :bottom :left))
					       (the (face-normal-vector :right))
					       (half (the-child width)))
		       (the (face-normal-vector :top))
		       (half (the-child height)))
	      :wall-length (the nominal-length)
	      :wall-height (the nominal-height))

   (right-wall :type 'wall
	       :pass-down (brick-height
			   brick-length
			   brick-width
			   mortar-joint-width)
	       :center (translate-along-vector
			(translate-along-vector (the (edge-center :bottom :right))
						(the (face-normal-vector :left))
						(half (the-child width)))
			(the (face-normal-vector :top))
			(half (the-child height)))
	       :wall-length (the nominal-length)
	       :wall-height (the nominal-height))

   (rear-wall :type 'wall
	      :pass-down (brick-height
			  brick-length
			  brick-width
			  mortar-joint-width)
	      :center (translate-along-vector
			(translate-along-vector (the (edge-center :bottom :rear))
						(the (face-normal-vector :front))
						(half (the-child width)))
			(the (face-normal-vector :top))
			(half (the-child height)))
	      :orientation (alignment :rear (the (face-normal-vector :right)))
	      :wall-length (the nominal-width)
	      :wall-height (the nominal-height))

   (roof-truss :type 'truss
	       :sequence (:size (the number-of-roof-trusses))
	       :truss-length (the rear-wall length)
	       :center (translate-along-vector
			(translate-along-vector (the (edge-center :front :top))
						(the (face-normal-vector :bottom))
						(half (the-child height)))
			(the (face-normal-vector :rear))
			(nth (the-child index) (the truss-offsets)))
	       :orientation (alignment :rear (the (face-normal-vector :right))
				       )		
	       :pass-down (truss-angle
			   beam-width
			   beam-height
			   wall-thickness
			   material-density))
   
   (roof-cladding-left :type 'box)
   (roof-cladding-right :type 'box)
   )
  )
(define-object full-start-wall (wall)
  :input-slots
  ((first-row :start-full)))

(define-object half-start-wall (wall)
  :input-slots
  ((first-row :start-half)))

(define-object half-start-wall-front-key (wall)
  :input-slots
  ((first-row :start-half)
   (front-edge :keyed)))

(define-object half-start-wall-rear-key (wall)
  :input-slots
  ((first-row :start-half)
   (rear-edge :keyed)))

(define-object half-start-wall-both-key (wall)
  :input-slots
  ((first-row :start-half)
   (front-edge :keyed)
   (rear-edge :keyed)))

(define-object wall(box)
  :input-slots
  ((brick-height 45)
   (brick-length 180)
   (brick-width 90)
   (mortar-joint-width 10)
   (wall-length 3700)
   (wall-height 900)
   (first-row :start-full)
   (front-edge :full)
   (rear-edge :full))

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
		 width
		 first-row
		 front-edge
		 rear-edge))))

(define-object row (box)
  :input-slots
  (full-bricks-per-row
   brick-height
   brick-length
   brick-width
   mortar-joint-width
   first-row
   front-edge
   rear-edge )

  :computed-slots
  ((full-brick-row? (cond ((eq (the first-row) :start-full)
			   (or (zerop (the index)) (evenp (the index))))
			  ((eq (the first-row) :start-half)
			   (not (or (zerop (the index)) (evenp (the index)))))))
		    
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
				  full-bricks-per-row
				  front-edge
				  rear-edge))
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
   mortar-joint-width
   front-edge
   rear-edge)

  :computed-slots
  ((first-full-brick-start-point (if (the full-brick-row?)
				     (the (face-center :front))
				     (the (mortar-joint 0) (face-center :rear))))
   
   (first-mortar-joint-start-point (cond ((the full-brick-row?) (the (full-brick 0) (face-center :rear)))
					 ((eq (the front-edge) :full) (the (half-brick 0) (face-center :rear)))
					 ((eq (the front-edge) :keyed) (translate-along-vector (the (face-center :front))
											       (the (face-normal-vector :rear))
											       (half (the brick-length))))))

   (number-of-full-bricks (if (the full-brick-row?)
			      (the full-bricks-per-row)
			      (- (the full-bricks-per-row) 1)))

   (number-of-half-bricks (cond ((the full-brick-row?) 0)
				((and (eq (the front-edge) :full)(eq (the rear-edge) :full)) 2)
				((and (eq (the front-edge) :keyed) (eq (the rear-edge) :full)) 1)
				((and (eq (the front-edge) :full) (eq (the rear-edge) :keyed)) 1)
				((and (eq (the front-edge) :keyed) (eq (the rear-edge) :keyed)) 0)))

   ;; whether or not the ends are :full or :keyed, the number of mortar joints remains the same since the mortar joint
   ;; when it is :keyed is used to connect to the full brick of the other wall
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
	       :sequence (:size (the number-of-half-bricks))
	       :center (cond ((and (= (the-child index) 0)
				   (eq (the front-edge) :full)) (the first-half-brick-center!))
			     ((and (= (the-child index) 0)
				   (eq (the front-edge) :keyed)
				   (eq (the rear-edge) :full)) (the last-half-brick-center!))
			     ((eq (the rear-edge) :full) (the last-half-brick-center!)))
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








(define-object truss (box)
  :input-slots
  ((truss-length 2000)
   (truss-height nil)
   (truss-angle 30)

   (beam-width 30)
   (beam-height 50)
   (wall-thickness 3)

   (material-density 7800)
   (tonne-rate 450))

  :computed-slots
  ((length (the truss-length))
   (height (cond ((the truss-height)(the truss-height))
		 ((the truss-angle) (+ (* (half (the truss-length))
					  (tan (degrees-to-radians (the truss-angle))))
				       (the beam-height)))))
   (width (the beam-width))
  
   (truss-front-slope-vector (the (get-slope-vector! :front)))
   (truss-rear-slope-vector (the (get-slope-vector! :rear)))

   (front-slope-length (the (get-slope-length! :front)))
   (rear-slope-length (the (get-slope-length! :rear)))
   
   (front-slope-center (the (get-slope-center! :front)))
   (rear-slope-center (the (get-slope-center! :rear)))
   

   (beam-properties (mapsend (the children) :beam-properties))
   (total-mass (round-to-nearest
		(apply '+
		       (mapcar #'(lambda(a) (getf a :mass-kg))
			       (the beam-properties)))
		0.001))
   (total-cost (round-to-nearest
		(apply '+
		       (mapcar #'(lambda(a) (getf a :cost-gbp))
			       (the beam-properties)))
		0.01))
   )

  :functions
  ((get-slope-vector! (beam-side)
		      (let ((v-key (the (get-v-key! beam-side)))
			    (l-key (the (get-l-key! beam-side))))
		      (subtract-vectors (the vertical-beam (edge-center :rear v-key))
					(the lower-beam (edge-center l-key :top)))))
   (get-slope-length! (beam-side)
		      (let ((v-key (the (get-v-key! beam-side)))
			    (l-key (the (get-l-key! beam-side))))
			(3d-distance (the vertical-beam (edge-center :rear v-key))
				     (the lower-beam (edge-center l-key :top)))))
   (get-slope-center!
    (beam-side)
    (let ((pt (case beam-side
		(:front (the front-slope-construction-line center))
		(:rear  (the rear-slope-construction-line center))))
	  (norm-vector (case beam-side
			 (:front (the front-slope-beam (face-normal-vector :bottom)))
			 (:rear (the rear-slope-beam (face-normal-vector :bottom))))))
      (translate-along-vector pt
			      norm-vector
			      (half (the beam-height)))))
   (get-v-key! (beam-side)
	       (case beam-side
		 (:front :top)
		 (:rear :bottom)))
   (get-l-key! (beam-side)
	       (case beam-side
		 (:front :front)
		 (:rear :rear)))

   
   )
   
				       
  :objects
  ((lower-beam :type 'beam
	       :pass-down (:beam-height :beam-width :wall-thickness :material-density :tonne-rate)
	       :beam-length (the truss-length)
	       :center (translate-along-vector (the (face-center :bottom))
					       (the (face-normal-vector :top))
					       (half (the beam-height))))
   (vertical-beam :type 'beam
		  :pass-down (:beam-height :beam-width :wall-thickness :material-density :tonne-rate)
		  
		  :beam-length (- (the height) (the beam-height))
		  :orientation (alignment :rear (the (face-normal-vector :top))
					  :right (the (face-normal-vector :right)))  
		  :center (translate-along-vector (the lower-beam (face-center :top))
						  (the lower-beam (face-normal-vector :top))
						  (half (the-child beam-length))))
   (front-slope-beam :type 'beam
		     :pass-down (:beam-height :beam-width :wall-thickness :material-density :tonne-rate)
		     :beam-length (the front-slope-length)
		     :center (the front-slope-center)
		     :orientation (alignment :rear (the truss-front-slope-vector)
					     :right (the (face-normal-vector :right))))

   (rear-slope-beam :type 'beam
		     :pass-down (:beam-height :beam-width :wall-thickness :material-density :tonne-rate)
		     :beam-length (the rear-slope-length)
		     :center (the rear-slope-center)
		    :orientation (alignment :rear (the truss-rear-slope-vector)
					    :left (the (face-normal-vector :right))))
   
   )

  :hidden-objects
  ((front-slope-construction-line :type 'line
				  :start (the lower-beam (edge-center :front :top))
				  :end (the vertical-beam (edge-center :rear :top)))
   (rear-slope-construction-line :type 'line
				 :start (the lower-beam (edge-center :rear :top))
				 :end (the vertical-beam (edge-center :rear :bottom)))
   
   (front-mid-pt :type 'sphere
		 :display-controls (list :color :blue)
		 :radius 5
		 :center (the front-slope-construction-line center))
   (rear-mid-pt :type 'sphere
		:display-controls (list :color :blue)
		:center (the rear-slope-construction-line center)
		:radius 5)
   (pt-1 :type 'sphere
	 :radius 5
	 :display-controls (list :color :green)
	 :center (the lower-beam (edge-center :rear :top)))
   (pt-2 :type 'sphere
	 :radius 5
	 :display-controls (list :color :red)
	 :center (the vertical-beam (edge-center :rear :bottom)) )

   

   (vector-line :type 'vector-line
		:start-point (the rear-slope-construction-line center)
		:vector (the rear-slope-beam (face-normal-vector :bottom))
		:length 150)
   
   )))

(define-object vector-line (box)
  :input-slots
  ((start-point (make-point 0 0 0))
   (vector (make-vector 1 0 1))
   (length 50)
   (width 1))
  :computed-slots
  ((height (div (the length) 5)))
  :objects
  ((v-line :type 'line
	   :start (the start-point)
	   :display-controls (list :color :red)
	   :end (translate-along-vector (the start-point)
					(the vector)
					(the length)))
   (arrow :type 'cone
	  :radius-1 0
	  :radius-2 (div (the length) 50)
	  :length (div (the length) 5)
	  :display-controls (list :color :red)
	  :center (translate-along-vector (the v-line end)
					  (the vector)
					  (half (the-child length)))
	  :orientation (alignment :front (the vector)))))

(define-object beam (box)
  :input-slots
  ((beam-length 1000)
   (beam-width 40)
   (beam-height 50)
   (wall-thickness 2)
   (material-density 7800)
   (tonne-rate 500))

  :computed-slots
  ((length (the beam-length))
   (width (the beam-width))
   (height (the beam-height))

   (beam-volume (- (the outer volume) (the inner volume)))
   (beam-mass (* (div (the beam-volume) 1000000000) (the material-density)))
   (beam-cost (* (the tonne-rate) (div (the beam-mass) 1000)))
   (beam-properties (list :volume-mm3 (the beam-volume)
			  :mass-kg (round-to-nearest (the beam-mass) 0.01)
			  :cost-gbp (round-to-nearest (the beam-cost) 0.01)
			  :length-mm (the beam-length)
			  :width-mm (the beam-width)
			  :height-mm (the beam-height)
			  :thickness-mm (the wall-thickness))))
   
  :objects
  ((outer :type 'box
	  :length (the beam-length)
	  :width (the beam-width)
	  :height (the beam-height))

   (inner :type 'box
	  :length (the beam-length)
	  :width (- (the beam-width) (twice (the wall-thickness)))
	  :height (- (the beam-height) (twice (the wall-thickness)))))
  )













