(in-package :gdl-user)

(defun degrees-to-radians (degrees)
  (div (* degrees pi) 180))

(define-object truss (box)
  :input-slots
  ((truss-length 2000)
   (truss-height 800)
   (truss-angle nil)

   (beam-width 50)
   (beam-height 50)
   (wall-thickness 3))

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

   (get-slope-center! (beam-side)
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
		 (:rear :rear))))
   
				       
  :objects
  ((lower-beam :type 'beam
	       :beam-height (the beam-height)
	       :beam-width (the beam-width)
	       :beam-length (the truss-length)
	       :center (translate-along-vector (the (face-center :bottom))
					       (the (face-normal-vector :top))
					       (half (the beam-height))))
   (vertical-beam :type 'beam
		  :beam-length (- (the height) (the beam-height))
		  :beam-height (the beam-height)
		  :beam-width (the beam-width)
		  :orientation (alignment :rear (the (face-normal-vector :top))
					  :right (the (face-normal-vector :right)))
		  :center (translate-along-vector (the lower-beam (face-center :top))
						  (the lower-beam (face-normal-vector :top))
						  (half (the-child beam-length))))
   (front-slope-beam :type 'beam
		     :beam-length (the front-slope-length)
		     :beam-height (the beam-height)
		     :beam-width (the beam-width)
		     :center (the front-slope-center)
		     :orientation (alignment :rear (the truss-front-slope-vector)
					     :right (the (face-normal-vector :right))))

   (rear-slope-beam :type 'beam
		     :beam-length (the rear-slope-length)
		     :beam-height (the beam-height)
		     :beam-width (the beam-width)
		     :center (the rear-slope-center)
		    :orientation (alignment :rear (the truss-rear-slope-vector)
					    :left (the (face-normal-vector :right)))))

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
	 :center (the vertical-beam (edge-center :rear :top)) )
   (vector-line :type 'vector-line
		:start-point (the pt-1 center)
		:vector (the truss-rear-slope-vector)
		:length 150)))

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
