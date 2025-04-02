(in-package :gdl-user)
(define-object assembly (box)
  :computed-slots
  ((length 1)
  (width 1)
  (height 1)
  (alignment-vector-1 (the (face-normal-vector :left)))
  (alignment-vector-2 (the (face-normal-vector :rear)))
  ;; some other ways to make vectors
  ;; effectively the vector from 0,0,0 to 1,1,1
  (alignment-3 (make-vector 1 1 1))

  ;; points and vectors have the same structure so make-point and make-vector can be used interchangably
  (pt-1 (make-point 1 0 0))
  (pt-2 (make-point 2 5 6))
  ;; subtract vectors is used to get the vector from the second point to the first point
  ;; conceptualy (make-vector 1 1 1) is equivalent to (subtract-vectors (make-vector 1 1 1) (make-vector 0 0 0)))
  (vector-1 (subtract-vectors (the pt-2) (the pt-1) ))

  ;; distance between 2 points
  (distance-between-points (3d-distance (the pt-1) (the pt-2)))

  ;; translating a point
  (new-point (translate-along-vector (the pt-1) (the vector-1) (the distance-between-points)))

  ;; new-point should be exactly the same as pt-2, but we see some rounding has crept in as the value is #(2.0 5.0 6.000000000000001)
  ;; to test if 2 points are the 'same' use coincident-point? the tolerance defaults to *ZERO-EPSILON* (0.001 by default)
  (same-point? (coincident-point? (the pt-2) (the new-point) :tolerance 0.000001))

  ;; getting individual x y and z values from a point
  (new-point-x (get-x (the new-point)))
  (new-point-y (get-y (the new-point)))
  (new-point-z (get-z (the new-point)))

  ;; orthoginal vector
  ;; use cross vectors to get a vector that is orthogonal to the 2 input vectors
  (ortho-vector (let ((v1 (make-vector 1 0 0))
		      (v2 (make-vector 0 1 0)))
		  (cross-vectors v1 v2)))
  ;; returns #(0.0 0.0 1.0)

  (vector-angle-rads (let ((v1 (make-vector 1 0 0))
		      (v2 (make-vector 0 1 0)))
		       (angle-between-vectors v1 v2)))
  ;; returns 1.5707963267948966

  (vector-angle-degrees (let ((v1 (make-vector 1 0 0))
		      (v2 (make-vector 0 1 0)))
			  (angle-between-vectors-d v1 v2)))
  ;; returns 90
  ;; note that by default angle-between-vectors/angle-between-vectors-d returns the smallest angle.
  ;; However if a third vector is given it calculates the angle
  ;; based on the RH rule, around this third vector

  (vector-angle-degrees-1 (let ((v1 (make-vector 1 0 0))
				(v2 (make-vector 0 1 0))
				(ref-v (make-vector 0 0 1)))
			    (angle-between-vectors-d v1 v2 ref-v)))
  ;; returns 90
  (vector-angle-degrees-2 (let ((v1 (make-vector 1 0 0))
				(v2 (make-vector 0 1 0))
				(ref-v (make-vector 0 0 -1)))
			    (angle-between-vectors-d v1 v2 ref-v)))	       
  ;; returns 270
  )
  
  
  :objects
  ((bottom-post :type 'post
		;; in this example we align the :top axis of the post with alignment-vector-1
		;; if we change this to bottom it will rotate the post by 180 degrees
		:orientation (alignment :top
					(the alignment-vector-1)))
   (next-post :type 'post
		:orientation (alignment :top
					(the alignment-vector-2))))
  )

(define-object post (box)

  :computed-slots
  ((height (+ (the post-main height) (the base height)))
   (length (max (the post-main length) (the base length)))
   (width (max (the post-main width ) (the base width))))
  
  :objects
  ((post-main :type 'box
	      :height 1000
	      :width 50
	      :length 50
	      :center (translate-along-vector (the (face-center :top))
					      (the (face-normal-vector :bottom))
					      (half (the-child height))))
   (base :type 'box
	 :height 10
	 :width 200
	 :length 200
	 ;; the centre of the base will always be half the base height below the bottom of the post
	 :center (translate-along-vector (the post-main (face-center :bottom))
					 (the post-main (face-normal-vector :bottom))
					 (half (the-child height)))))
  )
  
