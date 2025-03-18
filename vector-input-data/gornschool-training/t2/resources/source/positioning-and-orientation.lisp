(in-package :gdl-user)

(define-object assembly-2 (base-object)

  :objects
  ((box-1 :type 'box
	  :length 5
	  :width 1
	  :height 1)

   (box-2 :type 'box
	  :length 10
	  :height 5
	  :width 3
	  :center (make-point 2 2 2))

   (box-3 :type 'box
	  :length 5
	  :height 5
	  :width 5
	  :center (translate-along-vector (the box-2 center)
					  (make-vector 1 1 0)
					  5))))
(define-object assembly-3 (base-object)

  :objects
  ((box-1 :type 'box
	  :length 5
	  :width 1
	  :height 1)

   (box-2 :type 'box
	  :length 10
	  :height 5
	  :width 3
	  :center (translate-along-vector (the box-1 (face-center :rear))
					  (the box-1 (face-normal-vector :rear))
					  (half (the-child length))))

   (box-3 :type 'box
	  :length 5
	  :height 5
	  :width 5
	  :center (translate-along-vector (the box-2 (face-center :rear))
					  (the box-2 (face-normal-vector :rear))
					  (half (the-child length))))))

  (define-object assembly-4 (base-object)

  :objects
  ((box-1 :type 'box
	  :length 5
	  :width 1
	  :height 1)

   (box-2 :type 'box
	  :length 5
	  :width 1
	  :height 1
	  :orientation (alignment :rear (the box-1 (face-normal-vector :top))))
   ))

(define-object assembly-5 (base-object)

  :objects
  ((box-1 :type 'box
	  :length 5
	  :width 1
	  :orientation (alignment :rear (the (face-normal-vector :top)))
	  :height 1)

   (box-2 :type 'box
	  :length 10
	  :height 5
	  :width 3
	 :orientation (alignment :rear (the box-1 (face-normal-vector :rear)))
	  :center (translate-along-vector (the box-1 (face-center :rear))
					  (the box-1 (face-normal-vector :rear))
					  (half (the-child length))))

   (box-3 :type 'box
	  :length 5
	  :height 5
	  :width 5
	  :orientation (alignment :rear (the box-2 (face-normal-vector :rear)))
	  :center (translate-along-vector (the box-2 (face-center :rear))
					  (the box-2 (face-normal-vector :rear))
					  (half (the-child length))))))
