(in-package :gdl-user)

(define-object pseudo-inputs (base-object)
  :computed-slots
  ((volumes (mapcar #'(lambda(a) (theo a volume)) (the children))))
  
  :objects
  ((my-box :type 'box
	   :length 2
	   :width 3
	   :height 4
	   :sequence (:size 3))
   (my-cylinder :type 'cylinder
		:radius 2
		:height 6
		:pseudo-inputs (:volume)
		:volume (div (* (the-child height)
				(expt (the-child radius) 2)
				pi) 2))))
