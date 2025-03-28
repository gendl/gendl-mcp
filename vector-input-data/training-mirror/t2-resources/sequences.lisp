(in-package :gdl-user)

(define-object assembly-6 (base-object)

  :input-slots
  ((number-of-boxes 3))

  :computed-slots
  ((first-box-volume-1 (the (my-box 0) volume))
   (first-box-volume-2 (the-object (the my-box last) volume))
   (second-box-volume (the (my-box 1) volume))
   (last-box-volume-1 (the-object (the my-box last) volume)))

  :objects
  ((my-box :type 'box
	   :sequence (:size (the number-of-boxes))
	   :length (+ 2 (* (the-child index) 3))
	   :width 2
	   :height 1
	   :center (make-point (* (the-child index) 6) 0 0))))

(define-object assembly-7 (base-object)
		 
  :input-slots
  ((number-of-boxes 3))
		 
  :objects
  ((my-box :type 'box
	   :sequence (:radial (the number-of-boxes))
	   :length (+ 2 (* (the-child index) 3))
	   :width 2
	   :height 1)))

