(in-package :gdl-user)

(define-object assembly-8 (base-object)
  :input-slots
  ((box-lengths (list 2 5 8 12)))
  :computed-slots
  ((number-of-boxes (if (> (length (the box-lengths)) 3)
			3
			(length (the box-lengths))))

   (box-centers (case (the number-of-boxes)
		  (1 (list (make-point 0 0 0)))
		  (2 (list (make-point 0 0 0)
			   (make-point 6 0 0)))
		  (3 (list (make-point 0 0 0)
			    (make-point 6 0 0)
			    (make-point 12 0 0)))))
   
   
   (box-volumes (list-elements (the my-box) (the-element volume)))
   (box-1-volume (nth 0 (the box-volumes))))

  :objects
  ((my-box :type 'box
	   :sequence (:size (the number-of-boxes))
	   :length (nth (the-child index) (the box-lengths))
	   :width 2
	   :height 1
	   :center (nth (the-child index) (the box-centers)))))
