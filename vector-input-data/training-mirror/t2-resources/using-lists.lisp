(in-package :gdl-user)

(define-object assembly-8 (base-object)

  :computed-slots
  ((box-lengths (list 2 5 8))
   (box-centers (list (make-point 0 0 0)
		      (make-point 6 0 0)
		      (make-point 12 0 0)))
   (number-of-boxes (length (the box-lengths)))
   (box-volumes (list-elements (the my-box) (the-element volume)))
   (box-1-volume (nth 0 (the box-volumes))))

  :objects
  ((my-box :type 'box
	   :sequence (:size (the number-of-boxes))
	   :length (nth (the-child index) (the box-lengths))
	   :width 2
	   :height 1
	   :center (nth (the-child index) (the box-centers)))))
