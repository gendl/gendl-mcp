(in-package :gdl-user)

(define-object hidden-objects (base-object)
  :computed-slots
  ((box-volume (the (my-box 0) volume))
   (hidden-box-volume (the (my-hidden-box 0) volume))
   (object-list (list-elements (the)))
   (all-objects (append (the children)
			(the hidden-children))))
  
  :objects
  ((my-box :type 'box
	   :length 2
	   :width 2
	   :height 2
	   :sequence (:size 3)))
  :hidden-objects
  ((my-hidden-box :type 'box
		  :length 3
		  :width 3
		  :height 3
		  :sequence (:size 2))))




