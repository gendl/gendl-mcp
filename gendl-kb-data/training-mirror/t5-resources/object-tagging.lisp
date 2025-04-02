(in-package :gdl-user)

(define-object object-tagging (base-object)
  :computed-slots
  ((all-geometry (remove nil
			 (mapcar #'(lambda(a) (when (typep a 'all-geometry-mixin) a))
				 (the children))))
   (3d-shapes (remove nil
		      (mapcar #'(lambda(a) (when (typep a '3d-shape-mixin) a))
			      (the children))))
   )
  :objects
  ((my-box :type 'my-box
	   :length 3
	   :width 4
	   :height 5)
   (my-sphere :type 'my-sphere
	      :radius 4)

   (my-line :type 'my-line
	    :start (make-point 0 0 0)
	    :end (make-point 10 0 0))))

(define-object my-box (box
		       3d-shape-mixin
		       all-geometry-mixin))

(define-object my-sphere (sphere
			  3d-shape-mixin
			  all-geometry-mixin))

(define-object my-line (line
			all-geometry-mixin))

(define-object all-geometry-mixin())

(define-object 3d-shape-mixin())

