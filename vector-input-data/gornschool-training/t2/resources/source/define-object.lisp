(in-package :gdl-user)
(define-object my-box-1a (box)
  :input-slots
  ((length 2)
   (width 3)
   (height 4)))

(define-object my-box-1b ()
  :input-slots
  ((length 2)
   (width 3)
   (height 4)))



(define-object my-box-2 (my-box-1b box))

(define-object my-box-3 (box my-box-1b))

(define-object my-box-4 (box)
  :input-slots
  (length
   (width 4)
   (height 4))

  :computed-slots
  ((density 7800)
   (mass (* (div (the volume) 1000000000) (the density)))))


(define-object assembly-1 (base-object)
  :objects
  ((my-box :type 'my-box-4
	   :length 10)
   
   (my-sphere :type 'sphere
	      :radius (the my-box width))))
