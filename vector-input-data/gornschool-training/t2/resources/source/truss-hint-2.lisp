(in-package :gdl-user)

(define-object truss ()

  :objects
  ((lower-beam :type 'beam)
   (vertical-beam :type 'beam)
   (front-slope-beam :type 'beam)
   (rear-slope-beam :type 'beam)))

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
