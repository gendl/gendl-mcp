(in-package :gdl-user)

(define-object truss ()
  :objects
  ((lower-beam :type 'beam)
   (vertical-beam :type 'beam)
   (front-slope-beam :type 'beam)
   (rear-slope-beam :type 'beam)))

(define-object beam ()
  :objects
  ((outer :type 'box)
   (inner :type 'box)))
