(in-package :gdl-user)

(define-object wall()
  :objects
  ((row :type 'row)))

(define-object row ()
  :objects
  ((bricks-and-mortar :type 'bricks-and-mortar)
   (mortar-bed :type 'box)))

(define-object bricks-and-mortar ()
  :objects
  ((full-brick :type 'box)
   (half-brick :type 'box)
   (mortar-joint :type 'box)))
