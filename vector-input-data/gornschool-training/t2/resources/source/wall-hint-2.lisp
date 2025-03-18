(in-package :gdl-user)

(define-object wall()
  :input-slots
  ((brick-height 45)
   (brick-length 180)
   (brick-width 90)
   (mortar-joint-width 10)
   (wall-length 3700)
   (wall-height 900))
     
  :objects
  ((row :type 'row
     :brick-height(the brick-height)
     :brick-length (the brick-length)
     :brick-width (the brick-width)
     :mortar-joint (the mortar-joint-width))))

(define-object row ()
  :input-slots
  (brick-height
   brick-length
   brick-width
   mortar-joint-width)

  :objects
  ((bricks-and-mortar :type 'bricks-and-mortar
		      :brick-height (the brick-height)
		      :brick-length (the brick-length)
		      :brick-width (the brick-width)
		      :mortar-joint-width (the mortar-joint-width))
   (mortar-bed :type 'box
	       :height (the mortar-joint-width))
   )
  )

(define-object bricks-and-mortar ()
  :input-slots
  (brick-height
   brick-length
   brick-width
   mortar-joint-width)
  
  :objects
  ((full-brick :type 'box
	       :length (the brick-length)
	       :height (the brick-height)
	       :width (the brick-width))
   (half-brick :type 'box
	       :length (half (the brick-length))
	       :height (the brick-height)
	       :width (the brick-width))
   (mortar-joint :type 'box
		 :height (the brick-height)
		 :width (the brick-width)
		 :length (the mortar-joint-width))))
