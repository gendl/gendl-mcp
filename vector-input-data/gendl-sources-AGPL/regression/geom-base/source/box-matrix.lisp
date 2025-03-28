(in-package :gdl-lift-tests)

(define-object box-container-sample (box-container) 
  :input-slots ((box-length 10 :settable)
                (box-width 10 :settable)
                (box-height 1 :settable)
                (width (* (the box-width) (the columns)))
                (length (* (the box-length) (the rows)))
                (rows 3)
                (columns 4)))
  

(define-object box-container (base-object)
  
  :input-slots (box-length box-width box-height rows columns)
  
  :computed-slots ((width (* (the box-width) (the columns)))
		   (length (* (the box-length) (the rows)))
                   ;; FLAG maybe add more r-t-d below:
                   (regression-test-data (mapsend (the boxes) :center))) 

  
  :objects
  ((boxes :type 'box
	  :length (the box-length)
	  :width (the box-width)
	  :height (the box-height)
          :quantify-box (:width (twice (the width)) :length (twice (the length)))
	  :sequence (:matrix :longitudinal (the rows) 
			     :lateral (the columns)))))


(register-test-definition 'box-container-sample)
