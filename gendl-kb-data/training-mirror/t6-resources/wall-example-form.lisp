(in-package :gwl-user)

(define-object wall-example-form (base-html-page)

  :computed-slots
  ((body (with-lhtml-string ()
		      (:table (:tr (:td (str (the form-controls-section div)))
			       (:td (str (the report-section div)))))))
   
   )

  :objects
  ((wall :type 'gdl-user::wall
	 :brick-height (the brick-height-fc value)
	 :brick-length (the brick-length-fc value)
	 :brick-width (the brick-width-fc value)
	 :mortar-joint-width (the mortar-width-fc value)
	 :wall-length (the wall-length value)
	 :wall-height  (the wall-height value))

   (form-controls-section :type 'base-html-div
			  :inner-html (with-lhtml-string ()
					(:table
					    (dolist (obj (the form-controls))
					      (htm (:tr (:td (str (theo obj prompt)))
							(:td (str (theo obj form-control)))))))))
   (report-section :type 'base-html-div
		   :inner-html (with-lhtml-string ()
				 (:table
				     (:tr (:td "Actual Wall Length")
				      (:td (str (the wall actual-wall-length))))
				   (:tr (:td "Actual Wall Height")
					(:td (str (the wall actual-wall-height))))
				   (:tr (:td "Number of Full Bricks")
					(:td (str (the wall full-bricks))))
				   (:tr (:td "Number of Half Bricks")
					(:td (str (the wall half-bricks))))
				   (:tr (:td "Mortar Mass")
					(:td (fmt "~,1f" (the wall mortar-mass)))))))

			  
   (brick-height-fc :type 'number-form-control
		    :prompt "Brick Height (mm)"
		    :default 45
		    :ajax-submit-on-change? t)
   (brick-length-fc :type 'number-form-control
		    :default 180
		    :prompt "Brick Length (mm)"
		    :ajax-submit-on-change? t)
   (brick-width-fc :type 'number-form-control
		   :default 90
		   :prompt "Brick Width (mm)"
		   :ajax-submit-on-change? t)
   (mortar-width-fc :type 'number-form-control
		    :default 10
		    :prompt "Mortar Joint Width (mm)"
		    :ajax-submit-on-change? t)
   (wall-length :type 'number-form-control
		:default 3700
		:prompt "Nominal Wall Length (mm)"
		:ajax-submit-on-change? t)
   
   (wall-height :type 'number-form-control
		:default 3700
		:prompt "Nominal Wall Height (mm)"
		:ajax-submit-on-change? t)))
		
(publish-gwl-app "/wall-example" "gwl-user::wall-example-form")
