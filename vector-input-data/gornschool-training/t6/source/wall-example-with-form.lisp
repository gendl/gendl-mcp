(in-package :training-6)

(define-object wall-example-with-form (base-training-sheet)
  :input-slots
  ((getting-started-tutorial nil)
   (io-tutorial nil))

  :computed-slots
  ((code-1 (list "(define-object wall(box)"
		 " :input-slots"
		 " ((brick-height 45)"
		 "  (brick-length 180)"
		 "  (brick-width 90)"
		 "  (mortar-joint-width 10)"
		 "  (wall-length 3700)"
		 "  (wall-height 900))"
		 "..."
		 ")"))
   (code-2 (list "(define-object wall-example-form (base-html-page)"
		 " :objects"
		 " ((brick-height-fc :type 'number-form-control"
		 "                      :prompt \"Brick Height (mm)\""
		 "                      :default 45"
		 "                      :ajax-submit-on-change? t)"
		 "  (brick-length-fc :type 'number-form-control"
		 "                      :default 180"
		 "                      :prompt \"Brick Length (mm)\""
		 "                      :ajax-submit-on-change? t)"
		 "  (brick-width-fc :type 'number-form-control"
		 "                     :default 90"
		 "                     :prompt \"Brick Width (mm)\""
		 "                     :ajax-submit-on-change? t)"
		 "  (mortar-width-fc :type 'number-form-control"
		 "                      :default 10"
		 "                      :prompt \"Mortar Joint Width (mm)\""
		 "                      :ajax-submit-on-change? t)"
		 "  (wall-length :type 'number-form-control"
		 "                  :default 3700"
		 "                  :prompt \"Nominal Wall Length (mm)\""
		 "                  :ajax-submit-on-change? t)"
		 "  (wall-height :type 'number-form-control"
		 "                  :default 3700"
		 "                  :prompt \"Nominal Wall Height (mm)\""
		 "                  :ajax-submit-on-change? t)))"))

   (code-3 (list "(define-object wall-example-form (base-html-page)"
		 " :computed-slots"
		 " ((main-sheet-body (with-lhtml-string ()"
		 "                     (:table" 
		 "                       (:tr"
		 "                         (:td (str (the form-controls-section div))))))))"

		 ":objects"
		 " ((form-controls-section "
		 "         :type 'base-html-div"
		 "         :inner-html (with-lhtml-string ()"
		 "                         (:table"
		 "                           (dolist (obj (the form-controls))"
		 "                             (htm (:tr (:td (str (theo obj prompt)))"
		 "                                          (:td (str (theo obj form-control)))))))))"
		 "..."
		 "))"))

   (code-4 (list "(define-object wall-example-form (base-html-page)"
		 "..."
		 "..."
		 " :objects"
		 " ((wall :type 'gdl-user::wall"
		 "          :brick-height (the brick-height-fc value)"
		 "          :brick-length (the brick-length-fc value)"
		 "          :brick-width (the brick-width-fc value)"
		 "          :mortar-joint-width (the mortar-width-fc value)"
		 "          :wall-length (the wall-length value)"
		 "          :wall-height  (the wall-height value))"
		 "  ..."
		 "  ..."
		 ")"
		 ")"))

   (code-5 (list "(define-object wall-example-form (base-html-page)"
		 " :computed-slots"
		 " ((main-sheet-body (with-lhtml-string ()"
		 "                      (:table "
		 "                        (:tr "
		 "                          (:td (str (the form-controls-section div)))"
		 "                          (:td (str (the report-section div)))))))"
		 " )"
		 " :objects"
		 " ("
		 "   ..."
		 "   ..."
		 "  (report-section "
		 "       :type 'base-html-div"
		 "       :inner-html (with-lhtml-string ()"
		 "                      (:table"
		 "                        (:tr "
		 "                          (:td \"Actual Wall Length\")"
		 "                          (:td (str (the wall actual-wall-length))))"
		 "                        (:tr "
		 "                          (:td \"Actual Wall Height\")"
		 "                          (:td (str (the wall actual-wall-height))))"
		 "                        (:tr "
		 "                          (:td \"Number of Full Bricks\")"
		 "                          (:td (str (the wall full-bricks))))"
		 "                        (:tr "
		 "                          (:td \"Number of Half Bricks\")"
		 "                          (:td (str (the wall half-bricks))))"
		 "                        (:tr "
		 "                          (:td \"Mortar Mass\")"
		 "                          (:td (fmt \"~,1f\" (the wall mortar-mass)))))))"
		 "         ..."
		 "         ..."
		 "  ))"))

   (getting-started-title (if (the getting-started-tutorial) (the getting-started-tutorial tutorial-name) "Getting Started with GendL"))
   (getting-started-url (when (the getting-started-tutorial) (the getting-started-tutorial url)))
   (io-title (if (the io-tutorial) (the io-tutorial tutorial-name) "Reading From and Writing To Files"))
   (io-url (when (the io-tutorial) (the io-tutorial url)))			
   (body-content (with-cl-who-string()
		   (:div :class "main-page-container"
			 (:div :class "main-page-item"
			       (:div :class "grid-container-2-650px"
				     (:div :class "grid-item")
				     (:p "In the tutorial " (if (the getting-started-url)
								(htm (:a :href (the getting-started-url) (str (the getting-started-title))))
								(htm (str (the getting-started-title))))
					 " we developed an example application to build a model of a brick built wall. In that application, all inputs were provided as "
					 (:span :class "object-keyword" "input-slots")" with default values. In this topic, we'll use the wall application and extend it to include a web based front end to gather inputs via a form.")
				     (:p "The basic design concept is to have a web page as a top level object, which gathers the inputs and passes them to the wall application, which is defined as a child object of the web form. We will then retrieve the outputs from the wall application and display these on the web form")
				     (:p "So first, we need to familiarise ourselves with the required inputs")
				     (str (code-example (the code-1)))
				     (:p "Now we define the "
					 (:span :class "object" "form-controls")" to capture these inputs")
				     (str (code-example (the code-2)))
				     (:p "Having done that, we need to present them on the web page; using a "
					 (:span :class "object" "base-html-div")" is a convenient way to do this. Note that we use the "
					 (:span :class "slot" "form-controls")" slot to iterate through all the "
					 (:span :class "object" "form-control") " objects to present them on the web page. Also we would normally define "
					 (:span :class "object-keyword" "validation-function")" inputs, but have skipped those in this example")
				     (str (code-example (the code-3)))
				     (:p "Then we need to link the  "
					 (:span :class "object" "form-control")" "
					 (:span :class "slot" "value")" slots to the "
					 (:span :class "object-keyword" ":inputs")" to the wall object")
				     (str (code-example (the code-4)))
				     (:p "Finally, we need to define a "
					 (:span :class "object" "base-html-div")" to present the wall outputs on the web page. Because all of the "
					 (:span :class "object" "form-control")" objects have been defined with "
					 (:span :class "object-keyword" ":ajax-submit-on-change?")" set to T, any change to the form inputs will automatically trigger an update to the outputs display")
				     (str (code-example (the code-5)))
				     (:image :src (format nil "/~a-images/building-example.png" (the publish-prefix)) :style "border: 2px solid; margin: 5px 0 0 3% ;width: auto; height: 200px;" )	 
				     )
			       ((:div :class "main-page-item")
				(:h2 "Resources")
				(str (the resource-links)))))))

  
  ))
