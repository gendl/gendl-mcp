(in-package :training-4)

(define-object file-io-example-2 (base-training-sheet)
  :input-slots
  (getting-started-url
   read-from-file-url)
  
  :computed-slots
  ((hint-1 nil :settable)
   (hint-2 nil :settable)
   (hint-3 nil :settable)
   (index-words (list "with-open-file" "read-safe-string"))
   
   (body-content (with-cl-who-string ()
		   (:div :class "main-page-container" :style "grid-template-columns: 700px auto;"
			 (:div :class "main-page-item"
			       (str (the start-section main-div))
			       (str (the hint-1-section main-div))
			       (str (the hint-2-section main-div))
			       (str (the hint-3-section main-div)))
			 (:div :class "main-page-item"
			       (:h2 "Resources")
			       (str (the resource-links))))))
   
   (code-1 (list "nominal-height 3000"
		 "nominal-width 3000"
		 "nominal-length 4000"
		 "brick-height 45"
		 "brick-length 180"
		 "brick-width 90"
		 "mortar-joint-width 10"
		 "truss-angle 30"
		 "beam-width 40"
		 "beam-height 50"
		 "wall-thickness 3"
		 "material-density 7800"
		 "roof-overhang 50"
		 "cladding-thickness 10"
		 "max-beam-spacing 1500"))
   
   (code-2 (list "(defun import-building-data (file)"
		 "  (let* ((raw-data (read-file file))"
		 "         (res (mapcar #'(lambda(a) (glisp:split-regexp \"\\\\s+\" a)) raw-data)))"
		 "    (mapcan #'(lambda(a) (list "
		 "                          (make-keyword (first a))" 
		 "                          (read-safe-string (second a)))) res)))"))

   (repl-1 (list (list :command "(import-building-data \"c:/temp/building-input.txt\")"
                       
		       :output (list "(:NOMINAL-HEIGHT 3000 :NOMINAL-WIDTH 3000 :NOMINAL-LENGTH 4000" 
				     ":BRICK-HEIGHT 45 :BRICK-LENGTH 180 :BRICK-WIDTH 90 "
				     ":MORTAR-JOINT-WIDTH 10 :TRUSS-ANGLE 30 :BEAM-WIDTH 40 "
				     ":BEAM-HEIGHT 50 :WALL-THICKNESS 3 :MATERIAL-DENSITY 7800"
				     ":ROOF-OVERHANG 50 :CLADDING-THICKNESS 10 :MAX-BEAM-SPACING 1500)"))))
   
   (code-3 (list "(defun building-bom (&key (nominal-height nil)"
		 "                          (nominal-width nil)"
		 "                          (nominal-length nil)"
		 "                          (roof-angle nil)"
		 "                          (input-filename nil)"
		 "                          (output-filename nil))"
		 "  (let ((obj (make-object 'building"
		 "                          :function-nominal-height nominal-height"
		 "                          :function-nominal-width nominal-width"
		 "                          :function-nominal-length nominal-length"
		 "                          :function-truss-angle roof-angle"
		 "                          :output-filename output-filename"
		 "                          :input-filename input-filename)))"
		 "   (if output-filename (theo obj write-bom-file!)"
		 "       (theo obj bom-formatted))))"))

   (code-4 (list "(define-object building (box)"
		 "   :input-slots"
		 "   ((function-nominal-height nil)"
		 "    (function-nominal-width nil)"
		 "    (function-nominal-length nil)"
		 "    (function-truss-angle  nil)"
		 "    (input-filename nil)"
		 "    ..."
		 "    ...)"
		 ""
		 "   :computed-slots"
		 "   ((nominal-height (or (the function-nominal-height) 3000))"
		 "    (nominal-width (or (the function-nominal-width) 3000))"
		 "    (nominal-length (or (the function-nominal-length) 2000))"
		 "    (truss-angle (or (the function-truss-angle) 30))"
		 "    ..."
		 "    ...)"
		 "..."
		 ")"))
   (code-5 (list "(define-object building (box)"
		 "  :computed-slots"
		 "  ((file-inputs (when (the input-filename) (import-building-data (the input-filename))))"
		 "   (nominal-height (or (the function-nominal-height)"
		 "                       (getf (the file-inputs) :nominal-height)"
		 "                       3000))"
		 "   (nominal-width (or (the function-nominal-width)"
		 "                      (getf (the file-inputs) :nominal-width)"
		 "                      3000))"
		 "   (nominal-length (or (the function-nominal-length)"
		 "                       (getf (the file-inputs) :nominal-length)"
		 "                       4000))"
		 "   (truss-angle (or (the function-truss-angle)"
		 "                    (getf (the file-inputs) :truss-angle)"
		 "                    30))"
		 "   ..."
		 "   ...)"
		 ")"))

   (code-6 (list "(define-object building (box)"
		 "  :input-slots"
		 "  (..."
		 "   ..."
		 "   (brick-height (or (getf (the file-inputs) :brick-height) 45))"
		 "   (brick-length (or (getf (the file-inputs) :brick-length) 180))"
		 "   (brick-width (or (getf (the file-inputs) :brick-width) 90))"
		 "   (mortar-joint-width (or (getf (the file-inputs) :mortar-joint-width) 10))"
		 "   (beam-width (or (getf (the file-inputs) :beam-width) 40))"
		 "   (beam-height (or (getf (the file-inputs) :beam-height) 50))"
		 "   (wall-thickness (or (getf (the file-inputs) :wall-thickness) 3))"
		 "   (material-density (or (getf (the file-inputs) :material-density) 7800))"
		 "   (roof-overhang (or (getf (the file-inputs) :roof-overhang) 50))"
		 "   (cladding-thickness (or (getf (the file-inputs) :cladding-thickness) 10))"
		 "   (max-beam-spacing (or (getf (the file-inputs) :max-beam-spacing) 1500))"
		 "  )"
		 "  ..."
		 ")"))

   (repl-2 (list (list :command "(building-bom)"
		       :output (list "\"Bricks"
				     "======"
				     "  Full Bricks 3109"
				     "  Half Bricks 162"
				     "Mortar"
				     "======"
				     "  Volume 0.667 m^3"
				     "Roof Cladding"
				     "======"
				     "  Qty 2"
				     "  Dimensions (L x W x T) 4080 x 1807 x 10"
				     "Beams"
				     "====="
				     "  Section (H x W x T) 50 x 40 x 3"
				     "  Qty 3 Length 875"
				     "  Qty 6 Length 1728"
				     "  Qty 3 Length 3030"
				     "\""))))

   (repl-3 (list (list :command "(building-bom :nominal-width 4000)"
		       :output (list "\"Bricks"
				     "======"
				     "  Full Bricks 3384"
				     "  Half Bricks 162"
				     "Mortar"
				     "======"
				     "  Volume 0.725 m^3"
				     "Roof Cladding"
				     "======"
				     "  Qty 2"
				     "  Dimensions (L x W x T) 4080 x 2355 x 10"
				     "Beams"
				     "====="
				     "  Section (H x W x T) 50 x 40 x 3"
				     "  Qty 3 Length 1149"
				     "  Qty 6 Length 2276"
				     "  Qty 3 Length 3980"
				     "\""))))
   (code-7 (list "nominal-width 5000"
		 "truss-angle 40"
		 "wall-thickness 4"))
   
   (repl-4 (list (list :command "(building-bom :input-filename \"c:/temp/building-input.txt\")"
		       :output (list "\"Bricks"
		       "======"
		       "  Full Bricks 3659"
		       "  Half Bricks 162"
		       "Mortar"
		       "======"
		       "  Volume 0.783 m^3"
		       "Roof Cladding"
		       "======"
		       "  Qty 2"
		       "  Dimensions (L x W x T) 4080 x 3281 x 10"
		       "Beams"
		       "====="
		       "  Section (H x W x T) 50 x 40 x 4"
		       "  Qty 3 Length 2068"
		       "  Qty 6 Length 3199"
		       "  Qty 3 Length 4930"
		       "\""))))

   (repl-5 (list (list :command "(building-bom :nominal-width 4000 :input-filename \"c:/temp/building-input.txt\")"
		       :output (list "\"Bricks"
				     "======"
				     "  Full Bricks 3384"
				     "  Half Bricks 162"
				     "Mortar"
				     "======"
				     "  Volume 0.725 m^3"
				     "Roof Cladding"
				     "======"
				     "  Qty 2"
				     "  Dimensions (L x W x T) 4080 x 2661 x 10"
				     "Beams"
				     "====="
				     "  Section (H x W x T) 50 x 40 x 4"
				     "  Qty 3 Length 1670"
				     "  Qty 6 Length 2579"
				     "  Qty 3 Length 3980"
				     "\""))))

   (repl-6 (list (list :command (list "(building-bom :nominal-width 4000 "
				      "              :input-filename \"c:/temp/building-input.txt\" "
				      "              :output-filename \"c:/temp/building-output.txt\")")
		       :output (list "Exporting the BOM to c:/temp/building-output.txt"
				     "Exporting complete"
				     "NIL"))))
   (code-8 (list "Bricks"
		 "======"
		 "  Full Bricks 3384"
		 "  Half Bricks 162"
		 "Mortar"
		 "======"
		 "  Volume 0.725 m^3"
		 "Roof Cladding"
		 "======"
		 "  Qty 2"
		 "  Dimensions (L x W x T) 4080 x 2661 x 10"
		 "Beams"
		 "====="
		 "  Section (H x W x T) 50 x 40 x 4"
		 "  Qty 3 Length 1670"
		 "  Qty 6 Length 2579"
		 "  Qty 3 Length 3980"))

	   
   )

  :functions
  ((hint-1! () (the (set-slot! :hint-1 (not (the hint-1)))))
   (hint-2! () (the (set-slot! :hint-2 (not (the hint-2)))))
   (hint-3! () (the (set-slot! :hint-3 (not (the hint-3))))))
  
  :objects
  ((start-section :type 'sheet-section
		  :inner-html (with-cl-who-string()
				(:h3 "Example Brief")
				(:p "Extend the "(:span :class "object")" building example developed during the "
				    (if (the getting-started-url) (htm ((:a :href (the getting-started-url))"Getting Started with GendL")) "Getting Started with GendL ")
				    " tutorial to take any or all of its inputs from an input file")
                                (str (the (hint-button :function-key :hint-1!)))))

   (hint-1-section :type 'sheet-section
		  :inner-html (with-cl-who-string()
				(when (the hint-1)
				  (htm (:p "The first task is to develop a strategy which will allow the "
					   (:span :class "object" "building")" object to be instantiated by taking none, some, or all of its inputs from an input file. If we continue with the concept of using a function ("
					   (:span :class "function" "building-bom")") to instantiate "
					   (:span :class "object" "building")" we could imagine a hierarchy of inputs"
					   (:ul (:li "If the "(:span :class "function" "building-bom")" function provides any inputs, these should be used as priority")
						(:li "If an input file is given to "
						     (:span :class "function" "building-bom")", any inputs in this file should be used, except where they are already specified in the function inputs. ")
						(:li "For any other inputs use the "
						     (:span :class "object" "building")" object defaults"))
					   "Having developed this strategy, we now need to think about the format of the input file as it will determine how we need to process it to deliver the data in a format that the building object can use. The simplest option would be to use one word and one value per line, seperated by one or more spaces, to identify the data field and its value.In other words, out input file could mirror the "
					   (:span :class "object-keyword" ":input-slots")" definition")
				       (str (code-example (the code-1)))
				       (:p "We could then use our general purpose "
					   (:span :class "function" "read-file")" function developed in the "(:a :href (the read-from-file-url) "Reading from a File") " topic and write a simple function to convert the supplied data into a plist"
					   (str (code-example (the code-2))))
				       (:p "Compiling and evaluating this function based on the data shown above would give")
				       (str (repl-example (the repl-1)))
				       (:p "Now we need to consider how we integrate this with the "
					   (:span :class "function" "building-bom")" function and the "
					   (:span :class "object" "building object"))
                                       (str (the (hint-button :function-key :hint-2!)))))))


   (hint-2-section :type 'sheet-section
		   :inner-html (with-cl-who-string()
				 (when (the hint-2)
				   (htm (:p "A few changes are required to the building-bom function. To implement a hierarchy of inputs we need to differentiate where inputs are coming from. Any inputs provided by the function will be renamed so the :nominal-height becomes :function-nominal-height and the default values will be changed to zero. Additionally, we need to provide an input for the input filename, here defined as :input-filename. So the updated "(:span :class "function" "building-bom")" function would look like this"
				       (str (code-example (the code-3)))
				       "and associated changes to the "(:spac :class "object" "building")" object "(:span :class "object-keyword" ":input-slots")" would look like this"
				       (str (code-example (the code-4)))
				       "Note that the default values for "
				       (:span :class "slot" "nominal-height")", "
				       (:span :class "slot" "nominal-width")", "
				       (:span :class "slot" "nominal-length")" and "
				       (:span :class "slot" "truss-angle")" have been maintained, but will be overwritten by values from the "
				       (:span :class "function" "building-bom")" function if those values are supplied")
				        (:p "Now we need to make changes in the building object to use any of the values provided in the input file")
                                        (str (the (hint-button :function-key :hint-3!)))))))

   (hint-3-section :type 'sheet-section
		   :inner-html (with-cl-who-string()
				 (when (the hint-3)
				   (htm (:p "Firstly we add a new "
					    (:span :class "object-keyword" ":computed-slot")" which delivers the input file data as a plist and update the "
					    (:span :class "object-keyword" ":computed-slots")" for "
					    (:span :class "slot" "nominal-height")", "
					    (:span :class "slot" "nominal-width")", "
					    (:span :class "slot" "nominal-length")" and "
					    (:span :class "slot" "truss-angle")". As per our strategy, we should use any function-supplied values if supplied, otherwise we should use values from the input file if present and if neither of those conditions are met we should use the standard default value. So "
					    (:span :class "object-keyword" ":computed-slots")" updates would look like this")

                                        
					 (str (code-example (the code-5)))

                                        
					(:p "Next we need to address the "
					    (:span :class "object-keyword" ":input-slots")". These are not optionally provided by the "
					    (:span :class "function""building-bom")" function, so we need to use the value from the input file if provided, otherwise the standard default. Updates to the "
					    (:span :class "object-keyword" ":input-slots")" would therefore look like this")

                                        
					 (str (code-example (the code-6)))

                                        
					(:p "If we evaluate "(:span :class "function" "building-bom")" with no arguments, we should get a BoM for the default values")

                                        
					 (str (repl-example (the repl-2)))

                                        
					(:p "If we evaluate "
					    (:span :class "function" "building-bom")" with a value specified for "
					    (:span :class "slot" "nominal-width")", we should see some BoM changes")

                                        
					 (str (repl-example (the repl-3)))
                                        
                                        
					(:p "If we now make an input file and evaluate "
					    (:span :class "function" "building-bom")" with "
					    (:span :class "general-keyword" ":input-filename")" specified, we will again see BoM changes")

                                        
					(str (code-example (the code-7)))

                                        
					(str (repl-example (the repl-4)))

                                        
					(:p "With the same input file, but specifying "
					    (:span :class "slot" "nominal-width")" as an argument to "
					    (:span :class "function" "building-bom")", we can see that the "
					    (:span :class "slot" "nominal-width")" value is taking precedence over the value supplied in the input file")

                                        
					(str (repl-example (the repl-5)))

                                        
					(:p "And finally, if we provide an "
					    (:span :class "general-keyword" ":output-filename")", the bom will be written to that file")

                                        
					(str (repl-example (the repl-6)))

                                        
					(str (code-example (the code-8)))))))
				       
   )


   )
