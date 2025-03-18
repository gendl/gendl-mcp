(in-package :training-4)

(define-object file-io-example-1 (base-training-sheet)
  :input-slots
  (getting-started-url)

  :computed-slots
  ((hint-1 nil :settable)
   (hint-2 nil :settable)
   (hint-3 nil :settable)
   (index-words (list "with-open-file" "standard-output"))
   (body-content (with-cl-who-string()
		   (:div :class "main-page-container" :style "grid-template-columns: 700px auto;"
			 (:div :class "main-page-item"
			       (str (the start-section main-div))
			       (str (the hint-1-section main-div))
			       (str (the hint-2-section main-div)))
			 (:div :class "main-page-item"
			       (:h2 "Resources")
			       (str (the resource-links))))))

   (code-1 (list "(define-object building (box)"
		    "  :input-slots"
		    "  (..."
		    "   ..."
		    "   (output-filename nil))"
		    "  ..."
		    "  ..."
		    "  :functions"
		    "  (..."
		    "   ..."
		    "   (write-bom-file! ()"
		    "                    (with-open-file (s (the output-filename) :direction :output"
		    "                                                          :if-exists :supersede"
		    "                                                          :if-does-not-exist :create)"
		    "                       (format t \"Exporting the BOM to ~a~%\" (the output-filename))"
		    "                       (format s \"~a\" (the bom-formatted))"
		    "                       (format t \"Exporting complete~%\"))))"
		 ")"))

   (code-2 (list "(defun building-bom (&key (nominal-height 3000)" 
		 "                          (nominal-width 3000)"
		 "                          (nominal-length 3000)"
		 "                          (roof-angle 30)"
		 "                          (output-filename nil))"
		 "  (let ((obj (make-object 'building"
		 "                          :nominal-height nominal-height"
		 "                          :nominal-width nominal-width"
		 "                          :nominal-length nominal-length"
		 "                          :truss-angle roof-angle"
		 "                          :output-filename output-filename)))"
		 "    (if output-filename (theo obj write-bom-file!)"
		 "        (theo obj bom-formatted))))"))

   (code-3 (list "Bricks"
		 "======"
		 "  Full Bricks 2559"
		 "  Half Bricks 162"
		 "Mortar"
		 "======"
		 "  Volume 0.550 m^3"
		 "Roof Cladding"
		 "======"
		 "  Qty 2"
		 "  Dimensions (L x W x T) 3130 x 1807 x 10"
		 "Beams"
		 "====="
		 "  Section (H x W x T) 50 x 40 x 3"
		 "  Qty 3 Length 875"
		 "  Qty 6 Length 1728"
		 "  Qty 3 Length 3030"))

   (repl-1 (list (list :command "(building-bom :output-filename \"c:/temp/building-bom.txt\")"
		       :output "Exporting the BOM to c:/temp/building-bom.txt")
		 (list :output "Exporting complete")
		 (list :output "NIL"))))
				 
 :functions
  ((hint-1! () (the (set-slot! :hint-1 (not (the hint-1)))))
   (hint-2! () (the (set-slot! :hint-2 (not (the hint-2))))))
 
  :objects
  ((start-section :type 'sheet-section
		  :inner-html (with-cl-who-string()
				(:h3 "Example Brief")
				(:p "Extend the "(:span :class "object")" building example developed during the "
				    (if (the getting-started-url) (htm ((:a :href (the getting-started-url))"Getting Started with GendL")) "Getting Started with GendL ")
				    " tutorial to optionally export the Bill of Materials to a file")
                                (str (the (hint-button :function-key :hint-1!)))))

   (hint-1-section :type 'sheet-section
		   :inner-html (with-cl-who-string()
				 (when (the hint-1)
				   (htm (:p "The first step is to determine how to write the output, bearing in mind we need to be able to export the BoM to a file as well as output it to the screen if required. The current implementation provides a "
					    (:span :class "slot" "bom-formatted")" slot which it a single formatted string. It would make sense to use this directly. It would also make sense to write a "
					    (:span :class "object-keyword" ":function")" to write the output to a file. The "
					    (:span :class "object-keyword" ":function")" would be pretty simple"
				            (str (code-example (the code-1)))
				            "In the above code, we introduce a new "
				            (:span :class "object-keyword" ":input-slot")"  "
				            (:span :class "slot" "output-filename")" which defaults to nil. We also add a new "
				            (:span :class "object-keyword" ":function")" "
				            (:span :class "function" "write-bom-file!")". This function connects a stream to the file specified in "
				            (:span :class "slot" "output-filename")" and writes "
				            (:span :class "slot" "(the bom-formatted)")" to that stream using the "
				            (:span :class "function" "format")" function. Also included are 2 progress messages which are written to "
				            (:em (:b "*standard-output*")))
				        (:p "The next task is to update the function which instantiates the "
					    (:span :class "object" "building")" object and either write the BoM to the screen or send it to a file")
                                        (str (the (hint-button :function-key :hint-2!)))))))


   (hint-2-section :type 'sheet-section
		   :inner-html (with-cl-who-string()
				 (when (the hint-2)
				   (htm (:p "The existing "(:span :class "function" "building-bom")" function instantiates the "
					    (:span :class "object" "building")" object and then calls the "
					    (:span :class "slot" "bom-formatted")" message to generate the output as a string. We need to retain this functionality and add in an option to output the content to a file. The simplest way to do this is provide a further keyword input, "
					    (:span :class "general-keyword" ":output-filename")" to the "
					    (:span :class "function" "building-bom")" function and defaut it to nil. If "
					    (:span :class "general-keyword" ":output-filename")" is set to a non-nil value then call the "
					    (:span :class "function" "write-bom-file!")" function, otherwise just call the "
					    (:span :class "slot" "bom-formatted")" message as previously"
					    (str (code-example (the code-2)))
					    "If we call the "
					    (:span :class "function" "building-bom")" function with a value for the "
					    (:span :class "general-keyword" ":output-filename")" keyword input, we get the following"
					    (str (repl-example (the repl-1)))
					    "and we get a file written with the following content"
					    (str (code-example (the code-3))))))))))

