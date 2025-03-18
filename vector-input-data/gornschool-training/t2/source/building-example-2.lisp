(in-package :training-2)

(define-object building-example-2 (base-training-sheet)
  :functions
  ((hint-1! () (the (set-slot! :hint-1 (not (the hint-1)))))
   (hint-2! () (the (set-slot! :hint-2 (not (the hint-2))))))

  :computed-slots
  ((index-words (list "format" "~f" "~{ ~}" "~a" ))
   (hint-1 nil :settable)
   (hint-2 nil :settable)
   (repl-1 (list (list :command "(make-self 'building)"
		       :output "#<BUILDING #x21045DF6AD>")
		  (list :command "(the building-materials)"
			:output (list "(:FULL-BRICKS 3109"
				      " :HALF-BRICKS 162"
				      " :MORTAR-VOLUME-M3 0.6665175"
				      " :BEAMS ((:LENGTH-MM 874.685657822283"
				      "          :QTY 3)"
				      "         (:LENGTH-MM 1727.7658984943535 "
				      "          :QTY 6)"
				      "         (:LENGTH-MM 3030 "
				      "          :QTY 3))"
				      " :ROOF-CLADDING (:LENGTH 4080 "
				      "                 :WIDTH 1806.755259207346 "
				      "                 :QTY 2))"))))

   (repl-2 (list (list :command (list "(format nil \"Bricks~%======~%  Full Bricks ~a~%  Half Bricks ~a~%\" "
				      "        (getf (the building-materials) :full-bricks)" 
				      "        (getf (the building-materials) :half-bricks))")
		       :output (list "\"Bricks\""
				     "======"
				     "  Full Bricks 3190"
				     "  Half Bricks 162"
				     "\""))))
   (repl-3 (list (list :command (list "(format nil \"Mortar~%======~%  Volume ~,2f m^3~%\""
				      "        (getf (the building-materials) :mortar-volume-m3))")
		       :output (list "\"Mortar"
				     "======"
				     "  Volume 0.67 m^3"
				     "\""))
		 (list :command (list "(format nil "
				      "  \"Roof Cladding~%======~%  Qty ~a~%  Dimensions (L x W x T) ~d x ~d x ~d\")"
				      "(getf (getf (the building-materials) :roof-cladding) :qty)"
				      "(round-to-nearest (getf (getf (the building-materials) :roof-cladding) "
				      "                        :length) 1)"
				      "(round-to-nearest (getf (getf (the building-materials) :roof-cladding) "
				      "                        :width) 1)"
				      "(the cladding-thickness)))")
		       
		       :output (list "\"Roof Cladding"
				     "======"
				     "  Qty 2"
				     "  Dimensions (L x W x T) 4080 x 1807 x 10"
				     "\""))))

   (repl-4 (list (list :command (list "(let* "
				      "  ((bom (the building-materials))"
				      "   (cladding (getf bom :roof-cladding))"
				      "   (bricks (format nil \"Bricks~%======~%  Full Bricks ~a~%  Half Bricks ~a~%\" "
				      "		          (getf bom :full-bricks) "
				      "		          (getf bom :half-bricks)))"
				      "   (mortar (format nil \"Mortar~%======~%  Volume ~,2f m^3~%\" "
				      "		          (getf bom :mortar-volume-m3)))"
				      "   (l (round-to-nearest (getf cladding :length) 1))"
				      "   (w (round-to-nearest (getf cladding :width) 1))"
				      "   (roof "
				      "      (format nil "
				      "       \"Roof Cladding~%======~%  Qty ~a~%  Dimensions (L x W x T) ~d x ~d x ~d~%\" "
				      "         (getf cladding :qty)"
				      "         l w (the cladding-thickness))))"
				      "  (format nil \"~@{~a~}\" bricks mortar roof))")
		       :output (list "\"Bricks"
				     "======"
				     "  Full Bricks 3109"
				     "  Half Bricks 162"
				     "Mortar"
				     "======"
				     "  Volume 0.67 m^3"
				     "Roof Cladding"
				     "======"
				     "  Qty 2"
				     "  Dimensions (L x W x T) 4080 x 1807 x 10"
				     "\""))))

   (repl-5 (list (list :command (list "(let* ((beams (getf (the building-materials) :beams))"
				      "       (beams-list (flatten "
				      "                       (mapcar #'(lambda(a)"
				      "                            (list (getf a :qty) "
				      "                                  (round-to-nearest (getf a :length-mm) 1)))"
				      "		                    beams)))"
				      "       (header (format nil \"Beams~%=====~%\"))"
				      "       (dimensions (format nil \"Section (H x W x T) ~a x ~a x ~a~%\" "
				      "                           (the beam-height) "
				      "                           (the beam-width) "
				      "                           (the wall-thickness)))"
				      "       (lengths (format nil \"~{  Qty ~a Length ~a~%~}\" beams-list)))"
				      " (format nil \"~@{~a~}\" header dimensions lengths))")
		       :output (list "\"Beams"
				     "====="
				     "Section (H x W x T) 50 x 40 x 3"
				     "  Qty 3 Length 875"
				     "  Qty 6 Length 1728"
				     "  Qty 3 Length 3030"
				     "\""))))

   (code-1 (list "define-object building(box)"
		 ":computed-slots"
		 "(..."
		 " ..."
		 "(bom-formatted "
		 "   (let* "
		 "     ((bom (the building-materials))"
		 "	(cladding (getf bom :roof-cladding))"
		 "	(bricks (format nil \"Bricks~%======~%  Full Bricks ~a~%  Half Bricks ~a~%\" "
		 "			(getf bom :full-bricks) "
		 "			(getf bom :half-bricks)))"
		 "	(mortar (format nil \"Mortar~%======~%  Volume ~,3f m^3~%\" "
		 "		        (getf bom :mortar-volume-m3)))"
		 "	(l (round-to-nearest (getf cladding :length) 1))"
		 "	(w (round-to-nearest (getf cladding :width) 1))"
		 "	(roof "
		 "        (format "
		 "           nil "
		 "            \"Roof Cladding~%======~%  Qty ~a~%  Dimensions (L x W x T) ~d x ~d x ~d~%\" "
		 "	        (getf cladding :qty) "
		 "              l w (the cladding-thickness)))"
		 "	(beams (getf (the building-materials) :beams))"
		 "	(beams-list (flatten"
		 "		      (mapcar #'(lambda(a)"
		 "				  (list (getf a :qty) "
		 "                                      (round-to-nearest (getf a :length-mm) 1)))"
		 "			  beams)))"
		 "	"
		 "	    (beams-header" 
		 "             (format "
		 "                nil "
		 "                 \"Beams~%=====~%  Section (H x W x T) ~a x ~a x ~a~%\""
		 "		 (the beam-height) (the beam-width) (the wall-thickness)))"
		 "	    (beam-lengths (format nil \"~{  Qty ~a Length ~a~%~}\" beams-list)))"
		 "   (format nil \"~@{~a~}\" bricks mortar roof beams-header beam-lengths))) "
		 "))"))

   (code-2 (list "(defun building-bom (&key (nominal-height 3000)"
		 "                          (nominal-width 3000)"
		 "                          (nominal-length 3000)"
		 "                          (roof-angle 30))"
		 "   (let ((obj (make-object 'building"
		 "			     :nominal-height nominal-height"
		 "			     :nominal-width nominal-width"
		 "			     :nominal-length nominal-length"
		 "			     :truss-angle roof-angle)))"
		 "    (theo obj bom-formatted)))"))

   (repl-6 (list (list :command "(building-bom :nominal-width 4000)"
		       :output (list "\"Bricks"
				     "======"
				     "  Full Bricks 2834"
				     "  Half Bricks 162"
				     "Mortar"
				     "======"
				     "  Volume 0.608 m^3"
				     "Roof Cladding"
				     "======"
				     "  Qty 2"
				     "  Dimensions (L x W x T) 3130 x 2355 x 10"
				     "Beams"
				     "====="
				     "  Section (H x W x T) 50 x 40 x 3"
				     "  Qty 3 Length 1149"
				     "  Qty 6 Length 2276"
				     "  Qty 3 Length 3980"
				     "\""))))
   
   (body-content (with-cl-who-string()
		      ((:div :class "main-page-container")
		       ((:div :class "main-page-item")
			(str (the intro-section main-div))
			(str (the hint-1-section main-div))
			(str (the hint-2-section main-div))
			)
		       ((:div :class "main-page-item")
			(:h2 "Resources")
			(str (the resource-links)))))))

  :objects
  ((intro-section :type 'sheet-section
		  :inner-html (with-cl-who-string()
				(:div :class "grid-container-2-650px"
				      (:div :class "grid-item"
					    (:p "The previous worked example generated a model of a building and a list of materials required.
In this example you will create a formatted bill of materials")
					    (:h3 "Brief")
					    (:p "Based on the building model, deliver a formatted bill of materials. Dimensions where required should be
to the nearest mm and morter volume to the nearest 0.01 m"(:sup "3") " The BOM shuld be delivered as a single formatted string")
                                            (str (the (hint-button :function-key :hint-1!)))))))

		  
   (hint-1-section :type 'sheet-section
		   :inner-html (with-cl-who-string()
				 (when (the hint-1)
				   (htm
				    (:div :class "grid-container-2-650px"
					  (:div :class "grid-item"
						(:p "If you instantiate the "
					((:span :class "object") "building")" object from the previous example and request "
					((:span :class "slot") "(the building-materials)")", you will see the plist identifying the material requirements.")
				   (str (repl-example (the repl-1) ))
				   (:p "Starting with the bricks, we can easily construct a formatted string:")
				   (str (repl-example (the repl-2) ))

				   (:p "And similarly we can construct for the morter and roof cladding.
Note that for the roof cladding we have to round the value before passing to "
				       ((:span :class "object")"format")", this is because if you were to use a "(:em ",0")" prefix parameter with the "
				       ((:span :class "format-directive")"~f")" directive you would still end up with a trailing decimal point.
With the Mortar, on the other hand, "
				       ((:span :class "format-directive")"~,2f")" delivers exactly what you want")

				   (str (repl-example (the repl-3) ))
				   (:p "You can tidy this up and make it a bit more efficient with the use of "
				       ((:span :class "macro") "let*") " with some local bindings")
				   (str (repl-example (the repl-4) ))

				   (:p "Now you need to process the beams")
                                   (str (the (hint-button :function-key :hint-2!)))))))))

   (hint-2-section :type 'sheet-section
		   :inner-html (with-cl-who-string()
				 (when (the hint-2)
				   (htm
				    (:div :class "grid-container-2-650px"
					  (:div :class "grid-item"
						(:p "If you can put the beams data into a list of pairs containing quantity and length for each distinct length, you can then use "
					((:span :class "object") "format")" with its iteration directive "
					((:span :class "format-directive")"~{ ~}") " to write the main output.")
				    (str (repl-example (the repl-5 ) ))
				    (:p "Putting it all together now, you can define a new "
					((:span :class "keyword")":computed-slot")" at the top level of "
					((:span :class "object")"building")" called "
					((:span :class "slot")"bom-formatted")", to deliver the required formatted bill of materials")

				    (str (code-example (the code-1) ))
				    (:p "Additionally you could define a custom constructor function with some keyword inputs which makes a "
					((:span :class "object") "building")" object instance using said inputs, and then calls "
					((:span :class "slot") "bom-formatted")" to yield the formatted BOM output.")
				    
				    (str (code-example (the code-2) ))
				    (str (repl-example (the repl-6 ) ))))))))))

