(in-package :training-4)

(define-object writing-to-a-file (base-training-sheet)


  :computed-slots
  ((index-words (list "with-open-file"))
   (repl-1 (list (list :command (list "(with-open-file (s \"c:/temp/my-file.txt\""
				      "                   :direction :output"
				      "                   :if-exists :supersede"
				      "                   :if-does-not-exist :create)"
				      "      (format s \"Line 1 of text~%\")"
				      "      (format s \"Line 1 of text~%\")")
		       :output "NIL")))
   (code-1 (list "Line 1 of text"
		 "Line 1 of text"))

   (code-2 (list "(define-object my-box (box)"
		 " :input-slots"
		 " ((output-filename \"c:/temp/my-box-report\"))"
		 ""
		 " :computed-slots"
		 " ((width 3)"
		 "  (height 4)"
		 "  (length 6))"
		 ""
		 " :functions"
		 " ((write-report!()"
		 "	(with-open-file (s (the output-filename)"
		 "			:direction :output"
		 "			:if-exists :supersede"
		 "			:if-does-not-exist :create)"
		 "	(let ((i 0))"
		 "	  (format t \"Begining output to ~a~%\" (the output-filename))"
		 "	  (format s \"Box Width ~a~%\" (the width))"
		 "	  (incf i)"
		 "	  (format s \"Box Length ~a~%\" (the length))"
		 "	  (incf i)"
		 "	  (format s \"Box Height ~a~%\" (the height))"
		 "	  (incf i)"
		 "	  (format s \"Box Center ~@{~,1f~^,~}~%\""
		 "	                    (get-x (the center))"
		 "	                    (get-y (the center))"
		 "	                    (get-z (the center)))"
		 "	  (incf i)"
		 "	  (format s \"Box Volume ~a~%\" (the volume))"
		 "	  (incf i)"
		 "	  (format t \"Output written (~a line~:p)~%\" i)))))"
		 "  ))"))

   (code-3 (list "(defun output-report (fname)"
		 "        (let ((obj (make-object 'my-box"
		 "			           :output-filename fname)))"
		 "   (theo obj write-report!)))"))
   (repl-2 (list (list :command "(output-report \"c:/temp/report.txt\")"
		 :output "Begining output to c:/temp/report.txt")
	   (list :output "Output written (5 lines)")
		 (list :output "NIL")))
   
   (code-4 (list "Box Width 3"
		 "Box Length 6"
		 "Box Height 4"
		 "Box Center 0.0,0.0,0.0"
		 "Box Volume 72"))		

   (body-content (with-cl-who-string()
		   (:div :class "main-page-container" :style "grid-template-columns: 700px auto;"
			 (:div :class "main-page-item"
			       (:p "In the previous topic, we concluded by recomending that the CL macro " (:span :class "macro" "with-open-file")" was used for any file input or output. So lets have a look at using this macro to write to a file")
			       (:p "To write output to a file C:/temp/my-file.txt using " (:span :class "macro" "with-open-file")" woud look like the following")
			       (str (repl-example (the repl-1)))
			       (:p "which will produce the following content in the file c:/temp/my-file.txt")
			       (str (code-example (the code-1)))
			       (:p "note that the stream "(:b "s")" only exists within the context of the " (:span :class "macro" "with-open-file")" body of expressions")
			       (:p "In many cases, file output is implemented as a :function in a GendL object, although it is perfectly feasible to implement it as a side effect of a :computed-slot. Consider the following simple example")
			       (str (code-example (the code-2)))
			       (:p "Here, as well as sending output to the stream connected to the file, we are also sending output to the REPL using the stream T (which defaults to "(:b "*standard-output*")". This is often a useful technique to assist with development and/or debugging. We could also write a small function which takes a filename as an input and executes the file output by calling the :function Write-report!")
			       (str (code-example (the code-3)))
			       (:p "And then in the REPL we would see this")
			       (str (repl-example (the repl-2)))
			       (:p "Resulting in the file c:/temp/my-file.txt being written with the following content")
			       (str (code-example (the code-4))))
			 (:div :class "main-page-item"
			       (:h2 "Resources")
			       (str (the resource-links))))))))
				  

