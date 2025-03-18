(in-package :training-4)

(define-object reading-from-a-file (base-training-sheet)

  :computed-slots
  ((index-words (list "with-open-file" "read-line" "regular expressions" "split-regexp" "mapcan" "mapcar" "do" "cond" "read-safe-string"))

   (code-1 (list "(defun read-file (file))"
		 "   (let ((result))"
		 "      (with-open-file (str file :direction :input)"
		 "            (do ((line (read-line str nil 'eof)"
		 "		         (read-line str nil 'eof)))"
		 "	          ((eql line 'eof) result)"
		 "	(setq result (append result (list line)))))))"))

   (code-2 (list "(defun import-data (file)"
		 "(let*"
		 "  ((raw-data (read-file file))"
		 "   (res (mapcar #'(lambda(a) (glisp:split-regexp \"\\\\s+\" a)) raw-data))"
		 "   (res-1 (mapcan #'(lambda(a) (list (make-keyword (second a)) (third a))) res))"
		 "   (keywords (remove nil res-1 :key #'(lambda(a) (keywordp a))))"
		 "   (r nil))"
		 " (dolist (k keywords r)"
		 "    (cond ((or (eq k :width) (eq k :length) (eq k :height))"
		 "           (setq r (append r (list k (read-safe-string (getf res-1 k))))))"
		 "          ((eq k :center)"
		 "           (let ((co-ord (glisp:split-regexp \",\" (getf res-1 k))))"
		 "	       (setq r (append"
		 "                       r"
		 "                       (list k (make-point (read-safe-string (first co-ord))"
		 "                                           (read-safe-string (second co-ord))"
		 "                                           (read-safe-string (third co-ord))))))))))))"))
   
   (repl-1 (list (list :command "(read-file \"c:/temp/report.txt\")"
		       :output "(\"Box Width 3\" \"Box Length 6\" \"Box Height 4\" \"Box Center 0.0,0.0,0.0\" \"Box Volume 72\")")))
   (repl-2 (list (list :command "(import-data \"c:/temp/report.txt\")"
		       :output "(:WIDTH 3 :LENGTH 6 :HEIGHT 4 :CENTER #(0.0 0.0 0.0))")))

   (body-content (with-cl-who-string()
		   (:div :class "main-page-container" :style "grid-template-columns: 700px auto;"
			 (:div :class "main-page-item"
			       (:p "Compared with writing to a file, reading from a file is a much more involved process. As with writing a file, a stream is opened and connected to the file. But then the file contents need to be read, line by line, until the end of the file. In general we will not know how many lines are to be read. Once the data has been read, it needs to be converted or conditioned into a format that the application can use. This implies "
				   (:ul (:li "We know something about the format of the data being supplied")
				        (:li "The data is supplied in that known format"))
				   "It is often the achilles heel of many applications which take data from the outside world and effort in making this interface as robust as possible is always time well spent")
			       (:p "A general purpose function to read data from a text file may look something like this")
			       (str (code-example (the code-1)))
			       (:p "We have introduced the use of the CL macro "
				   (:span :class "macro" "do")" as part of the body of expressions wrapped in "
				   (:span :class "macro" "with-open-file"))
			       (:p (:span :class "macro" "do")" accepts a list of variable expressions in the form "
				   (:em "(variable initial-iteration subsequent-iterations")"). The "
				   (:em "initial-iteration")" is an expression which is evaluated on the first iteration and "
				   (:em "subsequent-iterations")" is an expression evaluated on all subsequent iterations. In our case we are using the CL function "
				   (:span :class "function" "read-line")" The first argument is the stream which comes from "
				   (:span :class "macro" "with-open-file")", the second argument determines if an end of file error should be signalled, in this case nil, and the third argument is the value returned when end of file is encountered. The output of read-line is set to the variable line which is appended into the result variable. The second list in "
				   (:span :class "macro" "do")" defines the condition to be met to end iteration (in this case, the value of line is the end of file value) and the value to be returned when this condition is T, in this case "
				   (:span :class "slot" "result"))
			       (:p "Evaluating this function against the file created in the previous topic gives the following")
			       (str (repl-example (the repl-1)))
			       (:p "The next task when reading a data file is generally parsing it to convert it into a format that our application can use. The following function, making use of our read-file function will do that and return an appropriate plist")
			       (str (code-example (the code-2)))
			       (:p "Working through the function"
				   (:ul (:li "We first read the data into a variable "(:em (:b "raw-data"))". This is a list of strings, where each string is a separate line from the file")
				        (:li "The we use the function "
					     (:span :class "function" "glisp:split-regexp")" to split each string into separate words, breaking where there is one or more whitespaces (the regular-expression \"\\\\s+\")")
				        (:li "Because we know the structure of the data, we can comfortably discarded the first word from each line and convert the second word into a keyword. By using "(:span :class "function" "mapcan")" the local variable "(:em (:b "res-1")" becomes a plist of keyword and value, although each value is still a string")
					     (:li "We need to convert the values into the correct data types, but it's different depending on what value we are considering. "
					          (:span :class "general-keyword" "length")", "
					          (:span :class "general-keyword" "width")" and "
					          (:span :class "general-keyword" "height")" will all be numbers, whilst "
					          (:span :class "general-keyword" "center")" will be a point (vector).")
				             (:li "Finally, we use the keywords to determine how to process the data and return a plist. A key point to note here is that we are not using the CL function "(:span :class "function" "read-string")" as it does have some security vulnerabilities; to prevent this we use the GendL function "(:span :class "function" "read-safe-string")" to convert the numbers as strings into real numbers"))
				        "Evaluating the function in the REPL we get")
				   (str (repl-example (the repl-2)))
				   (:p "One final point to note in the example code; there is virtually no error handling. Given the data file is one we generated automatically and are therefore in full control of, this is probably acceptable, but in general the interface functions like shown above need to be extreemly robust")))
			 (:div :class "main-page-item"
			       (:h2 "Resources")
			       (str (the resource-links))))))))

