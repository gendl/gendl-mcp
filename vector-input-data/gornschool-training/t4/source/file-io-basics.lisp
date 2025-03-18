(in-package :training-4)

(define-object file-io-basics (base-training-sheet)
  :input-slots
  (getting-started-url)
  
  :computed-slots
  ((repl-1 (list (list :command (list "(defparameter *my-stream* (open \"c:/temp/missing-file.txt\""
				      "                                :direction :output"
				      "                                :if-does-not-exist :create")
		       :output "*MY-STREAM*")))
   (repl-2 (list(list :command "(format *MY-STREAM* \"Writing to a file for the first time\")"
		      :output "NIL")))
   (repl-3 (list(list :command "(close *MY-STREAM*)"
		      :output "T")))
   (code-1 (list "Writing to a file for the first time"))
   (body-content (with-cl-who-string()
		   (:div :class "main-page-container" :style "grid-template-columns: 600px auto;"
			 (:div :class "main-page-item"
			       (:h3 "The basics of file I/O")
			       (:p "To read and write from or to a file, we do this via a "
				   (:em "stream") " which is connected to the file. The most basic mechanism for connecting a stream to a file is by using the CL function "
				   (:span :class "function" "open")", which takes the following arguments"
				   (:ul (:li "filename - required")
					(:li "keyword argument "
					     (:span :class "general-keyword" ":direction")" which can be either "
					     (:span :class "general-keyword" ":input")" or "
					     (:span :class "general-keyword" ":output")" or "
					     (:span :class "general-keyword" ":io")" (both directions)")
					(:li "keyword argument "
					     (:span :class "general-keyword" ":if-exists")" - what to do if the file exists - the most common values being "
					     (:span :class "general-keyword" ":overwrite")", "
					     (:span :class "general-keyword" ":append")" or "
					     (:span :class "general-keyword" ":supersede")". The difference between "
					     (:span :class "general-keyword" ":overwrite")" and "
					     (:span :class "general-keyword" ":supersede")" is the former causes the existing file to be modified at the beginning of the output, whilst the latter delays the deletion of the original file until the stream is closed")
					(:li "keyword argument "
					     (:span :class "general-keyword" ":if-does-not-exist")" - the most common value is "
					     (:span :class "general-keyword" ":create")" where the "
					     (:span :class "general-keyword" ":direction")" is "
					     (:span :class "general-keyword" ":output")" or "
					     (:span :class "general-keyword" ":io")" and "
					     (:span :class "general-keyword" ":if-exists")" is neither "
					     (:span :class "general-keyword" ":overwrite")" nor "
					     (:span :class "general-keyword" ":append")))
				   "There are other keyword arguments and valid values, the above are just the most commonly used.")
			       (str (repl-example (the repl-1)))
			       (:p "Once the file is open we can then write to that stream, for example using the format function we covered in the "
				   (if (the getting-started-url)
				       (htm ((:a :href (the getting-started-url))"Getting Started with GendL"))
				       (htm "Getting Started with GendL "))
				   "tutorial.")
			       (str (repl-example (the repl-2)))
			       (:p "However, whilst the file has been created, we have to close the stream before the the output is seen in the file.")
			       (str (repl-example (the repl-3)))
			       (str (code-example (the code-1)))
			       (:h3 "Avoiding issues by using with-open-file")
			       (:p "And this leads on to one of the potential issues with "
				   (:span :class "function" "open")", the programmer has to remember to "
				   (:em (:b "always close the stream that gets opened"))", and ensure it gets closed even in the event of errors. When streams are left open, results may be unpredictable and performance often suffers.")
			       (:p "For this reason, the macro "
				   (:span :class "macro" "with-open-file")" is much preferred. It takes a symbol representing a stream and a path to the file as required arguments plus all of the keyword arguments used by "
				   (:span :class "function" "open")", plus a body of expressions. "
				   (:span :class "macro" "with-open-file")" opens the stream, evaluates the body of expressions (which will invariably refer to the stream), and then closes the stream. The macro wraps all of the code in the CL special operator "
				   (:span :class "special-operator" "unwind-protect") " to ensure that the stream will always be closed even in the event of errors being encountered.")))))))

				  
				  
