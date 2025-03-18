(in-package :training-6)

(define-object file-io-2 (base-training-sheet)


  :computed-slots
  ((index-words (list "after-set!" "upload" ":enctype" "multipart/form-data" "with-form-string" "with-lhtml-string"))
   
   (code-1 (list "(define-object file-upload-1 (base-html-page)"
		 "  :computed-slots"
		 "  ((uploaded-path  \" \" :settable)"
		 "   (body"
		 "     (with-lhtml-string ()"
		 "        (str (the development-links))"
		 "        (str (with-form-string (:enctype  \"multipart/form-data \")"
		 "           (:table (:tr (:td (:input :type  \"file \" :name :uploaded-path :value (the uploaded-path))))"
		 "                   (:tr (:td (:input :type  \"submit \" :name  \"upload \" :value  \"Upload \"))"
                 "                   (:td (str (the uploaded-path)))))))))))"))

   (code-2 (list "(define-object file-upload-2 (base-html-page)"
		 "  :computed-slots"
		 "  ((uploaded-path  \" \" :settable)"
		 "   (body"
		 "     (with-lhtml-string ()"
		 "        (str (the development-links))"
		 "        (when (= (length (the uploaded-path)) 0)"
		 "          (htm (str (with-form-string (:enctype  \"multipart/form-data \")"
		 "             (:table (:tr (:td (:input :type  \"file \" :name :uploaded-path :value (the uploaded-path))))"
		 "                     (:tr (:td (:input :type  \"submit \" :name  \"upload \" :value  \"Upload \"))))))))"
		 "        (when (> (length (the uploaded-path)) 0)"
		 "          (htm (str (fmt  \"The file has been uploaded to ~a\" (the uploaded-path)))"
		 "               (str (with-form-string (:enctype \"multipart/form-data\")"
		 "                          (:input :type \"submit\" :name \"reset\" :value \"Reset Form\")))))))"
		 " :functions"
		 "  ((after-set! ()"
		 "            (when (member \"Reset Form\" (the query-plist) :test 'equalp)"
		 "                 (when (probe-file (the uploaded-path)) (delete-file (the uploaded-path)))"
		 "                 (the (restore-slot-default! :uploaded-path ))))))"))

   (code-3 (list "(define-object file-upload-3 (base-html-page)"
		 "  :computed-slots"
		 "  ((uploaded-path  \" \" :settable)"
		 "   (body"
		 "     (with-lhtml-string ()"
		 "        (str (the development-links))"
		 "        (when (= (length (the uploaded-path)) 0)"
		 "          (htm (str (with-form-string (:enctype  \"multipart/form-data \")"
		 "             (:table (:tr (:td (:input :type  \"file \" :name :uploaded-path :value (the uploaded-path))))"
		 "                     (:tr (:td (:input :type  \"submit \" :name  \"upload \" :value  \"Upload \"))))))))"
		 "        (when (> (length (the uploaded-path)) 0)"
		 "          (htm (str (fmt  \"The file has been uploaded to ~a\" (the uploaded-path)))"
		 "               (:p \"The file contents are\")"
		 "                (:table :border 1 (:tr (:td :colspan 2 (str (first (first (the file-content))))))"
		 "                     (dolist (line (cdr (the file-content)))"
		 "                          (htm (:tr (:td (str (first line)))"
		 "                                    (:td (str (second line)))))))"
		 "               (str (with-form-string (:enctype \"multipart/form-data\")"
		 "                          (:input :type \"submit\" :name \"reset\" :value \"Reset Form\")))))))"
		 " :functions"
		 "  ((after-set! ()"
		 "            (when (member \"Reset Form\" (the query-plist) :test 'equalp)"
		 "                 (when (probe-file (the uploaded-path)) (delete-file (the uploaded-path)))"
		 "                 (the (restore-slot-default! :uploaded-path ))))))"))
                                    
   
   (body-content (with-cl-who-string ()
		   (:div :class "main-page-container"
			 (:div :class "main-page-item"
			       (:div :class "grid-container-2-650px"
				     (:div :class "grid-item"
					   (:h3 "Uploading files")
					   (:p "To upload a file we need to submit a form, containing the file, to the server. This is a traditional multipart form submission.")
					   (:p "Gendl provides the "
					       (:span :class "macro" "with-form-string") " macro to create a form. When using this macro for file uploads we must specify the keyword argunemt "(:span :class "general-keyword" "enctype") " with a value of "
					       (:em (:b "multipart/form-data")) " to ensure the file is ransmited to the server correctly")
					   (:p "Within the body of the form we need two "
					       (:em (:b "input")) " tags. "
					       (:ul
						(:li "The first is of type file which gives us a Browse button to enable a file to be selected. Within this tag we also need to assign a :name, the value of which must correspond with the name of a :settable :computed-slot (its symbol name, so preceeded witha : character). The default value of that slot shoud be an empty string")

						(:li "The second is a standard submit button, which submits the form")))
					   (str (code-example (the code-1)))
					   (:p "In the above example we also include an output of the value of "
					       (:span :class "slot" "uploaded-path") " The value of this slot is automatically set to the location of the uploaded file "
					       (:b "on the server") " when the form is submitted")
					   
					   (:p "Screenshot on opening the form ")
					   (:image :src (format nil "/~a-images/upload-1.png" (the publish-prefix))
						   :style "border: 2px solid; margin: 5px 0 0 3% ;width: auto; height: 160px;" )
		   
		   
					   (:p "Once a file has been selected the "
					       (:em (:b "no file selected")) " text changes to the filename of the selected file. ")
					   (:p "Screenshot after selecting the file, but before hitting "
					       (:em (:b "Upload")))
					   (:image :src (format nil "/~a-images/upload-1a.png" (the publish-prefix))
						   :style "border: 2px solid; margin: 5px 0 0 3% ;width: auto; height: 160px;" )
					   (:p "If " (:em (:b "Upload")) " is then clicked the file is uploaded to the server and the text alongside the Browse button will revert to "
					       (:em (:b "no file selected")) ". As part of this upload, as well as transmitting the file to the server, the slot representing the server pathname to the file (in this case "
					       (:span :class "slot" ":uploaded-path") ") is set. One important thing to note: the default value for "
					       (:span :class "slot" "uploaded-path")" is set to an empty string, rather then nil. This is to ensure that when the "
					       (:span :class "slot" "uploaded-path")" value is set as part of the upload it is set to a string value and not a symbol - on Windows in particular with a drive letter followed by a "
					       (:em (:b ":"))" character this will cause problems as Lisp will believe its a package...")
					   (:image :src (format nil "/~a-images/upload-1b.png" (the publish-prefix))
						   :style "border: 2px solid; margin: 5px 0 0 3% ;width: auto; height: 160px;" )
					   (:p "It is often useful to conditionalise the display of the Browse and Submit buttons depending on the value of the uploaded file slot and
possibly present an alternative button which will reset the form to original values. The code below includes a "
					       (:em (:b "Reset")) " button to perform the reset. We use the "
					       (:span :class "function" "after-set!")" function, check what values are in the "
					       (:span :class "slot" "query-plist")" and if the value of this button "
					       (:em (:b "(\"Reset Form\")")) " is present firstly delete the uploaded file if it exists and then run the "
					       (:span :class "function" "restore-slot-default!")" function on "
					       (:span :class "slot" "upoaded-path") )
					   (str (code-example (the code-2)))
					   
					   (:p "Screenshot after hitting "(:em (:b "Upload")))
					   (:image :src (format nil "/~a-images/upload-2.png" (the publish-prefix))
						   :style "border: 2px solid; margin: 5px 0 0 3% ;width: auto; height: 130px;" )
					   (:p "Screenshot after hitting "(:em (:b "Reset Form")))
					   (:image :src (format nil "/~a-images/upload-3.png" (the publish-prefix))
						   :style "border: 2px solid; margin: 5px 0 0 3% ;width: auto; height: 130px;" )
					   (:p "Once the file has been uploaded to the server, it will generally need processing to access the data it contains. In the example below the uploaded file is processed as the page demands "
					       (:span :class "slot" "(the file-content)")" to be evaluated, but for more complex data processing we may use the "
					       (:span :class "function" "after-set!")" function, test for presence of the Upload Button value ("(:em (:b "Upload"))") in the "(:span :class "slot" "query-plist")" and initiate processing based on that. (Note that the function "(:span :class "function" "read-file")" is a custom function included in the resources file)")
					   (str (code-example (the code-3)))
					   (:image :src (format nil "/~a-images/upload-4.png" (the publish-prefix))
						   :style "border: 2px solid; margin: 5px 0 0 3% ;width: auto; height: 170px;" ))))
			 (:div :class "main-page-item"
			       (:h2 "Resources")
			       (str (the resource-links))))))))
