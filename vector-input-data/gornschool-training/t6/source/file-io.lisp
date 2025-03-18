(in-package :training-6)

(define-object file-io (base-training-sheet)

  :computed-slots
  ((code-1 (list "(in-package :gwl-user)"
		 ""
		"(define-object simple-file-output (base-html-page)"
		" (:computed-slots"
		"  ((file-contents (let ((line (list 1 2 3 4 5 6 7)))"
		"                    (format nil \"~{Line ~a of my file~^~%~}\" line))))"
		 "  ))"))
   (code-2 (list "(in-package :gwl-user)"
		 ""
		 "(define-object simple-file-output (base-html-page)"
		 " :computed-slots"
		 "  ((file-contents (let ((line (list 1 2 3 4 5 6 7)))"
		 "                        (format nil \"~{Line ~a of my file~^~%~}\" line)))"
		 ""
		 "   (text-physical-file (let ((file-path (make-pathname :defaults (glisp:temporary-file)"
                 "                                                           :type \"txt\")))"
                 "                               (with-open-file (f file-path :direction :output :if-exists :supersede)"
                 "                                      (write-string (the file-contents) f))"
		 "                               file-path))"
		 "  )"))
   (code-3 (list "(in-package :gwl-user)"
		 ""
		 "(define-object simple-file-output (base-html-page)"
		 " :computed-slots"
		 "  ((file-contents (let ((line (list 1 2 3 4 5 6 7)))"
		 "                        (format nil \"~{Line ~a of my file~^~%~}\" line)))"
		 ""
		 "   (text-physical-file-url (let ((url (format nil \"/file-output-~a.txt\" (get-current-date-time)))"
		 "                                         (file-path (make-pathname :defaults (glisp:temporary-file)"
                 "                                                                          :type \"txt\")))"
                 "                                   (with-open-file (f file-path :direction :output :if-exists :supersede)"
                 "                                          (write-string (the file-contents) f))"
		 "                                   (publish-file :path url"
                 "                                                 :content-type \"text/plain\""
                 "                                                 :file file-path)"
		 "                                    url))"
		 "   (text-file-url (let ((url (format nil \"/stream-output-~a.txt\" (get-current-date-time))))"
                 "                      (publish :path url"
                 "                                  :content-type \"text/plain\""
                 "                                  :function #'(lambda(req ent)"
                 "                                      (with-http-response (req ent)"
                 "                                         (with-http-body (req ent)"
                 "                                            (write-string (the file-contents) *html-stream*)))))"
                 "                       url))"
		 "  ))"))
   

   (code-4 (list "(in-package :gwl-user)"
		 ""
		 "(define-object simple-file-output (base-html-page)"
		 " :computed-slots"
		 " ("
		 "  ..."
		 "  ..."
		 "  (body (with-lhtml-string ()"
		 "              (str (the development-links))"
		 "              (str (the export-section div))))"
		 " )"
		 " :objects"
		 " (((export-section :type 'page-section"
		 "                       :inner-html (with-lhtml-string ()"
                 "                                      (:p \"Click \""
                 "                                      (:a :href (the text-file-url) "
		 "                                          :download \"virtual-text-file.txt\" \"Here\")"
                 "                                       \" to download a virtual text file.\")"
                 "                                      (:p \"Click \""
                 "                                      (:a :href (the text-physical-file-url) "
		 "                                           :download \"physical-text-file.txt\" \"Here\")"
                 "                                       \" to download a physical text file.\")))))"
		 ")"))

   
		 

   

   (body-content (with-lhtml-string ()
		   (:div :class "main-page-container"
			 (:div :class "main-page-item"
			       (:div :class "grid-container-2-650px"
				     (:div :class "grid-item"
					   (:h3 "Generating text output for download")
					   (:p "There is often a requirement to generate output as a file which can then be downloaded. There are 4 basic steps to this process"
					       (:ul (:li "Generate the file content")
						    (:li "Write the content to either a static file or a html stream")
						    (:li "Publish the output")
						    (:li "Provide a link to the output on your web page")))
					   (:h4 (:b (:em "Generate the content")))
					   (:p "In general this will involve writing a formatted string")
					   (str (code-example (the code-1)))
					   (:h4 (:b (:em "Write the static output")))
					   (:p "This is just standard file output, as described in the tutorial "(:em (:b "File I/0")) " and its associated topic "(:em (:b "Writing to a file")))
					   (str (code-example (the code-2)))
					   (:p "The slot "(:span :class "slot" "text-physical-file")" returns the pathname of the physical file. We'll cover writing the content directly to a html stream in the next part as its done at the same time as publishing")
					   (:h4 (:b (:em "Publish the output")))
					   (:p "Publishing the output is done by "(:em "side affecting")". For the static file we define the url, write the static file to disc, publish the static file to the url and then return the url. (Note that the slot has been renamed in this example). For the output direct to the html stream we define a function as part of the publish to write the content on the fly and return the url.")
					   (str (code-example (the code-3)))    
					   (:p "Note that when publishing the stream output we have to provide a "
					       (:span :class "general-keyword" ":function")" argument to "
					       (:span :class "function" "publish")" function. We use a lambda function, and whilst it may appear somewhat complex it is a fairly standard boilerplate solution/syntax which may be used elsewhere")   
					   (:h4 (:b (:em "Provide a link for download")))
					   (:p "A "
					       (:span :class "object" "page-section")" is defined comprising links to each of the download options, and then displayed on the page by including its "
					       (:span :class "slot" "div")" in the "
					       (:span :class "slot" "body") " slot. Note the use of the :download atribute for the links, this ensures that on clicking the link the file is downloaded and gives the download file a name which appears in the browsers download window")
					   (str (code-example (the code-4)))    
					   (:image :src (format nil "/~a-images/file-download.png" (the publish-prefix))
						   :style "border: 2px solid; margin: 5px 0 0 3% ;width: auto; height: 300px;" )))
			       ((:div :class "main-page-item")
				(:h2 "Resources")
				(str (the resource-links))))))))
		   
  )
