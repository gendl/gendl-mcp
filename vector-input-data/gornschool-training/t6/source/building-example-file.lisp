(in-package :training-6)

(define-object building-example-file (base-training-sheet)
  :input-slots
  ((getting-started-tutorial nil)
   (io-tutorial nil))
  :computed-slots
  ((getting-started-title (if (the getting-started-tutorial) (the getting-started-tutorial tutorial-name) "Getting Started with GendL"))
   (getting-started-url (when (the getting-started-tutorial) (the getting-started-tutorial url)))
   (io-title (if (the io-tutorial) (the io-tutorial tutorial-name) "Reading From and Writing To Files"))
   (io-url (when (the io-tutorial) (the io-tutorial url)))

   (code-1 (list "(in-package :gwl-user)"
		 ""
		 " (define-object building-application-file (base-ajax-sheet)"
		 ""
		 "  :computed-slots"
		 "  ((uploaded-path \"\" :settable)"
		 "   (main-sheet-body (with-cl-who-string ()"
		 "               (str (the development-links))"
		 "               (when (= (length (the uploaded-path)) 0)"
		 "                  (htm (with-html-form (:multipart? t :cl-who? t)"
		 "                            (:table "
		 "                              (:tr "
		 "                                (:td"
		 "                                  (:input :type \"file\" :name :uploaded-path)))"
		 "                                    (:tr (:td (:input :type \"submit\" :name \"upload\" :value \"Upload\")))))))))"
		 ")"
		 ""
		 " :objects"
		 " ((building :type 'building"
		 "            :input-filename (when (> (length (the uploaded-path)) 1) (the uploaded-path)))"))

  (code-2 (list "(define-object building-application-file (base-ajax-sheet)"
		""
		" :computed-slots"
		" ((uploaded-path \"\" :settable)"
		"  (output-path (merge-pathnames \"building-output.txt\" (the uploaded-path)))"
		"  (main-sheet-body (with-cl-who-string ()"
		"                     (str (the development-links))"
		"                     (when (= (length (the uploaded-path)) 0)"
		"                        (htm (with-html-form (:multipart? t :cl-who? t)"
		"                            (:table"
		"                              (:tr"
		"                                (:td "
		"                                  (:input :type \"file\" :name :uploaded-path)))"
		"                              (:tr "
		"                                (:td "
		"                                  (:input :type \"submit\" :name \"upload\" :value \"Upload\")))))))"
		"                     (when (> (length (the uploaded-path)) 1)"
		"                         (htm (:p \"Click \"(:a :href (the output-url) \"here\") "
"                                                  \" to download the output\")))))"
		""
		"   (output-url (let ((url \"/output.txt\"))"
		"                 (the building write-bom-file!)"
		"                 (gwl::unpublish url)"
		"                 (gwl::publish-file :path url"
		"                                    :file (the output-path))"
		"                 url)))"
		""
		" :objects"
		" ((building :type 'building"
		"            :input-filename (when (> (length (the uploaded-path)) 1) (the uploaded-path))"
		"            :output-filename (when (> (length (the uploaded-path)) 1) (the output-path)) )))"))
	     
   (body-content (with-cl-who-string()
		   (:div :class "main-page-container"
			 (:div :class "main-page-item"
			       (:div :class "grid-container-2-650px"
				     (:div :class "grid-item")
				     (:p "In the tutorial " (if (the getting-started-url)
								(htm (:a :href (the getting-started-url) (str (the getting-started-title))))
		 						(htm (str (the getting-started-title))))
					 " we developed an example application to build a model of a building. In that application, all inputs were provided as "
					 (:span :class "keyword" "input-slots")" with default values. We then extended that in the tutorial "
					 (if (the io-url)
					     (htm (:a :href (the io-url) (str (the io-title))))
					     (htm (str (the io-title))))
					 " to take inputs from a file and deliver the output as a file. In this topic, we'll use the building application and extend it to include a web based front end to upload a file via a form.")
				     (:p "The basic design concept is to have a web page as a top level object, which provides a form to upload an input file. The building application based on file inputs will be a child object of the file upload form, and once the file has been uploaded it will be passed to te building application as an inout. An output file will be written and a link to that file displayed on the file upload page")
				     (:p "The first step is to create a file upload form and incorporate the building application as a child object")
				     (str (code-example (the code-1)))
				     (:p "Not that for expediency, the building object has been moved to the gwl-user package for this example")
				     (:p "To generate the output, we need to run the write-bom-file! function in the bulding object, following which we need to publish that file and provide a link to it on the web page. As we will demand the url of the published file, we can use side effecting to run the file generation and publishing, rather than running them as a function. To do this we use a computed-slot output-url and the body form of the let macro to evaulate the file write and publish functions sequentially. We've also defined the output filename, such that it will be in the same directory as the uploaded file, but with a different name"))))))))
