(in-package :training-6)

(defparameter *publish-prefix* "t6")
(defparameter *home* (merge-pathnames "../" (make-pathname :name nil :type nil :defaults (glisp:source-pathname))))

(with-all-servers
    (server)
    (gwl::publish-directory :prefix (format nil "/~a-css" *publish-prefix*)
                            :server server
                            :destination (namestring
                                          (merge-pathnames "resources/css/" *home*))))

(define-object assembly (base-tutorial-sheet)
  :input-slots
  ((previous-page nil)
   (next-page nil)
   (getting-started-tutorial nil)
   (io-tutorial nil)
   
   (define-object-url nil)
   (wall-example-url nil)
   (truss-example-url nil)
   (tutorial-name "Integrating with GendL's web server"))

  :computed-slots
  ((introduction (with-lhtml-string ()
		   (:p "One of the great features of GendL is its inbuilt webserver, the ability to generate web pages and the ability to link these web pages to the underlying GendL model. This tutorial cover the basics of delivering web pages and linking them with applications.")

                   (:p (:i "Note: The examples in this Tutorial depend on some patches to the GWL component of GendL which may not yet be built into the GendL distribution 
you are currently running. These patches can be loaded from the source file "
                           (:span :class "general-keyword" "gwl-patches.lisp")
                           " in the Resources section of the first slide linked below.")))))

  :objects
  ((simple-html-page :type 'simple-html-page
		       :pass-down (page-objects)
		       :publish-prefix *publish-prefix*
		       :page 1
		       :page-title "Creating and Publishing web pages"
		       :resources (list "gwl-patches.lisp"
					"basic-define-and-publish.lisp"
                                        (list :url (format nil "/~a-images/logo.png" *publish-prefix*) :title "Genworks Logo")
					(list :url (format nil "/~a-css/my-style.css" *publish-prefix*) :title"my-style.css")))
   
   (using-with-lhtml-string :type 'using-with-lhtml-string
			     :pass-down (page-objects)
			     :publish-prefix *publish-prefix*
			     :page 2
			     :page-title "Using with-lhtml-string"
			     :resources nil)
   
   (using-base-html-divs :type 'using-base-html-divs
		   :pass-down (page-objects)
		   :publish-prefix *publish-prefix*
		   :page 3
		   :page-title "Building pages from base-html-divs"
		   :resources (list "gwl-patches.lisp" "using-page-sections.lisp" )
		   :simple-html-page-title (the simple-html-page page-title)
		   :simple-html-page-url (the simple-html-page url)
		   :using-ajax-title (the using-ajax page-title)
		   :using-ajax-url (the using-ajax url))
   
   (using-form-controls :type 'using-form-controls
			:pass-down (page-objects)
			:publish-prefix *publish-prefix*
			:page 4
			:page-title "Gathering inputs using form-controls"
			:resources (list "gwl-patches.lisp" "basic-form-controls.lisp" "using-form-controls.lisp" ))
   
   (using-ajax :type 'using-ajax
	       :pass-down (page-objects)
	       :publish-prefix *publish-prefix*
	       :simple-html-page (the simple-html-page)
	       :using-form-controls (the using-form-controls)
	       :using-base-html-divs (the using-base-html-divs)
	       :page 5
	       :page-title "Using AJAX"
	       :resources nil)
   
   (file-io-1 :type 'file-io
	    :pass-down (page-objects)
	    :publish-prefix *publish-prefix*
	    :page 6
	    :page-title "Writing output for download"
	      :resources (list "gwl-patches.lisp" "simple-file-output.lisp"))
   
   (file-io-2 :type 'file-io-2
	    :pass-down (page-objects)
	    :publish-prefix *publish-prefix*
	    :page 7
	    :page-title "Uploading files"
	      :resources (list "gwl-patches.lisp" "file-upload-1.lisp" "file-upload-2.lisp" "file-upload-3.lisp" "birthdays.txt"))

   (displaying-graphics :type 'displaying-graphics
			:pass-down (page-objects)
			:publish-prefix *publish-prefix*
			:page 8
			:page-title "Displaying GendL graphics"
			:resources nil)
   
   (wall-example-form :type 'wall-example-with-form
		     :pass-down (page-objects)
		     :publish-prefix *publish-prefix*
		     :page 9
		     :getting-started-tutorial (the getting-started-tutorial)
		     :io-tutorial (the io-tutorial)
		     :page-title "Wall example - taking inputs from a form"
		     :resources (list "gwl-patches.lisp""wall.lisp" "wall-example-form.lisp"))
   
   (building-example-file :type 'building-example-file
		      :pass-down (page-objects)
		      :publish-prefix *publish-prefix*
		      :page 10
		      :getting-started-tutorial (the getting-started-tutorial)
		      :io-tutorial (the io-tutorial)
		      :page-title "Wall example - taking inputs from a file"
		      :resources (list "gwl-patches.lisp""building-application-file.lisp" "building-bom-input-output.lisp" "building-input.txt")))
   )
   
	       
   
  

;;(publish-gwl-app "/t6" "training-6::assembly")

;;(defparameter *t6-home*
;;  (merge-pathnames "../" (make-pathname :name nil :type nil :defaults (glisp:source-pathname))))

;;(gwl::publish-directory :prefix (format nil "/~a-images" *publish-prefix*)
;;			:destination (namestring (merge-pathnames "images/"  *t6-home*)))

;;(gwl::publish-directory :prefix (format nil "/~a-resources" *publish-prefix*)
;;			:destination (namestring (merge-pathnames "resources/source/" *t6-home*)))
;;(gwl::publish-directory :prefix (format nil "/~a-resource-images" *publish-prefix*)
;;			:destination (namestring (merge-pathnames "resources/images/" *t6-home*)))
;;
;;(gwl::publish-file :path "/style.css"
;;		   :file (namestring (merge-pathnames "css/style.css" *t6-home*)))










