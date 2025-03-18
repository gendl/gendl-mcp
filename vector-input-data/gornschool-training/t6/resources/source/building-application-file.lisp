(in-package :gwl-user)

(define-object building-application-file (base-html-page)

  :computed-slots
  ((uploaded-path "" :settable)
   
   (body (with-lhtml-string ()
	   (str (the development-links))
	   (when (= (length (the uploaded-path)) 0)
	     (str
	      (with-form-string (:enctype "multipart/form-data")
		(:table (:tr (:td (:input :type "file" :name :uploaded-path :value (the uploaded-path))))
		  (:tr (:td (:input :type "submit" :name "upload" :value "Upload")))))))
	   (when (> (length (the uploaded-path)) 0)
		    (htm (:p "Click " (:a :href (the output-url) :download "bom.txt" "Here") " to download the BoM")
		    (str (with-form-string (:enctype "multipart/form-data")
			   (:input :type "submit" :name "reset" :value "Reset Form")))))))
	   

   (output-path (merge-pathnames "building-output.txt" (the uploaded-path)))

   (output-url (let ((url "/output.txt"))
		 (the building write-bom-file!)
		 (gwl::unpublish url)
		 (gwl::publish-file :path url
				    :file (the output-path))
		 url)))

  :functions
  ((after-set! ()
	       (cond ((member "Reset Form" (the query-plist) :test 'equalp)
		      (when (probe-file (the uploaded-path)) (delete-file (the uploaded-path)))
		      (the (restore-slot-default! :uploaded-path ))))))
		      

  :objects
  ((building :type 'building
	     :input-filename (when (> (length (the uploaded-path)) 1) (the uploaded-path))
	     :output-filename (when (> (length (the uploaded-path)) 1) (the output-path)) )))

(publish-gwl-app "/building-example" "gwl-user::building-application-file")
