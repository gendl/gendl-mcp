(in-package :gwl-user)

(define-object file-upload-2 (base-html-page)
  :computed-slots
  ((uploaded-path "" :settable)
   (body
    (with-lhtml-string ()
       (str (the development-links))
       (when (= (length (the uploaded-path)) 0)
	 (htm (str (with-form-string (:enctype "multipart/form-data")
		(:table
		    (:tr (:td (:input :type "file" :name :uploaded-path :value (the uploaded-path))))
		  (:tr (:td (:input :type "submit" :name "upload" :value "Upload"))))))))

      (when (> (length (the uploaded-path)) 0)
	(htm (str (format nil "The file has been uploaded to ~a" (the uploaded-path))))
	(str (with-form-string (:enctype "multipart/form-data")
	       (:input :type "submit" :name "reset" :value "Reset Form"))))))

	 )

  :functions
  ((after-set! ()
	       (when (member "Reset Form" (the query-plist) :test 'equalp)
		 
		 (when (probe-file (the uploaded-path)) (delete-file (the uploaded-path)))
		(the (restore-slot-default! :uploaded-path ))))))

(publish-gwl-app "/file-upload-2" "gwl-user::file-upload-2")
