(in-package :gwl-user)

(define-object file-upload-3 (base-html-page)
  :computed-slots
  ((uploaded-path "" :settable)
   (file-content (let ((lines (read-file (the uploaded-path))))
		   (mapcar #'(lambda(a) (glisp::split-regexp "," a)) lines)))
   (body
    (with-lhtml-string ()
       (str (the development-links))
       (when (= (length (the uploaded-path)) 0)
	 (htm (str (with-form-string (:enctype "multipart/form-data")
		(:table
		    (:tr (:td (:input :type "file" :name :uploaded-path :value (the uploaded-path))))
		  (:tr (:td (:input :type "submit" :name "upload" :value "Upload"))))))))

      (when (> (length (the uploaded-path)) 0)
	(htm (str (fmt "The file has been uploaded to ~a" (the uploaded-path)))
	     (:p "The file contents are")
	     (:table :border 1 (:tr (:td :colspan 2 (str (first (first (the file-content))))))
	       (dolist (line (cdr (the file-content)))
		 (htm (:tr (:td (str (first line)))
			   (:td (str (second line)))))))
	     
	(str (with-form-string (:enctype "multipart/form-data")
	       (:input :type "submit" :name "reset" :value "Reset Form"))))))))

  :functions
  ((after-set! ()
	       (when (member "Reset Form" (the query-plist) :test 'equalp)
		 
		 (when (probe-file (the uploaded-path)) (delete-file (the uploaded-path)))
		 (the (restore-slot-default! :uploaded-path ))))))

(defun read-file (file )
  (let ((result))
    (with-open-file (str file :direction :input)
      (do ((line (read-line str nil 'eof)
		 (read-line str nil 'eof)))
	  ((eql line 'eof) result)
	(setq result (append result (list line)))))))

(publish-gwl-app "/file-upload-3" "gwl-user::file-upload-3")
