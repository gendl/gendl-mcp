(in-package :gwl-user)

(define-object simple-file-output (base-html-page)
  :computed-slots
  ((body (with-lhtml-string ()
           (str (the development-links))
	   (str (the export-section div))))

   (file-contents  (let ((line (list 1 2 3 4 5 6 7)))
		     (format nil "~{Line ~a of my file~^~%~}" line)))

   (text-physical-file-url (let ((url (format nil "/file-output-~a.txt" (get-current-date-time)))
                                 (file-path (make-pathname :defaults (glisp:temporary-file)
                                                                    :type "txt")))
                             (with-open-file (f file-path :direction :output :if-exists :supersede)
                               (write-string (the file-contents) f))
                             (publish-file :path url
                                           :content-type "text/plain"
                                           :file file-path)
                             url))

   (text-file-url (let ((url (format nil "/stream-output-~a.txt" (get-current-date-time))))
                    (publish :path url
                             :content-type "text/plain"
                             :function #'(lambda(req ent)
                                           (with-http-response (req ent)
                                             (with-http-body (req ent)
                                               (write-string (the file-contents) *html-stream*)))))
                    url)))
  :objects
  ((export-section
    :type 'page-section
    :inner-html (with-lhtml-string ()
                  (:p "Click "
                      (:a :href (the text-file-url) :download "virtual-text-file.txt" "Here")
                      " to download a virtual text file.")
                  (:p "Click "
                      (:a :href (the text-physical-file-url) :download "physical-text-file.txt" "Here")
                      " to download a physical text file.")))))

(defun get-current-date-time ()
  (let* ((d (multiple-value-bind (s m h d mo y da)
		(get-decoded-time)
	      (declare (ignore da))
	      (list d mo y h m s)))
	 (day (first d))
	 (month (second d))
	 (year (third d))
	 (hour (fourth d))
	 (mins (fifth d))
	 (sec (lastcar d)))
    (format nil "~2,,,'0@a-~2,,,'0@a-~a-~2,,,'0@a-~2,,,'0@a-~2,,,'0@a" day month year hour mins sec)))

(publish-gwl-app "/simple-file-output" "gwl-user::simple-file-output")
