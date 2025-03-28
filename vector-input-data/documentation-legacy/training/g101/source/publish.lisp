(in-package :training-g101)

(publish :path "/training-g101"
	 :function #'(lambda(req ent)
		       (gwl-make-object req ent "training-g101:assembly")))


(publish-directory :prefix "/g101/images/"
		   :destination (format nil "~a" (translate-logical-pathname "~/genworks/gendl/documentation/training/g101/images/")))


(publish-directory :prefix "/g101/style/"
		   :destination (format nil "~a" (translate-logical-pathname "~/genworks/gendl/documentation/training/g101/style/")))


(defun make! (&key (output-root (merge-pathnames "g101/" (glisp:temporary-folder))))
  (gwl:crawl :part "training-g101:assembly" :output-root output-root)
  (format t"Done. Your HTML file tree has been created or refreshed in ~a.~%" output-root))

	       
