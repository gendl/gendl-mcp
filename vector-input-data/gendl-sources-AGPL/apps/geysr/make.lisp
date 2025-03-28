(in-package :gdl-user)

(defparameter *systems* (list :gendl-patches :geysr))
(defparameter *monolithic-systems* (list :cl-markdown))

(load-quicklisp)

(defun ql-add-local (dir)
  (pushnew dir ql:*local-project-directories* :test #'equalp))

(defparameter *geysr-home* (make-pathname :name nil :type nil :defaults *load-pathname*))


(defun make-geysr-fasl (&key (output (translate-logical-pathname "~/tmp/geysr/")))

  (load-quicklisp)
  (mapc #'ql-add-local (list "~/genworks/gendl-patches/" *geysr-home*))


  (when (probe-file output)
    (uiop:delete-directory-tree output :validate #'(lambda(arg) (search "tmp" (namestring arg)))))
  (ensure-directories-exist output)

  (dolist (system *monolithic-systems*)
    (asdf:operate 'asdf:monolithic-compile-bundle-op system)
    (let ((path (asdf:output-file 'asdf:monolithic-compile-bundle-op system)))
      (uiop:copy-file path
		      (make-pathname :defaults output
				     :name (pathname-name path)
				     :type (pathname-type path)))))
  
  (dolist (system *systems*)
    (asdf:operate 'asdf:compile-bundle-op system)
    (let ((path (asdf:output-file 'asdf:compile-bundle-op system)))
      (uiop:copy-file path
		      (make-pathname :defaults output
				     :name (pathname-name path)
				     :type (pathname-type path)))))


  (glisp:copy-directory (merge-pathnames "static/" (glisp:system-home :geysr))
			(merge-pathnames "geysr-static/" output))
  
  
  (with-open-file (out (merge-pathnames "load.lisp" output)
		       :direction :output :if-exists :supersede :if-does-not-exist :create)

    (let ((format-string
	   (format nil "(in-package :gdl-user)

(defparameter *geysr-home* (make-pathname :name nil :type nil :defaults *load-pathname*))

~~{(load (merge-pathnames \"~~(~~a~~)--all-systems.~a\" *geysr-home*))~~^~~%~~}
~~{(load (merge-pathnames \"~~(~~a~~)--system.~a\" *geysr-home*))~~^~~%~~}

(geysr:initialize)

"
		   glisp:*fasl-extension* glisp:*fasl-extension*
		   )))

    (format out format-string *monolithic-systems* *systems*))))




