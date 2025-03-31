;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: gendl-skel; Base: 10 -*-

(in-package :gendl-skel)

;; The project directory object
(define-object project-directory (file-base)
  :documentation (:description "Represents the root project directory")
  
  :input-slots
  (("String. The name of the project" project-name)
   (name (the project-name))
   (directory? t))
  
  :objects
  ((source-dir :type 'source-directory
               :pass-down (project-name))
   (init-file :type 'init-file
              :pass-down (project-name))))

;; The source directory object
(define-object source-directory (file-base)
  :documentation (:description "Represents the source directory")
  
  :input-slots
  ((name "source")
   (directory? t))
  
  :objects
  ((package-file :type 'package-file
                 :pass-down (project-name))
   (file-ordering :type 'file-ordering-file
                  :pass-down (project-name))
   (assembly-file :type 'assembly-file
                  :pass-down (project-name))))

;; The init.lisp file object
(define-object init-file (file-base)
  :documentation (:description "Represents the project init.lisp file")
  
  :input-slots
  ((name "init")
   (extension "lisp"))
  
  :computed-slots
  ((file-content (format nil ";;; -*- Mode: LISP; Syntax: Common-Lisp; Package: gdl-user; Base: 10 -*-

(in-package :gdl-user)

;; Load Quicklisp
(load-quicklisp)

;; Define project path
(defparameter *~a-path* 
  (make-pathname :name nil :type nil
                :defaults (glisp:source-pathname)))

;; Generate/update the ASDF system definition if needed
(unless (probe-file (merge-pathnames \"~a.asd\" *~a-path*))
  (cl-lite *~a-path* :create-asd-file? t))

;; Add to Quicklisp's local projects
(pushnew *~a-path* ql:*local-project-directories* :test #'equalp)

;; Load the project
(ql:quickload :~a)

;; Create a default instance
(defparameter *~a* (make-object '~a:assembly))

;; Print success message
(format t \"~%~%~a project loaded successfully!~%\")
(format t \"A default instance has been created as *~a*~%\")
(format t \"Try: (theo *~a* <slot-name>)~%~%\")
" (the project-name) (the project-name) (the project-name) (the project-name) (the project-name) 
  (the project-name) (the project-name) (the project-name)
  (the project-name) (the project-name) (the project-name)))))

;; The package.lisp file object
(define-object package-file (file-base)
  :documentation (:description "Represents the package.lisp file")
  
  :input-slots
  ((name "package")
   (extension "lisp"))
  
  :computed-slots
  ((file-content (format nil ";;; -*- Mode: LISP; Syntax: Common-Lisp; Package: gdl-user; Base: 10 -*-

(in-package :gdl-user)

(gdl:define-package :~a
  (:export #:assembly))
" (the project-name)))))

;; The file-ordering.isc file object
(define-object file-ordering-file (file-base)
  :documentation (:description "Represents the file-ordering.isc file")
  
  :input-slots
  ((name "file-ordering")
   (extension "isc"))
  
  :computed-slots
  ((file-content "(\"package\")")))

;; The assembly.lisp file object
(define-object assembly-file (file-base)
  :documentation (:description "Represents the main assembly.lisp file")
  
  :input-slots
  ((name "assembly")
   (extension "lisp"))
  
  :computed-slots
  ((file-content (format nil ";;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ~a; Base: 10 -*-

(in-package :~a)

(define-object assembly (base-object)
  :documentation (:description \"Main assembly for the ~a project\")
  
  :input-slots
  ((\"Number. Example parameter\" parameter-1 100)
   (\"String. Example parameter\" parameter-2 \"Default Value\"))
   
  :computed-slots
  ((example-computed-slot (+ (the parameter-1) 42)))
   
  :objects
  ((example-box :type 'geom-base:box
                :width (the parameter-1)
                :length (the parameter-1)
                :height 50
                :display-controls (list :color :green :transparency 0.3))))
" (the project-name) (the project-name) (the project-name)))))

;; The main assembly object that generates the entire project structure
(define-object assembly (base-object)
  :documentation (:description "Gendl project skeleton generator")
  
  :input-slots
  (("String. The name of the project" project-name "dummy-project")
   ("String. The base path where the project will be created" 
    base-path "/projects/xfer/"))

  :computed-slots
  ((parent-path (the base-path)))
   
  :objects
  ((project-dir :type 'project-directory
		:directory? t
		:pass-down (parent-path project-name base-path )))
  
  :functions
  ((write-to-disk! 
    ()
    (when (probe-file (the project-dir full-path))
      (error "Project directory ~a exists. I cannot overwrite.
Please delete or pick a different name."
	     (the project-dir full-path)))
    (the project-dir write-to-disk!))))

(defun gendl-skel (&key (project-name "gendl-try") (base-path "/projects/xfer/tmp/"))
  (let ((self (make-object 'assembly :project-name project-name
				     :base-path base-path)))
    (the write-to-disk!)
    (cl-lite (the project-dir full-path) :create-asd-file? t)
    (format t "Done. Created ~a.~%~%" (the project-dir full-path))
    (the project-dir full-path)))
