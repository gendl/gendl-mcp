;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: gdl-user; Base: 10 -*-

;;;
;;; Template init.lisp for Gendl projects
;;; Note: You can use gendl-skel:gendl-skel to generate a complete project
;;; including a properly formatted init.lisp file
;;;

(in-package :gdl-user)

;;;
;;; Step 1: Load Quicklisp (if not already loaded)
;;;
(load-quicklisp)

;;;
;;; Step 2: Define the project path (adjust as needed)
;;;
(defparameter *project-path* (make-pathname :name nil :type nil
					    :defaults (glisp:source-pathname)))

;;;
;;; Step 3: Use cl-lite to generate/update the ASDF system definition if needed
;;;
(unless (probe-file (merge-pathnames "project-name.asd" *project-path*))
  (cl-lite *project-path* :create-asd-file? t))

;;;
;;; Step 4: Add the project directory to Quicklisp's local projects
;;;
(pushnew *project-path* ql:*local-project-directories* :test #'equalp)

;;;
;;; Step 5: Load the project using Quicklisp
;;;
(ql:quickload :project-name) ;; this name normally equals last component of *project-path*

;;;
;;; Optional: Set up a default instance for interactive use
;;;
(defparameter *project-name* 
  (make-object 'project-name:assembly))

;;;
;;; Optional: Print success message
;;;
(format t "~%~%Project loaded successfully!~%")
(format t "A default instance has been created as *project-name*~%")
(format t "Try: (theo *project-name* some-slot)~%~%")
