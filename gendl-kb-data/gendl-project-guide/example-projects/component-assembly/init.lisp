;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: gdl-user; Base: 10 -*-
(in-package :gdl-user)

(load-quicklisp)

(defparameter *component-assembly-project-path* (make-pathname :name nil :type nil
						    :defaults (glisp:source-pathname)))

;; Generate/update the ASDF system definition
(cl-lite *component-assembly-project-path* :create-asd-file? t)

;; Add to Quicklisp's local projects
(pushnew *component-assembly-project-path* ql:*local-project-directories* :test #'equalp)

;; Load the project
(ql:quickload :component-assembly)

;; Create a default instance for testing
(defparameter *assembly*
  (make-object 'component-assembly:assembly)) ;; assembly
					      ;; is :exported
					      ;; so only one
					      ;; colon (:)
					      ;; needed. Any
					      ;; use of double
					      ;; colons is
					      ;; considered bad
					      ;; style.

(format t "~%~%Example project loaded successfully!~%")
(format t "A component assembly has been created as *assembly*~%")
(format t "Try: (theo *assembly* (components 0) volume)~%~%")
