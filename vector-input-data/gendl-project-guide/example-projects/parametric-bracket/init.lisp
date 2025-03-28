;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: gdl-user; Base: 10 -*-
(in-package :gdl-user)

(load-quicklisp)

(defparameter *parametric-bracket-path* 
  (make-pathname :name nil :type nil
                :defaults (glisp:source-pathname)))

;; Generate/update the ASDF system definition
(cl-lite *parametric-bracket-path* :create-asd-file? t)

;; Add to Quicklisp's local projects
(pushnew *parametric-bracket-path* ql:*local-project-directories* :test #'equalp)

;; Load the project
(ql:quickload :parametric-bracket)

;; Create a bracket assembly for comparison
(defparameter *assembly* (make-object 'parametric-bracket:assembly))

(format t "~%~%Parametric bracket project loaded successfully!~%")
(format t "A single bracket has been created as *bracket*~%")
(format t "A bracket assembly has been created as *assembly*~%")
(format t "Try: (theo *bracket* width)~%")
(format t "  or: (theo *assembly* (brackets 0) width)~%~%")
