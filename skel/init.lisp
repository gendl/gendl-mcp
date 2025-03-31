;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: gdl-user; Base: 10 -*-

(in-package :gdl-user)

;; Load Quicklisp
(load-quicklisp)

;; Define project path
(defparameter *gendl-skel-path* 
  (make-pathname :name nil :type nil
                :defaults (glisp:source-pathname)))

;; Generate/update the ASDF system definition - skip for now
(unless (probe-file (merge-pathnames "gendl-skel.asd" *gendl-skel-path*))
  (cl-lite *gendl-skel-path* :create-asd-file? t))

;; Add to Quicklisp's local projects
(pushnew *gendl-skel-path* ql:*local-project-directories* :test #'equalp)

;; Load the project
(ql:quickload :gendl-skel)

;; Print success message
(format t "~%~%Gendl-skel project loaded successfully!~%")
(format t "You can create a new project skeleton with:~%")
(format t "(theo (make-object 'gendl-skel:assembly :project-name \"my-project\") write-to-disk!)~%~%")
