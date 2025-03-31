;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: gendl-skel; Base: 10 -*-

(in-package :gendl-skel)

(define-object file-base (base-object)
  :documentation (:description "Base mixin for file and directory objects in skeleton project")
  
  :input-slots
  (("String. The project name" project-name)
   ("String. The base path where the project will be created" 
    base-path "/projects/xfer/")
   ("String. The name for this file or directory" name)
   ("String or NIL. The file extension, or NIL if this is a directory" extension nil)
   ("Boolean. Flag indicating if this is a directory" directory? nil)


   (parent-path (let ((parent (the parent)))
                  (cond ((null parent) (the base-path))
                        ((and (typep parent 'file-base)
                              (theo parent directory?)) 
                         (the-object parent full-path))
                        ((typep parent 'file-base)
                         (the-object parent parent-path))
                        (t (error "Unexpected condition in parent-path~%"))))))
  
  :computed-slots
  ((full-name (format nil "~a~a" 
                     (the name) 
                     (if (the extension) 
                         (format nil ".~a" (the extension)) 
                         "")))
   
   (full-path (if (the directory?)
                 (format nil "~a~a/" (the parent-path) (the full-name))
                 (format nil "~a~a" (the parent-path) (the full-name))))
   
   (file-content ""))
  
  :functions
  ((ensure-directories-exist!
    ()
    (let ((path (if (the directory?) 
                    (the full-path)
                    (the parent-path))))
      (ensure-directories-exist path)))
   
   (write-to-disk!
    ()
    (the ensure-directories-exist!)
    (cond ((the directory?)
           (dolist (child (the children))
             (the-object child write-to-disk!)))
          (t
           (with-open-file (stream (the full-path)
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
	     (write-string (the file-content) stream))))
    (format t "~&Created: ~a~%" (the full-path))
    (the full-path))))
