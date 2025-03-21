;;;; -*- coding: utf-8 -*- ;
;;
;; Copyright 2002-2021 Genworks International
;;
;; This source file is part of the General-purpose Declarative
;; Language project (GDL).
;;
;; This source file contains free software: you can redistribute it
;; and/or modify it under the terms of the GNU Affero General Public
;; License as published by the Free Software Foundation, either
;; version 3 of the License, or (at your option) any later version.
;;
;; This source file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public
;; License along with this source file.  If not, see
;; <http://www.gnu.org/licenses/>.
;;
(in-package :gdl)

(defparameter *features-to-initialize* (list :base :glisp :geom-base :gwl
					     :gwl-graphics ;;:tasty
						   :yadd :robot :cl-lite :geysr))

(defun start-gendl! (&key (features *features-to-initialize*)
		       (banner? nil) 
                       (http-port gwl:*http-port*)(https-port gwl:*https-port*)
                       (https? gwl:*https?*))
  (let ((gwl:*http-port* http-port) (gwl:*https-port* https-port) (gwl:*https?* https?))
    (dolist (feature features)
      ;;
      ;; This assumes feature name = package name of its initialize!
      ;; function.  This is the case now. Let's hope we can keep it that
      ;; way.
      ;;
      (let ((package feature))
        (if (and (find-package package) (glisp:featurep feature))
	    (let ((function-sym (read-from-string (format nil "~a::initialize" package))))
	      (when (fboundp function-sym)
	        (let ((description (glisp:package-documentation package)))
		  (format t "~&Initializing ~a subsystem...~%" description)
		  (let ((anything-changed? (funcall function-sym)))
		    (format t "~&...Done~a~%"
			    (if anything-changed? "."
			        (format nil " (no new global settings).")))))))
	    (format t "~&Note: Feature ~s does not appear to be loaded at this time.~%" feature)))))

  ;;
  ;; Calling this again because we probably pushed more functions onto gwl:*publishers* since this
  ;; was called from within gwl:start-gwl.
  ;;
  (gwl:publish-uris)
  
  (gwl:announce-server-port)

  (when banner? (startup-banner))
  ;;(when init-files? (load-gdl-init-files) )
  (values))

(defun start-gdl! (&rest args) (apply #'start-gendl! args))


(let (loaded?)

  (defun reset-gdl-init-files-flag! () (setf loaded? nil))
  (defun set-gdl-init-files-flag! () (setf loaded? t))
  
  (defun load-gdl-init-files (&key force?)
    (when (or (not loaded?) force?)
      (set-gdl-init-files-flag!)
      (let* ((user-homedir (user-homedir-pathname))
             (current-directory (glisp:current-directory))
             (homedir-init-file (or (probe-file (merge-pathnames ".gdlinit.cl" user-homedir))
                                    (probe-file (merge-pathnames "gdlinit.cl" user-homedir))))
             (current-init-file (when (not (equalp user-homedir current-directory))
                                  (or (probe-file (merge-pathnames ".gdlinit.cl" current-directory))
                                      (probe-file (merge-pathnames "gdlinit.cl" current-directory))))))
        (let* ((command-args (glisp:basic-command-line-arguments))
               (homedir-init? (and (not (member "-q" command-args :test #'string-equal))
                                   (not (member "-qq" command-args :test #'string-equal))))
               (current-dir-init? (not (member "-qq" command-args :test #'string-equal))))
          (when (and current-dir-init? current-init-file) (load current-init-file))
          (when (and homedir-init? homedir-init-file) (load homedir-init-file)))))))


(defun quicklisp-copyright-string ()
  (if *already-loaded-systems*
      (let ((ql-libs 
	     (safe-sort 
	      (set-difference *already-loaded-systems*
			      (append (list "asdf" "crypt" "quicklisp" "base" "ent" "validate" "glisp" 
					    "monofasl" "pro" "enterprise")
				      *packages-to-lock*)
			      :key #'string :test #'string-equal) #'string-lessp)))
        
	(format nil "Also contains the following Common Lisp libraries
from Quicklisp, whose source files are available in the
Quicklisp repository at http://quicklisp.org, Copyright© their
respective authors:

~%~%~{~a~^, ~}.~%"
                
                ql-libs)) ""))

(defun development-build? ()

  (let ((version-int (parse-integer *gendl-version* :junk-allowed t)))
    (and version-int (numberp version-int) (oddp version-int))))


(defun startup-banner (&key (stream *standard-output*))

  (format stream
	  "

Gendl® Free Software (AGPL) Edition, version ~a~a
Within ~a ~a 
~a

Welcome to Gendl®
Copyright© 2024, Genworks® International, Bloomfield Hills, MI, USA.
All Rights Reserved.

This program contains free software: you can redistribute it and/or
modify it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public
License along with the source code for this program. If not, see:

http://www.gnu.org/licenses/

"
	  *gendl-version*
	  (let ((ci-data (when (boundp 'cl-user::*ci-data*) cl-user::*ci-data*)))
	    (let ((release-candidate-timestamp
		    (getf ci-data :release-candidate?))
		  (ci-timestamp (getf ci-data :timestamp)))
	      (cond (release-candidate-timestamp
		     (format nil " (Release Candidate. Timestamp: ~a)" release-candidate-timestamp))
		    ((and ci-timestamp (development-build?))
		     (format nil " (Development Build. Build Timestamp: ~a)" release-candidate-timestamp))
		    (t ""))))
	  (lisp-implementation-type)
	  (lisp-implementation-version)
	  (quicklisp-copyright-string)
	  )

  (let ((*standard-output* stream)) (gwl:announce-server-port)))


