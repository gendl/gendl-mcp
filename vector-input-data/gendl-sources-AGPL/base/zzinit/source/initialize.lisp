;;
;; Copyright 2002-2011 Genworks International
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


(defun initialize (&key startup-banner?)
  
  (setq glisp:*gdl-program-home* (probe-file (glisp:executable-homedir-pathname)))
  (setq glisp:*gdl-home*
        (make-pathname
         :name nil
	 :type nil
	 :directory (let ((lastcar (lastcar (pathname-directory glisp:*gdl-program-home*))))
                      (if (string-equal lastcar "program")
                          (butlast (pathname-directory glisp:*gdl-program-home*))
                          (pathname-directory glisp:*gdl-program-home*)))
	 :defaults glisp:*gdl-program-home*))
  (setq glisp:*gendl-home* glisp:*gdl-home*)

  (when (find-package :asdf) (funcall (read-from-string "asdf:initialize-output-translations")))
  
  (setq *quicklisp-home*
	(or (when (and (find-package :ql) (boundp (read-from-string "ql:*quicklisp-home*"))
		       (probe-file (symbol-value (read-from-string "ql:*quicklisp-home*"))))
	      (symbol-value (read-from-string "ql:*quicklisp-home*")))
	    (probe-file (merge-pathnames "quicklisp/" glisp:*gendl-home*))
	    (probe-file (merge-pathnames "genworks/quicklisp/" glisp:*gendl-home*))
	    (when glisp:*genworks-source-home*
	      (probe-file (merge-pathnames "quicklisp/" glisp:*genworks-source-home*)))
	    (probe-file (merge-pathnames "genworks/quicklisp/dists/quicklisp/distinfo.txt"
					 glisp:*gdl-home*))))

  (when (and (find-package :ql) (boundp (read-from-string "ql:*quicklisp-home*"))
	     (not (probe-file (symbol-value (read-from-string "ql:*quicklisp-home*")))))
    (setf (symbol-value (read-from-string "ql:*quicklisp-home*")) *quicklisp-home*))
  
  (when (fboundp 'cl-user::setup-cl+ssl) (funcall #'cl-user::setup-cl+ssl))
  
  (pushnew (make-keyword (format nil "gendl-~a" *gendl-version*)) *features*)
  (glisp::set-genworks-source-home-if-known)
  (glisp::set-gendl-source-home-if-known)
  (glisp:set-default-float-format)
  (glisp:set-defpackage-behavior)
  (glisp:set-default-package)
  (glisp:set-window-titles)

  (uiop:setup-temporary-directory)

  (when (find-package :gwl)
    (funcall (read-from-string "gwl:renew-wserver"))
    (flet ((true? (val) (and val (not (or (and (stringp val)
                                               (or (string-equal val "false")
                                                   (string-equal val "0")
                                                   (string-equal val "")
                                                   (string-equal val "nil")))
                                          (and (numberp val) (zerop val)))))))
      (let ((start-http? (or gwl:*start-http?*
                             (null (uiop:getenv "START_HTTP"))
                             (true? (uiop:getenv "START_HTTP"))))
            (start-https? (or gwl:*start-https?*
                              (true? (uiop:getenv "START_HTTPS")))))
        (setq gwl:*start-http?* start-http?
              gwl:*start-https?* start-https?))))
  
  (when (find-package :pdf)
    (setf (symbol-value (read-from-string "pdf::*cl-pdf-base-directory*")) glisp:*gdl-home*)
    (setf (symbol-value (read-from-string "pdf::*afm-files-directories*"))
		      (list (merge-pathnames "afm/"
					     (symbol-value
					      (read-from-string "pdf::*cl-pdf-base-directory*"))))))

  (when (find-package :cl-typesetting-hyphen)
    (setf (symbol-value (read-from-string "cl-typesetting-hyphen::*cl-typesetting-base-directory*")) glisp:*gdl-home*)

    (setf (symbol-value (read-from-string "cl-typesetting-hyphen::*hyphen-patterns-directory*")) 
	  (merge-pathnames "hyphen-patterns/"
	                   (symbol-value (read-from-string "cl-typesetting-hyphen::*cl-typesetting-base-directory*"))))
    (funcall (read-from-string "cl-typesetting::initialize!")
	     :afm-files-directories
             (symbol-value (read-from-string "pdf::*afm-files-directories*"))
	     :hyphen-patterns-directory 
	     (symbol-value (read-from-string
                            "cl-typesetting-hyphen::*hyphen-patterns-directory*"))))
    
  (load-gdl-init-files)

  (when startup-banner? (gendl::startup-banner))
  )


(defun deinitialize ()
  (setq glisp:*gdl-program-home* nil
        glisp:*gdl-home* nil
        glisp:*gendl-home* nil
        *quicklisp-home* nil
        ;; FLAG deal with ql:*quicklisp-home*

        glisp:*genworks-source-home* nil
        glisp:*gendl-source-home* nil
        
        gwl::*fullchain-pem-path* nil
        gwl::*privkey-pem-path* nil)

  (reset-gdl-init-files-flag!)

  (when (find-package :pdf)
    (setf (symbol-value (read-from-string "pdf::*cl-pdf-base-directory*")) nil)
    (setf (symbol-value (read-from-string "pdf::*afm-files-directories*")) nil)
    )

  (when (find-package :cl-typesetting-hyphen)
    (setf (symbol-value
           (read-from-string "cl-typesetting-hyphen::*cl-typesetting-base-directory*")) nil)
    (setf (symbol-value
           (read-from-string "cl-typesetting-hyphen::*hyphen-patterns-directory*")) nil))
  
  (when (and (find-package :zacl) (find-package :net.aserve))
    (when gwl:*http-server* (funcall (read-from-string "net.aserve:shutdown")
				     :server gwl:*http-server*))
    (when gwl:*https-server* (funcall (read-from-string "net.aserve:shutdown")
				     :server gwl:*https-server*))
    (makunbound (read-from-string "net.aserve:*wserver*"))
    (setq gwl:*http-server* nil)
    (setq gwl:*https-server* nil)
    (makunbound (read-from-string "excl:*initial-terminal-io*")))
  ;;
  ;; FLAG -- cffi prolly has one function to call to do all this & more.
  ;;
  (when (find-package :cffi)
    (setf (symbol-value (read-from-string "cffi:*foreign-library-directories*")) nil)
    (dolist (library (funcall (read-from-string "cffi:list-foreign-libraries")))
      (funcall (read-from-string "cffi:close-foreign-library") library))))

        
