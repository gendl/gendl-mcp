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

(in-package :geom-base)

(defparameter *try-string* nil)

(defun print-characters (string &key start-char search-length)
  "Print each character of the given string starting with finding character `start-char` 
and continuing for `search-length`."
  (unless start-char (setq start-char (char string 0)))
  (unless search-length (setq search-length (length string)))
  (let (active? active-count)
    (dotimes (i (length string))
      (let ((char (char string i)))
        (when (eql char start-char)
          (setq active? t active-count 0))
        (when active?
          (format t "~a~%" char)
          (when (> (incf active-count) search-length)
            (setq active? nil)))))))

;; Example usage:
;;(print-characters "Hello, Lisp!")


(defmacro with-format ((format stream-or-file &rest args) &body body)
  "Void [Macro]. Used to establish an output format and a stream to which data is to be sent. This 
supports a full range of output options such as page dimensions, view transforms, view scales, etc.

:example
<pre>
 (gdl::with-format (pdf \"/tmp/box.pdf\" :view-transform (getf *standard-views* :trimetric)) 
    (write-the-object (make-instance 'box :length 100 :width 100 :height 100) cad-output))
</pre>"

  (let ((flag (gensym)))
    `(let ((*%format%* (make-instance ',format ,@args))
	   (file? (or (stringp ,stream-or-file) (pathnamep ,stream-or-file))))

       (let ((*stream* (if file?
                           (apply #'open
				  (append
				   (list 
				    ,stream-or-file 
				    :if-does-not-exist
				    (or (format-slot if-does-not-exist) :create)
				    :if-exists (or (format-slot if-exists) :error)
				    :direction (or (format-slot direction) :output))
				   (let ((external-format (format-slot external-format)))
				     (when external-format
				       (list :external-format external-format)))
				   (let ((element-type (format-slot element-type)))
				     (when element-type
				       (list :element-type element-type)))))
                           ,stream-or-file))
             (,flag t))
	   
         (unwind-protect
              (progn
                (multiple-value-prog1
                    ,(case format
                       (pdf
                        `(let ((temp-file (glisp:temporary-file)))
			   (pdf:with-document() 
			     (pdf:with-page
				 (:bounds (make-array 4 :initial-contents 
						      (list 0 0 (format-slot page-width) 
							    (format-slot page-length))))
			       (with-format-slots (page-width page-length background-color)
				 (setq background-color (coerce (lookup-color background-color 
									      :ground :background) 'list))
				 (pdf:with-saved-state 
				   (apply #'pdf:set-rgb-stroke (coerce background-color 'list))
				   (apply #'pdf:set-rgb-fill (coerce background-color 'list)) 
				   (pdf:move-to 0 0) 
				   (pdf:line-to page-width 0) 
				   (pdf:line-to page-width page-length) (pdf:line-to 0 page-length) 
				   (pdf:line-to 0 0) (pdf:fill-and-stroke)))
			       (with-format-slots (page-width page-length)
				 (pdf:translate (half page-width) (half page-length)))
			       ,@body)
			     (pdf:write-document temp-file))

			   (let ((content (alexandria:read-file-into-string
					   temp-file)))
			     ;;(delete-file temp-file)
			     (write-string content *stream*))))

                       (pdf-multipage
                        `(let ((temp-file (glisp:temporary-file)))
			   (with-open-file (temp-out temp-file :direction :output
							       :if-exists :supersede)
			     (pdf:with-document() ,@body
                               (pdf:write-document temp-out)))
			   (let ((content (alexandria:read-file-into-string temp-file)))
			     (delete-file temp-file)
			     (write-string content *stream*))))
                              
		       ;;
		       ;; FLAG -- update multipage to match pdf case behavior
		       ;;

                              
                       ;; FLAG -- consider having default
                       ;; initialize-output and finalize-output
                       ;; methods instead of these special
                       ;; cases. Special case is only really
                       ;; needed when we need to wrap things like
                       ;; in PDF.
                       ;;
                       (dxf `(progn (format *stream* *dxf-header*
                                            (or (and (format-slot units)
                                                     (getf *dxf-units*
                                                           (format-slot units)))
                                                (getf *dxf-units* :no-units)))
                                    ,@body
                                    (write-string *dxf-footer* *stream*)))
                       (otherwise `(progn (write-env (:initialize-output) )
                                          ,@body
                                          (write-env (:finalize-output)))))
                  (setq ,flag nil)))
           (when (and (or (stringp ,stream-or-file) (pathnamep ,stream-or-file)) (streamp *stream*))
             (close *stream* :abort ,flag))) nil))))



(in-package :pdf)

(defmethod write-to-page ((string string) (encoding single-byte-encoding) &optional escape)
  ;; There is no point to interpret \n and others in a special way
  ;; as they are not control characters within content stream
   
  (write-char #\( *page-stream*)
  (if (or escape
	  ;;
	  ;; DJC -- commented out the next two lines, will lodge Issue.
	  ;;
          ;;#+lispworks (lw:text-string-p string)		; may include unicode
          ;;#+allegro t
	  )
      (loop with charset = (charset encoding)
            for char across string
            do (case char
                 ((#\( #\) #\\)
                  (write-char #\\ *page-stream*)
                  (write-char char *page-stream*))
                 ;(#\Newline
                 ; (write-string "\\n" *page-stream*))
                 ;(#\Return
                 ; (write-string "\\r" *page-stream*))
                 ;(#\Tab
                 ; (write-string "\\t" *page-stream*))
                 (otherwise
                  (write-char (if #+lispworks (lw:base-char-p char)
                                  #+(or allegro sbcl) (standard-char-p char)
                                  #-(or lispworks allegro sbcl) t
                                  char
                                  (code-char (char-external-code char charset)))
                              *page-stream*))))
      (write-string string *page-stream*))
  (write-string ") " *page-stream*))

