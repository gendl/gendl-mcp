;;
;; FLAG -- Below "newnames" code to be copied to 
;;

(in-package :gwl)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defmacro with-lhtml-string ((&rest args) &body body)
    "Form. Sets up body to be evaluated as lhtml and to return the resulting string."
    (let ((string-stream (gensym)))
      `(with-output-to-string (,string-stream)
         (cl-who:with-html-output (,string-stream nil ,@args)
           ,@body))))
  
  (define-object base-html-page (base-ajax-sheet)

    :input-slots
    ((doctype-string (call-next-method))

     (head-class (call-next-method))
     
     (title (call-next-method))

     (additional-header-content nil)

     (local-assets? t)
     (use-x3dom? t)
     (use-svgpanzoom? t)
     (use-fontawesome? nil)
     (use-anyresize? nil)
     (use-ajax? t)
     
     ("String of HTML. The main body of the page. 
This can be specified as input or overridden in subclass, otherwise it defaults
to the content produced by the :output-function of the same name 
in the applicable lens for  html-format."
      body "Empty Page Body")

     (body-attributes (list :class (the body-class)
                            :onpageshow (the body-onpageshow)
                            :onload (the body-onload)
                            :onresize (the body-onresize)))
     
     (body-class (call-next-method))
     (body-onpageshow (call-next-method))
     (body-onload (call-next-method))
     (body-onresize (call-next-method))
     (html-class (call-next-method))
     (lang "en")
     (charset "UTF-8")
     (favicon-type "image/x-icon")
     (favicon-path "/static/gwl/images/favicon.ico")
     
     )

    :computed-slots
    ((main-sheet (with-lhtml-string ()
                   (str (the doctype-string))
                   ((:html :lang (the lang) :class (the html-class))
                    ((:head :class (the head-class))
                     (:title (str (the title)))
                     (:meta :charset (the charset))
                     (:link :rel "icon" :type (the favicon-type) :href (the favicon-path))
                     (str (the additional-header-content))
                     (when (the use-x3dom?)
	               (htm ((:script :src (if (the local-assets?)
				               "/static/3rdpty/x3dom/x3dom.js"
				               "https://www.x3dom.org/download/1.8.1/x3dom.js")
		                      :id "x3dom_script"))))
                     (when (the use-svgpanzoom?)
	               (htm ((:script
	                      :id "svg-panzoom"
	                      :src (if (the local-assets?)
			               "/static/3rdpty/svgpanzoom/svg-pan-zoom.min.js"
			               "https://cdn.jsdelivr.net/npm/svg-pan-zoom@3.6.1/dist/svg-pan-zoom.min.js")))))
                     (when (the use-fontawesome?)
	               (htm ((:link :id "fontawesome-css"
		                    :rel "stylesheet"
		                    :href (if (the local-assets?)
			                      "/static/3rdpty/fa/css/all.min.css"
			                      "https://use.fontawesome.com/releases/v5.3.1/css/all.css")
		                    :integrity (unless (the local-assets?)
				                 "sha384-mzrmE5qonljUremFsqc01SB46JvROS7bZs3IO2EmfFsd15uHvIt+Y8vEf7N7fWAU")
		                    :crossorigin "anonymous"))))
                     (when (the use-anyresize?)
	               (htm ((:script :id "anyresize-script"
		                      :src (if (the local-assets?)
				               "/static/3rdpty/resize/any-resize-event.js"
				               "https://is.gd/sAeEPt")))))
                     (when (the use-ajax?)
	               (htm
	                ((:script) (fmt "~%var gdliid = '~a';" (the instance-id)))
	                ((:script :src (if (the local-assets?)
			                   "/static/gwl/js/gdlajax1595.js"
			                   "https://genworks.com/static/gwl/js/gdlajax1595.js"))))))

                    ((:body :class (the body-class)
	                    :onpageshow (the body-onpageshow)
                            :onload (the body-onload)
	                    :onresize (the body-onresize))
                     (the reset-html-sections!)
                     
                     (str (the body)))))))

    :functions
    ((write-html-sheet
      ()
      (with-format (html-format *html-stream*)
        (write-string (the main-sheet) *html-stream*)))))


  (define-object base-html-div (sheet-section)
    :computed-slots ((div (the main-div))))

  (define-object viewport-html-div (base-html-div)
    :input-slots ((display-list-objects nil) (display-list-object-roots nil))
    :objects ((view-object :type 'web-drawing
                           :objects (the display-list-objects)
                           :object-roots (the display-list-object-roots))))

  
  (defun publish-relative-file (&key home relative (path (string-append "/" relative)))
    (let ((file (probe-file (merge-pathnames relative home))))
      (if file (setq file (namestring file)) (error "Trying to publish ~a but it cannot be found.~%" file))
      (publish-file :path path :file file) path))


  (defun publish-relative-directory (&key home relative (prefix (string-append "/" (subseq relative 0 (1- (length relative))))))
    (let ((relative-namestring (namestring relative)))
      (unless (eql (aref relative-namestring (1- (length relative-namestring))) #\/)
        (error "Relative name must end with a slash (\"/\"), but ~a does not" relative-namestring)))
    (let ((destination (probe-file (merge-pathnames relative home))))
      (if destination (setq destination (namestring destination)) (error "Trying to publish ~a but it cannot be found.~%" destination))

      (print-variables destination prefix)
      
      (publish-directory :prefix prefix :destination destination) prefix))
  
  (export  '(base-html-page base-html-div viewport-html-div with-lhtml-string publish-relative-file
             publish-relative-directory page-viewport-div with-tagged-body) :gwl))



(defmacro with-form-string ((&key id name enctype target requestor on-submit local-anchor indent) &body body)
  (let ((fixed-prefix (gensym)))
    `(let ((,fixed-prefix (let ((prefix (the fixed-url-prefix)))
			    (and prefix (string-append "/" prefix)))))
       (with-lhtml-string (:indent ,indent)
         (:form :method :post
                :id ,(or id `(format nil "~a-form" (the root-path-string)))
                :name ,(or name `(format nil "~a-form" (the root-path-string)))
                :action (string-append
                         (or ,fixed-prefix "")
                         ,(if local-anchor `(format nil "/answer#~a" local-anchor) "/answer"))
                :enctype ,enctype
                :target ,target
                :on-submit ,on-submit
                (:input :type :hidden :name :|requestor| :value ,(if (null requestor) `(the url-encoded-root-path)
                                                                     `(the-object ,requestor url-encoded-root-path)))
                (:input :type :hidden :name :|iid| :value (the instance-id))
                ,@body)))))


(eval-when (:compile-toplevel :load-toplevel :execute) (export '(with-form-string) :gwl))


                     

;;
;; FLAG -- end of  "newnames" code to be copied to 
;;


;;
;; Patch to allegroserve to make file uplaods work in non-allegro
;;
#-allegro
(in-package :net.aserve)

#-allegro
(defmethod get-multipart-sequence ((req http-request)
				   buffer
				   &key (start 0)
				     (end (length buffer))
				     (external-format 
				      *default-aserve-external-format* 
				      ef-spec))
  ;; fill the buffer with the chunk of data.
  ;; start at 'start' and go no farther than (1- end) in the buffer
  ;; return the index of the first character not placed in the buffer.
  
  
  ;; Since external-format not used in all versions
  (declare (ignorable external-format ef-spec))


  (let* ((mp-info (getf (request-reply-plist req) 'mp-info))
	 mpbuffer 
	 cur
	 pos
	 kind
	 text-mode
	 after)

    
    (typecase buffer
      ((array (unsigned-byte 8) (*))
       )
      ((array character (*))
       (setq text-mode t))
      (t 
       (error 
	"This function only accepts (array (unsigned-byte 8)) or character arrays")))
    (if* (null mp-info)
       then (error "get-multipart-sequence called before get-multipart-header"))
    
    (setq mpbuffer (mp-info-buffer mp-info)
	  cur      (mp-info-cur mp-info))

    (loop
      (case (mp-info-state mp-info)
	((:header :boundary :last-boundary)
                                        ; no data left
	 (return-from get-multipart-sequence nil))
	(:start
	 (error "get-multipart-sequence called before get-multipart-header"))
	((:body :partial)
	 (if* (eq (mp-info-state mp-info) :partial)
	    then      ; this was set below. we will return the partial
                                        ; at then end of the buffer
		      (setf (mp-info-state mp-info) :body)
		      (setq pos (mp-info-end mp-info))
	    else (multiple-value-setq (pos kind after) (scan-forward mp-info))
		 (setf (mp-info-after mp-info) after)
		 (setq cur (mp-info-cur mp-info)) ; scan-forward can change
		 )
	 
	 (if* (> pos cur)
	    then                        ; got something to return
		 (let* ((tocopy (min (- end start) (- pos cur)))
			(items tocopy))

                   
		   (if* text-mode
		      then
			  (dotimes (i tocopy)
			    (setf (aref buffer (+ start i))
			          (code-char (aref mpbuffer (+ cur i)))))
		      else 
			   (dotimes (i tocopy)
			     (setf (aref buffer (+ start i))
			           (aref mpbuffer (+ cur i)))))

                   
		   (if* (zerop items)
		      then         ; didn't find enough bytes to make 
                                        ; a character
			           (if* (null (shift-buffer-up-and-read mp-info))
			              then ; no more bytes available
				           (return-from get-multipart-sequence nil))
                                        ; loop around
		      else (setf (mp-info-cur mp-info) (+ cur tocopy))
			   (return-from get-multipart-sequence 
			     (+ start items))))
	  elseif (eq kind :partial)
	    then                       ; may be a boundary, can't tell
		  (if* (null (shift-buffer-up-and-read mp-info))
		     then     ; no more data, partial will never match
                                        ; so return the partial, this special
                                        ; state is recognized in this routine
			      (setf (mp-info-state mp-info) :partial)
                                        ; loop around
			      )
	  elseif (or (eq kind :boundary)
		     (eq kind :last-boundary))
	    then              ; hit a boundary, nothing more to return
		 (setf (mp-info-state mp-info) kind
		       (mp-info-cur   mp-info) pos)
		 (return-from get-multipart-sequence nil)))))))




