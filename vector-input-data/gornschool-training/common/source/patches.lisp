(in-package :gwl)


(defvar *snap-home* nil)


(defun find-lost-session (req ent &key (snap-home *snap-home*))
  (let (url)
    (with-http-response (req ent) (setq url (lost-session-finder req :snap-home snap-home)))
    (if url (with-http-response (req ent :response *response-found*)
              (setf (reply-header-slot-value req :cache-control) "no-cache")
              (setf (reply-header-slot-value req :pragma) "no-cache")
              (setf (reply-header-slot-value req :location) (format nil "~a" url))
              (setf (reply-header-slot-value req :response) *response-found*)
              (with-http-body (req ent)))
        (with-http-response (req ent)
          (with-http-body (req ent)
            (format *html-stream* "Sorry, session was not found.~%"))))))


(defun  lost-session-finder (req &key (snap-home *snap-home*))
  (let* ((components (glisp:split-regexp "/" (uri-path (request-uri req))))
         (root-path (gwl::compute-root-path (reverse (rest (rest (rest components))))))
         (session-id (nth (1+ (position "sessions" components :test #'string-equal)) components))
         (new-session (restore-session! session-id :snap-home snap-home)))
    (if new-session (theo new-session (follow-root-path root-path) url))))

(defun restore-session! (session-id &key (snap-home *snap-home*))
  (when snap-home
    (let ((filename (merge-pathnames (format nil "~a.snap" session-id) snap-home)))
      (when (probe-file filename)
        (read-snapshot :filename filename)))))

(defun write-snap (self &key (snap-home *snap-home*))
  (when snap-home
    (let ((filename (merge-pathnames (format nil "~a.snap" (the instance-id)) snap-home)))
      (the root (write-snapshot :filename filename)))))


;; FLAG -make this work only for requests from the local machine.
;;
#+nil
(defun activate-poison-pill ()
  (publish :path "/terminate-code-108"
           :function #'(lambda(req ent)
                         (bt:make-thread #'(lambda()
                                             (sleep 5) (net.aserve:shutdown)
                                             #+ccl (ccl:quit)
                                             #+allegro (excl:exit 0 :no-unwind t)
                                             #-(or ccl allegro) (error "Need quit function for ~a.~%" (lisp-implementation-type)))
                                         :name "terminate-code-108")
                         (with-http-response (req ent)
                           (with-http-body (req ent)
                             (format *html-stream* "Terminating in five seconds..."))))))


(defun clear-all-instances ()
  "Void. Clears all instances from GWL's master table of root-level instances.
The instance IDs are the numbers you see in published GWL URIs, and are available
as the \"instance-id\" message within each GWL object which inherit from base-html-sheet.

Clearing all the instances makes available for garbage collection all memory used by
the object hierarchies rooted at the instances, as well as all associated published URIs.

:example <pre>
  (clear-all-instance)
  </pre>"
  (maphash #'(lambda(key val)
               (declare (ignore val))
               (unless (member key gwl::*keys-to-preserve*)
                 (clear-instance key))) *instance-hash-table*)
  
  (maphash #'(lambda(key val)
               (declare (ignore val))
               (unless (member key gwl::*keys-to-preserve*)
                 (gwl::unpublish-instance-urls key))) *url-hash-table*))
(define-object-amendment base-html-sheet ()
  :computed-slots
  ((compute-urls (let ((url-base (format nil "~{~(~a~)~^/~}"
					 (mapcar #'(lambda(component)
						     (if (listp component)
							 (format nil "~a/~a" (first component) (second component))
							 component))
						 (reverse (the root-path))))))
		   (append (mapcar #'(lambda(extension)
				       (string-append url-base (if (zerop (length url-base)) "" "/")  extension))
				   (the url-extensions))
			   (list url-base (the descriptive-url)))))))

;;
;; FLAG -- merge with  GDL 1598 
;;

(#+allegro excl:without-package-locks #-allegro progn
 (#+allegro excl:without-redefinition-warnings #-allegro progn
  (defun crawl-anchor (anchor host port output-root visited-urls)
    (let ((uri (net.uri:parse-uri (getf (rest (first anchor)) :href))))
      (let ((scheme (net.uri:uri-scheme uri))
	    (uri-host (net.uri:uri-host uri))
	    (uri-port (net.uri:uri-port uri))
	    (path (net.uri:uri-path uri)))
        (unless (or (null path) ;; empty path - likely hash mark anchor.
                    (not (string-equal (pathname-type path) "html")) ;; punt on non-html urls
		    scheme uri-host uri-port ;; this is external - don't crawl.
		    (gethash path visited-urls)) ;; already crawled - don't crawl
	  (crawl-url path host port output-root visited-urls)))))))


(defun publish-gwl-app (path string-or-symbol &key publish-args make-object-args host headers (server *http-server*))
  "Void. Publishes an application, optionally with some initial arguments to be passed in as input-slots.

:arguments (path \"String. The URL pathname component to be published.\"
            string-or-symbol \"String or symbol. The object type to insantiate.\")

:&key (make-object-args \"Plist. Extra arguments to pass to make-object.\")
"

  (apply #'publish
	 :path path
	 :server server
         :host host
         :headers headers
	 :function #'(lambda(req ent)
		       (gwl-make-object req ent 
					(format nil (if (stringp string-or-symbol) "~a" "~s")
						string-or-symbol)
					:make-object-args make-object-args))
	 publish-args))




(define-object-amendment skeleton-form-control ()
  :input-slots
  ((required? nil) (autocomplete nil)))


(defmacro with-expanded-html-output ((var &key stream prologue) &body body)
  (let ((new-body
          (mapcar #'(lambda(form)
                      (cond ((and (listp form)
                                  (listp (first form))
                                  (member (first (first form)) '(:input :select :textarea :button)))
                             (cons (append (first form)
                                           `(:required
                                             (the required?)
                                             :autocomplete (the autocomplete)


                                             :disabled 
                                             (if (the disabled?) t nil)
                                             ;;
                                             ;; The new HTML5 stuff:
                                             ;;
                                             :placeholder (the placeholder)
                                            
                                            
                                             :class (the class)
                                             :readonly (if (the readonly?) t nil)
                                             :ismap (if (the ismap?) t nil)
                                             :size  (the size)
                                             :maxlength (the maxlength)
                                             :src (the src)
                                             :alt (the alt)
                                             :usemap (the usemap)
                                             :tabindex (the tabindex)
                                             :accesskey (the accesskey)
                                             :onfocus (the onfocus)
                                             :onblur (the onblur)
                                             :onselect (the onselect)
                                             :onchange (the onchange)
                                             :ondblclick (the ondblclick)
                                             :onclick (the onclick)
                                             :onmousedown (the onmousedown)
                                             :onmouseup (the onmouseup)
                                             :onmouseover (the onmouseover)
                                             :onmousemove (the onmousemove)
                                             :onmouseout (the onmouseout)
                                             :onkeypress (the onkeypress)
                                             :onkeydown (the onkeydown)
                                             :onkeyup (the onkeyup)
                                             :accept (the accept)
                                             :lang (the lang)
                                             :title (the title)
                                             :style (the style)
                                             :align (the align)))
                                   (rest form)))
                            (t form))) body)))
    `(with-html-output (,var ,stream :prologue ,prologue :indent :indent)
       ,@new-body)))



;;
;; Just to add the max-id-value input
;;
(defun %gwl-make-object% (part &key make-object-args share? skin (max-id-value 9999999999999999999999999))
  (let* ((instance-id (if share? "share" (make-new-instance-id :max-value max-id-value)))
         (current (gethash (make-keyword-sensitive instance-id) *instance-hash-table*))
         (skin (if skin (make-instance skin) t))
         (root-part-and-version 
          (if (or (not share?) (not current))
              (list (apply #'make-object (read-safe-string part)
                           :instance-id instance-id
                           make-object-args) *dummy-version*) current)))
    (setf (gethash (first root-part-and-version) *weak-objects*) t)
    (setq root-part-and-version (append root-part-and-version (list skin)))
    (when (or (not share?) (not current))
      (setf (gethash (make-keyword-sensitive instance-id) *instance-hash-table*) root-part-and-version))
    (first root-part-and-version)))



(defparameter *user-name* nil)


(defun set-user-name ()
  (setq *user-name*
	#+linux
        (multiple-value-bind (name error code)
            (uiop:run-program (format nil "id -nu ~a" (glisp:getuid))
                              :output :string
                              :ignore-error-status t
                              :error-output :string)
          (if (zerop code) (glisp:replace-regexp name "\\s" "")
              (error "setting username resulted in code ~a with error: ~a~%" code error)))
	#+windows
	(lastcar (pathname-directory (user-homedir-pathname))))
  *user-name*)


(defun publish-sessions-catchall (&key (server *https-server*) host snap-home)
  (publish-prefix  :server server :host host :prefix "/sessions"
                   :function #'(lambda(req ent) (find-lost-session  req ent :snap-home snap-home))))

(defun publish-http-catchall (&key (server *http-server*) host)
  "Redirect all requests to the equivalent request on the *https-server*"
  (publish-prefix :server server :prefix "/"
                  :host host
                  :function #'(lambda(req ent) (redirect-to-https req ent))))

(defun redirect-to-https (req ent)
  (let ((https-host-port (or (uiop:getenv "HTTPS_HOST_PORT") *https-port*)))
    (unless (and *https-server* https-host-port)
      (error "redirect-to-https: *https-server* and environment HTTPS_HOST_PORT must be set"))
    (let* ((port (let ((try (parse-integer (format nil "~a" https-host-port))))
                   (unless (= try 443) try)))
           (scheme "https")
           (uri (slot-value req 'uri))
           (host (uri-host uri))
           (path (uri-path uri)))
      (with-http-response (req ent :response *response-found*)
        (setf (reply-header-slot-value req :cache-control) "no-cache")
        (setf (reply-header-slot-value req :pragma) "no-cache")
        (let ((target (format nil "~a://~a~a~a"
                              scheme host (if port (format nil ":~a" port) "") path)))
          (format t "Redirecting to: ~a~%" target)
          (setf (reply-header-slot-value req :location) target))
        (with-http-body (req ent))))))



