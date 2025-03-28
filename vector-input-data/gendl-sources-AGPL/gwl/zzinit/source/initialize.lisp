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

(in-package :gwl)

(defvar *static-home* nil)

(defun ensure-static-relative-pathname (relative)
  (let ((pathname (merge-pathnames relative *static-home*)))
    (or (probe-file pathname)
        (warn "Expected static subdirectory ~a does not appear to exist.~%" pathname))))

(defun publish-images (server)
  (let ((destination (ensure-static-relative-pathname "gwl/images/")))
    (when destination (publish-directory
                       :headers *publish-directory-headers*
                       :prefix "/images/gwl/"
                       :server server
                       :destination (namestring destination)))))

(defun publish-statics (server)
  (let ((destination (ensure-static-relative-pathname "")))
    (when destination (publish-directory
                       :headers *publish-directory-headers*
                       :prefix "/static/"
                       :server server
                       :destination (namestring destination)))))

(defun publish-style (server)
  (let ((destination (ensure-static-relative-pathname "gwl/style/")))
    (when destination (publish-directory
                       :headers *publish-directory-headers*
                       :prefix "/style/"
                       :server server
                       :destination (namestring destination)))))


(dolist (func (list 'publish-images 'publish-statics 'publish-style))
  (pushnew func *publishers*))



;;
;; FLAG -- get platform-specific stuff below into :glisp package.
;;


#+nil
(defun client-test (port)

  #+allegro

  (multiple-value-bind (result error)
      (ignore-errors
        (glisp:with-timeout (2 (error "AllegroServe port probe timed out on port ~a.
Perhaps a zombie process is holding port ~a?~%" port port))


          (#+nil
           net.aserve.client:do-http-request
           drakma:http-request (format nil "http://localhost:~a" port))))


    (declare (ignore result))
    (when (typep error 'error)
      port))

  #-allegro
  (let* ((result
          (handler-case
              (let ((sock (usocket:socket-listen "localhost" port)))
                (usocket:socket-close sock))
            (usocket:address-in-use-error (e) :in-use)
            (t (e) :unknown))))
    (unless (member result '(:in-use :unknown)) port)))


(defun client-test (port &key bypass?)
  (when bypass? (return-from client-test port))
  #-(or ccl sbcl)
  (multiple-value-bind (result error)
      (ignore-errors
       (glisp:with-timeout (3 (error "AllegroServe port probe timed out on port ~a.
Perhaps a zombie process is holding port ~a?~%" port port))
         (net.aserve.client:do-http-request (format nil "http://127.0.0.1:~a" port))))
    (declare (ignore result))
    (when (or (typep error #-allegro 'error #+allegro 'excl:socket-error)
              (typep error #-allegro 'error #+allegro 'simple-error)) port))
  #+(or ccl sbcl)
  (let* ((result
          (handler-case
              (let ((sock (usocket:socket-listen "127.0.0.1" port)))
                (usocket:socket-close sock))
            (usocket:address-in-use-error (e) (declare (ignore e)) :in-use)
            (t (e) (declare (ignore e)) :unknown))))
    (unless (or (member result '(:in-use :unknown))
                #+windows-host
                (ignore-errors
                  (net.aserve.client:do-http-request
                      (format nil "http://127.0.0.1:~a" port)))) port)))


(defvar *fullchain-pem-path* nil)
(defvar *privkey-pem-path* nil)



(defun start-gwl (&key (https? *https?*)
                    (port (if https? *https-port* *http-port*))
                    (https-port *https-port*)
                    (listeners *aserve-listeners*)
                    (external-format *http-server-external-format*)
                    aserve-start-args
                    ;;
                    ;; FLAG give proper continuations here to allow
                    ;; user to set the paths.
                    ;;
                    )
  
  (dolist (server-type (list :non-ssl :ssl))
    (let ((doit? (or (and (eql server-type :non-ssl) *start-http?* (not https?))
                     (and (eql server-type :ssl) (or *start-https?* https?)))))

      (when doit?
        (let* ((https? (eql server-type :ssl))
               (fullchain-pem
                 (when https?
                   (let ((path *fullchain-pem-path*))
                     (if path (namestring path)
                         (warn "~%Your fullchain.pem is missing.
Please set gwl:*fullchain-pem-path* to a valid fullchain.pem as
can be obtained for example from letsencrypt certbot.~&~%")))))
               (privkey-pem
                 (when https?
                   (let ((path *privkey-pem-path*))
                     (if path (namestring path)
                         (warn "~%Your privkey.pem is missing.
Please set gwl:*privkey-pem-path* to a valid privkey.pem as
can be obtained for example from letsencrypt certbot.~&~%"))))))
               
          ;;
          ;; FLAG -- make this into one last continuation where they have a choice to start a normal server
          ;; or finally set valid paths into the parameters.
          ;;
          (when (and https? (not (and fullchain-pem privkey-pem)))
            (error "~%ERROR: Can't start SSL server due to missing cert files, and
we're not gonna guess what it is you're trying to do and risk stepping on
other servers. Bailing.~&"))

    
          (let ((wait-time 1)
                (bypass-client-test?
                  (or (and (not https?) *http-server* (server-port :server *http-server*)
                           (= (server-port :server *http-server*) port))
                      (and https? *https-server* (server-port :server *https-server*)
                           (= (server-port :server *https-server*) port)))))

            (block :outer
              (do () (nil)
                (let ((port port))
                  (block :inner
                    (do ((port-free? (client-test port :bypass? bypass-client-test?)
                                     (client-test port :bypass? bypass-client-test?)))
                        (port-free?
                         (format t (if (> wait-time 1) "~&Retrying AllegroServe on ~a...~%"
                                       "~&Trying to start AllegroServe on ~a...~%") port)

                         (if (with-error-handling () ;;ignore-errors
                               (if https?
                                   (progn
                                     (setq *https-port* https-port
                                           *https-server*
                                           (apply #'net.aserve:start
                                                  #+clasp :host #+clasp "localhost"
                                                  :port *https-port* :listeners listeners
                                                  :external-format external-format
                                                  :server (or *https-server* :new)
                                                  :ssl fullchain-pem
                                                  :ssl-key privkey-pem
                                                  :ssl-args (list :key privkey-pem
                                                                  :certificate fullchain-pem)
                                                  ;;#+ccl :chunking #+ccl nil
                                                  aserve-start-args)))

                                   (progn
                                     (setq *http-port* port
                                           *http-server*
                                           (apply #'net.aserve:start 
                                                  #+clasp :host #+clasp "localhost"
                                                  ;; :server defaults to *http-server*
                                                  ;;:server (or *http-server* *wserver* :new)
                                                  :server (or *http-server* :new)
                                                  :port *http-port* :listeners listeners
                                                  :external-format external-format
                                                  ;;#+ccl :chunking #+ccl nil
                                                  aserve-start-args)
                                           ;;*wserver* *http-server*
                                           ))))
                             (return-from :outer port)
                             (progn (sleep (random wait-time)) (return-from :inner))))
                      (incf port))))
                (incf wait-time 0.1))))))))
        (publish-uris))



(defun settings (&key (privkey-pem-path *privkey-pem-path*)
		   (fullchain-pem-path *fullchain-pem-path*)
		   (static-home *static-home*))
  (list
   (list '*privkey-pem-path* privkey-pem-path
         #'(lambda()
             (or (and glisp:*gendl-home*
                      (or
                       (probe-file (merge-pathnames "certs/privkey.pem" glisp:*gendl-home*))
                       (probe-file (merge-pathnames "test-certs/privkey.pem" glisp:*gendl-home*))))
                 (probe-file "/etc/certs/privkey.pem"))))

   (list '*fullchain-pem-path* fullchain-pem-path
         #'(lambda()
             (or (and glisp:*gendl-home*
                      (or
                       (probe-file (merge-pathnames "certs/fullchain.pem" glisp:*gendl-home*))
                       (probe-file (merge-pathnames "test-certs/fullchain.pem" glisp:*gendl-home*))))
                 (probe-file "/etc/certs/fullchain.pem"))))

   (list '*static-home* static-home
         #'(lambda()
             (or (and glisp:*gendl-source-home*
                      (probe-file (merge-pathnames "gwl/static/"
                                                   glisp:*gendl-source-home*)))
                 (and glisp:*gdl-home*
                      (probe-file (merge-pathnames "static/"
                                                   glisp:*gdl-home*)))

                 (and glisp:*gdl-program-home*
                      (probe-file (merge-pathnames "static/"
                                                   glisp:*gdl-program-home*)))

                 (warn "~%Static home not found in source directory or parent of program directory.~%"))))))


(defvar *settings* (settings))

(defun reset-settings () (setq *settings* (settings)))

;;
;; FLAG -- move publishes into global param with the basic publishing data and
;;         call general-purpose publish function on this data.
;;    also detect changes in the publishing to include in return value.
;;
(defun initialize ()
  ;;
  ;; FLAG -- investigate if this initialize-multiprocessing business
  ;; is still needed, currently LW-only.
  ;;

  (and (uiop:getenv "HTTP_PORT") (setq *http-port* (parse-integer (uiop:getenv "HTTP_PORT"))))
  (and (uiop:getenv "HTTPS_PORT") (setq *https-port* (parse-integer (uiop:getenv "HTTPS_PORT"))))


  (glisp:initialize-multiprocessing)

  (setq *iid-random-state* (make-random-state t))

  (let (anything-changed?)
    (setq anything-changed? (glisp:set-settings *settings*))
    (start-gwl)
    anything-changed?))

(defun renew-wserver (&key (global-var-syms '(*http-server* *https-server*)))
  (when (find-package :zacl)
    (setq excl:*initial-terminal-io* *terminal-io*)
    (let ((enable-compression? (and (member :zlib-deflate *features*) t)))
      (dolist (global-var-sym global-var-syms)
        (unless (and (symbol-value global-var-sym)
                     (server-port :server (symbol-value global-var-sym)))
          (let ((new-wserver
                  (make-instance 'net.aserve:wserver
                                 :enable-compression enable-compression?)))
            (when (and (boundp global-var-sym) (symbol-value global-var-sym))
              (setf (net.aserve:wserver-locators new-wserver)
                    (net.aserve:wserver-locators (symbol-value global-var-sym))))
            (setf (symbol-value global-var-sym) new-wserver)))))))

;;
;; FLAG -- get platform-specific stuff into glisp package.
;;

#+(and ccl windows-target)
(in-package :ccl)


#-(and ccl windows-target)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *set-_?* nil)
  (defparameter *set-$?* nil)
  (unless (get-dispatch-macro-character #\# #\_)
    (setq *set-_?* t)
    (set-dispatch-macro-character #\# #\_ #'(lambda(s subchar arg) (declare (ignore s subchar arg))nil)))
  (unless (get-dispatch-macro-character #\# #\$)
    (setq *set-$?* t)
    (set-dispatch-macro-character #\# #\$ #'(lambda(s subchar arg) (declare (ignore s subchar arg)) nil))))

#+(and ccl windows-target)
(let (*warn-if-redefine-kernel*)
  (defun %windows-sleep (millis)

    #+nil
    (unless (typep millis 'unsigned-byte 32)
      (setq millis (coerce 1000000000 '(unsigned-byte 32))))

    (do ((new-value (round (/ millis 10)) (round (/ millis 10))))
        ((typep millis '(unsigned-byte 32)))
      (setq millis new-value))


    (do* ((start (floor (get-internal-real-time)
                        (floor internal-time-units-per-second 1000))
                 (floor (get-internal-real-time)
                        (floor internal-time-units-per-second 1000)))
          (millis millis (- stop start))
          (stop (+ start millis)))
         ((or (<= millis 0)
              (not (eql (#_SleepEx millis #$true) #$WAIT_IO_COMPLETION)))))))

#-(and ccl windows-target)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when *set-_?* (set-dispatch-macro-character #\# #\_ nil))
  (when *set-$?* (set-dispatch-macro-character #\# #\$ nil)))



(in-package :net.aserve)
(unless (find "wasm" net.aserve::*file-type-to-mime-type*
	      :test #'string-equal :key #'second)
  (setq *file-type-to-mime-type*
	(append *file-type-to-mime-type* 
		`(("application/wasm" "wasm"))))
  (setq *mime-types* nil)
  (build-mime-types-table))
