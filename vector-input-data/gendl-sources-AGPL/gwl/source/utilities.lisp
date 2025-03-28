(in-package :gwl)

(defparameter *always-error-for-with-error-handling?* nil)
  
(eval-when (:compile-toplevel :load-toplevel :execute)
  (#+allegro 
   excl:without-redefinition-warnings 
   #-allegro progn
	   
   (defmacro with-error-handling ((&key (error? nil)
					(timeout 2) 
					(timeout-body 
					 `(warn "Timed Out after ~a Seconds" ,timeout))) 
				  &body body)
     "[Macro]. Wraps the <b>body</b> of code with error-trapping and system timeout. 
A warning is given if an error condition occurs with <b>body</b>. 

:&key ((timeout 2) \"Timeout in Seconds.\"
          timeout-body \"Body of code to evaluate if timeout occurs. 
                         Default is to print a warning and return nil.\")

:&rest (body \"Body of code to be wrapped\")"
  
     (if error? `(progn ,@body)
	 (let ((values (gensym)) (error (gensym)))
	   (let ((code `(if *always-error-for-with-error-handling?*
                            (progn ,@body)
                            (let* ((,values (multiple-value-list (ignore-errors ,@body)))
			           (,error (second ,values)))
			      (if (and ,error (typep ,error 'error))
			          (progn (warn "~a" ,error)
				         (values nil ,error))
			          (apply #'values ,values))))))
	     (if timeout 
		 `(,(glisp:with-timeout-sym) (,timeout ,timeout-body) ,code)
		 code)))))))

(defun server-port (&key (server *http-server*))
  (when server
    (let ((socket (net.aserve:wserver-socket server)))
      (when socket
        (glisp:local-port socket)))))

(defun announce-server-port ()
  (let ((http-port (server-port :server *http-server*))
        (http-host-port (let ((value (uiop:getenv "HTTP_HOST_PORT")))
                          (when value (parse-integer value))))
        (https-port (server-port :server *https-server*))
        (https-host-port (let ((value (uiop:getenv "HTTPS_HOST_PORT")))
                           (when value (parse-integer value))))
        (swank-port (when (find-package :swank)
                      (or
                       (first (mapcar (lambda (s) (funcall (read-from-string "swank::local-port") (first s)))
                                      (symbol-value (read-from-string "swank::*servers*"))))
                       ;;(uiop:getenv "SWANK_PORT") 4200
                       )))
        (swank-host-port (let ((value (uiop:getenv "SWANK_HOST_PORT")))
                           (when value (parse-integer value)))))

    (flet ((display-http-port ()
             (format t "~&~%***** ~%Your http server is listening on port ~a.~%~a"
                     http-port (if http-host-port
                                   (format nil "(MAPPED to --> ~a on your host)~%*****~%" http-host-port)
                                   (format nil "*****~%"))))
           (display-https-port ()
             (format t "~&~%***** ~%Your https server is listening on port ~a.~%~a"
                     https-port (if https-host-port
                                    (format nil "(MAPPED to --> ~a on your host)~%*****~%" https-host-port)
                                    (format nil "*****~%"))))

           (display-test-certs-warning ()
             (when (and *https-server*
                        (search "test-certs" (slot-value *https-server* 'net.aserve::ssl)))
               (warn "~&~%You have started the https server with the default built-in 
test certificates. Your browser will give scary warnings, which you can safely 
click past for local development.

For production deployment, you should obtain your own certificates, 
e.g. for free from LetsEncrypt.

You can preset your fullchain.pem and privkey.pem paths 
by setting gwl:*fullchain-pem-path* and gwl:*privkey-pem-path* 
to the appropriate paths before starting the https server.~&~%")))

           (display-swank-port ()
             (format t "~&~%***** ~%Your swank server is listening on port ~a.~%~a"
                     swank-port (if swank-host-port
                                    (format nil "(MAPPED to --> ~a on your host)~%*****~%" swank-host-port)
                                    (format nil "*****~%")))))

      (when http-port (display-http-port))
      (when https-port (display-https-port) (display-test-certs-warning))
      (when swank-port (display-swank-port)))

    (unless http-port (format t "~%You can start an http server with (gendl:start-gendl!)~%~%"))
    (unless https-port (format t "~%You can start an https server with (gendl:start-gendl! :https? t)~%~%"))
    (unless swank-port (when (find-package :swank)
                         (format t "~%You can start a swank server with (swank:create-server :port ~a :interface \"0.0.0.0\" :dont-close t)~%~%"
                                 swank-port)))
      
    (format t "~%NOTE: Your publishings should be done inside a (gwl:with-all-servers (server) ...)

e.g. (gwl:with-all-servers (server)
        (publish :path \"/\" :function #'your-response-func :server server))

"
            )))

(defun enable-debug-xmit (&key (debug-features '(:xmit-server-response-headers :xmit-server-response-headers
						 :xmit-server-request-command
						 :xmit-server-request-headers :xmit-server-request-body)))
  (dolist (feature debug-features) (net.aserve::debug-on feature)))
