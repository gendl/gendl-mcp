(in-package :gdl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :ql) (load-quicklisp)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *training-home*
    (make-pathname :name nil :type nil :defaults (glisp:source-pathname)))
  ;;(unless (find-package :ql) (load-quicklisp))
  (pushnew (probe-file *training-home*) ql:*local-project-directories* :test #'equalp))
  

(defparameter *ci-commit-branch* (let ((branch (or (uiop:getenv "CI_COMMIT_BRANCH")
                                                   (uiop:run-program "git branch --show-current"
                                                                     :output :string
                                                                     :error :string
                                                                     :ignore-error-status t))))
                                   (if branch (make-keyword branch) :unspecified)))

(defparameter *ci-commit-sha* (uiop:getenv "CI_COMMIT_SHA"))

(defvar *ci-info* (list :branch *ci-commit-branch* :sha *ci-commit-sha*))


(define-object training (gdl-app)

  :input-slots
  ((deploy? nil))


  :computed-slots
  ((application-name (case *ci-commit-branch*
                       (:master "training")
                       (:devo "training-devo")
                       (otherwise "training-test")))

   (target-host "gornschool.com")

   ;;
   ;; FLAG Much of what's below consider to become generic for all
   ;;      deployments. extract "training" anything from below.
   ;;

   (fullchain-pem (format nil "/etc/keys/~a/fullchain.pem" (the target-host)))
   (privkey-pem (format nil "/etc/keys/~a/privkey.pem" (the target-host)))

   (build-level :gwl-graphics)

   (generate-scripts? t)   

   (overwrite? t)

   (overwrite-validation-function #'(lambda(pathname)
                                      (search (the application-name)
                                              (namestring pathname))))

   (pre-make-function (lambda()
                        (ql:quickload :training)
                        (ql:quickload :cl-smtp)))

   (post-load-form (progn

		     (print-variables (the fullchain-pem) (the privkey-pem))
		     
		     (remove
                      nil
                      `(progn
			 ,(when *ci-commit-branch*
                            `(setf (symbol-value (read-from-string "training-home::*ci-commit-branch*"))
                                   ,*ci-commit-branch*))
			 ,(when *ci-commit-sha*
                            `(setf (symbol-value (read-from-string "training-home::*ci-commit-shea*"))
                                   ,*ci-commit-sha*))))))


   (application-fasls
    (mapcar #'(lambda(system)
                (asdf:operate 'asdf:monolithic-compile-bundle-op system)
                (asdf:output-file 'asdf:monolithic-compile-bundle-op system))
            (list :training :cl-smtp)))

   (http-port (case *ci-commit-branch*
                ((:master :main) 80)
                (:devo 9080)
                (otherwise 9090)))

   (https-port (let ((branch *ci-commit-branch*))
		 (print-variables branch)
		 (let ((port
			 (case branch
			   ((:master :main) 443)
			   (:devo 9443)
			   (otherwise 9443))))
		   (print-variables port)
		   port)))

   (swank-port (case *ci-commit-branch*
                 ((:master :main) 1088)
                 (:devo 10088)
                 (otherwise 10098)))

   (restart-init-function

    (progn

      (format t "Building restart-init-function~%")
      (print-variables (the privkey-pem) (the fullchain-pem)
		       (the http-port) (the https-port) (the swank-port))
    
      `(lambda()

	 (format t "Now Starting training app...~%")

	 (setq gwl:*privkey-pem-path* ,(the privkey-pem))
	 (setq gwl:*fullchain-pem-path* ,(the fullchain-pem))

	 (gwl::reset-settings)

	 (print-variables gwl:*privkey-pem-path* gwl:*fullchain-pem-path*)

	 (if (and gwl:*privkey-pem-path*
                  (probe-file gwl:*privkey-pem-path*)
                  gwl:*fullchain-pem-path*
                  (probe-file gwl::*fullchain-pem-path*))
	     (progn
	       (setq gwl:*http-port* ,(the http-port)
		     gwl:*start-https?* t
		     gwl:*https-port* ,(the https-port))
	       (gendl:start-gendl!))
             (warn "Cannot find SSL certificate files: ~a and/or ~a"
                   gwl::*privkey-pem-path* gwl::*fullchain-pem-path*))

	 ;; #+zacl (setq cl+ssl:*default-cipher-list* "@SECLEVEL=0:kEECDH+ECDSA:kEECDH:kEDH:HIGH:MEDIUM:+3DES:+SHA:!RC4:!aNULL:!eNULL:!LOW:!MD5:!EXP")

	 ;;
	 ;; Demote this service  process to a normal user
	 ;;
	 (when (zerop (glisp:getuid))
           (format t "~&~%Demoting process to normal user ...~%")
           ;;
           ;; FLAG -- figure out how to not hardcode the gid/uid below.
           ;;
           (glisp:setgid 997) (glisp:setuid 1000))
	 
	 ;;
	 ;; Try to start swank listener after user is set correctly.
	 ;;
	 ;; FLAG -- may have to open permissions on /root/.slime/ because
	 ;; setuid does not change the effective home directory. Figure
	 ;; out how to change effective home directory.
	 ;;
	 (swank:create-server :port ,(the swank-port) :dont-close t)

	 ;;
	 ;; Set the main asset home directory then call all initializers in order.
	 ;;
	 (setf (symbol-value (read-from-string "training-common:*home*"))
	       (merge-pathnames "training-source/" (glisp:executable-homedir-pathname)))

         ;;
         ;; Application-level init
         ;;
	 (funcall (read-from-string "training-common:initialize-all"))
	 ;;
	 ;; FLAG -- extract this to another repo and load quiz as a  module only if needed.
	 ;;
	 (when (find-package :quiz)
           (setf (symbol-value (read-from-string "quiz:*vocab-pathname*"))
		 (merge-pathnames "sanskrit/vocab/" (symbol-value (read-from-string "training-common:*home*"))))
           (funcall (read-from-string "quiz:publish-endpoints")))

	 (setq gwl:*developing?* ,(case *ci-commit-branch*
				    ((:master :main :devo) nil)
				    (otherwise t)))

	 (format t "~&~%Sleeping the main thread to become a webserver...~%")

	 (force-output)

	 (do () () (sleep 10000)))))))




(defun training-app (&key deploy?)
  (let* ((self (make-object 'training :deploy? deploy?))
         ;;(*default-pathname-defaults* (the destination-directory))
         )
    (the make!)))


;; backups of DNS configuration entries:
#|

TXT record

default._domainkey

v=DKIM1; h=sha256; k=rsa; p=MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAhRL3xx1EyaUJjUYSaqSUC+FwIFOX84VPq7X/Y2Fcp3vq5XjdBNE4OUW/XNklLbzQ+ms6E7v3D9EbgDoVKXePba07QbeDL+0afv+YHm14IJZYlrpTniQLhsAHKxfKmlDhgEDLqHfIr0p+Pl8sXX6UPApHbTzr8Uo3z7RXLZSi/wnn0vkc5lYrvlUzuAsPuqapQwzebPnCyh/0BEWTJY5u+43vNtqEj/902NLED9cdFCBGOTTCm9v+Nuo6hMTWyO3HAXGt4UThFj4tWapBW7RVmb17QlZLbgmXZeJDTInecYPmiPIrkBTbLjzrA4100JXtpd5n/ZLl+qtrgwtqtsLBXQIDAQAB0;173;54M0;173;54m

Automatic


|#
