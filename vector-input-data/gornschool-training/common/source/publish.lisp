(in-package :training-common)

(defparameter *publish-prefix* "common")
(defparameter *initializers* nil)


(defparameter *home*
  (merge-pathnames "../../" (make-pathname :name nil :type nil :defaults (glisp:source-pathname))))


(defun set-snap-home (user-name)
  (setq gwl::*snap-home*
        (ensure-directories-exist
         (merge-pathnames "tutorial-sessions/"
                          (if user-name
                              #+linux
                              (merge-pathnames (format nil "~a/" user-name) "/home/")
                              #-linux
                              (user-homedir-pathname)
                              (user-homedir-pathname))))))


(defun initialize ()


  (setq gwl:*bypass-security-check?* t)

  (let ((user-name (when (glisp:featurep :linux) (gwl::set-user-name)))) (set-snap-home user-name))

  (unless (probe-file *home*) (error "Common home not found at ~A,
please arrange to have it set properly before calling ~s." *home* 'initialize))


  (let ((hostname (uiop:hostname))
        (https-server *https-server*)
        (http-server *http-server*))

 
    (unless http-server (error "No *http-server* found in training-common::initialize.
You may want to do `(gdl::initialize)` or `(gdl:start-gdl!)` before loading this code.~%"))
    
    
    (gwl::publish-http-catchall :server *http-server*)
    
    (unless https-server (error "No *https-server* found in training-common::initialize.
You may want to do `(gdl::initialize)` or `(gdl:start-gdl!)` before loading this code.~%"))


    (setq *start-https?* t)(setq *start-http?* t)
    
    (dolist (host (if (string-equal hostname "gornschool.com")
                      (list hostname (format nil "www.~a" hostname)
                            "localhost" "127.0.0.1"
                            "school.genworks.com" "school.gen.works")
                      (list nil)))
      
      (let ((server https-server)) ;;with-all-servers (server)

        
        
        ;; a cute favicon (need to get it designed).
        (publish-file :path "/gorn.png" :server server :host host  :content-type "image/png"
                      :file (namestring
                             (merge-pathnames "images/gorn-icon.png" *home*)))

        ;; the email gateway
        (publish-gwl-app "/" "email-gateway:assembly" :server server :host host)

        ;;
        ;; site-specific GWL config:
        ;;

        (gwl::publish-sessions-catchall :server server :host host :snap-home gwl::*snap-home*)


        ;;
        ;; And now the Application-level publishings
        ;;
        (publish-gwl-app "/gendl-self-start-backdoor" "training-home:assembly" :server server :host host)

        (publish-shared "training-home:assembly" :path "/shared" :server server :host host)

        (publish-file :server server :host host
                      :path "/css/training-style.css"
                      :file (namestring (merge-pathnames "css/training-style.css" *home*)))


        (let ((common-images (merge-pathnames "images/" *home*)))
          (print-variables common-images)
          (publish-directory :server server :host host
                             :prefix "/common-images/"
                             :destination (namestring common-images)))

        (mapc #'(lambda (publish-prefix object-type-string)
                  (format t "** Publishing an App as follows:~%~%")
                  (let ((path (format nil "/~a-backdoor" publish-prefix)))
                    (print-variables server host path)
                    (publish-gwl-app  path object-type-string :server server :host host))
                  ;;
                  ;; FLAG -- the below function is a mess because of mismatches between stored file paths and published prefixes. Fix it.
                  ;;
                  (dolist (subdir (list "images" "resources" "resources/images"))
                    (let ((path (merge-pathnames (format nil "~a/~a/~a" publish-prefix subdir (if (string-equal subdir "resources") "source/" "")) *home*)))
                      (when (probe-file path)
                        (let ((prefix (if (string-equal subdir "/resources/images") "resource-images"
                                          (format nil "/~a-~a" publish-prefix subdir))))

                          (format t "** Publishing a Directory as follows:~%~%")
                          (print-variables prefix (namestring path))
                          (publish-directory :prefix prefix
                                             :destination (namestring path)
                                             :server server :host host))))))
              (list "home" "t1" "t2" "t3" "t4" "t5" "t6")
              (list "training-home:assembly" "training-1:assembly" "training-2:assembly" "training-3:assembly"
                    "training-4:assembly" "training-5:assembly" "training-6:assembly")))))

  (force-output))



(pushnew 'initialize *initializers*)

(defun initialize-all () (mapc #'funcall (reverse *initializers*)))  ;; reverse is important because the above body must run first to set up e.g. *snap-home*.


;; (initialize)
