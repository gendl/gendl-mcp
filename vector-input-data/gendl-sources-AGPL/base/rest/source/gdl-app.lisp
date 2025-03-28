#|

 - Set SBCL_HOME correctly in start script for made app.
 \/ Disable :deploy? for non-linux platforms for now.

|#

(in-package :gdl)

(defparameter *ci-info* nil)

(defparameter *restart-app-function* nil)
(defparameter *restart-init-function* nil)


(define-object gdl-app-scripts-mixin ()

  :input-slots
  (application-name
   (ci-info *ci-info*)
   (generate-scripts? nil)
   (deploy-target-toplevel "/opt/")

   (scripts (list (the start-script) (the safe-start-script) (the rc-dot-local)
                  (the shutdown-script) (the restart-script)))

   (os (uiop:operating-system))

   (start-script
    (case (the os)
      (:linux
       (list :contents (format nil "#!/bin/bash
# meant to be run as root -- process will setuid to normal user after webserver startup.

killall -q -9 ~a || true

kitchen=`dirname $0`

cd ${kitchen}/..
mkdir -p logs/

if [ -e ./sbcl/ ]; then
  export SBCL_HOME=\"$(realpath ./sbcl/)\"
fi

./~a -n >> logs/~a.log 2>&1
"
                               (the application-name)
                               (the application-name)
                               (the application-name))
             :name  (format nil "start~a" (case (getf (the ci-info) :branch)
                                            ((:master :main) "")
                                            (:devo "-devo")
                                            (otherwise "-test")))))
      (otherwise (warn "start-script not yet implemented for ~a.~%" (the os)))))

   (rc-dot-local
    (case (the os)
      (:linux
       (list :contents (format nil "#!/bin/sh -e

# meant to be run as root e.g. at boot time.

kitchen=`dirname $0`

script=~a

${kitchen}/${script} &

exit 0
"
                               (getf (the safe-start-script) :name))
             :name "rc.local"))
      (otherwise (warn "start-script not yet implemented for ~a.~%" (the os)))))

   (restart-script
    (case (the os)
      (:linux (list :name "restart"
                    :contents (format nil "#!/bin/bash

sudo killall -q -9 ~a || true
"
                                      (the application-name))))
      (otherwise (warn "restart-script not yet implemented for ~a.~%" (the os)))))
                    
   (safe-start-script
    (case (the os)
      (:linux
       (list :contents (format nil "#!/bin/bash

# meant to be run as root

killall -q -9 ~a || true

kitchen=`dirname $0`

while [ 1 ]
do
  ${kitchen}/~a
done
"
                               (getf (the start-script) :name)
                               (getf (the start-script) :name))
             :name (format nil "safe-start~a" (case (getf (the ci-info) :branch)
                                                ((:master :main) "")
                                                (:devo "-devo")
                                                (otherwise "-test")))))
      (otherwise (warn "safe-start-script not yet implemented for ~a.~%" (the os)))))

                    
   (shutdown-script
    (case (the os)
      (:linux (list :name "shutdown"
                    :contents (format nil "#!/bin/bash

sudo killall -q -9 ~a || true
sudo killall -q -9 ~a || true
sudo killall -q -9 ~a || true
"
                                      (getf (the safe-start-script) :name)
                                      (getf (the start-script) :name)
                                      (the application-name))))
      (otherwise (warn "shutdown-script not yet implemented for ~a.~%" (the os)))))
                    
                    

   (deploy? nil))

  
  :functions
  ((generate-scripts!
    ()

    (format t "~&Generating scripts...~%")
    
    (cond ((and (the deploy?) (glisp:featurep :linux))
           (let ((target (merge-pathnames (string-append (the application-name) "/")
                                          (the deploy-target-toplevel))))
             (glisp:rsync (the destination-directory) target :options (list "z" "a" "v"))

             
             ;; Populate kitchen with scripts from in-memory:
             (let ((kitchen (ensure-directories-exist  (merge-pathnames "kitchen/" target))))
               (mapc #'(lambda(script)
                         (destructuring-bind (&key name contents) script
                           (with-open-file (out (merge-pathnames name kitchen)
                                                :direction :output
                                                :if-exists :supersede
                                                :if-does-not-exist :create)
                             (write-string contents out))))
                     (the scripts))
               (uiop:with-current-directory (kitchen) (uiop:run-program "chmod 755 *"))

               ;;     (Assume we are running build on deploy host as same user)
	       (uiop:with-current-directory (kitchen)
                 (when (probe-file (getf (the restart-script) :name))
		   (multiple-value-bind (output error result)
                       (uiop:run-program (format nil "./~a"  (getf (the restart-script) :name))
					 :ignore-error-status t)
		     (print-variables output error result)))))))

	  ((the deploy?)
           (warn "The :deploy? option is not implemented for non-Linux platforms at this juncture.

Please contact info@genworks.com and/or consult base/rest/source/gdl-app.lisp in the repository:

   https://gitlab.common-lisp.net/gendl/gendl

"))

          (t )))))



#-allegro
(define-object gdl-app (gdl-app-scripts-mixin)
  :documentation (:description "
This object serves as the driver for the build process for GDL runtime
applications. There is also an undocumented function called
<tt>make-gdl-app</tt>; in order to perform a runtime build process,
simply make an instance of this object with the appropriate input
values, and invoke <tt> (the make!)  </tt> on it, or call
<tt>make-gdl-app</tt> with the same arguments as the input-slot you
give to this object.
"

                               :examples "<pre>

 (in-package :gdl-user)

 (make-gdl-app :application-name \"moon-shot\" :destination-directory  \"/tmp/moon-shot/\"
               :overwrite? t :application-fasls (list \"/fasl-home/booster-rocket.fasl\"
                                                      \"/fasl-home/lunar-module.fasl\"))

</pre>
"

                               :author "Dave Cooper, Genworks International")



  :input-slots
  ((build-level :runtime)

   (application-class 'gendl-runtime-system)  ;; useful for CCL, have to port to SBCL and others.

   ("String. The name which will be used for your
                 application's executable and possibly image file. Defaults to
                 \"gdl-test-runtime\"."  application-name "gdl-test-runtime")


   (application-source-home nil) (static-folders nil)
   

   

   ("Lisp Function of zero arguments.
This function will be run in the initiating image before the build is begun."
    pre-make-function nil)


   ("Lisp Function of zero arguments.
This function will be run in the initiating image after the build is complete."
    post-make-function nil)

   ("Lisp expression. This form will be evaluated in the image being
       built, before the loading of application-fasls begins, but
       after the GDL runtime modules are loaded.  Defaults to nil."
    pre-load-form nil)

   ("Lisp expression. This form will be evaluated in the image being
       built, after the loading of application-fasls is complete.
       Defaults to nil."
    post-load-form nil)


   ("Boolean. Indicates whether a build will overwrite a previously
       existing destination directory. Defaults to nil."
    overwrite? nil)

   ("Function, t, or nil. Validates the target of overwrite? before
    deleting. T is unconditional \"yes\" - use at your own risk."
    overwrite-validation-function nil)

   ("Pathname. Indicates the directory to be created or overwritten
       for producing the runtime distribution.  Defaults to a
       directory called <tt>(the application-name)</tt> in the system
       temporary directory, returned by
       <tt>(glisp:temporary-folder)</tt>."
    destination-directory
    (merge-pathnames
     (make-pathname :directory (list :relative (the application-name)))
     (glisp:temporary-folder)))


   (display-string (string-capitalize (the application-name)))

   ("Lambda expression with empty argument list or symbol naming a
       function with no arguments. This will be run when the runtime
       application starts up. The alternative to using this to achieve
       initializations is to put expressions in a gdlinit.cl or
       .gdlinit.cl in the application directory or user home
       directory. Defaults to nil."  restart-init-function *restart-init-function*)

   ("Lambda expression with empty argument list or symbol naming a
       function with no arguments. This will be run when the runtime
       application starts up. The alternative to using this to achieve
       initializations is to put expressions in a gdlinit.cl or
       .gdlinit.cl in the application directory or user home
       directory. Defaults to nil."  restart-app-function *restart-app-function*)


   ("List of pathnames. This list should contain the pre-compiled
       fasls for your GDL application, in correct load order. These
       can be produced, for example, by calling
       <tt>genworks:cl-lite</tt> with the <tt>:create-fasl?</tt>
       keyword argument set to <tt>t</tt>. If you are using the ASDF
       build management system, note that ASDF3 is now capable of
       producing a single fasl file for your application including its
       ASDF/Quicklisp dependencies, using <pre>
         (asdf:operate 'asdf:monolithic-compile-bundle-op :your-application-system-name)
         (asdf:output-file 'asdf:monolithic-compile-bundle-op :your-application-system-name)
    </pre>

               See the ASDF documentation for details. "
    application-fasls nil)


   ;;
   ;; FLAG -- implement this.
   ;;
   ("Number. The size of the reserved space which will be requested
       from the OS when the produced application starts up.  Defaults
       to 800000000 (eight hundred million) bytes."  lisp-heap-size
       800000000)

   ("String. The contents of this string will be copied to a file
       gdlinit.cl and placed in the destination-directory. Default is
       empty string."
    gdlinit-content "")


   (swank-port (case (getf (the ci-info) :branch)
                 ((:master :main) 1088)
                 (:devo 10088)
                 (otherwise 10098))))

  :computed-slots
  ((ci-info (let ((branch (uiop:getenv "CI_COMMIT_BRANCH"))
                  (sha (uiop:getenv "CI_COMMIT_SHA")))
              (list :branch (and branch (make-keyword branch))
                    :sha (and sha (make-keyword sha)))))


   (destination-exe (merge-pathnames (format nil "~a~a"
                                             (the application-name)
                                             #+windows ".exe" #-windows "")
                                     (the destination-directory)))


   (parent-directory (merge-pathnames "../" (the destination-directory)))

   (load-fasls-expression (when (the application-fasls)
                            `(mapcar #'load ',(the application-fasls))))


   (static-folder-paths
    (and (the application-source-home)
         (the static-folders)
         (probe-file (the application-source-home))
         (mapcar #'(lambda(subdir)
                     (let ((path (merge-pathnames (format nil "~a/" subdir)
                                                  (the application-source-home))))
                       (or (probe-file path)
                           (error "~s does not exist.~%" path))))(the static-folders))))
   

   (save-application-and-die-form
    (let ((branch (getf (the ci-info) :branch))
          (sha (getf (the ci-info) :sha))
          (application-key (make-keyword (the application-name))))

      #-(or ccl sbcl)
      (error "Please implement save-application-and-die-form for ~a.~%"
             (lisp-implementation-type))

      `(progn
         (gendl::deinitialize)
         ,(when branch
            `(setf (getf (getf *ci-info* ,application-key) :branch) ,branch
                   (getf (getf *ci-info* ,application-key) :sha) ,sha))

         ;;(setq *restart-init-function* ,(the restart-init-function))
         ,(when (the restart-init-function)
            `(setf (symbol-function '*restart-init-function*) ,(the restart-init-function)))

         ;;
         ;; FLAG -- if destination-directory already exists, and no
         ;; validation specified, error with continuation to delete.
         ;;


         #+sbcl (require :sb-posix)
         #+sbcl (require :sb-bsd-sockets)
         #+sbcl (require :sb-cltl2)
         #+sbcl (require :sb-rotate-byte)

         #+sbcl (glisp::set-init-hooks)
         #+sbcl (sb-ext:save-lisp-and-die (progn (ensure-directories-exist ,(the destination-exe))
                                                 ,(the destination-exe))
                                          :executable t)

         #+ccl
         (ccl:save-application (progn (ensure-directories-exist ,(the destination-exe))
                                      ,(the destination-exe))
                               :prepend-kernel t

                               ;;:clear-clos-caches t
                               :purify t
                               :application-class ',(the application-class)
                               :toplevel-function ,(the toplevel-function))
         #+ccl (ccl:quit))))


   (toplevel-function (when (the restart-app-function)
                        `(lambda()
                           (gendl::initialize :startup-banner? t)
                           ,(when (the restart-init-function)
                              `(funcall #',(the restart-init-function)))
                           (funcall #',(the restart-app-function)))))

   (exe-path (first (glisp:basic-command-line-arguments)))

   (exe-dir (make-pathname :name nil :type nil :defaults (the exe-path))))

  :functions
  ((copy-with-warning
    (source target &key (overwrite? t))
    (if (not (and source (probe-file source)))
        (warn "Could not find ~a.~%Not copying to ~a~%" source target)
        (progn
          (when (and overwrite? (probe-file target))
            (uiop:delete-directory-tree
             target :validate #'(lambda(dir)
                                  (search (namestring (the destination-directory))
                                          (namestring dir)))))
          (glisp:copy-directory source target))))

   (populate-static-folders
    ()
    (ensure-directories-exist (the destination-directory))

    (when (and (the gdlinit-content) (not (zerop (length (the gdlinit-content)))))
      (with-open-file (out (merge-pathnames "gdlinit.cl" (the destination-directory))
                           :direction :output :if-exists :supersede :if-does-not-exist :create)
        (write-string (the gdlinit-content) out)))


    (when (member (the build-level) (list :pro :runtime :gwl-graphics :gwl))
      (the (copy-with-warning (merge-pathnames "geysr-static/" glisp:*gdl-home*)
                              (merge-pathnames "geysr-static/" (the destination-directory))))
      (the (copy-with-warning (merge-pathnames "static/" glisp:*gdl-home*)
                              (merge-pathnames "static/" (the destination-directory)))))


    (when (member (the build-level) (list :pro :runtime :gwl-graphics :gwl :geom-base))
      (the (copy-with-warning (or (probe-file (merge-pathnames "test-certs/" glisp:*gdl-home*))
                                  (when (find-package :asdf)
                                    (probe-file (merge-pathnames "test-certs/"
                                                                 (glisp:system-home :zacl)))))
                              (merge-pathnames "test-certs/" (the destination-directory))))
      
      (the (copy-with-warning (or (probe-file (merge-pathnames "afm/" glisp:*gdl-home*))
                                  (when (find-package :asdf)
                                    (probe-file (merge-pathnames "afm/"
                                                                 (glisp:system-home :cl-pdf)))))
                              (merge-pathnames "afm/" (the destination-directory))))

      (the (copy-with-warning (or (probe-file (merge-pathnames "hyphen-patterns/" glisp:*gdl-home*))
                                  (when (find-package :asdf)
                                    (probe-file
                                     (merge-pathnames "hyphen-patterns/"
                                                      (glisp:system-home :cl-typesetting)))))
                              (merge-pathnames "hyphen-patterns/" (the destination-directory)))))


    #+sbcl
    (the (copy-with-warning (merge-pathnames "sbcl/" (the exe-dir))
                            (merge-pathnames "sbcl/" (the destination-directory))))

    #+ccl
    (dolist (dir (list "library/" "x86-headers64/" "win64-headers/" "tools/"))
      (let ((source (merge-pathnames dir (the exe-dir))))
        (when (probe-file source)
          (the (copy-with-warning source (merge-pathnames dir (the destination-directory)))))))

    (dolist (static-folder (the static-folder-paths))
      (let* ((name (lastcar (pathname-directory static-folder)))
             (target-subdir (merge-pathnames (string-append name "/") target)))
        (glisp:rsync static-folder target-subdir
                     :options (list "z" "a" "d"))
        (when (search "patch" (namestring target-subdir))
          (uiop:run-program (format nil "chmod 770 ~a" target-subdir))))))


   ("Void. Does the application build and creates or replaces
  <tt>(the destination-directory)</tt> ."
    make!
    ()

    (when (the pre-make-function) (funcall (the pre-make-function)))

    (let ((load-file (merge-pathnames (format nil "~a-load.lisp" (gensym)) (glisp:temporary-folder))))
      (with-open-file (out load-file :direction :output :if-exists :supersede
                           :if-does-not-exist :create)
        (when (the pre-load-form) (print (the pre-load-form) out))
        (when (the load-fasls-expression) (print (the load-fasls-expression) out))
        (when (the post-load-form) (print (the post-load-form) out))
        (print (the save-application-and-die-form) out))


      (cond ((and (probe-file (the destination-directory)) (not (the overwrite?)))
             (restart-case
                 (error "~&The destination-directory, ~a, exists, and you
 have not specified `:overwrite? t` in your call to make-gdl-app.~%"
                        (the destination-directory))

               (do-nothing () (return-from :make! nil))
               (delete-anyway ()
                 :report "Delete the destination-directory and continue.
 (CAUTION: use this restart at your own risk.)"
                 (uiop:delete-directory-tree (probe-file (the destination-directory))
                                             :validate t))))

            ((probe-file (the destination-directory))
             (uiop:delete-directory-tree (probe-file (the destination-directory))
                                         :validate (the overwrite-validation-function))))

      ;;
      ;; Now fire up the appropriate executable with argument to load the above
      ;; load-file
      ;;
      (let ((rest-args #+ccl
                       (list "-n" "--batch" "-l" (namestring load-file) "-e" "(ccl:quit)")
                       #+sbcl
                       (list "--no-sysinit" "--no-userinit" "--load" (namestring load-file)
                             "--eval" "(ccl:quit)")
                       #-(or ccl sbcl) (error "Please implement rest-args for ~a"
                                              (lisp-implementation-type))))
        (multiple-value-bind
              (output error return-code)
            (uiop:with-current-directory ((the exe-dir))
              (uiop:run-program
               (cons (string-append "./" (file-namestring (the exe-path))) rest-args)
               :ignore-error-status t
               :error-output :string
               :output nil))
          (declare (ignore output))
          (unless (zerop return-code) (error error)))


        (the populate-static-folders))

      (when (the generate-scripts?) (the generate-scripts!))
      
      (when (the post-make-function) (funcall (the post-make-function)))))))

#-allegro
(defun make-gdl-app (&rest args)
    "Void. This function simply passes its arguments to an instance of gdl-app. Please
see that object definition for the documentation of the inputs."
  (the-object (apply #'make-object 'gdl-app args) make!))


(defun load-swank ()

  (flet
      ((get-swank-loader-path ()
         (let ((ql-home
                 (when glisp:*gendl-home*
                   (probe-file (merge-pathnames "quicklisp/" glisp:*gendl-home*)))))
           (unless ql-home (error "glisp:*gendl-home* not set or not set correctly.~%"))
           (let* ((swank-path-path
                    (probe-file
                     (merge-pathnames "dists/quicklisp/installed/systems/swank.txt" ql-home)))
                  (swank-asd-path
                    (and swank-path-path
                         (with-open-file (in swank-path-path) (read-line in)))))
             (and swank-asd-path
                  (merge-pathnames "swank-loader.lisp"
                                   (merge-pathnames swank-asd-path ql-home)))))))
    (let ((swank-loader-path (get-swank-loader-path)))
      (load swank-loader-path)
      (funcall (read-from-string "swank-loader:init") :setup t :load-contribs t :quiet t)
      (setf (symbol-value (read-from-string "swank-loader:*source-directory*"))
            (make-pathname :name nil :type nil :defaults swank-loader-path))
      (setf (symbol-value (read-from-string "swank-loader:*fasl-directory*"))
            (funcall (read-from-string "swank-loader::default-fasl-dir")))
      (setf (symbol-value (read-from-string "swank::*load-path*"))
            (list
             (funcall (read-from-string "swank-loader::contrib-dir")
                      (symbol-value (read-from-string "swank-loader:*source-directory*")))))
      (setf (symbol-value (read-from-string "swank-loader:*started-from-emacs*")) nil))


    (funcall (read-from-string "swank:swank-require")
             (read-from-string
              "(swank-io-package::swank-indentation
                swank-io-package::swank-trace-dialog
                swank-io-package::swank-package-fu
                swank-io-package::swank-presentations
                swank-io-package::swank-presentations
                swank-io-package::swank-macrostep
                swank-io-package::swank-fuzzy
                swank-io-package::swank-fancy-inspector
                swank-io-package::swank-c-p-c
                swank-io-package::swank-arglists
                swank-io-package::swank-repl)"))

    (setf (symbol-value (read-from-string "swank-loader::*fasl-directory*")) nil
          (symbol-value (read-from-string "swank-loader::*source-directory*")) nil
          (symbol-value (read-from-string "swank::*load-path*")) nil)))


    



      
