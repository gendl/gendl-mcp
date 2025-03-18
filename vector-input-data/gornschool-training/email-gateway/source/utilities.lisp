(in-package :email-gateway)

(defvar *user-hash* (make-hash-table :size 100 :test 'equalp))
(defvar *user-hash-path* nil)
(defvar *robot-log-path* nil)

(defun add-user (email-address session-id)
  (setf (gethash email-address *user-hash*) session-id)
  (let ((filename *user-hash-path*))
    (with-open-file (out filename :direction :output :if-exists :supersede :if-does-not-exist :create)
      (print (list-hash *user-hash*) out))))

(defun retrieve-object (email-address)
  (let ((instance-id  (gethash email-address *user-hash*)))
    (when instance-id
      ;;
      ;; FLAG Applicaton name  bleeding through the authemtication process here. Needs to but is there a better way to do this?
      ;;
      (let ((object (read-snapshot :filename (merge-pathnames (format nil "~a.snap" instance-id) gwl::*snap-home*))))
        object))))


(defun restore-saved-users  ()
  (let ((filename *user-hash-path*))
    (when (probe-file filename)
      (with-open-file (in filename)
        (let ((user-plist (read in))) ;; FLAG -- change strategy if more than 1,0000 users
          (mapc #'(lambda(key val) (setf (gethash key *user-hash*) val))
                (plist-keys user-plist) (plist-values user-plist)))))))

(defun set-user-hash-path ()
  (setq *user-hash-path*
        (ensure-directories-exist
         (merge-pathnames "user-hash.sexp" gwl::*snap-home*))))

(defun set-robot-log-path ()
  (setq *robot-log-path*
        (ensure-directories-exist
         (merge-pathnames "robot-log.sexps" gwl::*snap-home*))))


;;
;; FLAG Applicaton name  bleeding through the authemtication process here. Needs to but is there a better way to do this?
;;
(pushnew 'set-user-hash-path training-common:*initializers*)
(pushnew 'restore-saved-users training-common:*initializers*)
(pushnew 'set-robot-log-path training-common:*initializers*)
