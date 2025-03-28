
(in-package :gdl-user)

;; some functions supporting the other data structures section
(defun make-list-of-integers (n)
  (let* ((result))
    (dotimes (i (+ n 1))
       (setq result (push i result)))
     (cdr (reverse result))))
 

(defun make-array-of-integers (n)
  (make-array n 
	      :initial-contents (make-list-of-integers n)
	      ))

(defun make-hash-table-of-integers (n)
  (let* ((my-table (make-hash-table)))
    (dotimes (i n)
      (setf (gethash i my-table) i))
    my-table))

(defun make-large-plist (n)
  (let* ((result))
    (dotimes (i (+ n 1))
      (let ((key (make-keyword (format nil "~a" i))))
	(setq result (push key result))
	(setq result (push i result))
	)) 
    (cddr (reverse result))))

(defun make-large-hash-table (n)
  (let* ((my-table (make-hash-table)))
    (dotimes (i (+ n 1))
      (let ((key (make-keyword (format nil "~a" i))))
	(setf (gethash key my-table) i)))
    my-table))


(defun run-test (n func)
  (time
   (dotimes (i n)  ;; do it enough times to register on the timer
     (funcall func))))


;;; ************************************************
;;; Some useful hash table functions
;;; ************************************************

(defun find-hashtable-keys (my-hash-table)
  (let ((key-lst nil)) 
    (maphash #'(lambda (k v) 
		 (setf  key-lst (cons k key-lst)))
	     my-hash-table)
    key-lst))

(defun find-hashtable-values (my-hash-table)
  (let ((key-lst nil)) 
    (maphash #'(lambda (k v) 
		 (setf  key-lst (cons v key-lst)))
	     my-hash-table)
    key-lst))

(defun convert-hashtable-into-plist (my-hash-table)
  (let ((key-lst nil)) 
    (maphash #'(lambda (k v) 
		 (setf key-lst (cons (list k v) key-lst)))
	     my-hash-table)
    (apply #'append key-lst)))


;;GDL-USER> (setq large-list (make-list-of-integers 1000000))
;;(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 ...)
;;GDL-USER> (setq large-array (make-array-of-integers 1000000))
;;#(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 ...)
;;GDL-USER> (run-test 100 #'(lambda () (nth 999999 large-list)))
;Compiler warnings :
;   In an anonymous lambda form: Undeclared free variable LARGE-LIST
;;(DOTIMES (I N) (FUNCALL FUNC))
;;took 1,037,000 microseconds (1.037000 seconds) to run.
;;During that period, and with 4 available CPU cores,
;;     1,031,250 microseconds (1.031250 seconds) were spent in user mode
;;             0 microseconds (0.000000 seconds) were spent in system mode
;; 64 bytes of memory allocated.
;;NIL
;;GDL-USER> (run-test 100 #'(lambda () (aref large-array 999999)))
;Compiler warnings :
;   In an anonymous lambda form: Undeclared free variable LARGE-ARRAY
;;(DOTIMES (I N) (FUNCALL FUNC))
;;took 0 microseconds (0.000000 seconds) to run.
;;During that period, and with 4 available CPU cores,
;;     0 microseconds (0.000000 seconds) were spent in user mode
;;     0 microseconds (0.000000 seconds) were spent in system mode
;; 64 bytes of memory allocated.
;;NIL

;; summary
;; accessing the 999999th element of a list or an array
;;
;; data source   itterations    time
;; list          100            1.037000 seconds
;; array         100            doesn't register
;; array         1000           doesn't register
;; array         10000          doesn't register
;; array         100000         doesn't register
;; array         1000000        0.031000 seconds
;; array         45000000       1.002000 seconds
;; so basically with an array about 45 million access in the same time a list takes for 100 accesses

;; nth also takes progressively longer the deeperyougo into a list, wheras aref is virtually constant
;; data source   itterations  element     time
;; list          100000         9        0.016000 seconds
;; list          100000         99       0.115000 seconds
;; list          100000         999      0.843750 seconds
;; array         10000000       9        0.217000 seconds
;; array         10000000       99       0.316000 seconds
;; array         10000000       999      0.332000 seconds
;; array         10000000       99999    0.216000 seconds
