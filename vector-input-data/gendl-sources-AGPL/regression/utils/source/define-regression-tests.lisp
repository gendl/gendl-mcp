(in-package :gdl-lift-utils)


(defun define-regression-tests ()
  (let ((lift:*test-evaluate-when-defined?* nil))
    (lift:deftestsuite gdl-lift-utils::gdl-regression-tests () ()
		       (:setup (glisp:gc-full)))
    (dolist (symbol (reverse *gdl-test-definitions*))
      (let ((test
	     (format nil 
		     "(lift:addtest (gdl-lift-utils::gdl-regression-tests) ~s (lift:ensure-same (gdl-lift-utils::get-seed-data '~s) (gdl-lift-utils::get-regression-data '~s) :test 'gdl-lift-utils::equivalent-lists))" symbol symbol symbol)))
	(eval (read-from-string test))))))
