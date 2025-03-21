(in-package :gdl-user)


(let ((tmp-regression (translate-logical-pathname "~/tmp/regression/")))
  (ensure-directories-exist tmp-regression)
  (when (probe-file tmp-regression) (uiop:delete-directory-tree tmp-regression :validate t))
  (glisp:copy-directory (or (probe-file "/src/gendl/regression/")
                            (probe-file "/opt/gendl-regression/")
                            (probe-file "~/genworks/gendl/regression/")
                            (probe-file "/home/builder/genworks/manager/git/all/gendl/regression/"))
                        tmp-regression)

  (print-variables tmp-regression))


(load-quicklisp)
(pushnew "~/tmp/regression/" ql:*local-project-directories* :test #'equalp)
(ql:register-local-projects)
(ql:quickload :regression)

(gdl-lift-utils:define-regression-tests)

;;(gdl-lift-utils:seed-all-test-data)

(gdl-lift-utils:run-regression-tests-pass-fail)
(print-variables gdl-lift-utils:*regression-test-report*)

(let ((string gdl-lift-utils:*regression-test-report*))
  (unless (and (<= (count #\Newline string) 1) (search "all passed!" string))
    #+ccl (ccl:quit 1)
    #+sbcl (sb-ext:exit :code 1)
    #+allegro (excl:exit 1 :no-unwind t)))
