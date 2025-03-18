(in-package :gdl-user)

(load-quicklisp)

(push (truename "./") ql:*local-project-directories*)
(load "./make.lisp")

(let ((hostname (uiop:hostname)))
  (training-app :deploy? (string-equal hostname "gornschool.com")))
