(in-package :cl-user)

(defun hello (&key (entity "world"))
  (format t "Hello, ~a!~%" entity))
