(in-package :quiz)

(defparameter *vocab-pathname*
  (make-pathname
   :type nil :name nil :defaults (merge-pathnames "../../sanskrit/vocab/" (glisp:source-pathname))))
