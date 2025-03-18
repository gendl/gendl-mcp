(in-package :gdl-user)

(gwl:define-package :training-common
    (:export #:*home*
             #:*initializers*
             #:initialize-all
             #:base-site-mixin
             #:repl-example
	     #:code-example
	     #:index-page
	     #:base-training-sheet
	     #:base-tutorial-sheet))


(load (compile-file (merge-pathnames "../../t6/resources/source/gwl-patches.lisp" (glisp:source-pathname))))
