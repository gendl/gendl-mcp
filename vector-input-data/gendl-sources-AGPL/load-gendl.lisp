(in-package :common-lisp-user)

(load "/usr/src/app/quicklisp/setup")

(push "/usr/src/app/gendl/" ql:*local-project-directories*)

(ql:register-local-projects)

(ql:quickload :gendl)

(setq gwl:*aserve-port* #+ccl 8998 #+sbcl 8999)

(gendl:start-gendl!)
