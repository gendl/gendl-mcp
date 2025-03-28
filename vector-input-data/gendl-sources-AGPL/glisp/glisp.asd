;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:glisp :description
 "The Gendl®  Common Lisp Portability" :author
 "Genworks International" :license
 "Affero Gnu Public License (http://www.gnu.org/licenses/)" :serial t
 :version "20210427" :depends-on
 (:uiop :bordeaux-threads :cl-ppcre #+sbcl :sb-posix #-allegro :cl-base64  #-allegro :babel :base)
 :defsystem-depends-on nil :components
 ((:file "source/parameters") (:file "source/genworks")
  (:file "source/initialize") (:file "source/zzinit")))
