;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:cl-lite :description
 "The Gendl® Compile-and-Load Lite Utility" :author
 "Genworks International" :license
 "Affero Gnu Public License (http://www.gnu.org/licenses/)" :serial t
 :version "20210427" :depends-on (:glisp) :defsystem-depends-on nil
 :components
 ((:file "source/package") (:file "source/parameters")
  (:file "source/cl-lite") (:file "source/initialize")
  (:file "source/zzinit")))
