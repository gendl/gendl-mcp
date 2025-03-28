;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:source :description
 "The Gendl® source Subsystem" :author "Genworks International"
 :license "Affero Gnu Public License (http://www.gnu.org/licenses/)"
 :serial t :version "20220219" :depends-on nil :defsystem-depends-on
 nil :components
 ((:file "assembly") (:file "conclusion") (:file "control")
  (:file "data-structures") (:file "equality") (:file "functions")
  (:file "input-output") (:file "introduction") (:file "lists")
  (:file "macros") (:file "numbers") (:file "symbols")
  (:file "this-course") (:file "welcome")))
