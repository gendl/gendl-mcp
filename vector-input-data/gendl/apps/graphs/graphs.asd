;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:graphs :description
 "The Gendl® graphs Subsystem" :author "Genworks International"
 :license "Affero Gnu Public License (http://www.gnu.org/licenses/)"
 :serial t :version "20210427" :depends-on (:gwl-graphics)
 :defsystem-depends-on nil :components
 ((:file "source/package") (:file "source/assembly")
  (:file "source/publish") (:file "source/ui")))
