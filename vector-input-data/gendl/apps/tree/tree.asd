;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:tree :description
 "The Gendl® Tree component used by Tasty and potentially as a UI component on its own"
 :author "Genworks International" :license
 "Affero Gnu Public License (http://www.gnu.org/licenses/)" :serial t
 :version "20210427" :depends-on (:gwl-graphics)
 :defsystem-depends-on nil :components
 ((:file "source/package") (:file "source/parameters")
  (:file "source/ajax") (:file "source/assembly")
  (:file "source/newertree")))
