;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:ta2 :description
 "The Gendl® (legacy) Testing and Tracking Utility, version 2 (using Ajax but pre-gdlAjax, and no JQuery or CSS)"
 :author "Genworks International" :license
 "Affero Gnu Public License (http://www.gnu.org/licenses/)" :serial t
 :version "20210427" :depends-on (:gwl-graphics)
 :defsystem-depends-on nil :components
 ((:file "source/package") (:file "source/parameters")
  (:file "source/javascript") (:file "source/action-object")
  (:file "source/ajax") (:file "source/assembly")
  (:file "source/click-mode") (:file "source/inspector")
  (:file "source/object-tree") (:file "source/part-type-form")
  (:file "source/publish") (:file "source/viewport")))
