;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:robot :description
 "The Gendl® Simplified Android Robot example " :author
 "Genworks International" :license
 "Affero Gnu Public License (http://www.gnu.org/licenses/)" :serial t
 :version "20210427" :depends-on (:gwl-graphics)
 :defsystem-depends-on nil :components
 ((:file "source/package") (:file "source/assembly")
  (:file "source/initialize") (:file "source/parameters")
  (:file "source/zzinit")))
