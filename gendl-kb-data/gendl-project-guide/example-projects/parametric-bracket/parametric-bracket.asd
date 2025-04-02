;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:parametric-bracket
 :description "The Gendl® parametric-bracket Subsystem" :author
 "Genworks International" :license
 "Affero Gnu Public License (http://www.gnu.org/licenses/)" :serial t
 :version "20250326" :depends-on nil :components
 ((:file "source/package") (:file "source/hole-pattern")
  (:file "source/bracket") (:file "source/assembly")))
