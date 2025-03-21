;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:ledger :description
 "The Gendl® Ledger Bookkeeping Demo" :author
 "Genworks International" :license
 "Affero Gnu Public License (http://www.gnu.org/licenses/)" :serial t
 :version "20210427" :depends-on (#-gwl :gwl)
 :defsystem-depends-on nil :components
 ((:file "source/package") (:file "source/assembly")
  (:file "source/html") (:file "source/zzinit")))
