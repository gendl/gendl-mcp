;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:training :description
 "The Gendl® training Subsystem" :author "Genworks International"
 :license "Affero Gnu Public License (http://www.gnu.org/licenses/)"
 :serial t :version "20220328" :depends-on
 (:gwl-graphics :surf :yadd :bordeaux-threads) :defsystem-depends-on
 nil :components
 ((:file "slide-show/source/package")
  (:gendl "slide-show/source/assembly")
  (:file "slide-show/source/publish") (:file "g101/source/package")
  (:file "g101/source/publish")
  (:file "g101/examples/source/file-io")
  (:file "g101/examples/source/list-array-hash")
  (:file "g101/examples/source/solutions")
  (:file "g101/slides/source/assembly")
  (:file "g101/slides/source/conclusion")
  (:file "g101/slides/source/control")
  (:file "g101/slides/source/data-structures")
  (:file "g101/slides/source/equality")
  (:file "g101/slides/source/functions")
  (:file "g101/slides/source/input-output")
  (:file "g101/slides/source/introduction")
  (:file "g101/slides/source/lists")
  (:file "g101/slides/source/macros")
  (:file "g101/slides/source/numbers")
  (:file "g101/slides/source/symbols")
  (:file "g101/slides/source/this-course")
  (:file "g101/slides/source/welcome")))
