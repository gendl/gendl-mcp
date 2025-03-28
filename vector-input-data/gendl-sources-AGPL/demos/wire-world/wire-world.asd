;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:wire-world :description
 "The Gendl® Wire-World demo and test-case for wireframe tessellation and X3D output"
 :author "Genworks International" :license
 "Affero Gnu Public License (http://www.gnu.org/licenses/)" :serial t
 :version "20210427" :depends-on
 (#-gwl-graphics :gwl-graphics)
 :defsystem-depends-on nil :components
 ((:file "source/package") (:file "source/assembly")))
