;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:examples :description
 "The Gendl® examples Subsystem" :author "Genworks International"
 :license "Affero Gnu Public License (http://www.gnu.org/licenses/)"
 :serial t :version "20210427" :depends-on (:gwl-graphics :surf)
 :defsystem-depends-on nil :components
 ((:file "source/ch2-examples") (:file "source/ch3-examples")
  (:gdl "source/primi-plane") (:gdl "source/ui-primi-plane")))
