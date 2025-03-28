;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:4bar :description
 "The Gendl® 4bar Subsystem" :author "Genworks International"
 :license "Affero Gnu Public License (http://www.gnu.org/licenses/)"
 :serial t :version "20210427" :depends-on (:bus)
 :defsystem-depends-on nil :components
 ((:gdl "source/4bar") (:gdl "source/bus")))
