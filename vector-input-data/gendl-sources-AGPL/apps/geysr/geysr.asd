;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:geysr :description
 "The Gendl® geysr Subsystem" :author "Genworks International"
 :license "Affero Gnu Public License (http://www.gnu.org/licenses/)"
 :serial t :version "20210427" :depends-on (:gwl-graphics)
 :defsystem-depends-on (:gendl-asdf) :components
 ((:gdl "source/package") (:gdl "source/parameters")
  (:gdl "source/assembly") (:file "source/iframe")
  (:gdl "source/initialize") (:gdl "source/inspector")
  (:gdl "source/menu-node") (:gdl "source/menu")
  (:file "source/patches") (:gdl "source/tree")
  (:gdl "source/user-inputs") (:gdl "source/viewport")
  (:gdl "source/zzinit")))
