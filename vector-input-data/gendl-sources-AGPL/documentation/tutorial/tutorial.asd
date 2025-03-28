;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:tutorial :description
 "The Gendl® tutorial Subsystem" :author "Genworks International"
 :license "Affero Gnu Public License (http://www.gnu.org/licenses/)"
 :serial t :version "20210427" :depends-on (:dom)
 :defsystem-depends-on nil :components
 ((:gdl "apps/yoyodyne/booster-rocket/source/package")
  (:gdl "apps/yoyodyne/booster-rocket/source/parameters")
  (:gdl "apps/yoyodyne/booster-rocket/source/assembly")
  (:gdl "apps/yoyodyne/booster-rocket/source/rules")
  (:file "source/package") (:file "source/parameters")
  (:file "source/introduction") (:file "source/installation")
  (:file "source/basic-operation") (:file "source/upgrade-notes")
  (:gdl "source/understanding-common-lisp")
  (:gdl "source/understanding-gendl")
  (:gdl "source/advanced-common-lisp") (:gdl "source/advanced-gendl")
  (:file "source/tasty-environment") (:file "source/gendl-geometry")
  (:file "source/custom-user-interfaces")
  (:file "source/bibliography") (:file "source/assembly")
  (:file "source/styles")))
