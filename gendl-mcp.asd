;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:gendl-mcp :description
 "The Gendl® gendl-mcp Subsystem" :author "Genworks International"
 :license "Affero Gnu Public License (http://www.gnu.org/licenses/)"
 :serial t :version "20250328" :depends-on (:cl-json) :components
 ((:file "source/package") (:file "source/mcp-server-wrapper")
  (:file "source/mcp-endpoints") (:file "source/functions")))
