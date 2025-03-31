;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:gendl-mcp :description
 "The Gendl® gendl-mcp Subsystem" :author "Genworks International"
 :license "Affero Gnu Public License (http://www.gnu.org/licenses/)"
 :serial t :version "20250331" :depends-on (:cl-json) :components
 ((:file "skel/source/package") (:file "skel/source/assembly")
  (:file "skel/source/file-base") (:file "source/package")
  (:file "source/mcp-endpoints") (:file "source/functions")
  (:file "source/mcp-server-final")))
