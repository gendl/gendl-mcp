;;;; -*- coding: utf-8 -*-

(asdf:defsystem #:regression :description
 "The Gendl® regression Subsystem" :author "Genworks International"
 :license "Affero Gnu Public License (http://www.gnu.org/licenses/)"
 :serial t :version "20241018" :depends-on (:lift) :components
 ((:file "utils/source/package") (:file "utils/source/genworks")
  (:file "utils/source/parameters")
  (:file "utils/source/define-regression-tests")
  (:file "utils/source/functions") (:file "dgdl/source/remote")
  (:file "gdl/source/object-amendment")
  (:file "gdl/source/object-amendment-amendment")
  (:file "gdl/source/syntax-checker")
  (:file "geom-base/source/angle-between-vectors")
  (:file "geom-base/source/box-matrix")
  (:file "geom-base/source/drawing")
  (:file "geom-base/source/general-note")
  (:file "gwl/source/package") (:file "gwl/source/ajax-json")
  (:file "gwl/source/form-element-tests")
  (:file "gwl/source/form-element-validation")
  (:file "gwl/source/hey-now") (:file "gwl/source/large-data")
  (:file "gwl/source/short-form-element-test")
  (:file "gwl/source/svg-error-handling")
  (:file "gwl/source/test-seq") (:file "issues/source/issue-68")
  (:file "tasty/source/center-loss")
  (:file "threads/source/box-solid")))
