;;;; -*- coding: utf-8 -*-
;;
;; Copyright 2020 Genworks International 
;;
;; This source file is part of the General-purpose Declarative
;; Language project (Gendl).
;;
;; This source file contains free software: you can redistribute it
;; and/or modify it under the terms of the GNU Affero General Public
;; License as published by the Free Software Foundation, either
;; version 3 of the License, or (at your option) any later version.
;; 
;; This source file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Affero General Public License for more details.
;; 
;; You should have received a copy of the GNU Affero General Public
;; License along with this source file.  If not, see
;; <http://www.gnu.org/licenses/>.
;; 


(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *asdf-found?* t)
  (unless (find-package :asdf)
    (setq *asdf-found?* nil)
    (defpackage :asdf (:nicknames :asdf/interface asdf/lisp-action))))

(in-package :asdf)

(when (find-package :asdf)
  (defclass asdf::gdl (asdf::cl-source-file) ((type :initform "gdl")))
  (defclass asdf::gendl (asdf::cl-source-file) ((type :initform "gendl")))
  (defclass asdf::lisp (asdf::cl-source-file) ())

  (defclass asdf::table (asdf::cl-source-file) ((type :initform "table")))

  #+genworks-gdl
  (defmethod perform ((o compile-op) (c asdf::table))
    (perform-peruse-file o c))

  #+genworks-gdl
  (defmethod perform ((o load-op) (c asdf::table))
    (perform-peruse-file o c))

  #+genworks-gdl
  (defun perform-peruse-file (o c)
    "Perform the perusal of icad-style catalog table."

    (declare (ignore o))
  
    (let ((input-file (slot-value c 'asdf/component:absolute-pathname)))

      (funcall (read-from-string "gendl::peruse-file") input-file
               :load t))))


(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless *asdf-found?* (delete-package :asdf)))

