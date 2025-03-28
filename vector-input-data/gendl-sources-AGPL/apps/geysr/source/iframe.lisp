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


(in-package :geysr)

(define-object iframe (sheet-section)

  :input-slots (url)

  :computed-slots ((div-class "gwl-iframe-section")
		   (inner-html
		    (with-cl-who-string ()
				 (if (the url)
				   (htm ((:iframe :id "EmbeddedUI" :title "Embedded UI"
						  :height "100%" :width "100%"
						  :seamless :seamless
						  :src (the url))))
				   (htm
				    (:p (:h2 (fmt "Welcome to Geysr.")))

				    (:p (fmt
		    "Geysr is for inspecting Genworks GDL models
                   and user interfaces while you're developing them."))

				    (:p (fmt "No object is currently selected and you're 
in User Interface mode."))

				    (:p (fmt "Select a node from the tree atleft 
to display its User Interface, if one exists."))))))))
