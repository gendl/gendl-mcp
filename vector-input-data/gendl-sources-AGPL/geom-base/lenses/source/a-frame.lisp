;;
;; Copyright 2022 Genworks International
;;
;; This source file is part of the General-purpose Declarative
;; Language project (GDL).
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

(in-package :geom-base)

(define-format a-frame (base-format))

(defparameter *onclick* nil)

(defun orientation-to-euler (orientation)

  (let* ((sy (sqrt (+ (* (aref orientation 0 0)
                         (aref orientation 0 0))
                      (* (aref orientation 1 0)
                         (aref orientation 0 1)))))
         (singular (< sy 1e-6)))
    (let (x y z)
      (if (not singular)
          (setq x (atan (aref orientation 2 1) (aref orientation 2 2))
                y (atan (- (aref orientation 2 0)) sy)
                z (atan (aref orientation 1 0) (aref orientation 0 0)))
          (setq x (atan (- (aref orientation 1 2)) (aref orientation 1 1))
                y (atan (- (aref orientation 2 0) sy))
                z 0))
      (setq x (- (radians-to-degrees x)) y (- (radians-to-degrees y)) z (- (radians-to-degrees z)))
      (let ((candidate (make-vector x y z))) (unless (every #'zerop candidate) candidate)))))


(define-lens (a-frame base-drawing)()
  :output-functions
  ((cad-output
    ()
    (cl-who:with-html-output (*stream* nil)
      (let ((background (lookup-color (the background-color))))
        (declare (ignore background))
        ;; set skycolor based on this background

        (let ((viewpoint (second (the viewpoints))))
          (let ((position (getf viewpoint :point))
                (euler (make-vector 90 0 0)))
            (declare (ignore viewpoint position euler))
            (mapc #'(lambda(view)
		      (let ((object-roots
                              (ensure-list
                               (the-object view object-roots)))
			    (objects
                              (ensure-list
                               (the-object view  objects))))
		        (when *debug?* (print-variables
                                        object-roots objects))
		        (mapc #'(lambda(root)
                                  (write-the-object
                                   root cad-output-tree))
                              object-roots)
		        (mapc #'(lambda(leaf)
                                  (write-the-object leaf
                                                    cad-output))
                              objects)))
	          (the views)))))))))

(define-lens (a-frame base-object)()
  :output-functions
  ((cad-output
    (&key euler position (from-root? t))
    (unless (and euler position)
      (let ((center (if from-root? (the center) (the local-center*)))
            (orientation (if from-root? (the orientation*) (the local-orientation*))))
        (setq euler (when orientation (let ((euler (orientation-to-euler orientation)))
                                        (when euler
                                          (format nil "~a ~a ~a" (svref euler 0) (svref euler 1) (svref euler 2)))))
              position (unless (every #'zerop center) (format nil "~a ~a ~a" (get-x center) (get-y center) (get-z center))))))
    (cl-who:with-html-output (*stream* nil :indent nil)
      (write-the (shape :euler euler :position position
                        :outline-nodes (when (typep self 'outline-specialization-mixin)
                                         (the outline-nodes))))))
                        
   (cad-output-tree
    (&key (from-root? t))
    (cl-who:with-html-output (*stream* nil :indent nil)
      (let ((children (the children))
            (center (if from-root? (the center)
                        (the local-center*)))
            (orientation (if from-root? (the orientation*)
                             (the local-orientation*))))
        (when (every #'zerop center) (setq center nil))
        (let ((euler (when orientation (orientation-to-euler orientation)))
              (x (when center (get-x center))) (y (when center (get-y center)))
              (z (when center (get-z center))))
          (let ((euler (when euler (format nil "~a ~a ~a" (svref euler 0) (svref euler 1) (svref euler 2))))
                (position (when center (format nil "~a ~a ~a" x y z))))
            (if children
                (cl-who:htm ((:a-entity :position position
                                        :rotation euler)

                             (format t "~&Inside a-entity tag content for ~a..~%" self)
                             
                             (mapc
                              #'(lambda(child)
                                  (write-the-object
                                   child
                                   (cad-output-tree :from-root? nil))) (the children))))
                (write-the (cad-output :euler euler :position position :from-root? nil))))))))))

(define-lens (a-frame t)()
  :output-functions
  ((cad-output (&key euler position) (declare (ignore euler position)))
   (cad-output-tree ())
   (shape (&key euler position outline-nodes) (declare (ignore euler position outline-nodes)))
   (outline-nodes
    (&key nodes)
    (when nodes
      (cl-who:with-html-output (*stream* nil :indent nil)
        (mapc #'(lambda(node) (write-the-object node (cad-output :from-root? nil)))
              (the outline-nodes)))))))


(define-lens (a-frame box)()
  :output-functions
  ((shape
    (&key position euler outline-nodes)
    (cl-who:with-html-output (*stream* nil :indent nil)
      ((:a-box
        :position position :rotation euler
        :src (getf (the display-controls) :texture-src)
        :width (number-round (the width) 5)
        :depth (number-round (the height) 5)
        :height (number-round (the length) 5)
        :scale (getf (the display-controls) :scale)
        :color (lookup-color (getf (the display-controls)
                                   :color) :format :hex))
       (write-the (outline-nodes :nodes outline-nodes)))))))

(define-lens (a-frame cylinder)()
  :output-functions
  ((shape
    (&key position euler outline-nodes)
    (cl-who:with-html-output (*stream* nil :indent nil)
      ((:a-cylinder
        :side "double"
        :open-ended (when (getf (the display-controls) :open-ended?) "true")
        :position position :rotation euler
        :radius (number-round (the radius) 5)
        :height (number-round (the length) 5)
        :scale (getf (the display-controls) :scale)
        :color (lookup-color (getf (the display-controls)
                                   :color) :format :hex))
       (write-the (outline-nodes :nodes outline-nodes)))))))

(define-lens (a-frame circle)()
  :output-functions
  ((shape
    (&key position euler outline-nodes)
    (cl-who:with-html-output  (*stream* nil :indent nil)
      ((:a-circle
        :side "double" ;; make this configurable
        :position position :rotation euler
        :radius (number-round (the radius) 5)
        :scale (getf (the display-controls) :scale)
        :color (lookup-color (or (getf (the display-controls) :fill-color)
                                 (getf (the display-controls) :color)) :format :hex))
       (write-the (outline-nodes :nodes outline-nodes)))))))












