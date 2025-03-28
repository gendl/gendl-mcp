;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: parametric-bracket; Base: 10 -*-

(in-package :parametric-bracket)

(define-object hole-pattern (base-object)
  :input-slots
  (("Integer. Number of holes in the pattern" num-holes 3 )
   ("Number. Diameter of each hole in mm" hole-diameter 8 )
   ("Number. Center-to-center spacing between holes in mm" hole-spacing 15 )
   ("3D Point. Starting point for the hole pattern" start-point (the center) )
   ("3D Vector. Direction vector for the pattern" direction-vector (the (face-normal-vector :right))))
   
  :computed-slots
  ((points (let ((result nil))
             (dotimes (i (the num-holes) (nreverse result))
               (push (translate-along-vector 
                      (the start-point)
                      (the direction-vector)
                      (* i (the hole-spacing)))
                     result)))))
  
  :objects
  ((holes :type 'cylinder
          :sequence (:size (the num-holes))
          :center (nth (the-child index) (the points))
          :radius (/ (the hole-diameter) 2)
          :height 100  ; Make it long enough to cut through material
          :display-controls (list :color :red :transparency 0.5))))
