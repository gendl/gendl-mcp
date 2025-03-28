;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: parametric-bracket; Base: 10 -*-

(in-package :parametric-bracket)

;;
;; FLAG -- we'll do a proper version of this with solids
;; (e.g. subtracted-solid for the holes) later, after we get the full
;; Genworks GDL wrapped as an MCP service.
;;

(define-object bracket (base-object)
  :input-slots
  (("Width of the bracket in mm" width 100 )
   ("Height of the bracket in mm" height 75 )
   ("Thickness of the bracket material in mm" thickness 10 )
   ("Length of the mounting flange in mm" flange-length 50 )
   ("Radius of the strengthening fillet" fillet-radius 15 )
   ("Number of mounting holes along horizontal flange" num-holes-horizontal 3 )
   ("Number of mounting holes along vertical flange" num-holes-vertical 2 )
   ("Diameter of mounting holes" hole-diameter 8 )
   ("Distance from edge to first hole center" hole-edge-distance 15 ))

  :computed-slots
  ((diagonal-length (sqrt (+ (expt (the flange-length) 2) 
                             (expt (the flange-length) 2))))
   (display-controls (list :color :green :transparency 0.0)))

  :objects
  ((base-box :type 'box
             :width (the width)
             :length (the flange-length)
             :height (the thickness)
             :center (translate (the center)
                                :front (/ (- (the flange-length) (the thickness)) 2)))
   
   (vertical-box :type 'box
                 :width (the width)
                 :length (the thickness)
                 :height (the height)
                 :center (translate (the center)
                                    :top (/ (the height) 2)))
   
    (horizontal-holes :type 'hole-pattern
                     :num-holes (the num-holes-horizontal)
                     :hole-diameter (the hole-diameter)
                     :hole-spacing (/ (- (the width) 
                                       (* 2 (the hole-edge-distance)))
                                    (max 1 (- (the num-holes-horizontal) 1)))
                     :start-point (translate (the center)
                                            :right (- (/ (the width) 2) (the hole-edge-distance))
                                            :front (/ (the flange-length) 2)
                                            :top (/ (the thickness) -2))
                     :direction-vector (the (face-normal-vector :left)))
   
   (vertical-holes :type 'hole-pattern
                   :num-holes (the num-holes-vertical)
                   :hole-diameter (the hole-diameter)
                   :hole-spacing (/ (- (the height) 
                                     (* 2 (the hole-edge-distance)))
                                  (max 1 (- (the num-holes-vertical) 1)))
                   :start-point (translate (the center)
                                          :top (- (the height) (the hole-edge-distance))
                                          :front (/ (- (the thickness)) 2))
                   :direction-vector (the (face-normal-vector :front)))))
   
