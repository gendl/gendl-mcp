;;;; house.lisp
;;;;
;;;; Simple house model for testing the Gendl MCP integration

(in-package :gdl-user)

(define-object house (base-object)
  :documentation "A simple house model"
  
  :input-slots
  ((width 10.0 :documentation "Width of the house in meters")
   (length 12.0 :documentation "Length of the house in meters")
   (height 8.0 :documentation "Height of the house in meters")
   (roof-height 3.0 :documentation "Height of the roof peak above the house")
   (door-width 1.0 :documentation "Width of the front door")
   (door-height 2.1 :documentation "Height of the front door")
   (window-width 1.2 :documentation "Width of each window")
   (window-height 1.5 :documentation "Height of each window")
   (window-count 4 :documentation "Number of windows"))
  
  :computed-slots
  ((volume (* (the width) (the length) (the height))
           :documentation "Volume of the main house structure in cubic meters")
   (total-height (+ (the height) (the roof-height))
                :documentation "Total height including roof")
   (floor-area (* (the width) (the length))
              :documentation "Floor area in square meters")
   (wall-area (* 2 (+ (* (the width) (the height))
                      (* (the length) (the height))))
              :documentation "Total wall area in square meters")
   (door-area (* (the door-width) (the door-height))
              :documentation "Area of the front door")
   (window-area (* (the window-width) (the window-height))
               :documentation "Area of a single window")
   (total-window-area (* (the window-area) (the window-count))
                      :documentation "Total area of all windows")
   (wall-area-minus-openings (- (the wall-area) 
                               (the door-area) 
                               (the total-window-area))
                            :documentation "Wall area minus doors and windows"))
  
  :objects
  ((walls :type 'box
          :width (the width)
          :length (the length)
          :height (the height)
          :display-controls (list :color :cornsilk
                                 :transparency 0.2))
   
   (roof :type 'cone-frustum
         :bottom-radius (/ (sqrt (+ (expt (the width) 2)
                                   (expt (the length) 2)))
                          2)
         :top-radius 0.1
         :height (the roof-height)
         :center (translate (the walls center) 
                           :up (half (the walls height)))
         :display-controls (list :color :firebrick
                                :transparency 0.3))
   
   (door :type 'box
         :width (the door-width)
         :length 0.1
         :height (the door-height)
         :center (translate (the walls center)
                           :front (half (the walls length))
                           :down (- (half (the walls height)) 
                                   (half (the door-height))))
         :display-controls (list :color :sienna))))

(define-object house-with-windows (house)
  :documentation "House with explicitly modeled windows"
  
  :objects
  ((windows :type 'box
            :sequence (:size (the window-count))
            :width (the window-width)
            :length 0.1
            :height (the window-height)
            :center (translate (the walls center)
                             :right (- (* (the width) 0.4) 
                                      (* (floor (the-child index) 2) (the width) 0.6))
                             :front (if (oddp (the-child index))
                                       (half (the walls length))
                                       (- (half (the walls length))))
                             :up (* (the height) 0.1))
            :display-controls (list :color :lightsteelblue
                                   :transparency 0.5))))

;;; Define a sample instance
(define-object house-sample (house-with-windows)
  :computed-slots
  ((width 15.0)
   (length 10.0)
   (roof-height 5.0)
   (window-count 6)))
