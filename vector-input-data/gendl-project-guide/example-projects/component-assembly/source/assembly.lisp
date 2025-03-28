;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: example-project; Base: 10 -*-

(in-package :component-assembly)

(define-object base-component (base-object)
  :input-slots
  ((width 100)
   (length 100)
   (height 50)
   (color :blue))
  
  :computed-slots
  ((volume (* (the width) (the length) (the height)))
   (display-controls (list :color (the color))))
  
  :objects
  ((geometry :type 'box
             :width (the width)
             :length (the length) 
             :height (the height))))

(define-object assembly (base-object)
  :input-slots
  ((num-components 3))
  
  :objects
  ((components :type 'base-component
               :sequence (:size (the num-components))
               :center (translate (the center)
                                  :right (* (the-child index) 150))
               :color (case (the-child index)
                        (0 :red)
                        (1 :green)
                        (t :blue)))))
