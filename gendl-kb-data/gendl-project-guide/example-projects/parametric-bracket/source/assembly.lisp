;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: parametric-bracket; Base: 10 -*-

(in-package :parametric-bracket)

(define-object assembly (base-object)
  :input-slots
  (("Number of bracket variations to display" num-brackets 3)
   ("Spacing between brackets" spacing 150))
  
  :computed-slots
  ((bracket-widths (list 80 100 120))
   (bracket-heights (list 60 75 90))
   (bracket-thicknesses (list 8 10 12)))
  
  :objects
  ((brackets :type 'bracket
             :sequence (:size (the num-brackets))
             :center (translate (the center)
                               :right (* (the-child index) (the spacing)))
             :width (nth (min (the-child index) 
                             (1- (length (the bracket-widths))))
                        (the bracket-widths))
             :height (nth (min (the-child index) 
                              (1- (length (the bracket-heights))))
                         (the bracket-heights))
             :thickness (nth (min (the-child index) 
                                 (1- (length (the bracket-thicknesses))))
                            (the bracket-thicknesses))
             :fillet-radius (+ 10 (* (the-child index) 5)))))
