# Gendl Documentation - tutorial_4_object_definition

## file-output.lisp - header
Source: gornschool-training/t4/resources/source/file-output.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## file-output.lisp - output-report
Source: gornschool-training/t4/resources/source/file-output.lisp
Type: tutorial

```
(defun output-report (fname)
  (let ((obj (make-object 'my-box
			  :output-filename fname)))
    (theo obj write-report!)))
			


```

---

## file-output.lisp - my-box
Source: gornschool-training/t4/resources/source/file-output.lisp
Type: tutorial

```
(define-object my-box (box)
  :input-slots
  ((output-filename "c:/temp/my-box-report"))
  
  :computed-slots
  ((width 3)
   (height 4)
   (length 6))

  :functions
  ((write-report!()
		 (with-open-file (s (the output-filename)
				    :direction :output
				    :if-exists :supersede
				    :if-does-not-exist :create)
		   (let ((i 0))
		     (format t "Begining output to ~a~%" (the output-filename))
		     (format s "Box Width ~a~%" (the width))
		     (incf i)
		     (format s "Box Length ~a~%" (the length))
		     (incf i)
		     (format s "Box Height ~a~%" (the height))
		     (incf i)
		     (format s "Box Center ~@{~,1f~^, ~}~%"
			     (get-x (the center))
			     (get-y (the center))
			     (get-z (the center)))
		     (incf i)
		     (format s "Box Volume ~a~%" (the volume))
		     (incf i)
		     (format t "Output written (~a line~:p)~%" i)))))
  ))

```

---

## building-bom-output.lisp - header
Source: gornschool-training/t4/resources/source/building-bom-output.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## building-bom-output.lisp - building-bom
Source: gornschool-training/t4/resources/source/building-bom-output.lisp
Type: tutorial

```
(defun building-bom (&key (nominal-height 3000) 
		       (nominal-width 3000)
		       (nominal-length 3000)
		       (roof-angle 30)
		       (output-filename nil))
  (let ((obj (make-object 'building
			  :nominal-height nominal-height
			  :nominal-width nominal-width
			  :nominal-length nominal-length
			  :truss-angle roof-angle
			  :output-filename output-filename)))
    (if output-filename (theo obj write-bom-file!)
	(theo obj bom-formatted))))


```

---

## building-bom-output.lisp - building
Source: gornschool-training/t4/resources/source/building-bom-output.lisp
Type: tutorial

```
(define-object building (box)
  :input-slots
  ((nominal-height 3000)
   (nominal-width 3000)
   (nominal-length 4000)
   (brick-height 45)
   (brick-length 180)
   (brick-width 90)
   (mortar-joint-width 10)
   (truss-angle 30)
   (beam-width 40)
   (beam-height 50)
   (wall-thickness 3)
   (material-density 7800)
   (roof-overhang 50)
   (cladding-thickness 10)
   (max-beam-spacing 1500)
   (output-filename nil))

  :computed-slots
  ((length (the left-wall length))
   (width (the rear-wall length))
   (height (+ (the left-wall height) (the (roof-truss 0) height)))

   (number-of-roof-trusses (let ((trusses (ceiling (the left-wall length) 1500)))
			     (max trusses 2)))

   (truss-spacing (div (- (the left-wall length) (the beam-width))
		       (- (the number-of-roof-trusses) 1)))
   (truss-offsets (let ((res nil))
		    (dotimes (n (the number-of-roof-trusses) (nreverse res))
		      (push (+ (half (the beam-width))
			       (* n (the truss-spacing))) res))))

   (roof-length (+ (the left-wall length) (twice (the roof-overhang))))
   (roof-width (the cladding-thickness))
   (roof-height (let ((apex (the (roof-truss 0) apex-point))
		      (gutter (the (roof-truss 0) front-gutter-point)))
		  (+ (3d-distance apex gutter) (the roof-overhang))))

   ;; building properties
   (walls (remove nil (mapcar #'(lambda(a) (when (typep a 'wall) a)) (the children))))
   (full-bricks (apply '+ (mapsend (the walls) :full-bricks)))
   (half-bricks (apply '+ (mapsend (the walls) :half-bricks)))
   (mortar-volume (apply '+ (mapsend (the walls) :mortar-volume)))
   (cladding-dimensions (list :length (the roof-length)
			      :width (the roof-height)))
   (beam-properties (the (roof-truss 0) beam-properties))
   (beam-qty-by-size (let ((res nil))
		       (dolist (plis (the beam-properties) )
			 (let* ((trusses (the number-of-roof-trusses))
				(l (getf plis :length-mm))
				(p (position l res :key #'(lambda(a) (getf a :length-mm))))
				(qty (when p (getf (nth p res) :qty))))
			   (if p (setf (getf (nth p res) :qty) (+ qty trusses))
			       (setq res (append (list (list :length-mm l :qty trusses)) res)))))
		       (safe-sort res '< :key #'(lambda(a) (getf a :length-mm)))))
		       

   (roof-truss-mass (* (apply '+ (mapcar #'(lambda(a) (getf a :mass-kg))
					 (the beam-properties)))
		       (the number-of-roof-trusses)))

   (building-materials (list :full-bricks (the full-bricks)
			     :half-bricks (the half-bricks)
			     :mortar-volume-m3 (div (the mortar-volume) 1000000000)
			     :beams (the beam-qty-by-size)
			     :roof-cladding (append (the cladding-dimensions) (list :qty 2))))

  (bom-formatted (let* ((bom (the building-materials))
			(cladding (getf bom :roof-cladding))
			(bricks (format nil "Bricks~%======~%  Full Bricks ~a~%  Half Bricks ~a~%" 
					(getf bom :full-bricks) 
					(getf bom :half-bricks)))
			(mortar (format nil "Mortar~%======~%  Volume ~,3f m^3~%" 
					(getf bom :mortar-volume-m3)))
			(l (round-to-nearest (getf cladding :length) 1))
			(w (round-to-nearest (getf cladding :width) 1))
			(roof (format nil "Roof Cladding~%======~%  Qty ~a~%  Dimensions (L x W x T) ~d x ~d x ~d~%" 
				      (getf cladding :qty)
				      l w (the cladding-thickness)))
			(beams (getf (the building-materials) :beams))
			(beams-list (flatten
				     (mapcar #'(lambda(a)
						 (list (getf a :qty) (round-to-nearest (getf a :length-mm) 1)))
					     beams)))
			
			(beams-header (format nil "Beams~%=====~%  Section (H x W x T) ~a x ~a x ~a~%"
						 (the beam-height) (the beam-width) (the wall-thickness)))
			(beam-lengths (format nil "~{  Qty ~a Length ~a~%~}" beams-list)))
		   (format nil "~@{~a~}" bricks mortar roof beams-header beam-lengths))) 
   
		
   )

  :functions
  ((write-bom-file! ()
		    (with-open-file (s (the output-filename) :direction :output
						:if-exists :supersede
						:if-does-not-exist :create)
		      (format t "Exporting the BOM to ~a~%" (the output-filename))
		      (format s "~a" (the bom-formatted))
		      (format t "Exporting complete~%")))



   (get-roof-mid-point! (first-gutter last-gutter last-index)
		       (let*((mid-gutter (midpoint first-gutter last-gutter))
			     (first-apex (the (roof-truss 0) apex-point))
			     (last-apex (the (roof-truss last-index) apex-point))
			     (mid-apex (midpoint first-apex last-apex))
			     (vec (subtract-vectors mid-gutter mid-apex))
			     (mid-edge (translate-along-vector mid-gutter vec (the roof-overhang))))
			 (midpoint mid-apex mid-edge))) )
  
  :objects
  ((left-wall :type 'wall
	      :pass-down (brick-height
			  brick-length
			  brick-width
			  mortar-joint-width)
	      :center (translate-along-vector
		       (translate-along-vector (the (edge-center :bottom :left))
					       (the (face-normal-vector :right))
					       (half (the-child width)))
		       (the (face-normal-vector :top))
		       (half (the-child height)))
	      :wall-length (the nominal-length)
	      :wall-height (the nominal-height))

   (right-wall :type 'wall
	       :pass-down (brick-height
			   brick-length
			   brick-width
			   mortar-joint-width)
	       :center (translate-along-vector
			(translate-along-vector (the (edge-center :bottom :right))
						(the (face-normal-vector :left))
						(half (the-child width)))
			(the (face-normal-vector :top))
			(half (the-child height)))
	       :wall-length (the nominal-length)
	       :wall-height (the nominal-height))

   (rear-wall :type 'wall
	      :pass-down (brick-height
			  brick-length
			  brick-width
			  mortar-joint-width)
	      :center (translate-along-vector
			(translate-along-vector (the (edge-center :bottom :rear))
						(the (face-normal-vector :front))
						(half (the-child width)))
			(the (face-normal-vector :top))
			(half (the-child height)))
	      :orientation (alignment :rear (the (face-normal-vector :right)))
	      :wall-length (the nominal-width)
	      :wall-height (the nominal-height))

   (roof-truss :type 'truss
	       :sequence (:size (the number-of-roof-trusses))
	       :truss-length (the rear-wall length)
	       :center (translate-along-vector
			(translate-along-vector (the (edge-center :front :top))
						(the (face-normal-vector :bottom))
						(half (the-child height)))
			(the (face-normal-vector :rear))
			(nth (the-child index) (the truss-offsets)))
	       :orientation (alignment :rear (the (face-normal-vector :right))
				       )		
	       :pass-down (truss-angle
			   beam-width
			   beam-height
			   wall-thickness
			   material-density))
   
   (roof-cladding-left
    :type 'box
    :length (the roof-length)
    :height (the roof-height)
    :width (the cladding-thickness)
    :orientation (alignment :left (the (roof-truss 0) front-slope-normal))
    :center (let* ((last-index (- (the number-of-roof-trusses) 1))
		   (first-gutter (the (roof-truss 0) front-gutter-point))
		   (last-gutter (the (roof-truss last-index) front-gutter-point))
		   (mid-ctr (the (get-roof-mid-point! first-gutter last-gutter last-index))))
	      (translate-along-vector mid-ctr
				      (the (roof-truss 0) front-slope-normal)
				      (half (the cladding-thickness)))))
   
   (roof-cladding-right :type 'box
			:length (the roof-length)
			:height (the roof-height)
			:width (the cladding-thickness)
			:orientation (alignment :left (the (roof-truss 0) rear-slope-normal))
			 :center (let* ((last-index (- (the number-of-roof-trusses) 1))
					(first-gutter (the (roof-truss 0) rear-gutter-point))
					(last-gutter (the (roof-truss last-index) rear-gutter-point))
					(mid-ctr (the (get-roof-mid-point! first-gutter last-gutter last-index))))
				 (translate-along-vector mid-ctr
							 (the (roof-truss 0) rear-slope-normal)
							 (half (the cladding-thickness)))))
   )

  
		       
  )



```

---

## building-bom-output.lisp - wall
Source: gornschool-training/t4/resources/source/building-bom-output.lisp
Type: tutorial

```
(define-object wall(box)
  :input-slots
  ((brick-height 45)
   (brick-length 180)
   (brick-width 90)
   (mortar-joint-width 10)
   (wall-length 3700)
   (wall-height 900)
   (first-row :start-full)
   (front-edge :full)
   (rear-edge :full))

  :computed-slots
  ((row-height (+ (the brick-height) (the mortar-joint-width)))
   (number-of-rows (round-to-nearest (div (the wall-height) (the row-height)) 1))
   (actual-wall-height (* (the row-height) (the number-of-rows)))
   
   ;; for the wall-length we need the number of full bricks
   ;; if there are n full bricks then there will be (n-1) mortar joints
   ;; so n*brick-length + n-1*mortar-joint-width = overall-length
   ;; or n(brick-length + mortar-join-width) - mortar-joint-width = overall-length
   ;; or n = (overall-length - mortar-joint-width)/(brick-length + mortar-joint-width)
   (number-of-bricks (round-to-nearest (div (- (the wall-length) (the mortar-joint-width))
					    (+ (the brick-length) (the mortar-joint-width)))
				       1))
   (actual-wall-length (+ (* (the number-of-bricks) (the brick-length))
			  (* (- (the number-of-bricks) 1) (the mortar-joint-width))))

   ;; box inputs - gives the wall bounding box
   (height (the actual-wall-height))
   (width (the brick-width))
   (length (the actual-wall-length))

   (full-bricks (apply '+ (list-elements (the row) (the-element full-bricks))))
   (half-bricks (apply '+ (list-elements (the row) (the-element half-bricks))))
   (mortar-volume (apply '+ (list-elements (the row) (the-element mortar-volume))))
   (mortar-density 2162)
   (mortar-mass (* (the mortar-density) (div (the mortar-volume) 1000000000)))
   )
   
     
  :objects
  ((row :type 'row
     :sequence (:size (the number-of-rows))
     :center (translate-along-vector (the (face-center :bottom))
				     (the (face-normal-vector :top))
				     (+ (half (the-child height))
					(* (the-child index) (the-child height))))
     :height (+ (the brick-height) (the mortar-joint-width))
     :full-bricks-per-row (the number-of-bricks)
     :pass-down (brick-height
		 brick-length
		 brick-width
		 mortar-joint-width
		 length
		 width
		 first-row
		 front-edge
		 rear-edge))))


```

---

## building-bom-output.lisp - row
Source: gornschool-training/t4/resources/source/building-bom-output.lisp
Type: tutorial

```
(define-object row (box)
  :input-slots
  (full-bricks-per-row
   brick-height
   brick-length
   brick-width
   mortar-joint-width
   first-row
   front-edge
   rear-edge )

  :computed-slots
  ((full-brick-row? (cond ((eq (the first-row) :start-full)
			   (or (zerop (the index)) (evenp (the index))))
			  ((eq (the first-row) :start-half)
			   (not (or (zerop (the index)) (evenp (the index)))))))
		    
   (full-bricks (the bricks-and-mortar full-bricks))
   (half-bricks (the bricks-and-mortar half-bricks))
   (mortar-volume (+ (the bricks-and-mortar mortar-joint-volume)
		     (the mortar-bed volume))))

  :objects
  ((bricks-and-mortar :type 'bricks-and-mortar
		      :height (the brick-height)
		      :center (translate-along-vector (the mortar-bed (face-center :top))
						      (the mortar-bed (face-normal-vector :top))
						      (half (the-child height)))
		      :pass-down (width
				  length
				  full-brick-row?
				  brick-height
				  brick-length
				  brick-width
				  mortar-joint-width
				  full-bricks-per-row
				  front-edge
				  rear-edge))
   (mortar-bed :type 'box
	       :height (the mortar-joint-width)
	       :center (translate-along-vector (the (face-center :bottom))
					       (the (face-normal-vector :top))
					       (half (the-child height)))
	       :pass-down (width
			   length))))


```

---

## building-bom-output.lisp - bricks-and-mortar
Source: gornschool-training/t4/resources/source/building-bom-output.lisp
Type: tutorial

```
(define-object bricks-and-mortar (box)
  :input-slots
  (full-brick-row?
   full-bricks-per-row
   brick-height
   brick-length
   brick-width
   mortar-joint-width
   front-edge
   rear-edge)

  :computed-slots
  ((first-full-brick-start-point (if (the full-brick-row?)
				     (the (face-center :front))
				     (the (mortar-joint 0) (face-center :rear))))
   
   (first-mortar-joint-start-point (cond ((the full-brick-row?) (the (full-brick 0) (face-center :rear)))
					 ((eq (the front-edge) :full) (the (half-brick 0) (face-center :rear)))
					 ((eq (the front-edge) :keyed) (translate-along-vector (the (face-center :front))
											       (the (face-normal-vector :rear))
											       (half (the brick-length))))))

   (number-of-full-bricks (if (the full-brick-row?)
			      (the full-bricks-per-row)
			      (- (the full-bricks-per-row) 1)))

   (number-of-half-bricks (cond ((the full-brick-row?) 0)
				((and (eq (the front-edge) :full)(eq (the rear-edge) :full)) 2)
				((and (eq (the front-edge) :keyed) (eq (the rear-edge) :full)) 1)
				((and (eq (the front-edge) :full) (eq (the rear-edge) :keyed)) 1)
				((and (eq (the front-edge) :keyed) (eq (the rear-edge) :keyed)) 0)))

   ;; whether or not the ends are :full or :keyed, the number of mortar joints remains the same since the mortar joint
   ;; when it is :keyed is used to connect to the full brick of the other wall
   (number-of-mortar-joints (if (the full-brick-row?)
				(- (the number-of-full-bricks) 1)
				(+ (the number-of-full-bricks) 1)))

   ;; if it isn't a full brick row then there will be an extra joint because one
   ;; full brick is replaced with 2 half bricks so without correcting the
   ;; mortar-joint-width the ends of a full brick row and one starting and
   ;; finishing with half bricks won't align. So we need to correct
   ;; the mortar-joint-width
   (corrected-joint-width (if (the full-brick-row?)
				     (the mortar-joint-width)
				     (let ((total-gap (* (- (the number-of-mortar-joints) 1)
									(the mortar-joint-width))))
				       (div total-gap (the number-of-mortar-joints)))))
   ;; collating the output. We could do this analytically, but for this example we'll use the geometry
   (full-bricks (length (list-elements (the full-brick))))
   (half-bricks (length (list-elements (the half-brick))))
   (mortar-joint-volume (apply '+ (list-elements (the mortar-joint)
						 (the-element volume))))
   )
  
  :functions
  ((first-full-brick-center!
    ()
    (translate-along-vector (the first-full-brick-start-point)
			    (the (face-normal-vector :rear))
			    (half (the brick-length))))
   
   (other-full-brick-center!
    (index)
    ;; if its a full brick row, (full-brick 1) is positioned relative to (joint 0)
    ;; if its a half brick row, (full-brick 1) is positioned relative to (joint 1)
    (let ((ind (if (the full-brick-row?) (- index 1) index)))
	  (translate-along-vector (the (mortar-joint ind) (face-center :rear))
				  (the (face-normal-vector :rear))
				  (half (the brick-length)))))
   
    (first-joint-center!
     ()
     (translate-along-vector (the first-mortar-joint-start-point)
			     (the (face-normal-vector :rear))
			     (half (the corrected-joint-width))))
   (other-joint-center!
    (index)
    ;; if its a full brick row, (joint 1) is positioned relative to (full-brick 1)
    ;; if its a half brick row, (joint 1) is positioned relative to (full-brick 0)
    (let ((ind (if (the full-brick-row?) index (- index 1))))
      (translate-along-vector (the (full-brick ind) (face-center :rear))
			      (the (face-normal-vector :rear))
			      (half (the corrected-joint-width)))))

    (first-half-brick-center!
     ()
     (translate-along-vector (the (face-center :front))
			     (the (face-normal-vector :rear))
			     (half (half (the brick-length)))))
   
   (last-half-brick-center!
     ()
     (translate-along-vector (theo (the mortar-joint last) (face-center :rear))
			     (the (face-normal-vector :rear))
			     (half (half (the brick-length))))))
 			     
  
  :objects
  ((full-brick :type 'box
	       :sequence (:size (the number-of-full-bricks))
	       :center (if (= (the-child index) 0)
			   (the first-full-brick-center!)
			   (the (other-full-brick-center! (the-child index))))
	       :length (the brick-length)
	       :height (the brick-height)
	       :width (the brick-width))

   (half-brick :type 'box
	       :sequence (:size (the number-of-half-bricks))
	       :center (cond ((and (= (the-child index) 0)
				   (eq (the front-edge) :full)) (the first-half-brick-center!))
			     ((and (= (the-child index) 0)
				   (eq (the front-edge) :keyed)
				   (eq (the rear-edge) :full)) (the last-half-brick-center!))
			     ((eq (the rear-edge) :full) (the last-half-brick-center!)))
	       :length (half (the brick-length))
	       :height (the brick-height)
	       :width (the brick-width))

   (mortar-joint :type 'box
		 :sequence (:size (the number-of-mortar-joints))
		 :center (if (= (the-child index) 0)
			     (the first-joint-center!)
			  (the (other-joint-center! (the-child index))))   
		 :height (the brick-height)
		 :width (the brick-width)
		 :length (the corrected-joint-width))))



```

---

## building-bom-output.lisp - degrees-to-radians
Source: gornschool-training/t4/resources/source/building-bom-output.lisp
Type: tutorial

```
(defun degrees-to-radians (degrees)
  (div (* degrees pi) 180))



```

---

## building-bom-output.lisp - truss
Source: gornschool-training/t4/resources/source/building-bom-output.lisp
Type: tutorial

```
(define-object truss (box)
  :input-slots
  ((truss-length 2000)
   (truss-height nil)
   (truss-angle 30)

   (beam-width 30)
   (beam-height 50)
   (wall-thickness 3)

   (material-density 7800)
   (tonne-rate 450))

  :computed-slots
  ((length (the truss-length))
   (height (cond ((the truss-height)(the truss-height))
		 ((the truss-angle) (+ (* (half (the truss-length))
					  (tan (degrees-to-radians (the truss-angle))))
				       (the beam-height)))))
   (width (the beam-width))
  
   (truss-front-slope-vector (the (get-slope-vector! :front)))
   (truss-rear-slope-vector (the (get-slope-vector! :rear)))

   (front-slope-length (the (get-slope-length! :front)))
   (rear-slope-length (the (get-slope-length! :rear)))
   
   (front-slope-center (the (get-slope-center! :front)))
   (rear-slope-center (the (get-slope-center! :rear)))
   

   (beam-properties (mapsend (the children) :beam-properties))
   (total-mass (round-to-nearest
		(apply '+
		       (mapcar #'(lambda(a) (getf a :mass-kg))
			       (the beam-properties)))
		0.001))
   (total-cost (round-to-nearest
		(apply '+
		       (mapcar #'(lambda(a) (getf a :cost-gbp))
			       (the beam-properties)))
		0.01))

   ;; messages to support roof cladding sizing and positioning
   (apex-point (inter-line-plane (the rear-slope-construction-line end)
			   (the truss-rear-slope-vector)
			   (the lower-beam center)
				 (the (face-normal-vector :rear))))
   (front-gutter-point (the front-slope-construction-line start))
   (rear-gutter-point (the rear-slope-construction-line start))
   (front-slope-normal (the front-slope-beam (face-normal-vector :top)))
   (rear-slope-normal (the rear-slope-beam (face-normal-vector :top)))
   )

  :functions
  ((get-slope-vector! (beam-side)
		      (let ((v-key (the (get-v-key! beam-side)))
			    (l-key (the (get-l-key! beam-side))))
		      (subtract-vectors (the vertical-beam (edge-center :rear v-key))
					(the lower-beam (edge-center l-key :top)))))
   (get-slope-length! (beam-side)
		      (let ((v-key (the (get-v-key! beam-side)))
			    (l-key (the (get-l-key! beam-side))))
			(3d-distance (the vertical-beam (edge-center :rear v-key))
				     (the lower-beam (edge-center l-key :top)))))
   (get-slope-center!
    (beam-side)
    (let ((pt (case beam-side
		(:front (the front-slope-construction-line center))
		(:rear  (the rear-slope-construction-line center))))
	  (norm-vector (case beam-side
			 (:front (the front-slope-beam (face-normal-vector :bottom)))
			 (:rear (the rear-slope-beam (face-normal-vector :bottom))))))
      (translate-along-vector pt
			      norm-vector
			      (half (the beam-height)))))
   (get-v-key! (beam-side)
	       (case beam-side
		 (:front :top)
		 (:rear :bottom)))
   (get-l-key! (beam-side)
	       (case beam-side
		 (:front :front)
		 (:rear :rear)))

   
   )
   
  
  
  :objects
  ((lower-beam :type 'beam
	       :pass-down (:beam-height :beam-width :wall-thickness :material-density :tonne-rate)
	       :beam-length (the truss-length)
	       :center (translate-along-vector (the (face-center :bottom))
					       (the (face-normal-vector :top))
					       (half (the beam-height))))
   (vertical-beam :type 'beam
		  :pass-down (:beam-height :beam-width :wall-thickness :material-density :tonne-rate)
		  
		  :beam-length (- (the height) (the beam-height))
		  :orientation (alignment :rear (the (face-normal-vector :top))
					  :right (the (face-normal-vector :right)))  
		  :center (translate-along-vector (the lower-beam (face-center :top))
						  (the lower-beam (face-normal-vector :top))
						  (half (the-child beam-length))))
   (front-slope-beam :type 'beam
		     :pass-down (:beam-height :beam-width :wall-thickness :material-density :tonne-rate)
		     :beam-length (the front-slope-length)
		     :center (the front-slope-center)
		     :orientation (alignment :rear (the truss-front-slope-vector)
					     :right (the (face-normal-vector :right))))

   (rear-slope-beam :type 'beam
		    :pass-down (:beam-height :beam-width :wall-thickness :material-density :tonne-rate)
		    :beam-length (the rear-slope-length)
		    :center (the rear-slope-center)
		    :orientation (alignment :rear (the truss-rear-slope-vector)
					    :left (the (face-normal-vector :right))))
   
   )

  :hidden-objects
  ((apex-pt :type 'sphere
	    :radius 5
	    :display-controls (list :color :green)
	    :center (the apex-point))
   (front-slope-construction-line :type 'line
				  :start (the lower-beam (edge-center :front :top))
				  :end (the vertical-beam (edge-center :rear :top)))
   (rear-slope-construction-line :type 'line
				 :start (the lower-beam (edge-center :rear :top))
				 :end (the vertical-beam (edge-center :rear :bottom)))
   
   (front-mid-pt :type 'sphere
		 :display-controls (list :color :blue)
		 :radius 5
		 :center (the front-slope-construction-line center))
   (rear-mid-pt :type 'sphere
		:display-controls (list :color :blue)
		:center (the rear-slope-construction-line center)
		:radius 5)
   (pt-1 :type 'sphere
	 :radius 5
	 :display-controls (list :color :green)
	 :center (the lower-beam (edge-center :rear :top)))
   (pt-2 :type 'sphere
	 :radius 5
	 :display-controls (list :color :red)
	 :center (the vertical-beam (edge-center :rear :bottom)) )

   

   (vector-line :type 'vector-line
		:start-point (the rear-slope-construction-line center)
		:vector (the rear-slope-beam (face-normal-vector :bottom))
		:length 150)
   
   ))


```

---

## building-bom-output.lisp - vector-line
Source: gornschool-training/t4/resources/source/building-bom-output.lisp
Type: tutorial

```
(define-object vector-line (box)
  :input-slots
  ((start-point (make-point 0 0 0))
   (vector (make-vector 1 0 1))
   (length 50)
   (width 1))
  :computed-slots
  ((height (div (the length) 5)))
  :objects
  ((v-line :type 'line
	   :start (the start-point)
	   :display-controls (list :color :red)
	   :end (translate-along-vector (the start-point)
					(the vector)
					(the length)))
   (arrow :type 'cone
	  :radius-1 0
	  :radius-2 (div (the length) 50)
	  :length (div (the length) 5)
	  :display-controls (list :color :red)
	  :center (translate-along-vector (the v-line end)
					  (the vector)
					  (half (the-child length)))
	  :orientation (alignment :front (the vector)))))

			 
		  

```

---

## building-bom-output.lisp - beam
Source: gornschool-training/t4/resources/source/building-bom-output.lisp
Type: tutorial

```
(define-object beam (box)
  :input-slots
  ((beam-length 1000)
   (beam-width 40)
   (beam-height 50)
   (wall-thickness 2)
   (material-density 7800)
   (tonne-rate 500))

  :computed-slots
  ((length (the beam-length))
   (width (the beam-width))
   (height (the beam-height))

   (beam-volume (- (the outer volume) (the inner volume)))
   (beam-mass (* (div (the beam-volume) 1000000000) (the material-density)))
   (beam-cost (* (the tonne-rate) (div (the beam-mass) 1000)))
   (beam-properties (list :volume-mm3 (the beam-volume)
			  :mass-kg (round-to-nearest (the beam-mass) 0.01)
			  :cost-gbp (round-to-nearest (the beam-cost) 0.01)
			  :length-mm (the beam-length)
			  :width-mm (the beam-width)
			  :height-mm (the beam-height)
			  :thickness-mm (the wall-thickness))))
   
  :objects
  ((outer :type 'box
	  :length (the beam-length)
	  :width (the beam-width)
	  :height (the beam-height))

   (inner :type 'box
	  :length (the beam-length)
	  :width (- (the beam-width) (twice (the wall-thickness)))
	  :height (- (the beam-height) (twice (the wall-thickness)))))
  )










```

---

## building-bom-input-output.lisp - header
Source: gornschool-training/t4/resources/source/building-bom-input-output.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## building-bom-input-output.lisp - read-file
Source: gornschool-training/t4/resources/source/building-bom-input-output.lisp
Type: tutorial

```
(defun read-file (file )
  (let ((result))
    (with-open-file (str file :direction :input)
      (do ((line (read-line str nil 'eof)
		 (read-line str nil 'eof)))
	  ((eql line 'eof) result)
	(setq result (append result (list line)))))))


```

---

## building-bom-input-output.lisp - import-building-data
Source: gornschool-training/t4/resources/source/building-bom-input-output.lisp
Type: tutorial

```
(defun import-building-data (file)
  (let* ((raw-data (read-file file))
	 (res (mapcar #'(lambda(a) (glisp:split-regexp "\\s+" a)) raw-data)))
    (mapcan #'(lambda(a) (list
			  (make-keyword (first a))
			  (read-safe-string (second a)))) res)))
       


```

---

## building-bom-input-output.lisp - building-bom
Source: gornschool-training/t4/resources/source/building-bom-input-output.lisp
Type: tutorial

```
(defun building-bom (&key (nominal-height nil) 
		       (nominal-width nil)
		       (nominal-length nil)
		       (roof-angle nil)
		       (input-filename nil)
		       (output-filename nil))
  (let ((obj (make-object 'building
			  :function-nominal-height nominal-height
			  :function-nominal-width nominal-width
			  :function-nominal-length nominal-length
			  :function-truss-angle roof-angle
			  :output-filename output-filename
			  :input-filename input-filename)))
    (if output-filename (theo obj write-bom-file!)
	(theo obj bom-formatted))))


```

---

## building-bom-input-output.lisp - building
Source: gornschool-training/t4/resources/source/building-bom-input-output.lisp
Type: tutorial

```
(define-object building (box)
  :input-slots
  ((function-nominal-height nil)
   (function-nominal-width nil)
   (function-nominal-length nil)
   (function-truss-angle  nil)
   (input-filename nil)
   (output-filename nil)
   
   (brick-height (or (getf (the file-inputs) :brick-height) 45))
   (brick-length (or (getf (the file-inputs) :brick-length) 180))
   (brick-width (or (getf (the file-inputs) :brick-width) 90))
   (mortar-joint-width (or (getf (the file-inputs) :mortar-joint-width) 10))
   (beam-width (or (getf (the file-inputs) :beam-width) 40))
   (beam-height (or (getf (the file-inputs) :beam-height) 50))
   (wall-thickness (or (getf (the file-inputs) :wall-thickness) 3))
   (material-density (or (getf (the file-inputs) :material-density) 7800))
   (roof-overhang (or (getf (the file-inputs) :roof-overhang) 50))
   (cladding-thickness (or (getf (the file-inputs) :cladding-thickness) 10))
   (max-beam-spacing (or (getf (the file-inputs) :max-beam-spacing) 1500))
   )

  :computed-slots
  ((file-inputs (when (the input-filename) (import-building-data (the input-filename))))
   (nominal-height (or (the function-nominal-height)
		       (getf (the file-inputs) :nominal-height)
		       3000))
   (nominal-width (or (the function-nominal-width)
		      (getf (the file-inputs) :nominal-width)
		      3000))
   (nominal-length (or (the function-nominal-length)
		       (getf (the file-inputs) :nominal-length)
		       4000))
   (truss-angle (or (the function-truss-angle)
		    (getf (the file-inputs) :truss-angle)
		    30))

   
   
   (length (the left-wall length))
   (width (the rear-wall length))
   (height (+ (the left-wall height) (the (roof-truss 0) height)))

   (number-of-roof-trusses (let ((trusses (ceiling (the left-wall length) 1500)))
			     (max trusses 2)))

   (truss-spacing (div (- (the left-wall length) (the beam-width))
		       (- (the number-of-roof-trusses) 1)))
   (truss-offsets (let ((res nil))
		    (dotimes (n (the number-of-roof-trusses) (nreverse res))
		      (push (+ (half (the beam-width))
			       (* n (the truss-spacing))) res))))

   (roof-length (+ (the left-wall length) (twice (the roof-overhang))))
   (roof-width (the cladding-thickness))
   (roof-height (let ((apex (the (roof-truss 0) apex-point))
		      (gutter (the (roof-truss 0) front-gutter-point)))
		  (+ (3d-distance apex gutter) (the roof-overhang))))

   ;; building properties
   (walls (remove nil (mapcar #'(lambda(a) (when (typep a 'wall) a)) (the children))))
   (full-bricks (apply '+ (mapsend (the walls) :full-bricks)))
   (half-bricks (apply '+ (mapsend (the walls) :half-bricks)))
   (mortar-volume (apply '+ (mapsend (the walls) :mortar-volume)))
   (cladding-dimensions (list :length (the roof-length)
			      :width (the roof-height)))
   (beam-properties (the (roof-truss 0) beam-properties))
   (beam-qty-by-size (let ((res nil))
		       (dolist (plis (the beam-properties) )
			 (let* ((trusses (the number-of-roof-trusses))
				(l (getf plis :length-mm))
				(p (position l res :key #'(lambda(a) (getf a :length-mm))))
				(qty (when p (getf (nth p res) :qty))))
			   (if p (setf (getf (nth p res) :qty) (+ qty trusses))
			       (setq res (append (list (list :length-mm l :qty trusses)) res)))))
		       (safe-sort res '< :key #'(lambda(a) (getf a :length-mm)))))
		       

   (roof-truss-mass (* (apply '+ (mapcar #'(lambda(a) (getf a :mass-kg))
					 (the beam-properties)))
		       (the number-of-roof-trusses)))

   (building-materials (list :full-bricks (the full-bricks)
			     :half-bricks (the half-bricks)
			     :mortar-volume-m3 (div (the mortar-volume) 1000000000)
			     :beams (the beam-qty-by-size)
			     :roof-cladding (append (the cladding-dimensions) (list :qty 2))))

  (bom-formatted (let* ((bom (the building-materials))
			(cladding (getf bom :roof-cladding))
			(bricks (format nil "Bricks~%======~%  Full Bricks ~a~%  Half Bricks ~a~%" 
					(getf bom :full-bricks) 
					(getf bom :half-bricks)))
			(mortar (format nil "Mortar~%======~%  Volume ~,3f m^3~%" 
					(getf bom :mortar-volume-m3)))
			(l (round-to-nearest (getf cladding :length) 1))
			(w (round-to-nearest (getf cladding :width) 1))
			(roof (format nil "Roof Cladding~%======~%  Qty ~a~%  Dimensions (L x W x T) ~d x ~d x ~d~%" 
				      (getf cladding :qty)
				      l w (the cladding-thickness)))
			(beams (getf (the building-materials) :beams))
			(beams-list (flatten
				     (mapcar #'(lambda(a)
						 (list (getf a :qty) (round-to-nearest (getf a :length-mm) 1)))
					     beams)))
			
			(beams-header (format nil "Beams~%=====~%  Section (H x W x T) ~a x ~a x ~a~%"
						 (the beam-height) (the beam-width) (the wall-thickness)))
			(beam-lengths (format nil "~{  Qty ~a Length ~a~%~}" beams-list)))
		   (format nil "~@{~a~}" bricks mortar roof beams-header beam-lengths))) 
   
		
   )

  :functions
  ((write-bom-file! ()
		    (with-open-file (s (the output-filename) :direction :output
						:if-exists :supersede
						:if-does-not-exist :create)
		      (format t "Exporting the BOM to ~a~%" (the output-filename))
		      (format s "~a" (the bom-formatted))
		      (format t "Exporting complete~%")))



   (get-roof-mid-point! (first-gutter last-gutter last-index)
		       (let*((mid-gutter (midpoint first-gutter last-gutter))
			     (first-apex (the (roof-truss 0) apex-point))
			     (last-apex (the (roof-truss last-index) apex-point))
			     (mid-apex (midpoint first-apex last-apex))
			     (vec (subtract-vectors mid-gutter mid-apex))
			     (mid-edge (translate-along-vector mid-gutter vec (the roof-overhang))))
			 (midpoint mid-apex mid-edge))) )
  
  :objects
  ((left-wall :type 'wall
	      :pass-down (brick-height
			  brick-length
			  brick-width
			  mortar-joint-width)
	      :center (translate-along-vector
		       (translate-along-vector (the (edge-center :bottom :left))
					       (the (face-normal-vector :right))
					       (half (the-child width)))
		       (the (face-normal-vector :top))
		       (half (the-child height)))
	      :wall-length (the nominal-length)
	      :wall-height (the nominal-height))

   (right-wall :type 'wall
	       :pass-down (brick-height
			   brick-length
			   brick-width
			   mortar-joint-width)
	       :center (translate-along-vector
			(translate-along-vector (the (edge-center :bottom :right))
						(the (face-normal-vector :left))
						(half (the-child width)))
			(the (face-normal-vector :top))
			(half (the-child height)))
	       :wall-length (the nominal-length)
	       :wall-height (the nominal-height))

   (rear-wall :type 'wall
	      :pass-down (brick-height
			  brick-length
			  brick-width
			  mortar-joint-width)
	      :center (translate-along-vector
			(translate-along-vector (the (edge-center :bottom :rear))
						(the (face-normal-vector :front))
						(half (the-child width)))
			(the (face-normal-vector :top))
			(half (the-child height)))
	      :orientation (alignment :rear (the (face-normal-vector :right)))
	      :wall-length (the nominal-width)
	      :wall-height (the nominal-height))

   (roof-truss :type 'truss
	       :sequence (:size (the number-of-roof-trusses))
	       :truss-length (the rear-wall length)
	       :center (translate-along-vector
			(translate-along-vector (the (edge-center :front :top))
						(the (face-normal-vector :bottom))
						(half (the-child height)))
			(the (face-normal-vector :rear))
			(nth (the-child index) (the truss-offsets)))
	       :orientation (alignment :rear (the (face-normal-vector :right))
				       )		
	       :pass-down (truss-angle
			   beam-width
			   beam-height
			   wall-thickness
			   material-density))
   
   (roof-cladding-left
    :type 'box
    :length (the roof-length)
    :height (the roof-height)
    :width (the cladding-thickness)
    :orientation (alignment :left (the (roof-truss 0) front-slope-normal))
    :center (let* ((last-index (- (the number-of-roof-trusses) 1))
		   (first-gutter (the (roof-truss 0) front-gutter-point))
		   (last-gutter (the (roof-truss last-index) front-gutter-point))
		   (mid-ctr (the (get-roof-mid-point! first-gutter last-gutter last-index))))
	      (translate-along-vector mid-ctr
				      (the (roof-truss 0) front-slope-normal)
				      (half (the cladding-thickness)))))
   
   (roof-cladding-right :type 'box
			:length (the roof-length)
			:height (the roof-height)
			:width (the cladding-thickness)
			:orientation (alignment :left (the (roof-truss 0) rear-slope-normal))
			 :center (let* ((last-index (- (the number-of-roof-trusses) 1))
					(first-gutter (the (roof-truss 0) rear-gutter-point))
					(last-gutter (the (roof-truss last-index) rear-gutter-point))
					(mid-ctr (the (get-roof-mid-point! first-gutter last-gutter last-index))))
				 (translate-along-vector mid-ctr
							 (the (roof-truss 0) rear-slope-normal)
							 (half (the cladding-thickness)))))
   )

  
		       
  )



```

---

## building-bom-input-output.lisp - wall
Source: gornschool-training/t4/resources/source/building-bom-input-output.lisp
Type: tutorial

```
(define-object wall(box)
  :input-slots
  ((brick-height 45)
   (brick-length 180)
   (brick-width 90)
   (mortar-joint-width 10)
   (wall-length 3700)
   (wall-height 900)
   (first-row :start-full)
   (front-edge :full)
   (rear-edge :full))

  :computed-slots
  ((row-height (+ (the brick-height) (the mortar-joint-width)))
   (number-of-rows (round-to-nearest (div (the wall-height) (the row-height)) 1))
   (actual-wall-height (* (the row-height) (the number-of-rows)))
   
   ;; for the wall-length we need the number of full bricks
   ;; if there are n full bricks then there will be (n-1) mortar joints
   ;; so n*brick-length + n-1*mortar-joint-width = overall-length
   ;; or n(brick-length + mortar-join-width) - mortar-joint-width = overall-length
   ;; or n = (overall-length - mortar-joint-width)/(brick-length + mortar-joint-width)
   (number-of-bricks (round-to-nearest (div (- (the wall-length) (the mortar-joint-width))
					    (+ (the brick-length) (the mortar-joint-width)))
				       1))
   (actual-wall-length (+ (* (the number-of-bricks) (the brick-length))
			  (* (- (the number-of-bricks) 1) (the mortar-joint-width))))

   ;; box inputs - gives the wall bounding box
   (height (the actual-wall-height))
   (width (the brick-width))
   (length (the actual-wall-length))

   (full-bricks (apply '+ (list-elements (the row) (the-element full-bricks))))
   (half-bricks (apply '+ (list-elements (the row) (the-element half-bricks))))
   (mortar-volume (apply '+ (list-elements (the row) (the-element mortar-volume))))
   (mortar-density 2162)
   (mortar-mass (* (the mortar-density) (div (the mortar-volume) 1000000000)))
   )
   
     
  :objects
  ((row :type 'row
     :sequence (:size (the number-of-rows))
     :center (translate-along-vector (the (face-center :bottom))
				     (the (face-normal-vector :top))
				     (+ (half (the-child height))
					(* (the-child index) (the-child height))))
     :height (+ (the brick-height) (the mortar-joint-width))
     :full-bricks-per-row (the number-of-bricks)
     :pass-down (brick-height
		 brick-length
		 brick-width
		 mortar-joint-width
		 length
		 width
		 first-row
		 front-edge
		 rear-edge))))


```

---

## building-bom-input-output.lisp - row
Source: gornschool-training/t4/resources/source/building-bom-input-output.lisp
Type: tutorial

```
(define-object row (box)
  :input-slots
  (full-bricks-per-row
   brick-height
   brick-length
   brick-width
   mortar-joint-width
   first-row
   front-edge
   rear-edge )

  :computed-slots
  ((full-brick-row? (cond ((eq (the first-row) :start-full)
			   (or (zerop (the index)) (evenp (the index))))
			  ((eq (the first-row) :start-half)
			   (not (or (zerop (the index)) (evenp (the index)))))))
		    
   (full-bricks (the bricks-and-mortar full-bricks))
   (half-bricks (the bricks-and-mortar half-bricks))
   (mortar-volume (+ (the bricks-and-mortar mortar-joint-volume)
		     (the mortar-bed volume))))

  :objects
  ((bricks-and-mortar :type 'bricks-and-mortar
		      :height (the brick-height)
		      :center (translate-along-vector (the mortar-bed (face-center :top))
						      (the mortar-bed (face-normal-vector :top))
						      (half (the-child height)))
		      :pass-down (width
				  length
				  full-brick-row?
				  brick-height
				  brick-length
				  brick-width
				  mortar-joint-width
				  full-bricks-per-row
				  front-edge
				  rear-edge))
   (mortar-bed :type 'box
	       :height (the mortar-joint-width)
	       :center (translate-along-vector (the (face-center :bottom))
					       (the (face-normal-vector :top))
					       (half (the-child height)))
	       :pass-down (width
			   length))))


```

---

## building-bom-input-output.lisp - bricks-and-mortar
Source: gornschool-training/t4/resources/source/building-bom-input-output.lisp
Type: tutorial

```
(define-object bricks-and-mortar (box)
  :input-slots
  (full-brick-row?
   full-bricks-per-row
   brick-height
   brick-length
   brick-width
   mortar-joint-width
   front-edge
   rear-edge)

  :computed-slots
  ((first-full-brick-start-point (if (the full-brick-row?)
				     (the (face-center :front))
				     (the (mortar-joint 0) (face-center :rear))))
   
   (first-mortar-joint-start-point (cond ((the full-brick-row?) (the (full-brick 0) (face-center :rear)))
					 ((eq (the front-edge) :full) (the (half-brick 0) (face-center :rear)))
					 ((eq (the front-edge) :keyed) (translate-along-vector (the (face-center :front))
											       (the (face-normal-vector :rear))
											       (half (the brick-length))))))

   (number-of-full-bricks (if (the full-brick-row?)
			      (the full-bricks-per-row)
			      (- (the full-bricks-per-row) 1)))

   (number-of-half-bricks (cond ((the full-brick-row?) 0)
				((and (eq (the front-edge) :full)(eq (the rear-edge) :full)) 2)
				((and (eq (the front-edge) :keyed) (eq (the rear-edge) :full)) 1)
				((and (eq (the front-edge) :full) (eq (the rear-edge) :keyed)) 1)
				((and (eq (the front-edge) :keyed) (eq (the rear-edge) :keyed)) 0)))

   ;; whether or not the ends are :full or :keyed, the number of mortar joints remains the same since the mortar joint
   ;; when it is :keyed is used to connect to the full brick of the other wall
   (number-of-mortar-joints (if (the full-brick-row?)
				(- (the number-of-full-bricks) 1)
				(+ (the number-of-full-bricks) 1)))

   ;; if it isn't a full brick row then there will be an extra joint because one
   ;; full brick is replaced with 2 half bricks so without correcting the
   ;; mortar-joint-width the ends of a full brick row and one starting and
   ;; finishing with half bricks won't align. So we need to correct
   ;; the mortar-joint-width
   (corrected-joint-width (if (the full-brick-row?)
				     (the mortar-joint-width)
				     (let ((total-gap (* (- (the number-of-mortar-joints) 1)
									(the mortar-joint-width))))
				       (div total-gap (the number-of-mortar-joints)))))
   ;; collating the output. We could do this analytically, but for this example we'll use the geometry
   (full-bricks (length (list-elements (the full-brick))))
   (half-bricks (length (list-elements (the half-brick))))
   (mortar-joint-volume (apply '+ (list-elements (the mortar-joint)
						 (the-element volume))))
   )
  
  :functions
  ((first-full-brick-center!
    ()
    (translate-along-vector (the first-full-brick-start-point)
			    (the (face-normal-vector :rear))
			    (half (the brick-length))))
   
   (other-full-brick-center!
    (index)
    ;; if its a full brick row, (full-brick 1) is positioned relative to (joint 0)
    ;; if its a half brick row, (full-brick 1) is positioned relative to (joint 1)
    (let ((ind (if (the full-brick-row?) (- index 1) index)))
	  (translate-along-vector (the (mortar-joint ind) (face-center :rear))
				  (the (face-normal-vector :rear))
				  (half (the brick-length)))))
   
    (first-joint-center!
     ()
     (translate-along-vector (the first-mortar-joint-start-point)
			     (the (face-normal-vector :rear))
			     (half (the corrected-joint-width))))
   (other-joint-center!
    (index)
    ;; if its a full brick row, (joint 1) is positioned relative to (full-brick 1)
    ;; if its a half brick row, (joint 1) is positioned relative to (full-brick 0)
    (let ((ind (if (the full-brick-row?) index (- index 1))))
      (translate-along-vector (the (full-brick ind) (face-center :rear))
			      (the (face-normal-vector :rear))
			      (half (the corrected-joint-width)))))

    (first-half-brick-center!
     ()
     (translate-along-vector (the (face-center :front))
			     (the (face-normal-vector :rear))
			     (half (half (the brick-length)))))
   
   (last-half-brick-center!
     ()
     (translate-along-vector (theo (the mortar-joint last) (face-center :rear))
			     (the (face-normal-vector :rear))
			     (half (half (the brick-length))))))
 			     
  
  :objects
  ((full-brick :type 'box
	       :sequence (:size (the number-of-full-bricks))
	       :center (if (= (the-child index) 0)
			   (the first-full-brick-center!)
			   (the (other-full-brick-center! (the-child index))))
	       :length (the brick-length)
	       :height (the brick-height)
	       :width (the brick-width))

   (half-brick :type 'box
	       :sequence (:size (the number-of-half-bricks))
	       :center (cond ((and (= (the-child index) 0)
				   (eq (the front-edge) :full)) (the first-half-brick-center!))
			     ((and (= (the-child index) 0)
				   (eq (the front-edge) :keyed)
				   (eq (the rear-edge) :full)) (the last-half-brick-center!))
			     ((eq (the rear-edge) :full) (the last-half-brick-center!)))
	       :length (half (the brick-length))
	       :height (the brick-height)
	       :width (the brick-width))

   (mortar-joint :type 'box
		 :sequence (:size (the number-of-mortar-joints))
		 :center (if (= (the-child index) 0)
			     (the first-joint-center!)
			  (the (other-joint-center! (the-child index))))   
		 :height (the brick-height)
		 :width (the brick-width)
		 :length (the corrected-joint-width))))



```

---

## building-bom-input-output.lisp - degrees-to-radians
Source: gornschool-training/t4/resources/source/building-bom-input-output.lisp
Type: tutorial

```
(defun degrees-to-radians (degrees)
  (div (* degrees pi) 180))



```

---

## building-bom-input-output.lisp - truss
Source: gornschool-training/t4/resources/source/building-bom-input-output.lisp
Type: tutorial

```
(define-object truss (box)
  :input-slots
  ((truss-length 2000)
   (truss-height nil)
   (truss-angle 30)

   (beam-width 30)
   (beam-height 50)
   (wall-thickness 3)

   (material-density 7800)
   (tonne-rate 450))

  :computed-slots
  ((length (the truss-length))
   (height (cond ((the truss-height)(the truss-height))
		 ((the truss-angle) (+ (* (half (the truss-length))
					  (tan (degrees-to-radians (the truss-angle))))
				       (the beam-height)))))
   (width (the beam-width))
  
   (truss-front-slope-vector (the (get-slope-vector! :front)))
   (truss-rear-slope-vector (the (get-slope-vector! :rear)))

   (front-slope-length (the (get-slope-length! :front)))
   (rear-slope-length (the (get-slope-length! :rear)))
   
   (front-slope-center (the (get-slope-center! :front)))
   (rear-slope-center (the (get-slope-center! :rear)))
   

   (beam-properties (mapsend (the children) :beam-properties))
   (total-mass (round-to-nearest
		(apply '+
		       (mapcar #'(lambda(a) (getf a :mass-kg))
			       (the beam-properties)))
		0.001))
   (total-cost (round-to-nearest
		(apply '+
		       (mapcar #'(lambda(a) (getf a :cost-gbp))
			       (the beam-properties)))
		0.01))

   ;; messages to support roof cladding sizing and positioning
   (apex-point (inter-line-plane (the rear-slope-construction-line end)
			   (the truss-rear-slope-vector)
			   (the lower-beam center)
				 (the (face-normal-vector :rear))))
   (front-gutter-point (the front-slope-construction-line start))
   (rear-gutter-point (the rear-slope-construction-line start))
   (front-slope-normal (the front-slope-beam (face-normal-vector :top)))
   (rear-slope-normal (the rear-slope-beam (face-normal-vector :top)))
   )

  :functions
  ((get-slope-vector! (beam-side)
		      (let ((v-key (the (get-v-key! beam-side)))
			    (l-key (the (get-l-key! beam-side))))
		      (subtract-vectors (the vertical-beam (edge-center :rear v-key))
					(the lower-beam (edge-center l-key :top)))))
   (get-slope-length! (beam-side)
		      (let ((v-key (the (get-v-key! beam-side)))
			    (l-key (the (get-l-key! beam-side))))
			(3d-distance (the vertical-beam (edge-center :rear v-key))
				     (the lower-beam (edge-center l-key :top)))))
   (get-slope-center!
    (beam-side)
    (let ((pt (case beam-side
		(:front (the front-slope-construction-line center))
		(:rear  (the rear-slope-construction-line center))))
	  (norm-vector (case beam-side
			 (:front (the front-slope-beam (face-normal-vector :bottom)))
			 (:rear (the rear-slope-beam (face-normal-vector :bottom))))))
      (translate-along-vector pt
			      norm-vector
			      (half (the beam-height)))))
   (get-v-key! (beam-side)
	       (case beam-side
		 (:front :top)
		 (:rear :bottom)))
   (get-l-key! (beam-side)
	       (case beam-side
		 (:front :front)
		 (:rear :rear)))

   
   )
   
  
  
  :objects
  ((lower-beam :type 'beam
	       :pass-down (:beam-height :beam-width :wall-thickness :material-density :tonne-rate)
	       :beam-length (the truss-length)
	       :center (translate-along-vector (the (face-center :bottom))
					       (the (face-normal-vector :top))
					       (half (the beam-height))))
   (vertical-beam :type 'beam
		  :pass-down (:beam-height :beam-width :wall-thickness :material-density :tonne-rate)
		  
		  :beam-length (- (the height) (the beam-height))
		  :orientation (alignment :rear (the (face-normal-vector :top))
					  :right (the (face-normal-vector :right)))  
		  :center (translate-along-vector (the lower-beam (face-center :top))
						  (the lower-beam (face-normal-vector :top))
						  (half (the-child beam-length))))
   (front-slope-beam :type 'beam
		     :pass-down (:beam-height :beam-width :wall-thickness :material-density :tonne-rate)
		     :beam-length (the front-slope-length)
		     :center (the front-slope-center)
		     :orientation (alignment :rear (the truss-front-slope-vector)
					     :right (the (face-normal-vector :right))))

   (rear-slope-beam :type 'beam
		    :pass-down (:beam-height :beam-width :wall-thickness :material-density :tonne-rate)
		    :beam-length (the rear-slope-length)
		    :center (the rear-slope-center)
		    :orientation (alignment :rear (the truss-rear-slope-vector)
					    :left (the (face-normal-vector :right))))
   
   )

  :hidden-objects
  ((apex-pt :type 'sphere
	    :radius 5
	    :display-controls (list :color :green)
	    :center (the apex-point))
   (front-slope-construction-line :type 'line
				  :start (the lower-beam (edge-center :front :top))
				  :end (the vertical-beam (edge-center :rear :top)))
   (rear-slope-construction-line :type 'line
				 :start (the lower-beam (edge-center :rear :top))
				 :end (the vertical-beam (edge-center :rear :bottom)))
   
   (front-mid-pt :type 'sphere
		 :display-controls (list :color :blue)
		 :radius 5
		 :center (the front-slope-construction-line center))
   (rear-mid-pt :type 'sphere
		:display-controls (list :color :blue)
		:center (the rear-slope-construction-line center)
		:radius 5)
   (pt-1 :type 'sphere
	 :radius 5
	 :display-controls (list :color :green)
	 :center (the lower-beam (edge-center :rear :top)))
   (pt-2 :type 'sphere
	 :radius 5
	 :display-controls (list :color :red)
	 :center (the vertical-beam (edge-center :rear :bottom)) )

   

   (vector-line :type 'vector-line
		:start-point (the rear-slope-construction-line center)
		:vector (the rear-slope-beam (face-normal-vector :bottom))
		:length 150)
   
   ))


```

---

## building-bom-input-output.lisp - vector-line
Source: gornschool-training/t4/resources/source/building-bom-input-output.lisp
Type: tutorial

```
(define-object vector-line (box)
  :input-slots
  ((start-point (make-point 0 0 0))
   (vector (make-vector 1 0 1))
   (length 50)
   (width 1))
  :computed-slots
  ((height (div (the length) 5)))
  :objects
  ((v-line :type 'line
	   :start (the start-point)
	   :display-controls (list :color :red)
	   :end (translate-along-vector (the start-point)
					(the vector)
					(the length)))
   (arrow :type 'cone
	  :radius-1 0
	  :radius-2 (div (the length) 50)
	  :length (div (the length) 5)
	  :display-controls (list :color :red)
	  :center (translate-along-vector (the v-line end)
					  (the vector)
					  (half (the-child length)))
	  :orientation (alignment :front (the vector)))))

			 
		  

```

---

## building-bom-input-output.lisp - beam
Source: gornschool-training/t4/resources/source/building-bom-input-output.lisp
Type: tutorial

```
(define-object beam (box)
  :input-slots
  ((beam-length 1000)
   (beam-width 40)
   (beam-height 50)
   (wall-thickness 2)
   (material-density 7800)
   (tonne-rate 500))

  :computed-slots
  ((length (the beam-length))
   (width (the beam-width))
   (height (the beam-height))

   (beam-volume (- (the outer volume) (the inner volume)))
   (beam-mass (* (div (the beam-volume) 1000000000) (the material-density)))
   (beam-cost (* (the tonne-rate) (div (the beam-mass) 1000)))
   (beam-properties (list :volume-mm3 (the beam-volume)
			  :mass-kg (round-to-nearest (the beam-mass) 0.01)
			  :cost-gbp (round-to-nearest (the beam-cost) 0.01)
			  :length-mm (the beam-length)
			  :width-mm (the beam-width)
			  :height-mm (the beam-height)
			  :thickness-mm (the wall-thickness))))
   
  :objects
  ((outer :type 'box
	  :length (the beam-length)
	  :width (the beam-width)
	  :height (the beam-height))

   (inner :type 'box
	  :length (the beam-length)
	  :width (- (the beam-width) (twice (the wall-thickness)))
	  :height (- (the beam-height) (twice (the wall-thickness)))))
  )










```

---

## assembly.lisp - header
Source: gornschool-training/t4/source/assembly.lisp
Type: tutorial

```
(in-package :training-4)

(defparameter *publish-prefix* "t4")  


```

---

## assembly.lisp - assembly
Source: gornschool-training/t4/source/assembly.lisp
Type: tutorial

```
(define-object assembly (base-tutorial-sheet)
  :input-slots
  ((getting-started-url nil)
   (tutorial-name "File I/O"))

  :computed-slots
  ((introduction (with-cl-who-string ()
		   (:p "A basic requirement for most applications is to be able to interface to the outside world. This may be done with a User Interface, but it may also be done by reading inputs from a file and/or writing outputs to a file"
		       (:p "This tutorial covers the basics of file i/o")))))
  

  :objects
  ((file-io-basics :type 'file-io-basics
		   :pass-down (page-objects)
		   :publish-prefix *publish-prefix*
		   :page 1
		   :getting-started-url (the getting-started-url)
		   :page-title "File IO Basics")
   
   (writing-to-a-file :type 'writing-to-a-file
		      :pass-down (page-objects)
		      :publish-prefix *publish-prefix*
		      :page 2
		      :page-title "Writing to a file"
		      :resources (list "file-output.lisp"))
   
   (reading-from-a-file :type 'reading-from-a-file
			:pass-down (page-objects)
			:publish-prefix *publish-prefix*
			:page 3
			:page-title "Reading from a file"
			:resources (list "read-input.lisp" "report.txt"))
   
   (example-1 :type 'file-io-example-1
	      :pass-down (page-objects)
	      :publish-prefix *publish-prefix*
	      :page 4
	      :getting-started-url (the getting-started-url)
	      :page-title "Example - outputting the Tutorial 2 Building BoM to a file"
	      :resources (list "building-bom-output.lisp") )

   (example-2 :type 'file-io-example-2
	      :pass-down (page-objects)
	      :publish-prefix *publish-prefix*
	      :page 5
	      :getting-started-url (the getting-started-url)
	      :read-from-file-url (the reading-from-a-file url)
	      :page-title "Example - instantiating the Tutorial 2 Building example from a file input"
	      :resources (list "building-bom-input-output.lisp"))

   )
  )









  
  
  
  
  

```

---

## reading-from-a-file.lisp - header
Source: gornschool-training/t4/source/reading-from-a-file.lisp
Type: tutorial

```
(in-package :training-4)


```

---

## reading-from-a-file.lisp - reading-from-a-file
Source: gornschool-training/t4/source/reading-from-a-file.lisp
Type: tutorial

```
(define-object reading-from-a-file (base-training-sheet)

  :computed-slots
  ((index-words (list "with-open-file" "read-line" "regular expressions" "split-regexp" "mapcan" "mapcar" "do" "cond" "read-safe-string"))

   (code-1 (list "
```

---

## reading-from-a-file.lisp - read-file
Source: gornschool-training/t4/source/reading-from-a-file.lisp
Type: tutorial

```
(defun read-file (file))"
		 "   (let ((result))"
		 "      (with-open-file (str file :direction :input)"
		 "            (do ((line (read-line str nil 'eof)"
		 "		         (read-line str nil 'eof)))"
		 "	          ((eql line 'eof) result)"
		 "	(setq result (append result (list line)))))))"))

   (code-2 (list "
```

---

## reading-from-a-file.lisp - import-data
Source: gornschool-training/t4/source/reading-from-a-file.lisp
Type: tutorial

```
(defun import-data (file)"
		 "(let*"
		 "  ((raw-data (read-file file))"
		 "   (res (mapcar #'(lambda(a) (glisp:split-regexp \"\\\\s+\" a)) raw-data))"
		 "   (res-1 (mapcan #'(lambda(a) (list (make-keyword (second a)) (third a))) res))"
		 "   (keywords (remove nil res-1 :key #'(lambda(a) (keywordp a))))"
		 "   (r nil))"
		 " (dolist (k keywords r)"
		 "    (cond ((or (eq k :width) (eq k :length) (eq k :height))"
		 "           (setq r (append r (list k (read-safe-string (getf res-1 k))))))"
		 "          ((eq k :center)"
		 "           (let ((co-ord (glisp:split-regexp \",\" (getf res-1 k))))"
		 "	       (setq r (append"
		 "                       r"
		 "                       (list k (make-point (read-safe-string (first co-ord))"
		 "                                           (read-safe-string (second co-ord))"
		 "                                           (read-safe-string (third co-ord))))))))))))"))
   
   (repl-1 (list (list :command "(read-file \"c:/temp/report.txt\")"
		       :output "(\"Box Width 3\" \"Box Length 6\" \"Box Height 4\" \"Box Center 0.0,0.0,0.0\" \"Box Volume 72\")")))
   (repl-2 (list (list :command "(import-data \"c:/temp/report.txt\")"
		       :output "(:WIDTH 3 :LENGTH 6 :HEIGHT 4 :CENTER #(0.0 0.0 0.0))")))

   (body-content (with-cl-who-string()
		   (:div :class "main-page-container" :style "grid-template-columns: 700px auto;"
			 (:div :class "main-page-item"
			       (:p "Compared with writing to a file, reading from a file is a much more involved process. As with writing a file, a stream is opened and connected to the file. But then the file contents need to be read, line by line, until the end of the file. In general we will not know how many lines are to be read. Once the data has been read, it needs to be converted or conditioned into a format that the application can use. This implies "
				   (:ul (:li "We know something about the format of the data being supplied")
				        (:li "The data is supplied in that known format"))
				   "It is often the achilles heel of many applications which take data from the outside world and effort in making this interface as robust as possible is always time well spent")
			       (:p "A general purpose function to read data from a text file may look something like this")
			       (str (code-example (the code-1)))
			       (:p "We have introduced the use of the CL macro "
				   (:span :class "macro" "do")" as part of the body of expressions wrapped in "
				   (:span :class "macro" "with-open-file"))
			       (:p (:span :class "macro" "do")" accepts a list of variable expressions in the form "
				   (:em "(variable initial-iteration subsequent-iterations")"). The "
				   (:em "initial-iteration")" is an expression which is evaluated on the first iteration and "
				   (:em "subsequent-iterations")" is an expression evaluated on all subsequent iterations. In our case we are using the CL function "
				   (:span :class "function" "read-line")" The first argument is the stream which comes from "
				   (:span :class "macro" "with-open-file")", the second argument determines if an end of file error should be signalled, in this case nil, and the third argument is the value returned when end of file is encountered. The output of read-line is set to the variable line which is appended into the result variable. The second list in "
				   (:span :class "macro" "do")" defines the condition to be met to end iteration (in this case, the value of line is the end of file value) and the value to be returned when this condition is T, in this case "
				   (:span :class "slot" "result"))
			       (:p "Evaluating this function against the file created in the previous topic gives the following")
			       (str (repl-example (the repl-1)))
			       (:p "The next task when reading a data file is generally parsing it to convert it into a format that our application can use. The following function, making use of our read-file function will do that and return an appropriate plist")
			       (str (code-example (the code-2)))
			       (:p "Working through the function"
				   (:ul (:li "We first read the data into a variable "(:em (:b "raw-data"))". This is a list of strings, where each string is a separate line from the file")
				        (:li "The we use the function "
					     (:span :class "function" "glisp:split-regexp")" to split each string into separate words, breaking where there is one or more whitespaces (the regular-expression \"\\\\s+\")")
				        (:li "Because we know the structure of the data, we can comfortably discarded the first word from each line and convert the second word into a keyword. By using "(:span :class "function" "mapcan")" the local variable "(:em (:b "res-1")" becomes a plist of keyword and value, although each value is still a string")
					     (:li "We need to convert the values into the correct data types, but it's different depending on what value we are considering. "
					          (:span :class "general-keyword" "length")", "
					          (:span :class "general-keyword" "width")" and "
					          (:span :class "general-keyword" "height")" will all be numbers, whilst "
					          (:span :class "general-keyword" "center")" will be a point (vector).")
				             (:li "Finally, we use the keywords to determine how to process the data and return a plist. A key point to note here is that we are not using the CL function "(:span :class "function" "read-string")" as it does have some security vulnerabilities; to prevent this we use the GendL function "(:span :class "function" "read-safe-string")" to convert the numbers as strings into real numbers"))
				        "Evaluating the function in the REPL we get")
				   (str (repl-example (the repl-2)))
				   (:p "One final point to note in the example code; there is virtually no error handling. Given the data file is one we generated automatically and are therefore in full control of, this is probably acceptable, but in general the interface functions like shown above need to be extreemly robust")))
			 (:div :class "main-page-item"
			       (:h2 "Resources")
			       (str (the resource-links))))))))


```

---

## writing-to-a-file.lisp - header
Source: gornschool-training/t4/source/writing-to-a-file.lisp
Type: tutorial

```
(in-package :training-4)


```

---

## writing-to-a-file.lisp - writing-to-a-file
Source: gornschool-training/t4/source/writing-to-a-file.lisp
Type: tutorial

```
(define-object writing-to-a-file (base-training-sheet)


  :computed-slots
  ((index-words (list "with-open-file"))
   (repl-1 (list (list :command (list "(with-open-file (s \"c:/temp/my-file.txt\""
				      "                   :direction :output"
				      "                   :if-exists :supersede"
				      "                   :if-does-not-exist :create)"
				      "      (format s \"Line 1 of text~%\")"
				      "      (format s \"Line 1 of text~%\")")
		       :output "NIL")))
   (code-1 (list "Line 1 of text"
		 "Line 1 of text"))

   (code-2 (list "
```

---

## writing-to-a-file.lisp - my-box
Source: gornschool-training/t4/source/writing-to-a-file.lisp
Type: tutorial

```
(define-object my-box (box)"
		 " :input-slots"
		 " ((output-filename \"c:/temp/my-box-report\"))"
		 ""
		 " :computed-slots"
		 " ((width 3)"
		 "  (height 4)"
		 "  (length 6))"
		 ""
		 " :functions"
		 " ((write-report!()"
		 "	(with-open-file (s (the output-filename)"
		 "			:direction :output"
		 "			:if-exists :supersede"
		 "			:if-does-not-exist :create)"
		 "	(let ((i 0))"
		 "	  (format t \"Begining output to ~a~%\" (the output-filename))"
		 "	  (format s \"Box Width ~a~%\" (the width))"
		 "	  (incf i)"
		 "	  (format s \"Box Length ~a~%\" (the length))"
		 "	  (incf i)"
		 "	  (format s \"Box Height ~a~%\" (the height))"
		 "	  (incf i)"
		 "	  (format s \"Box Center ~@{~,1f~^,~}~%\""
		 "	                    (get-x (the center))"
		 "	                    (get-y (the center))"
		 "	                    (get-z (the center)))"
		 "	  (incf i)"
		 "	  (format s \"Box Volume ~a~%\" (the volume))"
		 "	  (incf i)"
		 "	  (format t \"Output written (~a line~:p)~%\" i)))))"
		 "  ))"))

   (code-3 (list "
```

---

## writing-to-a-file.lisp - output-report
Source: gornschool-training/t4/source/writing-to-a-file.lisp
Type: tutorial

```
(defun output-report (fname)"
		 "        (let ((obj (make-object 'my-box"
		 "			           :output-filename fname)))"
		 "   (theo obj write-report!)))"))
   (repl-2 (list (list :command "(output-report \"c:/temp/report.txt\")"
		 :output "Begining output to c:/temp/report.txt")
	   (list :output "Output written (5 lines)")
		 (list :output "NIL")))
   
   (code-4 (list "Box Width 3"
		 "Box Length 6"
		 "Box Height 4"
		 "Box Center 0.0,0.0,0.0"
		 "Box Volume 72"))		

   (body-content (with-cl-who-string()
		   (:div :class "main-page-container" :style "grid-template-columns: 700px auto;"
			 (:div :class "main-page-item"
			       (:p "In the previous topic, we concluded by recomending that the CL macro " (:span :class "macro" "with-open-file")" was used for any file input or output. So lets have a look at using this macro to write to a file")
			       (:p "To write output to a file C:/temp/my-file.txt using " (:span :class "macro" "with-open-file")" woud look like the following")
			       (str (repl-example (the repl-1)))
			       (:p "which will produce the following content in the file c:/temp/my-file.txt")
			       (str (code-example (the code-1)))
			       (:p "note that the stream "(:b "s")" only exists within the context of the " (:span :class "macro" "with-open-file")" body of expressions")
			       (:p "In many cases, file output is implemented as a :function in a GendL object, although it is perfectly feasible to implement it as a side effect of a :computed-slot. Consider the following simple example")
			       (str (code-example (the code-2)))
			       (:p "Here, as well as sending output to the stream connected to the file, we are also sending output to the REPL using the stream T (which defaults to "(:b "*standard-output*")". This is often a useful technique to assist with development and/or debugging. We could also write a small function which takes a filename as an input and executes the file output by calling the :function Write-report!")
			       (str (code-example (the code-3)))
			       (:p "And then in the REPL we would see this")
			       (str (repl-example (the repl-2)))
			       (:p "Resulting in the file c:/temp/my-file.txt being written with the following content")
			       (str (code-example (the code-4))))
			 (:div :class "main-page-item"
			       (:h2 "Resources")
			       (str (the resource-links))))))))
				  


```

---

## file-io-basics.lisp - header
Source: gornschool-training/t4/source/file-io-basics.lisp
Type: tutorial

```
(in-package :training-4)


```

---

## file-io-basics.lisp - file-io-basics
Source: gornschool-training/t4/source/file-io-basics.lisp
Type: tutorial

```
(define-object file-io-basics (base-training-sheet)
  :input-slots
  (getting-started-url)
  
  :computed-slots
  ((repl-1 (list (list :command (list "(defparameter *my-stream* (open \"c:/temp/missing-file.txt\""
				      "                                :direction :output"
				      "                                :if-does-not-exist :create")
		       :output "*MY-STREAM*")))
   (repl-2 (list(list :command "(format *MY-STREAM* \"Writing to a file for the first time\")"
		      :output "NIL")))
   (repl-3 (list(list :command "(close *MY-STREAM*)"
		      :output "T")))
   (code-1 (list "Writing to a file for the first time"))
   (body-content (with-cl-who-string()
		   (:div :class "main-page-container" :style "grid-template-columns: 600px auto;"
			 (:div :class "main-page-item"
			       (:h3 "The basics of file I/O")
			       (:p "To read and write from or to a file, we do this via a "
				   (:em "stream") " which is connected to the file. The most basic mechanism for connecting a stream to a file is by using the CL function "
				   (:span :class "function" "open")", which takes the following arguments"
				   (:ul (:li "filename - required")
					(:li "keyword argument "
					     (:span :class "general-keyword" ":direction")" which can be either "
					     (:span :class "general-keyword" ":input")" or "
					     (:span :class "general-keyword" ":output")" or "
					     (:span :class "general-keyword" ":io")" (both directions)")
					(:li "keyword argument "
					     (:span :class "general-keyword" ":if-exists")" - what to do if the file exists - the most common values being "
					     (:span :class "general-keyword" ":overwrite")", "
					     (:span :class "general-keyword" ":append")" or "
					     (:span :class "general-keyword" ":supersede")". The difference between "
					     (:span :class "general-keyword" ":overwrite")" and "
					     (:span :class "general-keyword" ":supersede")" is the former causes the existing file to be modified at the beginning of the output, whilst the latter delays the deletion of the original file until the stream is closed")
					(:li "keyword argument "
					     (:span :class "general-keyword" ":if-does-not-exist")" - the most common value is "
					     (:span :class "general-keyword" ":create")" where the "
					     (:span :class "general-keyword" ":direction")" is "
					     (:span :class "general-keyword" ":output")" or "
					     (:span :class "general-keyword" ":io")" and "
					     (:span :class "general-keyword" ":if-exists")" is neither "
					     (:span :class "general-keyword" ":overwrite")" nor "
					     (:span :class "general-keyword" ":append")))
				   "There are other keyword arguments and valid values, the above are just the most commonly used.")
			       (str (repl-example (the repl-1)))
			       (:p "Once the file is open we can then write to that stream, for example using the format function we covered in the "
				   (if (the getting-started-url)
				       (htm ((:a :href (the getting-started-url))"Getting Started with GendL"))
				       (htm "Getting Started with GendL "))
				   "tutorial.")
			       (str (repl-example (the repl-2)))
			       (:p "However, whilst the file has been created, we have to close the stream before the the output is seen in the file.")
			       (str (repl-example (the repl-3)))
			       (str (code-example (the code-1)))
			       (:h3 "Avoiding issues by using with-open-file")
			       (:p "And this leads on to one of the potential issues with "
				   (:span :class "function" "open")", the programmer has to remember to "
				   (:em (:b "always close the stream that gets opened"))", and ensure it gets closed even in the event of errors. When streams are left open, results may be unpredictable and performance often suffers.")
			       (:p "For this reason, the macro "
				   (:span :class "macro" "with-open-file")" is much preferred. It takes a symbol representing a stream and a path to the file as required arguments plus all of the keyword arguments used by "
				   (:span :class "function" "open")", plus a body of expressions. "
				   (:span :class "macro" "with-open-file")" opens the stream, evaluates the body of expressions (which will invariably refer to the stream), and then closes the stream. The macro wraps all of the code in the CL special operator "
				   (:span :class "special-operator" "unwind-protect") " to ensure that the stream will always be closed even in the event of errors being encountered.")))))))

				  
				  

```

---

## file-io-example-2.lisp - header
Source: gornschool-training/t4/source/file-io-example-2.lisp
Type: tutorial

```
(in-package :training-4)


```

---

## file-io-example-2.lisp - file-io-example-2
Source: gornschool-training/t4/source/file-io-example-2.lisp
Type: tutorial

```
(define-object file-io-example-2 (base-training-sheet)
  :input-slots
  (getting-started-url
   read-from-file-url)
  
  :computed-slots
  ((hint-1 nil :settable)
   (hint-2 nil :settable)
   (hint-3 nil :settable)
   (index-words (list "with-open-file" "read-safe-string"))
   
   (body-content (with-cl-who-string ()
		   (:div :class "main-page-container" :style "grid-template-columns: 700px auto;"
			 (:div :class "main-page-item"
			       (str (the start-section main-div))
			       (str (the hint-1-section main-div))
			       (str (the hint-2-section main-div))
			       (str (the hint-3-section main-div)))
			 (:div :class "main-page-item"
			       (:h2 "Resources")
			       (str (the resource-links))))))
   
   (code-1 (list "nominal-height 3000"
		 "nominal-width 3000"
		 "nominal-length 4000"
		 "brick-height 45"
		 "brick-length 180"
		 "brick-width 90"
		 "mortar-joint-width 10"
		 "truss-angle 30"
		 "beam-width 40"
		 "beam-height 50"
		 "wall-thickness 3"
		 "material-density 7800"
		 "roof-overhang 50"
		 "cladding-thickness 10"
		 "max-beam-spacing 1500"))
   
   (code-2 (list "
```

---

## file-io-example-2.lisp - import-building-data
Source: gornschool-training/t4/source/file-io-example-2.lisp
Type: tutorial

```
(defun import-building-data (file)"
		 "  (let* ((raw-data (read-file file))"
		 "         (res (mapcar #'(lambda(a) (glisp:split-regexp \"\\\\s+\" a)) raw-data)))"
		 "    (mapcan #'(lambda(a) (list "
		 "                          (make-keyword (first a))" 
		 "                          (read-safe-string (second a)))) res)))"))

   (repl-1 (list (list :command "(import-building-data \"c:/temp/building-input.txt\")"
                       
		       :output (list "(:NOMINAL-HEIGHT 3000 :NOMINAL-WIDTH 3000 :NOMINAL-LENGTH 4000" 
				     ":BRICK-HEIGHT 45 :BRICK-LENGTH 180 :BRICK-WIDTH 90 "
				     ":MORTAR-JOINT-WIDTH 10 :TRUSS-ANGLE 30 :BEAM-WIDTH 40 "
				     ":BEAM-HEIGHT 50 :WALL-THICKNESS 3 :MATERIAL-DENSITY 7800"
				     ":ROOF-OVERHANG 50 :CLADDING-THICKNESS 10 :MAX-BEAM-SPACING 1500)"))))
   
   (code-3 (list "
```

---

## file-io-example-2.lisp - building-bom
Source: gornschool-training/t4/source/file-io-example-2.lisp
Type: tutorial

```
(defun building-bom (&key (nominal-height nil)"
		 "                          (nominal-width nil)"
		 "                          (nominal-length nil)"
		 "                          (roof-angle nil)"
		 "                          (input-filename nil)"
		 "                          (output-filename nil))"
		 "  (let ((obj (make-object 'building"
		 "                          :function-nominal-height nominal-height"
		 "                          :function-nominal-width nominal-width"
		 "                          :function-nominal-length nominal-length"
		 "                          :function-truss-angle roof-angle"
		 "                          :output-filename output-filename"
		 "                          :input-filename input-filename)))"
		 "   (if output-filename (theo obj write-bom-file!)"
		 "       (theo obj bom-formatted))))"))

   (code-4 (list "
```

---

## file-io-example-2.lisp - building
Source: gornschool-training/t4/source/file-io-example-2.lisp
Type: tutorial

```
(define-object building (box)"
		 "   :input-slots"
		 "   ((function-nominal-height nil)"
		 "    (function-nominal-width nil)"
		 "    (function-nominal-length nil)"
		 "    (function-truss-angle  nil)"
		 "    (input-filename nil)"
		 "    ..."
		 "    ...)"
		 ""
		 "   :computed-slots"
		 "   ((nominal-height (or (the function-nominal-height) 3000))"
		 "    (nominal-width (or (the function-nominal-width) 3000))"
		 "    (nominal-length (or (the function-nominal-length) 2000))"
		 "    (truss-angle (or (the function-truss-angle) 30))"
		 "    ..."
		 "    ...)"
		 "..."
		 ")"))
   (code-5 (list "
```

---

## file-io-example-2.lisp - building
Source: gornschool-training/t4/source/file-io-example-2.lisp
Type: tutorial

```
(define-object building (box)"
		 "  :computed-slots"
		 "  ((file-inputs (when (the input-filename) (import-building-data (the input-filename))))"
		 "   (nominal-height (or (the function-nominal-height)"
		 "                       (getf (the file-inputs) :nominal-height)"
		 "                       3000))"
		 "   (nominal-width (or (the function-nominal-width)"
		 "                      (getf (the file-inputs) :nominal-width)"
		 "                      3000))"
		 "   (nominal-length (or (the function-nominal-length)"
		 "                       (getf (the file-inputs) :nominal-length)"
		 "                       4000))"
		 "   (truss-angle (or (the function-truss-angle)"
		 "                    (getf (the file-inputs) :truss-angle)"
		 "                    30))"
		 "   ..."
		 "   ...)"
		 ")"))

   (code-6 (list "
```

---

## file-io-example-2.lisp - building
Source: gornschool-training/t4/source/file-io-example-2.lisp
Type: tutorial

```
(define-object building (box)"
		 "  :input-slots"
		 "  (..."
		 "   ..."
		 "   (brick-height (or (getf (the file-inputs) :brick-height) 45))"
		 "   (brick-length (or (getf (the file-inputs) :brick-length) 180))"
		 "   (brick-width (or (getf (the file-inputs) :brick-width) 90))"
		 "   (mortar-joint-width (or (getf (the file-inputs) :mortar-joint-width) 10))"
		 "   (beam-width (or (getf (the file-inputs) :beam-width) 40))"
		 "   (beam-height (or (getf (the file-inputs) :beam-height) 50))"
		 "   (wall-thickness (or (getf (the file-inputs) :wall-thickness) 3))"
		 "   (material-density (or (getf (the file-inputs) :material-density) 7800))"
		 "   (roof-overhang (or (getf (the file-inputs) :roof-overhang) 50))"
		 "   (cladding-thickness (or (getf (the file-inputs) :cladding-thickness) 10))"
		 "   (max-beam-spacing (or (getf (the file-inputs) :max-beam-spacing) 1500))"
		 "  )"
		 "  ..."
		 ")"))

   (repl-2 (list (list :command "(building-bom)"
		       :output (list "\"Bricks"
				     "======"
				     "  Full Bricks 3109"
				     "  Half Bricks 162"
				     "Mortar"
				     "======"
				     "  Volume 0.667 m^3"
				     "Roof Cladding"
				     "======"
				     "  Qty 2"
				     "  Dimensions (L x W x T) 4080 x 1807 x 10"
				     "Beams"
				     "====="
				     "  Section (H x W x T) 50 x 40 x 3"
				     "  Qty 3 Length 875"
				     "  Qty 6 Length 1728"
				     "  Qty 3 Length 3030"
				     "\""))))

   (repl-3 (list (list :command "(building-bom :nominal-width 4000)"
		       :output (list "\"Bricks"
				     "======"
				     "  Full Bricks 3384"
				     "  Half Bricks 162"
				     "Mortar"
				     "======"
				     "  Volume 0.725 m^3"
				     "Roof Cladding"
				     "======"
				     "  Qty 2"
				     "  Dimensions (L x W x T) 4080 x 2355 x 10"
				     "Beams"
				     "====="
				     "  Section (H x W x T) 50 x 40 x 3"
				     "  Qty 3 Length 1149"
				     "  Qty 6 Length 2276"
				     "  Qty 3 Length 3980"
				     "\""))))
   (code-7 (list "nominal-width 5000"
		 "truss-angle 40"
		 "wall-thickness 4"))
   
   (repl-4 (list (list :command "(building-bom :input-filename \"c:/temp/building-input.txt\")"
		       :output (list "\"Bricks"
		       "======"
		       "  Full Bricks 3659"
		       "  Half Bricks 162"
		       "Mortar"
		       "======"
		       "  Volume 0.783 m^3"
		       "Roof Cladding"
		       "======"
		       "  Qty 2"
		       "  Dimensions (L x W x T) 4080 x 3281 x 10"
		       "Beams"
		       "====="
		       "  Section (H x W x T) 50 x 40 x 4"
		       "  Qty 3 Length 2068"
		       "  Qty 6 Length 3199"
		       "  Qty 3 Length 4930"
		       "\""))))

   (repl-5 (list (list :command "(building-bom :nominal-width 4000 :input-filename \"c:/temp/building-input.txt\")"
		       :output (list "\"Bricks"
				     "======"
				     "  Full Bricks 3384"
				     "  Half Bricks 162"
				     "Mortar"
				     "======"
				     "  Volume 0.725 m^3"
				     "Roof Cladding"
				     "======"
				     "  Qty 2"
				     "  Dimensions (L x W x T) 4080 x 2661 x 10"
				     "Beams"
				     "====="
				     "  Section (H x W x T) 50 x 40 x 4"
				     "  Qty 3 Length 1670"
				     "  Qty 6 Length 2579"
				     "  Qty 3 Length 3980"
				     "\""))))

   (repl-6 (list (list :command (list "(building-bom :nominal-width 4000 "
				      "              :input-filename \"c:/temp/building-input.txt\" "
				      "              :output-filename \"c:/temp/building-output.txt\")")
		       :output (list "Exporting the BOM to c:/temp/building-output.txt"
				     "Exporting complete"
				     "NIL"))))
   (code-8 (list "Bricks"
		 "======"
		 "  Full Bricks 3384"
		 "  Half Bricks 162"
		 "Mortar"
		 "======"
		 "  Volume 0.725 m^3"
		 "Roof Cladding"
		 "======"
		 "  Qty 2"
		 "  Dimensions (L x W x T) 4080 x 2661 x 10"
		 "Beams"
		 "====="
		 "  Section (H x W x T) 50 x 40 x 4"
		 "  Qty 3 Length 1670"
		 "  Qty 6 Length 2579"
		 "  Qty 3 Length 3980"))

	   
   )

  :functions
  ((hint-1! () (the (set-slot! :hint-1 (not (the hint-1)))))
   (hint-2! () (the (set-slot! :hint-2 (not (the hint-2)))))
   (hint-3! () (the (set-slot! :hint-3 (not (the hint-3))))))
  
  :objects
  ((start-section :type 'sheet-section
		  :inner-html (with-cl-who-string()
				(:h3 "Example Brief")
				(:p "Extend the "(:span :class "object")" building example developed during the "
				    (if (the getting-started-url) (htm ((:a :href (the getting-started-url))"Getting Started with GendL")) "Getting Started with GendL ")
				    " tutorial to take any or all of its inputs from an input file")
                                (str (the (hint-button :function-key :hint-1!)))))

   (hint-1-section :type 'sheet-section
		  :inner-html (with-cl-who-string()
				(when (the hint-1)
				  (htm (:p "The first task is to develop a strategy which will allow the "
					   (:span :class "object" "building")" object to be instantiated by taking none, some, or all of its inputs from an input file. If we continue with the concept of using a function ("
					   (:span :class "function" "building-bom")") to instantiate "
					   (:span :class "object" "building")" we could imagine a hierarchy of inputs"
					   (:ul (:li "If the "(:span :class "function" "building-bom")" function provides any inputs, these should be used as priority")
						(:li "If an input file is given to "
						     (:span :class "function" "building-bom")", any inputs in this file should be used, except where they are already specified in the function inputs. ")
						(:li "For any other inputs use the "
						     (:span :class "object" "building")" object defaults"))
					   "Having developed this strategy, we now need to think about the format of the input file as it will determine how we need to process it to deliver the data in a format that the building object can use. The simplest option would be to use one word and one value per line, seperated by one or more spaces, to identify the data field and its value.In other words, out input file could mirror the "
					   (:span :class "object-keyword" ":input-slots")" definition")
				       (str (code-example (the code-1)))
				       (:p "We could then use our general purpose "
					   (:span :class "function" "read-file")" function developed in the "(:a :href (the read-from-file-url) "Reading from a File") " topic and write a simple function to convert the supplied data into a plist"
					   (str (code-example (the code-2))))
				       (:p "Compiling and evaluating this function based on the data shown above would give")
				       (str (repl-example (the repl-1)))
				       (:p "Now we need to consider how we integrate this with the "
					   (:span :class "function" "building-bom")" function and the "
					   (:span :class "object" "building object"))
                                       (str (the (hint-button :function-key :hint-2!)))))))


   (hint-2-section :type 'sheet-section
		   :inner-html (with-cl-who-string()
				 (when (the hint-2)
				   (htm (:p "A few changes are required to the building-bom function. To implement a hierarchy of inputs we need to differentiate where inputs are coming from. Any inputs provided by the function will be renamed so the :nominal-height becomes :function-nominal-height and the default values will be changed to zero. Additionally, we need to provide an input for the input filename, here defined as :input-filename. So the updated "(:span :class "function" "building-bom")" function would look like this"
				       (str (code-example (the code-3)))
				       "and associated changes to the "(:spac :class "object" "building")" object "(:span :class "object-keyword" ":input-slots")" would look like this"
				       (str (code-example (the code-4)))
				       "Note that the default values for "
				       (:span :class "slot" "nominal-height")", "
				       (:span :class "slot" "nominal-width")", "
				       (:span :class "slot" "nominal-length")" and "
				       (:span :class "slot" "truss-angle")" have been maintained, but will be overwritten by values from the "
				       (:span :class "function" "building-bom")" function if those values are supplied")
				        (:p "Now we need to make changes in the building object to use any of the values provided in the input file")
                                        (str (the (hint-button :function-key :hint-3!)))))))

   (hint-3-section :type 'sheet-section
		   :inner-html (with-cl-who-string()
				 (when (the hint-3)
				   (htm (:p "Firstly we add a new "
					    (:span :class "object-keyword" ":computed-slot")" which delivers the input file data as a plist and update the "
					    (:span :class "object-keyword" ":computed-slots")" for "
					    (:span :class "slot" "nominal-height")", "
					    (:span :class "slot" "nominal-width")", "
					    (:span :class "slot" "nominal-length")" and "
					    (:span :class "slot" "truss-angle")". As per our strategy, we should use any function-supplied values if supplied, otherwise we should use values from the input file if present and if neither of those conditions are met we should use the standard default value. So "
					    (:span :class "object-keyword" ":computed-slots")" updates would look like this")

                                        
					 (str (code-example (the code-5)))

                                        
					(:p "Next we need to address the "
					    (:span :class "object-keyword" ":input-slots")". These are not optionally provided by the "
					    (:span :class "function""building-bom")" function, so we need to use the value from the input file if provided, otherwise the standard default. Updates to the "
					    (:span :class "object-keyword" ":input-slots")" would therefore look like this")

                                        
					 (str (code-example (the code-6)))

                                        
					(:p "If we evaluate "(:span :class "function" "building-bom")" with no arguments, we should get a BoM for the default values")

                                        
					 (str (repl-example (the repl-2)))

                                        
					(:p "If we evaluate "
					    (:span :class "function" "building-bom")" with a value specified for "
					    (:span :class "slot" "nominal-width")", we should see some BoM changes")

                                        
					 (str (repl-example (the repl-3)))
                                        
                                        
					(:p "If we now make an input file and evaluate "
					    (:span :class "function" "building-bom")" with "
					    (:span :class "general-keyword" ":input-filename")" specified, we will again see BoM changes")

                                        
					(str (code-example (the code-7)))

                                        
					(str (repl-example (the repl-4)))

                                        
					(:p "With the same input file, but specifying "
					    (:span :class "slot" "nominal-width")" as an argument to "
					    (:span :class "function" "building-bom")", we can see that the "
					    (:span :class "slot" "nominal-width")" value is taking precedence over the value supplied in the input file")

                                        
					(str (repl-example (the repl-5)))

                                        
					(:p "And finally, if we provide an "
					    (:span :class "general-keyword" ":output-filename")", the bom will be written to that file")

                                        
					(str (repl-example (the repl-6)))

                                        
					(str (code-example (the code-8)))))))
				       
   )


   )

```

---

## file-io-example-1.lisp - header
Source: gornschool-training/t4/source/file-io-example-1.lisp
Type: tutorial

```
(in-package :training-4)


```

---

## file-io-example-1.lisp - file-io-example-1
Source: gornschool-training/t4/source/file-io-example-1.lisp
Type: tutorial

```
(define-object file-io-example-1 (base-training-sheet)
  :input-slots
  (getting-started-url)

  :computed-slots
  ((hint-1 nil :settable)
   (hint-2 nil :settable)
   (hint-3 nil :settable)
   (index-words (list "with-open-file" "standard-output"))
   (body-content (with-cl-who-string()
		   (:div :class "main-page-container" :style "grid-template-columns: 700px auto;"
			 (:div :class "main-page-item"
			       (str (the start-section main-div))
			       (str (the hint-1-section main-div))
			       (str (the hint-2-section main-div)))
			 (:div :class "main-page-item"
			       (:h2 "Resources")
			       (str (the resource-links))))))

   (code-1 (list "
```

---

## file-io-example-1.lisp - building
Source: gornschool-training/t4/source/file-io-example-1.lisp
Type: tutorial

```
(define-object building (box)"
		    "  :input-slots"
		    "  (..."
		    "   ..."
		    "   (output-filename nil))"
		    "  ..."
		    "  ..."
		    "  :functions"
		    "  (..."
		    "   ..."
		    "   (write-bom-file! ()"
		    "                    (with-open-file (s (the output-filename) :direction :output"
		    "                                                          :if-exists :supersede"
		    "                                                          :if-does-not-exist :create)"
		    "                       (format t \"Exporting the BOM to ~a~%\" (the output-filename))"
		    "                       (format s \"~a\" (the bom-formatted))"
		    "                       (format t \"Exporting complete~%\"))))"
		 ")"))

   (code-2 (list "
```

---

## file-io-example-1.lisp - building-bom
Source: gornschool-training/t4/source/file-io-example-1.lisp
Type: tutorial

```
(defun building-bom (&key (nominal-height 3000)" 
		 "                          (nominal-width 3000)"
		 "                          (nominal-length 3000)"
		 "                          (roof-angle 30)"
		 "                          (output-filename nil))"
		 "  (let ((obj (make-object 'building"
		 "                          :nominal-height nominal-height"
		 "                          :nominal-width nominal-width"
		 "                          :nominal-length nominal-length"
		 "                          :truss-angle roof-angle"
		 "                          :output-filename output-filename)))"
		 "    (if output-filename (theo obj write-bom-file!)"
		 "        (theo obj bom-formatted))))"))

   (code-3 (list "Bricks"
		 "======"
		 "  Full Bricks 2559"
		 "  Half Bricks 162"
		 "Mortar"
		 "======"
		 "  Volume 0.550 m^3"
		 "Roof Cladding"
		 "======"
		 "  Qty 2"
		 "  Dimensions (L x W x T) 3130 x 1807 x 10"
		 "Beams"
		 "====="
		 "  Section (H x W x T) 50 x 40 x 3"
		 "  Qty 3 Length 875"
		 "  Qty 6 Length 1728"
		 "  Qty 3 Length 3030"))

   (repl-1 (list (list :command "(building-bom :output-filename \"c:/temp/building-bom.txt\")"
		       :output "Exporting the BOM to c:/temp/building-bom.txt")
		 (list :output "Exporting complete")
		 (list :output "NIL"))))
				 
 :functions
  ((hint-1! () (the (set-slot! :hint-1 (not (the hint-1)))))
   (hint-2! () (the (set-slot! :hint-2 (not (the hint-2))))))
 
  :objects
  ((start-section :type 'sheet-section
		  :inner-html (with-cl-who-string()
				(:h3 "Example Brief")
				(:p "Extend the "(:span :class "object")" building example developed during the "
				    (if (the getting-started-url) (htm ((:a :href (the getting-started-url))"Getting Started with GendL")) "Getting Started with GendL ")
				    " tutorial to optionally export the Bill of Materials to a file")
                                (str (the (hint-button :function-key :hint-1!)))))

   (hint-1-section :type 'sheet-section
		   :inner-html (with-cl-who-string()
				 (when (the hint-1)
				   (htm (:p "The first step is to determine how to write the output, bearing in mind we need to be able to export the BoM to a file as well as output it to the screen if required. The current implementation provides a "
					    (:span :class "slot" "bom-formatted")" slot which it a single formatted string. It would make sense to use this directly. It would also make sense to write a "
					    (:span :class "object-keyword" ":function")" to write the output to a file. The "
					    (:span :class "object-keyword" ":function")" would be pretty simple"
				            (str (code-example (the code-1)))
				            "In the above code, we introduce a new "
				            (:span :class "object-keyword" ":input-slot")"  "
				            (:span :class "slot" "output-filename")" which defaults to nil. We also add a new "
				            (:span :class "object-keyword" ":function")" "
				            (:span :class "function" "write-bom-file!")". This function connects a stream to the file specified in "
				            (:span :class "slot" "output-filename")" and writes "
				            (:span :class "slot" "(the bom-formatted)")" to that stream using the "
				            (:span :class "function" "format")" function. Also included are 2 progress messages which are written to "
				            (:em (:b "*standard-output*")))
				        (:p "The next task is to update the function which instantiates the "
					    (:span :class "object" "building")" object and either write the BoM to the screen or send it to a file")
                                        (str (the (hint-button :function-key :hint-2!)))))))


   (hint-2-section :type 'sheet-section
		   :inner-html (with-cl-who-string()
				 (when (the hint-2)
				   (htm (:p "The existing "(:span :class "function" "building-bom")" function instantiates the "
					    (:span :class "object" "building")" object and then calls the "
					    (:span :class "slot" "bom-formatted")" message to generate the output as a string. We need to retain this functionality and add in an option to output the content to a file. The simplest way to do this is provide a further keyword input, "
					    (:span :class "general-keyword" ":output-filename")" to the "
					    (:span :class "function" "building-bom")" function and defaut it to nil. If "
					    (:span :class "general-keyword" ":output-filename")" is set to a non-nil value then call the "
					    (:span :class "function" "write-bom-file!")" function, otherwise just call the "
					    (:span :class "slot" "bom-formatted")" message as previously"
					    (str (code-example (the code-2)))
					    "If we call the "
					    (:span :class "function" "building-bom")" function with a value for the "
					    (:span :class "general-keyword" ":output-filename")" keyword input, we get the following"
					    (str (repl-example (the repl-1)))
					    "and we get a file written with the following content"
					    (str (code-example (the code-3))))))))))


```

---

