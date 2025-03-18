(in-package :gdl-user)

(defun make-integer-list (elements &key (start 0) (interval 1))
  ;; build a list if integers a specified number of elements long
  ;; by default it starts at zero and increments each element by 1, but these values may be ammended

  (let ((result nil))
    ;; we can use nreverse here for the return value, since while it is distructive
    ;; result will never be used once the return value has been sent
    (dotimes (e elements (nreverse result))
      (if (= e 0)
	  (push start result)
	  (push (+ start (* e interval)) result)))))

;; GDL-USER> (make-integer-list 5)
;; (0 1 2 3 4)
;; GDL-USER> (make-integer-list 5 :start 8)
;; (8 9 10 11 12)
;; GDL-USER> (make-integer-list 5 :start 8 :interval 2)
;; (8 10 12 14 16)
;; GDL-USER> (make-integer-list 5 :interval 2)
;; (0 2 4 6 8)

(defun get-plist-keys-dolist (plist)
  ;; get just keywords from a plist
  ;; in contrast to make-integer-list we have not set result to nil explicitly
  ;; if no value is assigned it will automatically be nil
  (let ((result))
    (dolist (p plist (nreverse result))
    (when (keywordp p) (push p result)))))

(defun get-plist-keys-mapcar (plist)
  (remove nil (mapcar #'(lambda(a) (when (keywordp a) a)) plist)))

;; GDL-USER> (setq plist (list :k1 1 :k2 2 :k3 3 :k4 4 :k5 5 :k6 6))
;; (:K1 1 :K2 2 :K3 3 :K4 4 :K5 5 :K6 6)
;; GDL-USER> (get-plist-keys-dolist plist)
;; (:K1 :K2 :K3 :K4 :K5 :K6)
;; GDL-USER> (get-plist-keys-mapcar plist)
;; (:K1 :K2 :K3 :K4 :K5 :K6)



(define-object mapping (base-object)
  :computed-slots
  ((integer-list (list 1 2 3 4 5 6))
   (plist (list :k1 1 :k2 2 :k3 3 :k4 4 :k5 5 :k6 6))
  

   (add-2 (mapcar #'(lambda(i) (+ i 2)) (the integer-list)))
   ;; (3 4 5 6 7 8)
   
   (keys-only (remove nil (mapcar #'(lambda(p) (when (keywordp p) p)) (the plist))))
   ;; (:k1 :k2 :k3 :k4 :k5 :k6)

   (values-only (remove nil (mapcar #'(lambda(p) (when (not (keywordp p)) p)) (the plist))))
   ;; (1 2 3 4 5 6)

   (spliced-plist (mapcan #'list (the keys-only) (the add-2)))
   ;; (:K1 3 :K2 4 :K3 5 :K4 6 :K5 7 :K6 8)

   (add-plists (mapcar #'(lambda(k) (+ (getf (the plist) k)
				       (getf (the spliced-plist) k)))
		       (the keys-only)))
   ;; (4 6 8 10 12 14)

   ;; slightly more efficient, making only a single 'the' call for each plist
   (add-plists-2 (let ((plist-1 (the plist))
		       (plist-2 (the spliced-plist)))
		   (mapcar #'(lambda(k) (+ (getf plist-1 k)
					   (getf plist-2 k)))
			   (the keys-only))))
   ;; (4 6 8 10 12 14)

   (add-plists-3 (let ((result)
		       (plist-1 (the plist))
		       (plist-2 (the spliced-plist)))
		   (mapc #'(lambda(x) (when (keywordp x)
					(push (+ (getf plist-1 x) (or (getf plist-2 x) 0)) result)))
			 plist-1)
		   (nreverse result)))

    ;; (4 6 8 10 12 14)
		   
   )
  )
