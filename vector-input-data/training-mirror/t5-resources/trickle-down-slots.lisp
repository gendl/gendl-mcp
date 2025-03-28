(in-package :gdl-user)

(define-object top-level-1 (base-object)
   :computed-slots
  ((my-slot-1 5))
  
  :objects
  ((child-1 :type 'child-1
	    :my-slot-1 (the my-slot-1))

   (child-2 :type 'child-2)))

(define-object child-1 (base-object)
  :input-slots
  (my-slot-1))
  
(define-object child-2 (base-object)
  :input-slots
  ((my-slot-1 3)))

;;;;;;;;;;;;;;;;;;;;

(define-object top-level-2 (base-object)
   :computed-slots
  ((my-slot-1 5))
  
  :objects
  ((child-1 :type 'child-1
	    :pass-down (my-slot-1))))

   
;;;;;;;;;;;;;;;;;;;;;;

(define-object top-level-3 (base-object)
  :computed-slots
  ((my-slot-2 2))
  
  :trickle-down-slots
  (my-slot-2)
  
  :objects
  ((child-3 :type 'child-3)
   (child-4 :type 'child-4)))

(define-object child-3 (base-object)
  :input-slots ((my-slot-2 3 :defaulting)))

(define-object child-4 (base-object)
  :input-slots ((my-slot-2 5)))

;;;;;;;;;;;;;;;;;;;;;;

(define-object top-level-4 (base-object)
  :computed-slots
  ((my-slot-3 2))
  
  :trickle-down-slots
  (my-slot-3)
  
  :objects
  ((child-5 :type 'child-5
	    :my-slot-3 1)))

(define-object child-5 (base-object)
  :input-slots
  ((my-slot-3 5 :defaulting)))
