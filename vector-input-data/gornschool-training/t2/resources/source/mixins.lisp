(in-package :gdl-user)

(define-object object-1 (box)

  :input-slots
  ((length 2)
   (width 3)
   (height 4))
  )

(define-object my-object (base-object)
  :computed-slots
  ((volume "not computed"))
  )
   

(define-object object-2 (object-1 my-object)
  )

(define-object object-3 (my-object object-1)
  )

;;
;; make object-1, volume is the standard box version of volume (* length width height)
;;
;; GDL-USER> (make-self 'object-1)
;; #<OBJECT-1 #x21041A35AD>
;; GDL-USER> (the volume)
;; 24
;;
;; now make object-2, whilst my-object defines volume, because its mixed in after object-1, the
;; object-1 definition for volume, which comes from box, takes precedence
;;
;; GDL-USER> (make-self 'object-2)
;; #<OBJECT-2 #x21041981AD>
;; GDL-USER> (the volume)
;; 24
;;
;; now make object-3. the definition of volume from my-ovject takes precedence over the definition in
;; object-1 which is derived from box
;;
;; GDL-USER> (make-self 'object-3)
;; #<OBJECT-3 #x21042FBB5D>
;; GDL-USER> (the volume)
;; "not computed"
;;
;; to get an idea of whats happening, with object-3 still set as self we can send it the message (the mixins)
;; which returns us the mixins list for object-3
;; GDL-USER> (the mixins)
;; (MY-OBJECT OBJECT-1)
;;
;; not hugely helpful. But if we send it (the all-mixins) message we get a list back of all of the mixins
;; used in object-3 which is derived recursively, placed in order of priority and any duplicates removed
;;
;; GDL-USER> (the all-mixins)
;; (MY-OBJECT BASE-OBJECT VANILLA-MIXIN VANILLA-MIXIN* STANDARD-OBJECT T GENDL::GDL-BASIS OBJECT-1 BOX)
;;
;; so first we get my-object, but it has base-object mixed in. base-object however mixies in vanilla-mixin
;; which in turn mixes in vanilla-mixin* etc. when all of the mixins used in my-object have been listed
;; we then start with object-1, which has box mixed into it, but then nothing after box...
;;
;; so if we make box and send it (the all-mixins) message we can see that it mixes in base-object, which we
;; have already covered from my-object
;;
;; GDL-USER> (make-self 'box)
;; #<BOX #x21044BA30D>
;; GDL-USER> (the all-mixins)
;; (BASE-OBJECT VANILLA-MIXIN VANILLA-MIXIN* STANDARD-OBJECT T GENDL::GDL-BASIS)
;;
;; (the all-mixins) effectively gives us all of the mixins in use for the current object ordered left to
;; right by priority
