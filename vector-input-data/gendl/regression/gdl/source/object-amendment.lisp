(in-package :gdl-lift-tests)


(define-object trickle-top ()

  :computed-slots
  ((a "aye")
   (b "bee")
   (c "cee")
   (d "dee")
   (e "eee"))


  :trickle-down-slots (a e)

  :objects
  ((kid-1 :type 'trickle-down-1
	  :c (the c))))

(define-object trickle-down-1 ()
  :input-slots (c)

  :objects ((kid-2 :type 'trickle-down-2)))

(define-object trickle-down-2 ()
  :input-slots ((b "bee-local" :defaulting)
		(d "dee-local" :defaulting)
		e))

(register-test-definition 'trickle-top)



