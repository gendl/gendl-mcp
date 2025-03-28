(in-package :gdl-lift-tests)

(define-object-amendment trickle-top ()

  :computed-slots
  ((regression-test-data (list :kid-1-a (the kid-1 a)
                               :kid-1-b (ignore-errors (the kid-1 b))
                               :kid-1-c (the kid-1 c)
                               :kid-1-d (the kid-1 d)
			       :kid-1-e (the kid-1 e)
			       :kid-2-a (the kid-1 kid-2 a)
			       :kid-2-b (ignore-errors (the kid-1 kid-2 b))
			       ;;:kid-2-c (the kid-1 kid-2 c)
			       :kid-2-d (the kid-1 kid-2 d)
			       :kid-2-e (the kid-1 kid-2 e))))
			       
  :trickle-down-slots (d))

;;(register-test-definition 'trickle-top)
