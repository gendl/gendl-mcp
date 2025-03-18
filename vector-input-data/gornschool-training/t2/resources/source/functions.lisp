(in-package :gdl-user)

(defun kinetic-energy (&key (mass 5) (velocity 12)
			 (div (* mass velocity velocity) 2)))

(define-object function-example(base-object)

  :computed-slots
  ((mass 5)
   (velocity 12)
   (ke-1 (the kinetic-energy-1!))
   (ke-2 (the kinetic-energy-2!))
   (ke-3 (the (kinetic-energy-2! :mass 10)))
   (ke-4 (the (kinetic-energy-2! :velocity 24)))
   (ke-5 (the (kinetic-energy-2! :velocity 24 :mass 10)))

   (ke-6 (kinetic-energy :mass 10 :velocity 24))
   )

  :functions
  ((kinetic-energy-1! () (div (* (the mass) (the velocity) (the velocity)) 2))

   (kinetic-energy-2! (&key (mass (the mass)) (velocity (the velocity)))
		      (div (* mass velocity velocity) 2))
   )
  )
