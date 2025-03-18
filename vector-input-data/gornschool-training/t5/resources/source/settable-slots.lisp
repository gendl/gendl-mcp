(in-package :gdl-user)

(define-object settable-slots (base-object)

  :computed-slots
  ((speed 25 :settable)
   (time 15 :settable)
   (distance (* (the speed) (the time)) :settable))

  :functions
  ((set-speed! (&key (value 20)) (the (set-slot! :speed value)))
   (set-time! (&key (value 10)) (the (set-slot! :time value)))
   (set-distance! () (the (set-slot! :distance 100)))
   (reset-distance! () (the (restore-slot-default! :distance)))
   (reset-all! () (the (restore-slot-defaults! (list :speed :time :distance))))))

