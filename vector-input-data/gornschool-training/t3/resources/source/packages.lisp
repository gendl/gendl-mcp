(in-package :gdl-user)

(define-package :my-app
    (:export #:my-slot))

(define-package :my-other-app
    (:use :my-app))

(define-package :yet-another-app
  (:export #:my-slot-1
	   #:my-slot-2)
  (:use :my-app))
