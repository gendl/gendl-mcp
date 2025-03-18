(in-package :training-6)

(define-object displaying-graphics (base-training-sheet)
  :computed-slots
  ((:body-content (with-cl-who-string()
		   (:div :class "main-page-container"
			 (:div :class "main-page-item"
			       (:div :class "grid-container-2-650px"
				     (:div :class "grid-item")
				     (:p "We're still working on this, should be available soon!"))))))))
