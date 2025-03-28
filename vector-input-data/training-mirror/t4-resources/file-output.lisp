(in-package :gdl-user)

(defun output-report (fname)
  (let ((obj (make-object 'my-box
			  :output-filename fname)))
    (theo obj write-report!)))
			

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
