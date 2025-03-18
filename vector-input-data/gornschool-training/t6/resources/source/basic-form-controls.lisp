(in-package :gwl-user)

(define-object basic-form-controls (base-html-page)

  :computed-slots
  ((body (with-lhtml-string ()
		      (when gwl:*developing?* (str (the development-links)))
		      (:p (str (the text-fc html-string)))
		      (:table
			  (:tr
			   (:td (str (the text-fc prompt)))
			   (:td (str (the text-fc form-control))))
			(:tr
			   (:td (str (the number-fc prompt)))
			   (:td (str (the number-fc form-control))))
			(:tr
			   (:td (str (the password-fc prompt)))
			   (:td (str (the password-fc form-control))))
			(:tr
			   (:td (str (the dropdown-fc prompt)))
			   (:td (str (the dropdown-fc form-control))))
			(:tr
			   (:td (str (the radio-fc prompt)))
			   (:td (str (the radio-fc form-control))))
			(:tr
			   (:td (str (the checkbox-fc prompt)))
			   (:td (str (the checkbox-fc form-control))))
			   )))

		      
		    )
   
  :objects
  ((text-fc :type 'text-form-control
	    :size 12
	    :default nil
	    :prompt "Text Form Control")
   (number-fc :type 'number-form-control
	      :size 12
	      :default nil
	      :prompt "Number Form Control")
   (password-fc :type 'password-form-control
		:size 12
		:default nil
		:prompt "Password Form Control")
   (dropdown-fc :type 'menu-form-control
		:prompt "Select Form Control"
		:choice-list (list "Select" "Paul" "John" "Peter" )
		:default (first (the-child choice-list))
		:size 1)
   (checkbox-fc :type 'checkbox-form-control
		:prompt "Checkbox Form Control"
		:default T)
   (radio-fc :type 'radio-form-control
	     :prompt "Radio Form Control"
	     :choice-list (list "Paul" "John" "Peter")
	     :default (first (the-child choice-list)))
		
   )
  )

(publish-gwl-app "/fc1" "gwl-user::basic-form-controls")
