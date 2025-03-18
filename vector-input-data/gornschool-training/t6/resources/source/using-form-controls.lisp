(in-package :gwl-user)

(define-object form-control-layout (base-html-page)
  :computed-slots ((additional-header-content
                    (with-cl-who-string()
                      ((:link :rel "stylesheet" :href *css-published-path*))))
                   (body
                    (with-cl-who-string ()
                      (when gwl:*developing?* (str (the development-links)))
                      (:h3 "Just using the form-control message")
                      (str (the fc-1 form-control))
                      (:h3 "Using :prompt and form-control in my own table layout")
                      (:table (:tr
                               (:td (str (the fc-2 prompt)))
                               (:td (str (the fc-2 form-control)))))
                      (:h3 "Using html-string (defaulting to :layout-position :as-div)")
                      (str (the fc-3 html-string))
                      (:h3 "Using other :layout-position")
                      (str (the fc-4 html-string))
                      (:br)
                      (str (the fc-5 html-string)))))
  :objects  ((fc-1 :type 'text-form-control
                   :size 12
                   :default nil)
             (fc-2 :type 'text-form-control
                   :size 12
                   :prompt "My form control":default nil)
             (fc-3 :type 'text-form-control
                   :size 12
                   :prompt "My form control with html-string":default nil)
             (fc-4 :type 'text-form-control
                   :size 12
                   :prompt "Label display #1 - prepended label"
                   :default nil
                   :label-position :prepend)
             (fc-5 :type 'text-form-control
                   :size 12
                   :prompt "Label display #2 - appended label"
                   :default nil
                   :label-position :append)))
(publish-gwl-app "/form-control-layout" 'form-control-layout)

(define-object form-control-validation (base-html-page)
  :computed-slots
  ((body (with-lhtml-string ()
	   (when gwl:*developing?* (str (the development-links)))
	   (:h3 "Form control validation")
	   (str (the fc-section div)))))
  :objects
  ((fc-section :type 'base-html-div
	       :inner-html (with-lhtml-string ()
			     (:p (str (the number-fc html-string)))
			     (when (the number-fc value)
			       (htm (:p (fmt "the number value is ~a"
					     (the number-fc value)))))
			     (when (the number-fc error)
			       (html (:p (fmt "error is ~a, failed value is ~a"
					      (the number-fc error)
					      (the number-fc failed-value)))))))
   (number-fc :type 'number-form-control
	      :default nil
	      :size 12
	      :ajax-submit-on-change? t
	      :validation-function #'(lambda(input)
				       (cond ((or (<= input 50) (>= input 60))
					      (list :validated-value input
						    :error :value-out-of-range))
					     (t t))))))
(publish-gwl-app "/form-control-validation" 'form-control-validation)
