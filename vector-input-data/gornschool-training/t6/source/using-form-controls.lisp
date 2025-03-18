(in-package :training-6)

(define-object using-form-controls (base-training-sheet)

  :computed-slots
  ((index-words (list "error" "failed-value" "html-string" "form control validation" "restore-defaults!" "Reset a form control" "form-control"))

   (code-1 (list "(define-object form-control-layout (base-html-page)"
		 ""
		 " :computed-slots"
		 " ((additional-header-content (with-lhtml-string()"
		 "                                ((:link :rel \"stylesheet\" :href \"/style.css\"))))"
		 ""
		 "  (body (with-lhtml-string ()"
		 "                      (when gwl:*developing?* (str (the development-links)))"
		 "                      (:h3 \"Just using the form-control message\")"
		 "                      (str (the fc-1 form-control))"
		 "                      (:h3 \"Using :prompt and form-control in my own table layout\")"
		 "                      (:table (:tr (:td (str (the fc-2 prompt)))"
		 "                                   (:td (str (the fc-2 form-control)))))"
		 "                      (:h3 \"Using html-string (defaulting to :layout-position :as-div)\")"
		 "                      (str (the fc-3 html-string))"
		 "                      (:h3 \"Using other :layout-position\")"
		 "                      (str (the fc-4 html-string))"
		 "                      (:br)"
		 "                      (str (the fc-5 html-string)))))"
		 " :objects"
		 " ((fc-1 :type 'text-form-control"
		 "        :size 12"
		 "        :default nil)"
		 "  (fc-2 :type 'text-form-control"
		 "        :size 12"
		 "        :prompt \"My form control\""
		 "        :default nil)"
		 "  (fc-3 :type 'text-form-control"
		 "        :size 12"
		 "        :prompt \"My form control with html-string\""
		 "        :default nil)"
		 "  (fc-4 :type 'text-form-control"
		 "        :size 12"
		 "        :prompt \"Label display #1 - prepended label\""
		 "        :default nil"
		 "        :label-position :prepend)"
		 "  (fc-5 :type 'text-form-control"
		 "        :size 12"
		 "        :prompt \"Label display #2 - appended label\""
		 "        :default nil"
		 "        :label-position :append)))"))

   (repl-1 (list (list :prompt "GWL-USER"
		       :command (list "(make-self 'menu-form-control"
				      ":size 1"
				      ":choice-list (list \"Peter\" \"Paul\" \"John\")"
				      ":default \"Peter\")")
		       :output "#<MENU-FORM-CONTROL #x2104C2296D>")
		 (list :prompt "GWL-USER"
		       :command "(the form-control)"
		       :output (list "\""
				     "<select name=\"TklM\" id=\"TklM\" size=\"1\">"
				     "   <option value=\"Peter\" selected=\"selected\">Peter</option>"
				     "   <option value=\"Paul\">Paul</option>"
				     "   <option value=\"John\">John</option>"
                                     "</select>\""))
		 (list :prompt "GWL-USER"
		       :command "(the value)"
		       :output "\"Peter\"")))
   (repl-2 (list (list :prompt "GWL-USER"
		       :command (list "(make-self 'menu-form-control"
				      ":size 1"
				      ":choice-plist (list :name-1 \"Peter\" "
				      "                    :name-2 \"Paul\" "
				      "                    :name-3 \"John\")"
				      ":default \"Peter\")")
		       :output "#<MENU-FORM-CONTROL #x2104C2296D>")
		 (list :prompt "GWL-USER"
		       :command "(the form-control)"
		       :output (list "\""
				     "<select name=\"TklM\" id=\"TklM\" size=\"1\">"
				     "   <option value=\":NAME-1\" selected=\"selected\">Peter</option>"
				     "   <option value=\":NAME-2\">Paul</option>"
				     "   <option value=\":NAME-3\">John</option>"
                                     "</select>\""))
		 (list :prompt "GWL-USER"
		       :command "(the value)"
		       :output ":NAME-1")))
   
   (code-2 (list "(define-object form-control-validation (base-html-page)"
		 " :computed-slots"
		 " ((main-sheet-body (with-lhtml-string ()"
		 "                       (when gwl:*developing?* (str (the development-links)))"
		 "                       (:h3 \"Form control validation\")"
		 "                       (str (the fc-section main-div)))))"
		 " :objects"
		 " ((fc-section :type 'sheet-section"
		 "              :inner-html (with-lhtml-string ()"
		 "                               (:p (str (the number-fc html-string)))"
		 "                               (when (the number-fc value)"
		 "                                   (htm (:p (fmt \"the number value is ~a\""
		 "                                                (the number-fc value)))))"
		 "                               (when (the number-fc error)"
		 "                                   (html (:p (fmt \"error is ~a, failed value is ~a\""
		 "                                                (the number-fc error)"
		 "                                                (the number-fc failed-value)))))))"
		 ""
		 " (number-fc :type 'number-form-control"
		 "            :default nil"
		 "            :size 12"
		 "            :ajax-submit-on-change? t"
		 "            :validation-function #'(lambda(input)"
		 "                                       (cond ((or (<= input 50) (>= input 60))"
		 "                                              (list :validated-value input"
		 "                                                    :error :value-out-of-range))"
		 "                                             (T T))))))"))

   
   (body-content (with-lhtml-string ()
		   (:div :class "main-page-container"
			 (:div :class "main-page-item"
			       (:div :class "grid-container-2-650px"
				     (:div :class "grid-item"
					   (:p "html defines a number of input types which are used to gather inputs from users. In GendL, each input type is called a form control. Form Controls are implemented as objects. The following table shows the most commonly used html form types and the associated gendL object")
					   (:table :border 1 :border-style "solid"
					     (:tr (:th "html type") (:th "GendL object"))
					     (:tr (:td (esc "<input type=\"text\">"))(:td "text-form-control"))
					     (:tr (:td (esc "<input type=\"number\">"))(:td "number-form-control"))
					     (:tr (:td (esc "<input type=\"password\">"))(:td "password-form-control"))
					     (:tr (:td (esc "<input type=\"checkbox\">"))(:td "checkbox-form-control"))
					     (:tr (:td (esc "<input type=\"radio\">"))(:td "radio-form-control"))
					     (:tr (:td (esc "<select>")) (:td "menu-form-control")))
					   (:h3 "Displaying a form control")
					   (:p "Once a form control object has been created, there are a number of ways to display it in a web form"
					       (:ul (:li "Use the form control objects "
							 (:span :class "general-keyword" ":form-control")" message. This evaluates to the basic html string required for the form control")
							     
						    (:li "Define a "(:span :class "object-keyword" ":prompt")" input for the form control object and use the form control objects "
							 (:span :class "general-keyword" ":prompt")" and "
							 (:span :class "general-keyword" ":form-control")" messages and do your own custom layout")
						    (:li "Define a "(:span :class "object-keyword" ":prompt")" input for the form control object and use the form control objects "
							 (:span :class "general-keyword" ":html-string")" message to display a label and the form control html")
						    (:li "Define a "
							 (:span :class "object-keyword" ":layout-position")" input for the form control object to give more control over where the label is positioned in the html-string ")
						    )))
					 
				     (:div :class "grid-item")
				     (:div :class "grid-item"
					   (str (code-example (the code-1))))
				     (:div :class "grid-item"
					   (:image :src (format nil "/~a-images/fc-display.png" (the publish-prefix))
						   :style "border: 2px solid; margin: 5px 0 0 3% ;width: auto; height: 300px;" ))
				     (:div :class "grid-item"
					   (:h3 "Form control inputs")
					   (:p "Form controls can take a large number of inputs to provide the desired control. YADD provides a comprehensive guide to these inputs (GWL..base-form-control). We won't cover them all here, but the most common are")
					   (:p (:ul (:li (:span :class "object-keyword" ":default")" (required) - the default value for the form control")
						    (:li (:span :class "object-keyword" ":prompt")" (optional) - the label associated with the form control")
						    (:li (:span :class "object-keyword" ":layout-position")" (optional) - control over where the label is presented in the html-string message")
						    (:li (:span :class "object-keyword" ":size")" (optional) - for text, number and password form controls it defines the length of the input box. For menu form controls it defines the number of options displayed")
						    (:li (:span :class "object-keyword" ":choice-list")" (optional) - for menu and radio form controls, defines the options to be displayed. When selected the displayed value is the value returned by the form control")
						    (:li (:span :class "object-keyword" ":choice-plist")" (optional) - for menu and radio form controls, defines the options to be displayed (value element of the plist). In contrast to choice-list, when an option is selected the form controls returns the plist keyword associated with the selected value"))))
				     (:div :class "grid-item")
				     (:div :class "grid-item" (str (repl-example (the repl-1))))
				     (:div :class "grid-item" (str (repl-example (the repl-2))))
				     (:div :class "grid-item"
					   (:h3 "Returning values from Form Controls")
					   (:p "Every form control support the "
					       (:span :class "general-keyword" ":value")" message, which is the selected or entered value for the form control following form submission")
					   (:p "Note that for text form controls, if the form control is empty on submission then NIL will be returned if the "
					       (:span :class "object-keyword" ":nullify-empty-string?")" input to the form control is T, otherwise an empty string will be returned")
						      
					   (:h3 "Capturing a form control value")
					   (:p "Any form containing form controls needs to be submitted in order for the form control entered value to be captured. The simplest, and reccomended, way to do this, is to use the "
					       (:span :class "object-keyword" ":ajax-submit-on-change?")" input to the form control. With this set to T whenever the form control value is changed an AJAX update will be performed to submit the form and capture the value. We'll cover using AJAX in the next topic.")
						
					   (:h3 "Resetting a form control")
					   (:p "Every form constrol support a function "(:span :class "function" "restore-defaults!")" which sets "
					       (:span :class "object-keyword" ":value")", "
					       (:span :class "object-keyword" ":failed-value")" and "
					       (:span :class "object-keyword" ":error")" back to default values")
					   (:h3 "Validating inputs")
					   (:p "All form controls have a "
					       (:span :class "object-keyword" ":validation-function")" input. This function defines the rule(s) against which the provided input is tested to determine if it is valid. Any function may be used, but quite often a lambda function is a good solution as the validation is generally specific to the form control and its applcation. The function return values are used to determine what happens with the input"
					       (:ul (:li "If it returns nil, the input is considered invalid and the "
							 (:span :class "slot" "error")" slot is set to "
							 (:span :class "general-keyword" ":unspecified-validation-fail"))
						    (:li "It may return a plist with keys "
							 (:span :class "general-keyword" ":validated-value")" and "
							 (:span :class "general-keyword" ":error")". If "
							 (:span :class "general-keyword" ":error")" is non-nil this signifies validation has failed and the error value will be appended to "
							 (:span :class "slot" "html-string"))
						    (:li "If any other value is returned, the input is considered valid")))
					   )
				     (:div :class "grid-item")
				     (:div :class "grid-item"
					   (str (code-example (the code-2))))
				     (:div :class "grid-item"
					   (:image :src (format nil "/~a-images/valid-fc.png" (the publish-prefix))
						   :style "border: 2px solid; margin: 5px 0 0 3% ;width: auto; height: 200px;" )
					   (:br)
					   (:image :src (format nil "/~a-images/invalid-fc.png" (the publish-prefix))
						   :style "border: 2px solid; margin: 5px 0 0 3% ;width: auto; height: 200px;" ))
				     (:div :class "grid-item"
					   (:p "Note that in the invalid entry, the text formatted red is output (and styled) as part of "
					       (:span :class "slot" "html-string")", whilst the strings below it directly access the "
					       (:span :class "slot" "value")", "
					       (:span :class "slot" "error")" and "
					       (:span :class "slot" "failed-value")" slots"))))
						   
			     
			 ((:div :class "main-page-item")
			  (:h2 "Resources")
			  (str (the resource-links))))))
  ))

