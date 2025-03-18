(in-package :training-6)

(define-object using-ajax (base-training-sheet)

  :input-slots
  (simple-html-page
      using-form-controls
    using-base-html-divs)
  
  :computed-slots
  ((index-words (list "AJAX" "main-div" "base-ajax-sheet" "sheet-section" ":ajax-submit-on-change?"))

   (body-content (with-lhtml-string()
		   (:div :class "grid-container-2-650px"
			 (:div :class "grid-item"
			       (:h3 "What is AJAX?")
			       (:p "AJAX is a term you don't hear much anymore but it originally stood for \"Asynchronous JavaScript and XML.\"
This is technique for updating one or more sections of a web page based on a request sent to the web server.")
			       (:p "Early web applications were limited to transmitting information to and from the sever using 
synchronous requests, one page at a time. It means your user would fill out a form, hit submit, and get directed to a new page with new 
information from the server. With AJAX, when you click a button, change a value, or do certain other interactions with a page,
the JavaScript engine running in your browser will make a request to the server, which interprets the results and sends a response,
then the JavaScript engine will update the user's web page accordingly. The user would not be directly made aware 
that anything was transmitted to and from the server.")
			       
			       (:h3 "Using AJAX with GendL")
			       (:p "The most basic requirement is to use "
				   (:span :class "mixin" "base-html-page")" as your web pages mixin. This provides most 
of the functionality, and was covered in the first topic of this tutorial, "
				   (:a :href (theo (the simple-html-page) url) (str (theo (the simple-html-page) page-title)) ))
			       (:p "Next, to trigger an AJAX submit, form controls need to have their input "
				   (:span :class "general-keyword" ":ajax-submit-on-change?")" set to T. This was covered 
in the tutorial topic "
				   (:a :href (theo (the using-form-controls) url) (str (theo (the using-form-controls) page-title))))
			       (:p "And finally, the page is best constructed from "(:span :class "object" "base-html-divs")
				   ", as shown in the topic "
				   (:a :href (theo (the using-base-html-divs) url) (str (theo (the using-base-html-divs) page-title))))

			       (:h3 "Performance Considerations")
			       (:p "The javascript update is performed by GendL at the "
				   (:span :class "object" "base-html-div")" level; GendL identifies which "
				   (:span :class "object" "base-html-divs")" will be impacted by the change to the 
form control(s), recomputes the value of the "
				   (:span :class "object" "base-html-divs")" "
				   (:span :class "slot" "div")" and updates the page with that content. So for best 
performance there is a balance to be struck between having sufficient "
				   (:span :class "object" "base-html-divs")" so that large sections of the page which 
don't change are updated and sent from the server to the browser, and having the page defined with so much 
granularity that a large number of (smaller) inserts/updates have to be made")
			       (:p "In the main, however, any optimisation can be performed at the end of the 
development cycle although it is worth bearing in mind the simplicity/granularity tradeoff when building 
the page in the first place. This was also discussed in the topic "
				   (:a :href (theo (the using-base-html-divs) url) (str (theo (the using-base-html-divs) page-title))))
			       (:p "Using AJAX with GendL is really this simple!")))))
		      

    
   ))
