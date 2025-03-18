(in-package :training-6)

(define-object using-base-html-divs (base-training-sheet)
  :input-slots
  (simple-html-page-title
      simple-html-page-url
    using-ajax-title
    using-ajax-url)
  
  :computed-slots
  ((index-words (list "base-html-div" "div" "inner-html" "AJAX"))
   (code-1 (list "(in-package :gwl-user)"
		 "(define-object simple-page-with-section (base-html-page)"
		 ""
		 " :computed-slots"
		 " ((body "
		 "       (with-lhtml-string ()"
		 "          (when gwl:*developing?* (str (the development-links)))"
		 "          (:h2 \"Basic Page Sections\")"
		 "          (str (the section-1 div)))))"
		 " :objects"
		 " ((section-1 :type 'base-html-div"
		 "                   :inner-html" 
		 "                        (with-litml-string ()"
		 "                           (:p \"Using page sections to provide content\")))))"
		 ""
		 "(publish-gwl-app \"/simple-base-html-div\" \"gwl-user::simple-page-with-section\")"))

   (code-2 (list "(in-package :gwl-user)"
		 "(define-object simple-page-with-custom-section (base-html-page)"
		 ""
		 " :computed-slots"
		 " ((input-list (list 1 2 3))"
		 "  (body "
		 "       (with-lhtml-string ()"
		 "          (when gwl:*developing?* (str (the development-links)))"
		 "          (:h2 \"Basic Page Sections\")"
		 "          (str (the section-1 div)))))"
		 " :objects"
		 " ((section-1 :type 'base-html-div-1"
		 "                   :input-list (the input-list))))"
		 ""
		 "(define-object base-html-div-1 (base-html-div)"
		 " :input-slots"
		 " (input-list)"
		 " :computed-slots"
		 " ((inner-html (with-lhtml-string ()"
		 "                    (:table :border 1"
		 "                        (:tr (:th \"Content\"))"
		 "                        (dolist (c (the input-list))"
		 "                            (htm (:tr (:td (fmt \"Cell ~a content\" c))))))))))"
		 ""
		 "(publish-gwl-app \"/simple-page-with-custom-section\" \"gwl-user::\simple-page-with-custom-section\")"))

   (repl-1 (list (list :prompt "GWL-USER"
		       :command "(make-self 'base-html-div-object)"
		       :output "#<BASE-HTML-DIV-OBJECT #x210462B07D>")
		 (list :prompt "GWL-USER"
		       :command "(the section-1 inner-html)"
		       :output (list "<table border=\"1\">"
				     "  <tr><th>Content</th></tr>"
				     "  <tr><td>Cell 1 content</td></tr>"
				     "  <tr><td>Cell 2 content</td></tr>"
				     "  <tr><td>Cell 3 content</td></tr>"
				     "</table>"))
		 (list :prompt "GWL-USER"
		       :command "(the section-1 div)"
		       :output (list "<div id=\"KDpTSEVFVC1TRUNUSU9OLTEp\">"
				     "  <table border=\"1\">"
				     "    <tr><th>Content</th></tr>"
				     "    <tr><td>Cell 1 content</td></tr>"
				     "    <tr><td>Cell 2 content</td></tr>"
				     "    <tr><td>Cell 3 content</td></tr>"
				     "  </table>"
				     "</div>"))))

   (body-content (with-lhtml-string()
		   (:div :class "main-page-container"
			 (:div :class "main-page-item"
			       (:div :class "grid-container-2-650px"
			             (:div :class "grid-item"
				           (:p "In the " (:a :href (the simple-html-page-url) (str (the simple-html-page-title))) " topic, the slot "
				               (:span :class "slot" "body")" was identified as the slot that needed to be populated with a html string in order for the object to be rendered as a web page. The whole page and be defined as a single string in this slot if required, but building a page from "(:span :class "object" "base-html-divs")" may lead to an easier to manage page and encourages modularity and reuse, which in turn provides a common look and feel to pages")
				           (:p "A "
				               (:span :class "object" "base-html-div")" is an object, which evaluates the "
				               (:span :class "slot" "div")" message to a html string and then we include that message in the objects parent "
				               (:span :class "slot" "body")". To populate the "
				               (:span :class "slot" "div")" slot, we need to write a html string to the "
				               (:span :class "slot" "inner-html")" slot. The "
				               (:span :class "object" "base-html-div")" object then wraps this string with some standard html when "
				               (:span :class "object" "div")" is evaluated."))
			             (:div :class "grid-item")
			             (:div :class "grid-item" 
				           (str (code-example (the code-1))))
			             (:div :class "grid-item" 
				           (:image :src (format nil "/~a-images/simple-base-html-div.png" (the publish-prefix)) :style "border: 2px solid; margin: 5px 0 0 3% ;width: auto; height: 200px;" ))
			             (:div :class "grid-item"
				           (:p "This method, passing the value of "
				               (:span :class "slot" "inner-html")" directly into a "
				               (:span :class "object" "base-html-div")" object works well for fairly simple cases and avoids the need for creating bespoke objects based on "
				               (:span :class "object" "base-html-div")", but for more complex calculations of the "
				               (:span :class "slot" "inner-html")" value, potentially involving a number of different inputs, it is often more practical and manageable to create specific objects by mixing in "
				               (:span :class "object" "base-html-div")))
			             (:div :class "grid-item")
			             (:div :class "grid-item" 
				           (str (code-example (the code-2))))
			             (:div :class "grid-item" 
				           (:image :src (format nil "/~a-images/base-html-div-object.png" (the publish-prefix)) :style "border: 2px solid; margin: 5px 0 0 3% ;width: auto; height: 200px;" ))
			             (:div :class "grid-item"
				           (:p "The difference between a "
				               (:span :class "object" "base-html-div")" "
				               (:span :class "slot" "inner-html")" and "
				               (:span :class "slot" "div")" is that "
				               (:span :class "slot" "div")" wraps the html defined in "
				               (:span :class "slot" "inner-html")" with a html div tag and an internally generated id attribue for that tag. "
				               (str (repl-example (the repl-1)))
				               "This div may then be surgically updated using AJAX, without the page being resubmitted. We'll cover using AJAX with GendL in the "
				               (:a :href (the using-ajax-url) (str (the using-ajax-title))) " topic, but for now the main thing to take away is how the use of AJAX may affect the granularity of a "
				               (:span :class "object" "base-html-div")". If a single "
				               (:span :class "object" "base-html-div")" was used for the whole page, then an AJAX update would have only marginal impact on both speed of display and bandwidth used. If every element in the page was its own "
				               (:span :class "object" "base-html-div")" the overhead would be significant. So there's obviously a compromise somewhere in the middle. Best advice would be to group elements likely to be updated as a result of the same event together in the same "
				               (:span :class "object" "base-html-div")" if possible."))))
			 ((:div :class "main-page-item")
			  (:h2 "Resources")
			  (str (the resource-links))))))))

				      
