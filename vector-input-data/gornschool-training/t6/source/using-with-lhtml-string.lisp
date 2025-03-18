(in-package :training-6)

(define-object using-with-lhtml-string (base-training-sheet)

  :computed-slots
  ((index-words (list "lhtml" "with-lhtml-string"  "str" "htm" "fmt"))

   (repl-0 (list (list :prompt "GWL-USER"
		       :command (list "(with-lhtml-string ()"
				      "   (:table (:tr (:td))))")
		       :output "\"<table><tr><td></td></tr></table>\"")
		 (list :prompt "GWL-USER"
		       :command (list "(with-lhtml-string (:indent t)"
				      "   (:table (:tr (:td))))")
		       :output (list "\""
				     "<table>"
				     "  <tr>"
				     "    <td></td>"
				     "  </tr>"
				     "</table>\""))))
		       
   (repl-1 (list (list :prompt "GWL-USER"
		       :command (list "(with-lhtml-string()"
				      "  (:br))")
		       :output "<br />")))

   (repl-2 (list (list :prompt "GWL-USER"
		       :command (list "(with-lhtml-string()"
				      "  (:span :class \"myclass\"))")
		       :output "\"<span class=\"myclass\"></span>\"")))
   (repl-3 (list (list :prompt "GWL-USER"
		       :command (list "(with-lhtml-string()"
				      "  (:table :border 1 :cellspacing 3 :cell-padding 5))")
		       :output "\"<table border=\"1\" cellspacing=\"3\" cell-padding=\"5\"></table>\"")))
   (repl-4 (list (list :prompt "GWL-USER"
		       :command (list "(with-lhtml-string()"
				      "  (:p \"This is content\"))")
		       :output "\"<p>This is content</p>\"")
		 (list :prompt "GWL-USER"
		       :command (list "(with-lhtml-string()"
				      "  (:span :class \"myclass\" \"This is more content\"))")
		       :output "\"<span class=\"myclass\">This is more content</span>\"")))
   (repl-5 (list (list :prompt "GWL-USER"
		       :command (list "(with-lhtml-string()"
				      "  (:span :class \"myclass\" \"This is more content\"))")
		       :output "\"<span class=\"myclass\">This is more content</span>\"")
		 (list :prompt "GWL-USER"
		       :command (list "(with-lhtml-string()"
				      "  ((:span :class \"myclass\") \"This is more content\")))")
		       :output "\"<span class=\"myclass\">This is more content</span>\"")))
   (repl-6 (list (list :prompt "GWL-USER"
		       :command (list "(with-lhtml-string()"
				      "  (:table :border 1" 
				      "    (:tr"  
				      "      (:td \"Cell 1\")"
				      "      (:td \"Cell 2\"))))")
		       :output "\"<table border=\"1\"><tr><td>Cell 1</td><td>Cell 2</td></tr></table>\"")))
   (repl-7 (list (list :prompt "GWL-USER"
		       :command (list "(let ((cell-1 \"Cell 1 content\")"
				      "      (cell-2 \"Cell 2 content\"))"
				      "  (with-lhtml-string ()"
				      "     (:table (:tr (:td (str cell-1)) "
				      "                  (:td (str cell-2))))))")
		       :output "\"<table><tr><td>Cell 1 content</td><td>Cell 2 content</td></tr></table>\"")))
   (repl-8 (list (list :prompt "GWL-USER"
		       :command (list "(let ((cell-1 1)"
				      "      (cell-2 2))"
				      "  (with-lhtml--string ()"
				      "     (:table (:tr (:td (fmt \"Cell ~a content\" cell-1)) "
				      "                  (:td (fmt \"Cell ~a content\" cell-2))))))")
		       :output "\"<table><tr><td>Cell 1 content</td><td>Cell 2 content</td></tr></table>\"")))

   (repl-9 (list (list :prompt "GWL-USER"
		       :command (list "(with-lhtml-string ()"
				      "  (:table (:tr "
				      "     (let ((lis (list 1 2)))"
				      "        (dolist (a lis)"
				      "           (htm (:td (fmt \"Cell ~a\" a))))))))")
		       :output "\"<table><tr><td>Cell 1</td><td>Cell 2</td></tr></table>\"")))	       
		       

   (body-content (with-lhtml-string ()
		   (:div :class "grid-container-2-650px"
			 (:div :class "grid-item"
			       (:p (:em (:b "LHTML")) " is a Markup Language included in Gendl (standing for "
				   (:i "Lisp HTML")") which provides a convenient way to generate html using lisp-like expressions. It is intended to be portable and shoud work with all ANSI standard Common Lisp Implementations.")
			       (:p "The main element of "(:em (:b "lhtml"))" is the macro "
				   (:span :class "macro" "with-lhtml-string")", which transforms its body into a html string")
			       
			       (:p "In general, we use "
				   (:span :class "macro" "with-lhtml-string")" when creating html strings for use in GWL web pages")
			       (:p (:span :class "macro" "with-lhtml-string")" takes an optional input "
				   (:span :class "general-keyword" ":indent")". By default it is nil which results in html with no unnecessary whitespace to minimise the bandwith being used. But for debugging purposes, the resulting html may be difficult to read. Setting "
				   (:span :class "general-keyword" ":indent")" to t causes line breaks to be inserted and nested tags properly indented"
				   (str (repl-example (the repl-0))))
			       (:h3 "Markup")
			       (:p "To generate html we use a nested list of s-expressions. Each list beginning with a keyword is transformed into a html tag of the same name by the following rules:"
				   (:ul (:li "If the list contains nothing but a "
					     (:em (:b "keyword"))" an empty element is written"
					     (str (repl-example (the repl-1))))
					(:li "If the "
					     (:em (:b "keyword"))" is followed by another "
					     (:em (:b "keyword"))", it's interpreted as an attribute and the next form is the value of that attribute"
					     (str (repl-example (the repl-2))))
					(:li "Multiple "
					     (:em (:b "attribute-value pairs"))" may be specified"
					     (str (repl-example (the repl-3))))
					(:li "The first form which isn't a keyword and follows either the tag or an attribute value is interpreted as the "
					     (:em (:b "tag content"))
					     (str (repl-example (the repl-4))))
					(:li "To make it slightly easier to read, the "
					     (:em (:b "tag and all attribute-value pairs"))" may be enclosed in an additional list, but this is purely optional"
					     (str (repl-example (the repl-5))))
					(:li "Tags may be embedded in other tags"
					     (str (repl-example (the repl-6))))))
			       (:h3 "Markup with computed content")
			       (:p "So far any tag content we have shown is static, so how do we handle computed content? If we just wish to output the value of a slot, we must wrap that slot with the macro "
				   (:span :class "macro" "str")
				   (str (repl-example (the repl-7))))
			       (:p "If we want to present the value of a slot with some formatting, rather than use the format function to format the value of the slot, we could wrap the value in the macro "
				   (:span :class "macro" "fmt")", which takes the same directives as the "
				   (:span :class "function" "format") "function"
				   (str (repl-example (the repl-8))) )
			       (:p "It is also possible to embed any type of Lisp processing within the body of "
				   (:span :class "macro" "with-lhtml-string")", although whenever we break out of the html generation to do some processing, once we restart html generation again we need to wrap the html generation in the " (:span :class "macro" "htm")" macro"
				   (str (repl-example (the repl-9))))
			       (:p "Finally, a word of warning. "
				   (:em (:b " lhtml knows nothing about html, it just processes what it is given according to the rules above")". So there is no checking to see if the first keyword is a valid html tag, or if the second keyword is a valid tag attribute. Thats your job!"))))))))
