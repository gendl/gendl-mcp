# Gendl Documentation - tutorial_6_object_definition

## wall-example-with-form.lisp - header
Source: gornschool-training/t6/source/wall-example-with-form.lisp
Type: tutorial

```
(in-package :training-6)


```

---

## wall-example-with-form.lisp - wall-example-with-form
Source: gornschool-training/t6/source/wall-example-with-form.lisp
Type: tutorial

```
(define-object wall-example-with-form (base-training-sheet)
  :input-slots
  ((getting-started-tutorial nil)
   (io-tutorial nil))

  :computed-slots
  ((code-1 (list "
```

---

## wall-example-with-form.lisp - wall
Source: gornschool-training/t6/source/wall-example-with-form.lisp
Type: tutorial

```
(define-object wall(box)"
		 " :input-slots"
		 " ((brick-height 45)"
		 "  (brick-length 180)"
		 "  (brick-width 90)"
		 "  (mortar-joint-width 10)"
		 "  (wall-length 3700)"
		 "  (wall-height 900))"
		 "..."
		 ")"))
   (code-2 (list "
```

---

## wall-example-with-form.lisp - wall-example-form
Source: gornschool-training/t6/source/wall-example-with-form.lisp
Type: tutorial

```
(define-object wall-example-form (base-html-page)"
		 " :objects"
		 " ((brick-height-fc :type 'number-form-control"
		 "                      :prompt \"Brick Height (mm)\""
		 "                      :default 45"
		 "                      :ajax-submit-on-change? t)"
		 "  (brick-length-fc :type 'number-form-control"
		 "                      :default 180"
		 "                      :prompt \"Brick Length (mm)\""
		 "                      :ajax-submit-on-change? t)"
		 "  (brick-width-fc :type 'number-form-control"
		 "                     :default 90"
		 "                     :prompt \"Brick Width (mm)\""
		 "                     :ajax-submit-on-change? t)"
		 "  (mortar-width-fc :type 'number-form-control"
		 "                      :default 10"
		 "                      :prompt \"Mortar Joint Width (mm)\""
		 "                      :ajax-submit-on-change? t)"
		 "  (wall-length :type 'number-form-control"
		 "                  :default 3700"
		 "                  :prompt \"Nominal Wall Length (mm)\""
		 "                  :ajax-submit-on-change? t)"
		 "  (wall-height :type 'number-form-control"
		 "                  :default 3700"
		 "                  :prompt \"Nominal Wall Height (mm)\""
		 "                  :ajax-submit-on-change? t)))"))

   (code-3 (list "
```

---

## wall-example-with-form.lisp - wall-example-form
Source: gornschool-training/t6/source/wall-example-with-form.lisp
Type: tutorial

```
(define-object wall-example-form (base-html-page)"
		 " :computed-slots"
		 " ((main-sheet-body (with-lhtml-string ()"
		 "                     (:table" 
		 "                       (:tr"
		 "                         (:td (str (the form-controls-section div))))))))"

		 ":objects"
		 " ((form-controls-section "
		 "         :type 'base-html-div"
		 "         :inner-html (with-lhtml-string ()"
		 "                         (:table"
		 "                           (dolist (obj (the form-controls))"
		 "                             (htm (:tr (:td (str (theo obj prompt)))"
		 "                                          (:td (str (theo obj form-control)))))))))"
		 "..."
		 "))"))

   (code-4 (list "
```

---

## wall-example-with-form.lisp - wall-example-form
Source: gornschool-training/t6/source/wall-example-with-form.lisp
Type: tutorial

```
(define-object wall-example-form (base-html-page)"
		 "..."
		 "..."
		 " :objects"
		 " ((wall :type 'gdl-user::wall"
		 "          :brick-height (the brick-height-fc value)"
		 "          :brick-length (the brick-length-fc value)"
		 "          :brick-width (the brick-width-fc value)"
		 "          :mortar-joint-width (the mortar-width-fc value)"
		 "          :wall-length (the wall-length value)"
		 "          :wall-height  (the wall-height value))"
		 "  ..."
		 "  ..."
		 ")"
		 ")"))

   (code-5 (list "
```

---

## wall-example-with-form.lisp - wall-example-form
Source: gornschool-training/t6/source/wall-example-with-form.lisp
Type: tutorial

```
(define-object wall-example-form (base-html-page)"
		 " :computed-slots"
		 " ((main-sheet-body (with-lhtml-string ()"
		 "                      (:table "
		 "                        (:tr "
		 "                          (:td (str (the form-controls-section div)))"
		 "                          (:td (str (the report-section div)))))))"
		 " )"
		 " :objects"
		 " ("
		 "   ..."
		 "   ..."
		 "  (report-section "
		 "       :type 'base-html-div"
		 "       :inner-html (with-lhtml-string ()"
		 "                      (:table"
		 "                        (:tr "
		 "                          (:td \"Actual Wall Length\")"
		 "                          (:td (str (the wall actual-wall-length))))"
		 "                        (:tr "
		 "                          (:td \"Actual Wall Height\")"
		 "                          (:td (str (the wall actual-wall-height))))"
		 "                        (:tr "
		 "                          (:td \"Number of Full Bricks\")"
		 "                          (:td (str (the wall full-bricks))))"
		 "                        (:tr "
		 "                          (:td \"Number of Half Bricks\")"
		 "                          (:td (str (the wall half-bricks))))"
		 "                        (:tr "
		 "                          (:td \"Mortar Mass\")"
		 "                          (:td (fmt \"~,1f\" (the wall mortar-mass)))))))"
		 "         ..."
		 "         ..."
		 "  ))"))

   (getting-started-title (if (the getting-started-tutorial) (the getting-started-tutorial tutorial-name) "Getting Started with GendL"))
   (getting-started-url (when (the getting-started-tutorial) (the getting-started-tutorial url)))
   (io-title (if (the io-tutorial) (the io-tutorial tutorial-name) "Reading From and Writing To Files"))
   (io-url (when (the io-tutorial) (the io-tutorial url)))			
   (body-content (with-cl-who-string()
		   (:div :class "main-page-container"
			 (:div :class "main-page-item"
			       (:div :class "grid-container-2-650px"
				     (:div :class "grid-item")
				     (:p "In the tutorial " (if (the getting-started-url)
								(htm (:a :href (the getting-started-url) (str (the getting-started-title))))
								(htm (str (the getting-started-title))))
					 " we developed an example application to build a model of a brick built wall. In that application, all inputs were provided as "
					 (:span :class "object-keyword" "input-slots")" with default values. In this topic, we'll use the wall application and extend it to include a web based front end to gather inputs via a form.")
				     (:p "The basic design concept is to have a web page as a top level object, which gathers the inputs and passes them to the wall application, which is defined as a child object of the web form. We will then retrieve the outputs from the wall application and display these on the web form")
				     (:p "So first, we need to familiarise ourselves with the required inputs")
				     (str (code-example (the code-1)))
				     (:p "Now we define the "
					 (:span :class "object" "form-controls")" to capture these inputs")
				     (str (code-example (the code-2)))
				     (:p "Having done that, we need to present them on the web page; using a "
					 (:span :class "object" "base-html-div")" is a convenient way to do this. Note that we use the "
					 (:span :class "slot" "form-controls")" slot to iterate through all the "
					 (:span :class "object" "form-control") " objects to present them on the web page. Also we would normally define "
					 (:span :class "object-keyword" "validation-function")" inputs, but have skipped those in this example")
				     (str (code-example (the code-3)))
				     (:p "Then we need to link the  "
					 (:span :class "object" "form-control")" "
					 (:span :class "slot" "value")" slots to the "
					 (:span :class "object-keyword" ":inputs")" to the wall object")
				     (str (code-example (the code-4)))
				     (:p "Finally, we need to define a "
					 (:span :class "object" "base-html-div")" to present the wall outputs on the web page. Because all of the "
					 (:span :class "object" "form-control")" objects have been defined with "
					 (:span :class "object-keyword" ":ajax-submit-on-change?")" set to T, any change to the form inputs will automatically trigger an update to the outputs display")
				     (str (code-example (the code-5)))
				     (:image :src (format nil "/~a-images/building-example.png" (the publish-prefix)) :style "border: 2px solid; margin: 5px 0 0 3% ;width: auto; height: 200px;" )	 
				     )
			       ((:div :class "main-page-item")
				(:h2 "Resources")
				(str (the resource-links)))))))

  
  ))

```

---

## assembly.lisp - header
Source: gornschool-training/t6/source/assembly.lisp
Type: tutorial

```
(in-package :training-6)

(defparameter *publish-prefix* "t6")
(defparameter *home* (merge-pathnames "../" (make-pathname :name nil :type nil :defaults (glisp:source-pathname))))

(with-all-servers
    (server)
    (gwl::publish-directory :prefix (format nil "/~a-css" *publish-prefix*)
                            :server server
                            :destination (namestring
                                          (merge-pathnames "resources/css/" *home*))))


```

---

## assembly.lisp - assembly
Source: gornschool-training/t6/source/assembly.lisp
Type: tutorial

```
(define-object assembly (base-tutorial-sheet)
  :input-slots
  ((previous-page nil)
   (next-page nil)
   (getting-started-tutorial nil)
   (io-tutorial nil)
   
   
```

---

## assembly.lisp - nil
Source: gornschool-training/t6/source/assembly.lisp
Type: tutorial

```
(define-object-url nil)
   (wall-example-url nil)
   (truss-example-url nil)
   (tutorial-name "Integrating with GendL's web server"))

  :computed-slots
  ((introduction (with-lhtml-string ()
		   (:p "One of the great features of GendL is its inbuilt webserver, the ability to generate web pages and the ability to link these web pages to the underlying GendL model. This tutorial cover the basics of delivering web pages and linking them with applications.")

                   (:p (:i "Note: The examples in this Tutorial depend on some patches to the GWL component of GendL which may not yet be built into the GendL distribution 
you are currently running. These patches can be loaded from the source file "
                           (:span :class "general-keyword" "gwl-patches.lisp")
                           " in the Resources section of the first slide linked below.")))))

  :objects
  ((simple-html-page :type 'simple-html-page
		       :pass-down (page-objects)
		       :publish-prefix *publish-prefix*
		       :page 1
		       :page-title "Creating and Publishing web pages"
		       :resources (list "gwl-patches.lisp"
					"basic-define-and-publish.lisp"
                                        (list :url (format nil "/~a-images/logo.png" *publish-prefix*) :title "Genworks Logo")
					(list :url (format nil "/~a-css/my-style.css" *publish-prefix*) :title"my-style.css")))
   
   (using-with-lhtml-string :type 'using-with-lhtml-string
			     :pass-down (page-objects)
			     :publish-prefix *publish-prefix*
			     :page 2
			     :page-title "Using with-lhtml-string"
			     :resources nil)
   
   (using-base-html-divs :type 'using-base-html-divs
		   :pass-down (page-objects)
		   :publish-prefix *publish-prefix*
		   :page 3
		   :page-title "Building pages from base-html-divs"
		   :resources (list "gwl-patches.lisp" "using-page-sections.lisp" )
		   :simple-html-page-title (the simple-html-page page-title)
		   :simple-html-page-url (the simple-html-page url)
		   :using-ajax-title (the using-ajax page-title)
		   :using-ajax-url (the using-ajax url))
   
   (using-form-controls :type 'using-form-controls
			:pass-down (page-objects)
			:publish-prefix *publish-prefix*
			:page 4
			:page-title "Gathering inputs using form-controls"
			:resources (list "gwl-patches.lisp" "basic-form-controls.lisp" "using-form-controls.lisp" ))
   
   (using-ajax :type 'using-ajax
	       :pass-down (page-objects)
	       :publish-prefix *publish-prefix*
	       :simple-html-page (the simple-html-page)
	       :using-form-controls (the using-form-controls)
	       :using-base-html-divs (the using-base-html-divs)
	       :page 5
	       :page-title "Using AJAX"
	       :resources nil)
   
   (file-io-1 :type 'file-io
	    :pass-down (page-objects)
	    :publish-prefix *publish-prefix*
	    :page 6
	    :page-title "Writing output for download"
	      :resources (list "gwl-patches.lisp" "simple-file-output.lisp"))
   
   (file-io-2 :type 'file-io-2
	    :pass-down (page-objects)
	    :publish-prefix *publish-prefix*
	    :page 7
	    :page-title "Uploading files"
	      :resources (list "gwl-patches.lisp" "file-upload-1.lisp" "file-upload-2.lisp" "file-upload-3.lisp" "birthdays.txt"))

   (displaying-graphics :type 'displaying-graphics
			:pass-down (page-objects)
			:publish-prefix *publish-prefix*
			:page 8
			:page-title "Displaying GendL graphics"
			:resources nil)
   
   (wall-example-form :type 'wall-example-with-form
		     :pass-down (page-objects)
		     :publish-prefix *publish-prefix*
		     :page 9
		     :getting-started-tutorial (the getting-started-tutorial)
		     :io-tutorial (the io-tutorial)
		     :page-title "Wall example - taking inputs from a form"
		     :resources (list "gwl-patches.lisp""wall.lisp" "wall-example-form.lisp"))
   
   (building-example-file :type 'building-example-file
		      :pass-down (page-objects)
		      :publish-prefix *publish-prefix*
		      :page 10
		      :getting-started-tutorial (the getting-started-tutorial)
		      :io-tutorial (the io-tutorial)
		      :page-title "Wall example - taking inputs from a file"
		      :resources (list "gwl-patches.lisp""building-application-file.lisp" "building-bom-input-output.lisp" "building-input.txt")))
   )
   
	       
   
  

;;(publish-gwl-app "/t6" "training-6::assembly")

;;(defparameter *t6-home*
;;  (merge-pathnames "../" (make-pathname :name nil :type nil :defaults (glisp:source-pathname))))

;;(gwl::publish-directory :prefix (format nil "/~a-images" *publish-prefix*)
;;			:destination (namestring (merge-pathnames "images/"  *t6-home*)))

;;(gwl::publish-directory :prefix (format nil "/~a-resources" *publish-prefix*)
;;			:destination (namestring (merge-pathnames "resources/source/" *t6-home*)))
;;(gwl::publish-directory :prefix (format nil "/~a-resource-images" *publish-prefix*)
;;			:destination (namestring (merge-pathnames "resources/images/" *t6-home*)))
;;
;;(gwl::publish-file :path "/style.css"
;;		   :file (namestring (merge-pathnames "css/style.css" *t6-home*)))











```

---

## displaying-graphics.lisp - header
Source: gornschool-training/t6/source/displaying-graphics.lisp
Type: tutorial

```
(in-package :training-6)


```

---

## displaying-graphics.lisp - displaying-graphics
Source: gornschool-training/t6/source/displaying-graphics.lisp
Type: tutorial

```
(define-object displaying-graphics (base-training-sheet)
  :computed-slots
  ((:body-content (with-cl-who-string()
		   (:div :class "main-page-container"
			 (:div :class "main-page-item"
			       (:div :class "grid-container-2-650px"
				     (:div :class "grid-item")
				     (:p "We're still working on this, should be available soon!"))))))))

```

---

## using-form-controls.lisp - header
Source: gornschool-training/t6/source/using-form-controls.lisp
Type: tutorial

```
(in-package :training-6)


```

---

## using-form-controls.lisp - using-form-controls
Source: gornschool-training/t6/source/using-form-controls.lisp
Type: tutorial

```
(define-object using-form-controls (base-training-sheet)

  :computed-slots
  ((index-words (list "error" "failed-value" "html-string" "form control validation" "restore-defaults!" "Reset a form control" "form-control"))

   (code-1 (list "
```

---

## using-form-controls.lisp - form-control-layout
Source: gornschool-training/t6/source/using-form-controls.lisp
Type: tutorial

```
(define-object form-control-layout (base-html-page)"
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
   
   (code-2 (list "
```

---

## using-form-controls.lisp - form-control-validation
Source: gornschool-training/t6/source/using-form-controls.lisp
Type: tutorial

```
(define-object form-control-validation (base-html-page)"
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


```

---

## building-example-file.lisp - header
Source: gornschool-training/t6/source/building-example-file.lisp
Type: tutorial

```
(in-package :training-6)


```

---

## building-example-file.lisp - building-example-file
Source: gornschool-training/t6/source/building-example-file.lisp
Type: tutorial

```
(define-object building-example-file (base-training-sheet)
  :input-slots
  ((getting-started-tutorial nil)
   (io-tutorial nil))
  :computed-slots
  ((getting-started-title (if (the getting-started-tutorial) (the getting-started-tutorial tutorial-name) "Getting Started with GendL"))
   (getting-started-url (when (the getting-started-tutorial) (the getting-started-tutorial url)))
   (io-title (if (the io-tutorial) (the io-tutorial tutorial-name) "Reading From and Writing To Files"))
   (io-url (when (the io-tutorial) (the io-tutorial url)))

   (code-1 (list "(in-package :gwl-user)"
		 ""
		 " 
```

---

## building-example-file.lisp - building-application-file
Source: gornschool-training/t6/source/building-example-file.lisp
Type: tutorial

```
(define-object building-application-file (base-ajax-sheet)"
		 ""
		 "  :computed-slots"
		 "  ((uploaded-path \"\" :settable)"
		 "   (main-sheet-body (with-cl-who-string ()"
		 "               (str (the development-links))"
		 "               (when (= (length (the uploaded-path)) 0)"
		 "                  (htm (with-html-form (:multipart? t :cl-who? t)"
		 "                            (:table "
		 "                              (:tr "
		 "                                (:td"
		 "                                  (:input :type \"file\" :name :uploaded-path)))"
		 "                                    (:tr (:td (:input :type \"submit\" :name \"upload\" :value \"Upload\")))))))))"
		 ")"
		 ""
		 " :objects"
		 " ((building :type 'building"
		 "            :input-filename (when (> (length (the uploaded-path)) 1) (the uploaded-path)))"))

  (code-2 (list "
```

---

## building-example-file.lisp - building-application-file
Source: gornschool-training/t6/source/building-example-file.lisp
Type: tutorial

```
(define-object building-application-file (base-ajax-sheet)"
		""
		" :computed-slots"
		" ((uploaded-path \"\" :settable)"
		"  (output-path (merge-pathnames \"building-output.txt\" (the uploaded-path)))"
		"  (main-sheet-body (with-cl-who-string ()"
		"                     (str (the development-links))"
		"                     (when (= (length (the uploaded-path)) 0)"
		"                        (htm (with-html-form (:multipart? t :cl-who? t)"
		"                            (:table"
		"                              (:tr"
		"                                (:td "
		"                                  (:input :type \"file\" :name :uploaded-path)))"
		"                              (:tr "
		"                                (:td "
		"                                  (:input :type \"submit\" :name \"upload\" :value \"Upload\")))))))"
		"                     (when (> (length (the uploaded-path)) 1)"
		"                         (htm (:p \"Click \"(:a :href (the output-url) \"here\") "
"                                                  \" to download the output\")))))"
		""
		"   (output-url (let ((url \"/output.txt\"))"
		"                 (the building write-bom-file!)"
		"                 (gwl::unpublish url)"
		"                 (gwl::publish-file :path url"
		"                                    :file (the output-path))"
		"                 url)))"
		""
		" :objects"
		" ((building :type 'building"
		"            :input-filename (when (> (length (the uploaded-path)) 1) (the uploaded-path))"
		"            :output-filename (when (> (length (the uploaded-path)) 1) (the output-path)) )))"))
	     
   (body-content (with-cl-who-string()
		   (:div :class "main-page-container"
			 (:div :class "main-page-item"
			       (:div :class "grid-container-2-650px"
				     (:div :class "grid-item")
				     (:p "In the tutorial " (if (the getting-started-url)
								(htm (:a :href (the getting-started-url) (str (the getting-started-title))))
		 						(htm (str (the getting-started-title))))
					 " we developed an example application to build a model of a building. In that application, all inputs were provided as "
					 (:span :class "keyword" "input-slots")" with default values. We then extended that in the tutorial "
					 (if (the io-url)
					     (htm (:a :href (the io-url) (str (the io-title))))
					     (htm (str (the io-title))))
					 " to take inputs from a file and deliver the output as a file. In this topic, we'll use the building application and extend it to include a web based front end to upload a file via a form.")
				     (:p "The basic design concept is to have a web page as a top level object, which provides a form to upload an input file. The building application based on file inputs will be a child object of the file upload form, and once the file has been uploaded it will be passed to te building application as an inout. An output file will be written and a link to that file displayed on the file upload page")
				     (:p "The first step is to create a file upload form and incorporate the building application as a child object")
				     (str (code-example (the code-1)))
				     (:p "Not that for expediency, the building object has been moved to the gwl-user package for this example")
				     (:p "To generate the output, we need to run the write-bom-file! function in the bulding object, following which we need to publish that file and provide a link to it on the web page. As we will demand the url of the published file, we can use side effecting to run the file generation and publishing, rather than running them as a function. To do this we use a computed-slot output-url and the body form of the let macro to evaulate the file write and publish functions sequentially. We've also defined the output filename, such that it will be in the same directory as the uploaded file, but with a different name"))))))))

```

---

## file-io-2.lisp - header
Source: gornschool-training/t6/source/file-io-2.lisp
Type: tutorial

```
(in-package :training-6)


```

---

## file-io-2.lisp - file-io-2
Source: gornschool-training/t6/source/file-io-2.lisp
Type: tutorial

```
(define-object file-io-2 (base-training-sheet)


  :computed-slots
  ((index-words (list "after-set!" "upload" ":enctype" "multipart/form-data" "with-form-string" "with-lhtml-string"))
   
   (code-1 (list "
```

---

## file-io-2.lisp - file-upload-1
Source: gornschool-training/t6/source/file-io-2.lisp
Type: tutorial

```
(define-object file-upload-1 (base-html-page)"
		 "  :computed-slots"
		 "  ((uploaded-path  \" \" :settable)"
		 "   (body"
		 "     (with-lhtml-string ()"
		 "        (str (the development-links))"
		 "        (str (with-form-string (:enctype  \"multipart/form-data \")"
		 "           (:table (:tr (:td (:input :type  \"file \" :name :uploaded-path :value (the uploaded-path))))"
		 "                   (:tr (:td (:input :type  \"submit \" :name  \"upload \" :value  \"Upload \"))"
                 "                   (:td (str (the uploaded-path)))))))))))"))

   (code-2 (list "
```

---

## file-io-2.lisp - file-upload-2
Source: gornschool-training/t6/source/file-io-2.lisp
Type: tutorial

```
(define-object file-upload-2 (base-html-page)"
		 "  :computed-slots"
		 "  ((uploaded-path  \" \" :settable)"
		 "   (body"
		 "     (with-lhtml-string ()"
		 "        (str (the development-links))"
		 "        (when (= (length (the uploaded-path)) 0)"
		 "          (htm (str (with-form-string (:enctype  \"multipart/form-data \")"
		 "             (:table (:tr (:td (:input :type  \"file \" :name :uploaded-path :value (the uploaded-path))))"
		 "                     (:tr (:td (:input :type  \"submit \" :name  \"upload \" :value  \"Upload \"))))))))"
		 "        (when (> (length (the uploaded-path)) 0)"
		 "          (htm (str (fmt  \"The file has been uploaded to ~a\" (the uploaded-path)))"
		 "               (str (with-form-string (:enctype \"multipart/form-data\")"
		 "                          (:input :type \"submit\" :name \"reset\" :value \"Reset Form\")))))))"
		 " :functions"
		 "  ((after-set! ()"
		 "            (when (member \"Reset Form\" (the query-plist) :test 'equalp)"
		 "                 (when (probe-file (the uploaded-path)) (delete-file (the uploaded-path)))"
		 "                 (the (restore-slot-default! :uploaded-path ))))))"))

   (code-3 (list "
```

---

## file-io-2.lisp - file-upload-3
Source: gornschool-training/t6/source/file-io-2.lisp
Type: tutorial

```
(define-object file-upload-3 (base-html-page)"
		 "  :computed-slots"
		 "  ((uploaded-path  \" \" :settable)"
		 "   (body"
		 "     (with-lhtml-string ()"
		 "        (str (the development-links))"
		 "        (when (= (length (the uploaded-path)) 0)"
		 "          (htm (str (with-form-string (:enctype  \"multipart/form-data \")"
		 "             (:table (:tr (:td (:input :type  \"file \" :name :uploaded-path :value (the uploaded-path))))"
		 "                     (:tr (:td (:input :type  \"submit \" :name  \"upload \" :value  \"Upload \"))))))))"
		 "        (when (> (length (the uploaded-path)) 0)"
		 "          (htm (str (fmt  \"The file has been uploaded to ~a\" (the uploaded-path)))"
		 "               (:p \"The file contents are\")"
		 "                (:table :border 1 (:tr (:td :colspan 2 (str (first (first (the file-content))))))"
		 "                     (dolist (line (cdr (the file-content)))"
		 "                          (htm (:tr (:td (str (first line)))"
		 "                                    (:td (str (second line)))))))"
		 "               (str (with-form-string (:enctype \"multipart/form-data\")"
		 "                          (:input :type \"submit\" :name \"reset\" :value \"Reset Form\")))))))"
		 " :functions"
		 "  ((after-set! ()"
		 "            (when (member \"Reset Form\" (the query-plist) :test 'equalp)"
		 "                 (when (probe-file (the uploaded-path)) (delete-file (the uploaded-path)))"
		 "                 (the (restore-slot-default! :uploaded-path ))))))"))
                                    
   
   (body-content (with-cl-who-string ()
		   (:div :class "main-page-container"
			 (:div :class "main-page-item"
			       (:div :class "grid-container-2-650px"
				     (:div :class "grid-item"
					   (:h3 "Uploading files")
					   (:p "To upload a file we need to submit a form, containing the file, to the server. This is a traditional multipart form submission.")
					   (:p "Gendl provides the "
					       (:span :class "macro" "with-form-string") " macro to create a form. When using this macro for file uploads we must specify the keyword argunemt "(:span :class "general-keyword" "enctype") " with a value of "
					       (:em (:b "multipart/form-data")) " to ensure the file is ransmited to the server correctly")
					   (:p "Within the body of the form we need two "
					       (:em (:b "input")) " tags. "
					       (:ul
						(:li "The first is of type file which gives us a Browse button to enable a file to be selected. Within this tag we also need to assign a :name, the value of which must correspond with the name of a :settable :computed-slot (its symbol name, so preceeded witha : character). The default value of that slot shoud be an empty string")

						(:li "The second is a standard submit button, which submits the form")))
					   (str (code-example (the code-1)))
					   (:p "In the above example we also include an output of the value of "
					       (:span :class "slot" "uploaded-path") " The value of this slot is automatically set to the location of the uploaded file "
					       (:b "on the server") " when the form is submitted")
					   
					   (:p "Screenshot on opening the form ")
					   (:image :src (format nil "/~a-images/upload-1.png" (the publish-prefix))
						   :style "border: 2px solid; margin: 5px 0 0 3% ;width: auto; height: 160px;" )
		   
		   
					   (:p "Once a file has been selected the "
					       (:em (:b "no file selected")) " text changes to the filename of the selected file. ")
					   (:p "Screenshot after selecting the file, but before hitting "
					       (:em (:b "Upload")))
					   (:image :src (format nil "/~a-images/upload-1a.png" (the publish-prefix))
						   :style "border: 2px solid; margin: 5px 0 0 3% ;width: auto; height: 160px;" )
					   (:p "If " (:em (:b "Upload")) " is then clicked the file is uploaded to the server and the text alongside the Browse button will revert to "
					       (:em (:b "no file selected")) ". As part of this upload, as well as transmitting the file to the server, the slot representing the server pathname to the file (in this case "
					       (:span :class "slot" ":uploaded-path") ") is set. One important thing to note: the default value for "
					       (:span :class "slot" "uploaded-path")" is set to an empty string, rather then nil. This is to ensure that when the "
					       (:span :class "slot" "uploaded-path")" value is set as part of the upload it is set to a string value and not a symbol - on Windows in particular with a drive letter followed by a "
					       (:em (:b ":"))" character this will cause problems as Lisp will believe its a package...")
					   (:image :src (format nil "/~a-images/upload-1b.png" (the publish-prefix))
						   :style "border: 2px solid; margin: 5px 0 0 3% ;width: auto; height: 160px;" )
					   (:p "It is often useful to conditionalise the display of the Browse and Submit buttons depending on the value of the uploaded file slot and
possibly present an alternative button which will reset the form to original values. The code below includes a "
					       (:em (:b "Reset")) " button to perform the reset. We use the "
					       (:span :class "function" "after-set!")" function, check what values are in the "
					       (:span :class "slot" "query-plist")" and if the value of this button "
					       (:em (:b "(\"Reset Form\")")) " is present firstly delete the uploaded file if it exists and then run the "
					       (:span :class "function" "restore-slot-default!")" function on "
					       (:span :class "slot" "upoaded-path") )
					   (str (code-example (the code-2)))
					   
					   (:p "Screenshot after hitting "(:em (:b "Upload")))
					   (:image :src (format nil "/~a-images/upload-2.png" (the publish-prefix))
						   :style "border: 2px solid; margin: 5px 0 0 3% ;width: auto; height: 130px;" )
					   (:p "Screenshot after hitting "(:em (:b "Reset Form")))
					   (:image :src (format nil "/~a-images/upload-3.png" (the publish-prefix))
						   :style "border: 2px solid; margin: 5px 0 0 3% ;width: auto; height: 130px;" )
					   (:p "Once the file has been uploaded to the server, it will generally need processing to access the data it contains. In the example below the uploaded file is processed as the page demands "
					       (:span :class "slot" "(the file-content)")" to be evaluated, but for more complex data processing we may use the "
					       (:span :class "function" "after-set!")" function, test for presence of the Upload Button value ("(:em (:b "Upload"))") in the "(:span :class "slot" "query-plist")" and initiate processing based on that. (Note that the function "(:span :class "function" "read-file")" is a custom function included in the resources file)")
					   (str (code-example (the code-3)))
					   (:image :src (format nil "/~a-images/upload-4.png" (the publish-prefix))
						   :style "border: 2px solid; margin: 5px 0 0 3% ;width: auto; height: 170px;" ))))
			 (:div :class "main-page-item"
			       (:h2 "Resources")
			       (str (the resource-links))))))))

```

---

## simple-html-page.lisp - header
Source: gornschool-training/t6/source/simple-html-page.lisp
Type: tutorial

```
(in-package :training-6)


```

---

## simple-html-page.lisp - simple-html-page
Source: gornschool-training/t6/source/simple-html-page.lisp
Type: tutorial

```
(define-object simple-html-page (base-training-sheet)

  :computed-slots
  ((index-words (list "with-lhtml-string" "gwl:*delevoping?*" "publish-gwl-app" "base-html-page" "merge-pathnames"
		      "gwl::publish-file" "publish-file" "gwl::publish-directory" "publish-directory" "CSS" "images"
		      "body" "additional-header-content" "development-links"))
		
   (code-1 (list"(in-package :gwl-user)"
		""
		"
```

---

## simple-html-page.lisp - sample-page
Source: gornschool-training/t6/source/simple-html-page.lisp
Type: tutorial

```
(define-object sample-page (base-html-page)"
		"  :computed-slots"
		"   ((body \"My first web page\")))"
		""
		"(publish-gwl-app \"/sample-page\" 'sample-page)"))

   (code-2 (list "
```

---

## simple-html-page.lisp - sample-html-page
Source: gornschool-training/t6/source/simple-html-page.lisp
Type: tutorial

```
(define-object sample-html-page (base-html-page)"
		 "  :computed-slots"
		 "  ((body (with-lhtml-string ()"
		 "                        (:table"
		 "                            (:tr (:th \"Day\") (:th \"Number\"))"
		 "                            (:tr (:td \"Sunday\") (:td \"1\"))"
		 "                            (:tr (:td \"Monday\") (:td \"2\"))"
		 "                            (:tr (:td \"Tuesday\") (:td \"3\"))"
		 "                            (:tr (:td \"Wednesday\") (:td \"4\"))"
		 "                            (:tr (:td \"Thursday\") (:td \"5\"))"
		 "                            (:tr (:td \"Friday\") (:td \"6\"))"
		 "                            (:tr (:td \"Saturday\") (:td \"7\")))))))"
		 ""
		 "(publish-gwl-app \"/sample-html-page\" 'sample-html-page)"))

   (code-3 (list "
```

---

## simple-html-page.lisp - sample-inline-styled-html-page
Source: gornschool-training/t6/source/simple-html-page.lisp
Type: tutorial

```
(define-object sample-inline-styled-html-page  (base-html-page)"
		 "  :computed-slots"
		 "  ((body (with-lhtml-string ()"
		 "              (:table :style \"border-style: solid;\""
		 "                (:tr (:th \"Day\") (:th \"Number\"))"
		 "                (:tr (:td \"Sunday\") (:td :style \"text-align: center;\" \"1\"))"
		 "                (:tr (:td \"Monday\") (:td :style \"text-align: center;\" \"2\"))"
		 "                (:tr (:td \"Tuesday\") (:td :style \"text-align: center;\" \"3\"))"
		 "                (:tr (:td \"Wednesday\") (:td :style \"text-align: center;\" \"4\"))"
		 "                (:tr (:td \"Thursday\") (:td :style \"text-align: center;\" \"5\"))"
		 "                (:tr (:td \"Friday\") (:td :style \"text-align: center;\" \"6\"))"
		 "                (:tr (:td \"Saturday\") (:td :style \"text-align: center;\" \"7\")))))))"
		 ""
		 "(publish-gwl-app \"/sample-inline-styled-html-page\" 'sample-inline-styled-html-page)"))
   (css-1 (list "table {border-style: solid;}"
		"td {text-align: center;"
		"    font-size: 12px;}"))

   (code-4 (list "(defparameter *home*"
		 "(merge-pathnames \"../\" "
		 "                  (make-pathname :name nil" 
		 "                                 :type nil"
		 "                                 :defaults (glisp:source-pathname))))"
		 "(gwl::publish-file :path \"/my-style.css\""
		 "                   :file (namestring (merge-pathnames \"css/my-style.css\" *home*)))" ))
   
   (code-5 (list "
```

---

## simple-html-page.lisp - sample-external-styled-html-page
Source: gornschool-training/t6/source/simple-html-page.lisp
Type: tutorial

```
(define-object sample-external-styled-html-page (base-html-page)"
		 "  :computed-slots"
		 "  ((additional-header-content (with-lhtmlstring()
				                 ((:link :rel \"stylesheet\" :href \"/my-style.css\"))))"
		 "   (main-sheet-body "
		 "    (with-lhtml-string ()"
		 "     (:table"
		 "      (:tr (:th \"Day\") (:th \"Number\"))"
		 "      (:tr (:td \"Sunday\") (:td \"1\"))"
		 "      (:tr (:td \"Monday\") (:td \"2\"))"
		 "      (:tr (:td \"Tuesday\") (:td \"3\"))"
		 "      (:tr (:td \"Wednesday\") (:td \"4\"))"
		 "      (:tr (:td \"Thursday\") (:td \"5\"))"
		 "      (:tr (:td \"Friday\") (:td \"6\"))"
		 "      (:tr (:td \"Saturday\") (:td \"7\")))))))"
		 ""
		 "(publish-gwl-app \"/sample-external-styled-html-page\" 'sample-external-styled-html-page)"))

   (code-6 (list "(gwl::publish-directory :prefix \"/images\""
		 "                        :destination (namestring (merge-pathnames"
		 "                                                     \"images/\""
		 "                                                     *home*)))"))

   
   (code-7 (list "
```

---

## simple-html-page.lisp - sample-image-html-page
Source: gornschool-training/t6/source/simple-html-page.lisp
Type: tutorial

```
(define-object sample-image-html-page (base-html-page)"
		 " :computed-slots"
		 " ((additional-header-content (with-lhtml-string()"
		 "                              (:link :rel \"stylesheet\" :href \"/my-style.css\")))"
		 "  (body (with-lhtml-string ()"
		 "              (:p \"The Genworks International Logo\")"
		 "              (:img :src \"/images/logo.png\")))))"
		 ""
		 "(publish-gwl-app \"/sample-image-html-page\" 'sample-image-html-page)"))
   
   (code-8 (list "
```

---

## simple-html-page.lisp - sample-image-development-links
Source: gornschool-training/t6/source/simple-html-page.lisp
Type: tutorial

```
(define-object sample-image-development-links (base-html-page)"
		 " :computed-slots"
		 " ((additional-header-content (with-lhtml-string()"
		 "                              (:link :rel \"stylesheet\" :href \"/my-style.css\")))"
		 "  (body (with-lhtml-string ()"
		 "            (when gwl:*developing?* (str (the development-links)))"
		 "              (:p \"The Genworks International Logo\")"
		 "              (:img :src \"/images/logo.png\")))))"
		 " "
                 " (publish-gwl-app \"/sample-image-development-links\" 'sample-image-development-links)"))

   
   (body-content (with-lhtml-string ()
		   (:div :class "grid-container-2-650px"
			 (:div :class "grid-item"
			       (:p "Fundamentally, the steps for creating and publishing a web page are:"
				   (:ul (:li "Define an object mixing in " (:span :class "object" "base-html-page"))
					(:li "In that object define a slot called "
					     (:span :class "slot" "body"))
					(:li "Assign a string, or a function/macro that evaluates to a string, to the slot "
					     (:span :class "slot" "body"))
					(:li "Call the function "
					     (:span :class "function" "publish-gwl-app") "to publish the object to a specified url"))
                                   "The above steps are sufficient to spin up the web page while using your currently running GendL
                                   development session as the host (server). Running this way, and as described in the rest of this Tutorial, you as well as colleagues/clients
on your local private network will be able to access the web page/application as you are developing it. If you want to distribute and scale up your website/application
more broadly, then you'd have to arrange for appropriate hosting and devops/continuous integration resources to launch and maintain your site. "
                                   (:a :href "https://infra.gornskew.com" "Gorn Skew Infrastructure")
                                   " (a sibling entity to the Gorn School) hopes soon to start offering turnkey hosting services for Gendl applications, so please stay tuned if you are interested.")
                               
			       (:p "Now on to some examples. Consider the following code"
				   (str (code-example (the code-1))))
			       (:p "The first thing to note is that we are working in the "
				   (:span :class "package-name" "gwl-user")
                                   " package. This is a pre-defined package which gives us access to all the same symbols as "
                                   (:span :class "package-name" "gdl-user") 
                                   " does, plus some extra web-specific ones."
                                   "The object is using the "
				   (:span :class "object" "base-html-page") " mixin and we have just assigned a simple string to the "
				   (:span :class "slot" "body")" slot. In order to publish the web page, we use the "
				   (:span :class "function" "publish-gwl-app")" function. The first argument is the url that we wish to publish to and the second-argument is the object being used to deliver the web page content. "
				   (:em "Note that we are prepending the object name with the package-name.")" If we now go to a web browser and evaluate "
				   (:em (:b "http://localhost:9080/basic"))" (assuming your GendL session is running on post 9080 we will see the following)")
			       (:image :src (format nil "/~a-images/basic-page.png" (the publish-prefix)) :style "margin: 5px 0 0 3% ;width: auto; height: 100px;" )
			       (:p "A couple of things to note here:"
				   (:ul (:li "Although we entered a url "
					     (:em (:b "basic"))", the url has been re-written once the page has presented. This is done automatically to allow GendL to track sessions and deliver pages to the correct session")
					(:li "The browser tab is automatically named with the object being displayed (although this behaviour can be over-ridden)")))
			       (:h3 "Writing html")
			       (:p "Within the text body of the slot "
				   (:span :class "slot" "body")", standard html may be included. However, using the "
				   (:span :class "macro" "with-lhtml-string")" macro enables html markup to be specified in a lisp-like way. We'll cover "
				   (:span :class "macro" "with-lhtml-string")" in more detail later, but just as a taster heres what it looks like"
				   (str (code-example (the code-2)))
				   "which generates the web page below")
			       (:image :src (format nil "/~a-images/basic-lhtml.png" (the publish-prefix)) :style "margin: 5px 0 0 3% ;width: auto; height: 200px;" )
			       (:h3 "Applying Style")
			       (:p "There are 2 general approached we can take for stying a page, in-line style or incorporation of a cascading style sheet (CSS)")
			       (:p "In-line style is just a case of updaing the html with style attributes"
				   (str (code-example (the code-3)))
				   "which generates the web page below")
			       (:image :src (format nil "/~a-images/basic-styled.png" (the publish-prefix)) :style "margin: 5px 0 0 3% ;width: auto; height: 200px;" )
			       (:p "If we want to use a style sheet there are 3 steps"
				   (:ul (:li "Define the style sheet")
					(:li "Publish the style sheet")
					(:li "Load the style sheet in the page by including it in the additional-header-content slot")
					(:li "Update the html to reference the style class")))
			       (:p "So, first the cascading style sheet which we have named my-style.css")
			       (str (code-example (the css-1)))
			       (:p "We are storing the css file in a folder called css at the same level as the source file folder. To publish the style sheet we have to povide its location in the filesystem. We could do this explicitly, and that may be fine if the code is being developed on the same operating system as it is deployed on. However if that isn't the case then the pathnames would be different (drive letter for windows, // for unix or linux). So you get around that we can use the "
				   (:span :class "function" "merge-pathnames")" function which provides operating system independency. We would still need to maintain the relative location of the source code folder and the css folder, but if we could do this then the "
				   (:span :class "function" "merge-pathnames")" approach is quite an attractive solution. We choose to define a home directory as the folder above the source code folder and assign it as a parameter "
				   (:em (:b "*home*"))" so we can access it globally. Having done that we then call the "
				   (:span :class "function" "gwl::publish-file")" function, referencing our stylesheet and publishing it as \"/my-style.css\"")
			       (str (code-example (the code-4)))
			       (:p "Next we add a slot to our object called "
				   (:span :class "slot" "additional-header-content")" and specify a stylesheet link to the my-style.css. Because the stylesheet specifies styles for the "
				   (:em (:b "table"))" and "
				   (:em (:b "td"))" html tags we don't need to add any class attributes to the html. Finally we publish the object using "
				   (:span :class "function" "publish-gwl-app"))
			       (str (code-example (the code-5)))
			       (:p "The resulting web-page will look like this")
			       (:image :src (format nil "/~a-images/basic-css.png" (the publish-prefix)) :style "margin: 5px 0 0 3% ;width: auto; height: 200px;" )
			       (:h3 "Adding images")
			       (:p "The process of adding an image is similar to the style sheet process"
				   (:ul (:li "Store an image on the file system")
					(:li "Publish the image")
					(:li "Reference the published image in the web page object")))
			       (:p "We could use the "
				   (:span :class "function" "gwl::publish-file")" function to publish the image, however we may have a number of images so the "
				   (:span :class "function" "gwl::publish-directory")" function is quite a useful alternative. It simply takes a directory and publishes each file in that directory, using the files name and extension, optionally with a prepended virtual directory. In the case below all the files in the images folder below "
				   (:em (:b "*home*"))" are being published to the /images virtual directory")
			       (str (code-example (the code-6)))
			       (:p "Finally we use the :img tag to display the image stores in our images folder")
			       (str (code-example (the code-7)))
			       (:image :src (format nil "/~a-images/basic-logo.png" (the publish-prefix)) :style "margin: 5px 0 0 3% ;width: auto; height: 200px;" )
			       (:h3 "Debugging Aids")
			       (:p "When developing web page content, it is often useful to incorporate a few debugging aids. The most important is probably to include links to enable the object model to be updated dynamically (equivalent to Mode..Update in Geysr or "
				   (:span :class "function" "(the update!)")" in the REPL) and to be able to break on the object model directly from the web page itself (equivalent to Mode..Set Self in Geysr). To do this, we print a predefined slot "
				   (:span :class "slot" "(the development-links)")" onto the web page. For convenience its often useful to conditionalise this based on the value of a paremeter indication whether or not we are in development mode. In this case we are using the "
				   (:em (:b "gwl:*developing?*"))" parameter, which defaults to T")
			       (str (code-example (the code-8))) 
			       (:image :src (format nil "/~a-images/basic-devlinks.png" (the publish-prefix)) :style "margin: 5px 0 0 3% ;width: auto; height: 200px;" ))
			 (:div :class "grid-item"
			       (:h2 "Resources")
			       (str (the resource-links))))))))



```

---

## using-ajax.lisp - header
Source: gornschool-training/t6/source/using-ajax.lisp
Type: tutorial

```
(in-package :training-6)


```

---

## using-ajax.lisp - using-ajax
Source: gornschool-training/t6/source/using-ajax.lisp
Type: tutorial

```
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

```

---

## using-base-html-divs.lisp - header
Source: gornschool-training/t6/source/using-base-html-divs.lisp
Type: tutorial

```
(in-package :training-6)


```

---

## using-base-html-divs.lisp - using-base-html-divs
Source: gornschool-training/t6/source/using-base-html-divs.lisp
Type: tutorial

```
(define-object using-base-html-divs (base-training-sheet)
  :input-slots
  (simple-html-page-title
      simple-html-page-url
    using-ajax-title
    using-ajax-url)
  
  :computed-slots
  ((index-words (list "base-html-div" "div" "inner-html" "AJAX"))
   (code-1 (list "(in-package :gwl-user)"
		 "
```

---

## using-base-html-divs.lisp - simple-page-with-section
Source: gornschool-training/t6/source/using-base-html-divs.lisp
Type: tutorial

```
(define-object simple-page-with-section (base-html-page)"
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
		 "
```

---

## using-base-html-divs.lisp - simple-page-with-custom-section
Source: gornschool-training/t6/source/using-base-html-divs.lisp
Type: tutorial

```
(define-object simple-page-with-custom-section (base-html-page)"
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
		 "
```

---

## using-base-html-divs.lisp - base-html-div-1
Source: gornschool-training/t6/source/using-base-html-divs.lisp
Type: tutorial

```
(define-object base-html-div-1 (base-html-div)"
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

				      

```

---

## using-with-lhtml-string.lisp - header
Source: gornschool-training/t6/source/using-with-lhtml-string.lisp
Type: tutorial

```
(in-package :training-6)


```

---

## using-with-lhtml-string.lisp - using-with-lhtml-string
Source: gornschool-training/t6/source/using-with-lhtml-string.lisp
Type: tutorial

```
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

```

---

## file-io.lisp - header
Source: gornschool-training/t6/source/file-io.lisp
Type: tutorial

```
(in-package :training-6)


```

---

## file-io.lisp - file-io
Source: gornschool-training/t6/source/file-io.lisp
Type: tutorial

```
(define-object file-io (base-training-sheet)

  :computed-slots
  ((code-1 (list "(in-package :gwl-user)"
		 ""
		"
```

---

## file-io.lisp - simple-file-output
Source: gornschool-training/t6/source/file-io.lisp
Type: tutorial

```
(define-object simple-file-output (base-html-page)"
		" (:computed-slots"
		"  ((file-contents (let ((line (list 1 2 3 4 5 6 7)))"
		"                    (format nil \"~{Line ~a of my file~^~%~}\" line))))"
		 "  ))"))
   (code-2 (list "(in-package :gwl-user)"
		 ""
		 "
```

---

## file-io.lisp - simple-file-output
Source: gornschool-training/t6/source/file-io.lisp
Type: tutorial

```
(define-object simple-file-output (base-html-page)"
		 " :computed-slots"
		 "  ((file-contents (let ((line (list 1 2 3 4 5 6 7)))"
		 "                        (format nil \"~{Line ~a of my file~^~%~}\" line)))"
		 ""
		 "   (text-physical-file (let ((file-path (make-pathname :defaults (glisp:temporary-file)"
                 "                                                           :type \"txt\")))"
                 "                               (with-open-file (f file-path :direction :output :if-exists :supersede)"
                 "                                      (write-string (the file-contents) f))"
		 "                               file-path))"
		 "  )"))
   (code-3 (list "(in-package :gwl-user)"
		 ""
		 "
```

---

## file-io.lisp - simple-file-output
Source: gornschool-training/t6/source/file-io.lisp
Type: tutorial

```
(define-object simple-file-output (base-html-page)"
		 " :computed-slots"
		 "  ((file-contents (let ((line (list 1 2 3 4 5 6 7)))"
		 "                        (format nil \"~{Line ~a of my file~^~%~}\" line)))"
		 ""
		 "   (text-physical-file-url (let ((url (format nil \"/file-output-~a.txt\" (get-current-date-time)))"
		 "                                         (file-path (make-pathname :defaults (glisp:temporary-file)"
                 "                                                                          :type \"txt\")))"
                 "                                   (with-open-file (f file-path :direction :output :if-exists :supersede)"
                 "                                          (write-string (the file-contents) f))"
		 "                                   (publish-file :path url"
                 "                                                 :content-type \"text/plain\""
                 "                                                 :file file-path)"
		 "                                    url))"
		 "   (text-file-url (let ((url (format nil \"/stream-output-~a.txt\" (get-current-date-time))))"
                 "                      (publish :path url"
                 "                                  :content-type \"text/plain\""
                 "                                  :function #'(lambda(req ent)"
                 "                                      (with-http-response (req ent)"
                 "                                         (with-http-body (req ent)"
                 "                                            (write-string (the file-contents) *html-stream*)))))"
                 "                       url))"
		 "  ))"))
   

   (code-4 (list "(in-package :gwl-user)"
		 ""
		 "
```

---

## file-io.lisp - simple-file-output
Source: gornschool-training/t6/source/file-io.lisp
Type: tutorial

```
(define-object simple-file-output (base-html-page)"
		 " :computed-slots"
		 " ("
		 "  ..."
		 "  ..."
		 "  (body (with-lhtml-string ()"
		 "              (str (the development-links))"
		 "              (str (the export-section div))))"
		 " )"
		 " :objects"
		 " (((export-section :type 'page-section"
		 "                       :inner-html (with-lhtml-string ()"
                 "                                      (:p \"Click \""
                 "                                      (:a :href (the text-file-url) "
		 "                                          :download \"virtual-text-file.txt\" \"Here\")"
                 "                                       \" to download a virtual text file.\")"
                 "                                      (:p \"Click \""
                 "                                      (:a :href (the text-physical-file-url) "
		 "                                           :download \"physical-text-file.txt\" \"Here\")"
                 "                                       \" to download a physical text file.\")))))"
		 ")"))

   
		 

   

   (body-content (with-lhtml-string ()
		   (:div :class "main-page-container"
			 (:div :class "main-page-item"
			       (:div :class "grid-container-2-650px"
				     (:div :class "grid-item"
					   (:h3 "Generating text output for download")
					   (:p "There is often a requirement to generate output as a file which can then be downloaded. There are 4 basic steps to this process"
					       (:ul (:li "Generate the file content")
						    (:li "Write the content to either a static file or a html stream")
						    (:li "Publish the output")
						    (:li "Provide a link to the output on your web page")))
					   (:h4 (:b (:em "Generate the content")))
					   (:p "In general this will involve writing a formatted string")
					   (str (code-example (the code-1)))
					   (:h4 (:b (:em "Write the static output")))
					   (:p "This is just standard file output, as described in the tutorial "(:em (:b "File I/0")) " and its associated topic "(:em (:b "Writing to a file")))
					   (str (code-example (the code-2)))
					   (:p "The slot "(:span :class "slot" "text-physical-file")" returns the pathname of the physical file. We'll cover writing the content directly to a html stream in the next part as its done at the same time as publishing")
					   (:h4 (:b (:em "Publish the output")))
					   (:p "Publishing the output is done by "(:em "side affecting")". For the static file we define the url, write the static file to disc, publish the static file to the url and then return the url. (Note that the slot has been renamed in this example). For the output direct to the html stream we define a function as part of the publish to write the content on the fly and return the url.")
					   (str (code-example (the code-3)))    
					   (:p "Note that when publishing the stream output we have to provide a "
					       (:span :class "general-keyword" ":function")" argument to "
					       (:span :class "function" "publish")" function. We use a lambda function, and whilst it may appear somewhat complex it is a fairly standard boilerplate solution/syntax which may be used elsewhere")   
					   (:h4 (:b (:em "Provide a link for download")))
					   (:p "A "
					       (:span :class "object" "page-section")" is defined comprising links to each of the download options, and then displayed on the page by including its "
					       (:span :class "slot" "div")" in the "
					       (:span :class "slot" "body") " slot. Note the use of the :download atribute for the links, this ensures that on clicking the link the file is downloaded and gives the download file a name which appears in the browsers download window")
					   (str (code-example (the code-4)))    
					   (:image :src (format nil "/~a-images/file-download.png" (the publish-prefix))
						   :style "border: 2px solid; margin: 5px 0 0 3% ;width: auto; height: 300px;" )))
			       ((:div :class "main-page-item")
				(:h2 "Resources")
				(str (the resource-links))))))))
		   
  )

```

---

## gwl-patches.lisp - header
Source: gornschool-training/t6/resources/source/gwl-patches.lisp
Type: tutorial

```
;;
;; FLAG -- Below "newnames" code to be copied to 
;;

(in-package :gwl)

(eval-when (:compile-toplevel :load-toplevel :execute)

  
```

---

## gwl-patches.lisp - with-lhtml-string
Source: gornschool-training/t6/resources/source/gwl-patches.lisp
Type: tutorial

```
(defmacro with-lhtml-string ((&rest args) &body body)
    "Form. Sets up body to be evaluated as lhtml and to return the resulting string."
    (let ((string-stream (gensym)))
      `(with-output-to-string (,string-stream)
         (cl-who:with-html-output (,string-stream nil ,@args)
           ,@body))))
  
  
```

---

## gwl-patches.lisp - base-html-page
Source: gornschool-training/t6/resources/source/gwl-patches.lisp
Type: tutorial

```
(define-object base-html-page (base-ajax-sheet)

    :input-slots
    ((doctype-string (call-next-method))

     (head-class (call-next-method))
     
     (title (call-next-method))

     (additional-header-content nil)

     (local-assets? t)
     (use-x3dom? t)
     (use-svgpanzoom? t)
     (use-fontawesome? nil)
     (use-anyresize? nil)
     (use-ajax? t)
     
     ("String of HTML. The main body of the page. 
This can be specified as input or overridden in subclass, otherwise it defaults
to the content produced by the :output-function of the same name 
in the applicable lens for  html-format."
      body "Empty Page Body")

     (body-attributes (list :class (the body-class)
                            :onpageshow (the body-onpageshow)
                            :onload (the body-onload)
                            :onresize (the body-onresize)))
     
     (body-class (call-next-method))
     (body-onpageshow (call-next-method))
     (body-onload (call-next-method))
     (body-onresize (call-next-method))
     (html-class (call-next-method))
     (lang "en")
     (charset "UTF-8")
     (favicon-type "image/x-icon")
     (favicon-path "/static/gwl/images/favicon.ico")
     
     )

    :computed-slots
    ((main-sheet (with-lhtml-string ()
                   (str (the doctype-string))
                   ((:html :lang (the lang) :class (the html-class))
                    ((:head :class (the head-class))
                     (:title (str (the title)))
                     (:meta :charset (the charset))
                     (:link :rel "icon" :type (the favicon-type) :href (the favicon-path))
                     (str (the additional-header-content))
                     (when (the use-x3dom?)
	               (htm ((:script :src (if (the local-assets?)
				               "/static/3rdpty/x3dom/x3dom.js"
				               "https://www.x3dom.org/download/1.8.1/x3dom.js")
		                      :id "x3dom_script"))))
                     (when (the use-svgpanzoom?)
	               (htm ((:script
	                      :id "svg-panzoom"
	                      :src (if (the local-assets?)
			               "/static/3rdpty/svgpanzoom/svg-pan-zoom.min.js"
			               "https://cdn.jsdelivr.net/npm/svg-pan-zoom@3.6.1/dist/svg-pan-zoom.min.js")))))
                     (when (the use-fontawesome?)
	               (htm ((:link :id "fontawesome-css"
		                    :rel "stylesheet"
		                    :href (if (the local-assets?)
			                      "/static/3rdpty/fa/css/all.min.css"
			                      "https://use.fontawesome.com/releases/v5.3.1/css/all.css")
		                    :integrity (unless (the local-assets?)
				                 "sha384-mzrmE5qonljUremFsqc01SB46JvROS7bZs3IO2EmfFsd15uHvIt+Y8vEf7N7fWAU")
		                    :crossorigin "anonymous"))))
                     (when (the use-anyresize?)
	               (htm ((:script :id "anyresize-script"
		                      :src (if (the local-assets?)
				               "/static/3rdpty/resize/any-resize-event.js"
				               "https://is.gd/sAeEPt")))))
                     (when (the use-ajax?)
	               (htm
	                ((:script) (fmt "~%var gdliid = '~a';" (the instance-id)))
	                ((:script :src (if (the local-assets?)
			                   "/static/gwl/js/gdlajax1595.js"
			                   "https://genworks.com/static/gwl/js/gdlajax1595.js"))))))

                    ((:body :class (the body-class)
	                    :onpageshow (the body-onpageshow)
                            :onload (the body-onload)
	                    :onresize (the body-onresize))
                     (the reset-html-sections!)
                     
                     (str (the body)))))))

    :functions
    ((write-html-sheet
      ()
      (with-format (html-format *html-stream*)
        (write-string (the main-sheet) *html-stream*)))))


  
```

---

## gwl-patches.lisp - base-html-div
Source: gornschool-training/t6/resources/source/gwl-patches.lisp
Type: tutorial

```
(define-object base-html-div (sheet-section)
    :computed-slots ((div (the main-div))))

  
```

---

## gwl-patches.lisp - viewport-html-div
Source: gornschool-training/t6/resources/source/gwl-patches.lisp
Type: tutorial

```
(define-object viewport-html-div (base-html-div)
    :input-slots ((display-list-objects nil) (display-list-object-roots nil))
    :objects ((view-object :type 'web-drawing
                           :objects (the display-list-objects)
                           :object-roots (the display-list-object-roots))))

  
  
```

---

## gwl-patches.lisp - publish-relative-file
Source: gornschool-training/t6/resources/source/gwl-patches.lisp
Type: tutorial

```
(defun publish-relative-file (&key home relative (path (string-append "/" relative)))
    (let ((file (probe-file (merge-pathnames relative home))))
      (if file (setq file (namestring file)) (error "Trying to publish ~a but it cannot be found.~%" file))
      (publish-file :path path :file file) path))


  
```

---

## gwl-patches.lisp - publish-relative-directory
Source: gornschool-training/t6/resources/source/gwl-patches.lisp
Type: tutorial

```
(defun publish-relative-directory (&key home relative (prefix (string-append "/" (subseq relative 0 (1- (length relative))))))
    (let ((relative-namestring (namestring relative)))
      (unless (eql (aref relative-namestring (1- (length relative-namestring))) #\/)
        (error "Relative name must end with a slash (\"/\"), but ~a does not" relative-namestring)))
    (let ((destination (probe-file (merge-pathnames relative home))))
      (if destination (setq destination (namestring destination)) (error "Trying to publish ~a but it cannot be found.~%" destination))

      (print-variables destination prefix)
      
      (publish-directory :prefix prefix :destination destination) prefix))
  
  (export  '(base-html-page base-html-div viewport-html-div with-lhtml-string publish-relative-file
             publish-relative-directory page-viewport-div with-tagged-body) :gwl))




```

---

## gwl-patches.lisp - with-form-string
Source: gornschool-training/t6/resources/source/gwl-patches.lisp
Type: tutorial

```
(defmacro with-form-string ((&key id name enctype target requestor on-submit local-anchor indent) &body body)
  (let ((fixed-prefix (gensym)))
    `(let ((,fixed-prefix (let ((prefix (the fixed-url-prefix)))
			    (and prefix (string-append "/" prefix)))))
       (with-lhtml-string (:indent ,indent)
         (:form :method :post
                :id ,(or id `(format nil "~a-form" (the root-path-string)))
                :name ,(or name `(format nil "~a-form" (the root-path-string)))
                :action (string-append
                         (or ,fixed-prefix "")
                         ,(if local-anchor `(format nil "/answer#~a" local-anchor) "/answer"))
                :enctype ,enctype
                :target ,target
                :on-submit ,on-submit
                (:input :type :hidden :name :|requestor| :value ,(if (null requestor) `(the url-encoded-root-path)
                                                                     `(the-object ,requestor url-encoded-root-path)))
                (:input :type :hidden :name :|iid| :value (the instance-id))
                ,@body)))))


(eval-when (:compile-toplevel :load-toplevel :execute) (export '(with-form-string) :gwl))


                     

;;
;; FLAG -- end of  "newnames" code to be copied to 
;;


;;
;; Patch to allegroserve to make file uplaods work in non-allegro
;;
#-allegro
(in-package :net.aserve)

#-allegro
(defmethod get-multipart-sequence ((req http-request)
				   buffer
				   &key (start 0)
				     (end (length buffer))
				     (external-format 
				      *default-aserve-external-format* 
				      ef-spec))
  ;; fill the buffer with the chunk of data.
  ;; start at 'start' and go no farther than (1- end) in the buffer
  ;; return the index of the first character not placed in the buffer.
  
  
  ;; Since external-format not used in all versions
  (declare (ignorable external-format ef-spec))


  (let* ((mp-info (getf (request-reply-plist req) 'mp-info))
	 mpbuffer 
	 cur
	 pos
	 kind
	 text-mode
	 after)

    
    (typecase buffer
      ((array (unsigned-byte 8) (*))
       )
      ((array character (*))
       (setq text-mode t))
      (t 
       (error 
	"This function only accepts (array (unsigned-byte 8)) or character arrays")))
    (if* (null mp-info)
       then (error "get-multipart-sequence called before get-multipart-header"))
    
    (setq mpbuffer (mp-info-buffer mp-info)
	  cur      (mp-info-cur mp-info))

    (loop
      (case (mp-info-state mp-info)
	((:header :boundary :last-boundary)
                                        ; no data left
	 (return-from get-multipart-sequence nil))
	(:start
	 (error "get-multipart-sequence called before get-multipart-header"))
	((:body :partial)
	 (if* (eq (mp-info-state mp-info) :partial)
	    then      ; this was set below. we will return the partial
                                        ; at then end of the buffer
		      (setf (mp-info-state mp-info) :body)
		      (setq pos (mp-info-end mp-info))
	    else (multiple-value-setq (pos kind after) (scan-forward mp-info))
		 (setf (mp-info-after mp-info) after)
		 (setq cur (mp-info-cur mp-info)) ; scan-forward can change
		 )
	 
	 (if* (> pos cur)
	    then                        ; got something to return
		 (let* ((tocopy (min (- end start) (- pos cur)))
			(items tocopy))

                   
		   (if* text-mode
		      then
			  (dotimes (i tocopy)
			    (setf (aref buffer (+ start i))
			          (code-char (aref mpbuffer (+ cur i)))))
		      else 
			   (dotimes (i tocopy)
			     (setf (aref buffer (+ start i))
			           (aref mpbuffer (+ cur i)))))

                   
		   (if* (zerop items)
		      then         ; didn't find enough bytes to make 
                                        ; a character
			           (if* (null (shift-buffer-up-and-read mp-info))
			              then ; no more bytes available
				           (return-from get-multipart-sequence nil))
                                        ; loop around
		      else (setf (mp-info-cur mp-info) (+ cur tocopy))
			   (return-from get-multipart-sequence 
			     (+ start items))))
	  elseif (eq kind :partial)
	    then                       ; may be a boundary, can't tell
		  (if* (null (shift-buffer-up-and-read mp-info))
		     then     ; no more data, partial will never match
                                        ; so return the partial, this special
                                        ; state is recognized in this routine
			      (setf (mp-info-state mp-info) :partial)
                                        ; loop around
			      )
	  elseif (or (eq kind :boundary)
		     (eq kind :last-boundary))
	    then              ; hit a boundary, nothing more to return
		 (setf (mp-info-state mp-info) kind
		       (mp-info-cur   mp-info) pos)
		 (return-from get-multipart-sequence nil)))))))





```

---

## define-basic.lisp - header
Source: gornschool-training/t6/resources/source/define-basic.lisp
Type: tutorial

```
(in-package :gwl-user)


```

---

## define-basic.lisp - define-basic
Source: gornschool-training/t6/resources/source/define-basic.lisp
Type: tutorial

```
(define-object define-basic (base-html-page)
  :computed-slots
  ((body "My first web page"))
  )


```

---

## define-basic.lisp - define-basic-lhtml
Source: gornschool-training/t6/resources/source/define-basic.lisp
Type: tutorial

```
(define-object define-basic-lhtml (base-html-page)
  :computed-slots
  ((body (with-lhtml-string ()
		      (:table
			  (:tr (:th "Day") (:th "Number"))
			(:tr (:td "Sunday") (:td "1"))
			(:tr (:td "Monday") (:td "2"))
			(:tr (:td "Tuesday") (:td "3"))
			(:tr (:td "Wednesday") (:td "4"))
			(:tr (:td "Thursday") (:td "5"))
			(:tr (:td "Friday") (:td "6"))
			(:tr (:td "Saturday") (:td "7"))))))
			
  )


```

---

## define-basic.lisp - define-basic-styled
Source: gornschool-training/t6/resources/source/define-basic.lisp
Type: tutorial

```
(define-object define-basic-styled (base-html-page)
  :computed-slots
  ((body (with-lhtml-string ()
		      (:table :style "border-style: solid;"
			  (:tr (:th "Day") (:th "Number"))
			(:tr (:td "Sunday") (:td :style "text-align: center;" "1"))
			(:tr (:td "Monday") (:td :style "text-align: center;" "2"))
			(:tr (:td "Tuesday") (:td :style "text-align: center;" "3"))
			(:tr (:td "Wednesday") (:td :style "text-align: center;" "4"))
			(:tr (:td "Thursday") (:td :style "text-align: center;" "5"))
			(:tr (:td "Friday") (:td :style "text-align: center;" "6"))
			(:tr (:td "Saturday") (:td :style "text-align: center;" "7"))))))
			
  )



```

---

## define-basic.lisp - define-basic-css
Source: gornschool-training/t6/resources/source/define-basic.lisp
Type: tutorial

```
(define-object define-basic-css (base-ajax-sheet)
  :computed-slots
  ((additional-header-content (with-cl-who-string()
				(:link :rel "stylesheet" :href "/my-style.css")))
   (main-sheet-body (with-cl-who-string ()
		      (:table
			  (:tr (:th "Day") (:th "Number"))
			(:tr (:td "Sunday") (:td "1"))
			(:tr (:td "Monday") (:td "2"))
			(:tr (:td "Tuesday") (:td "3"))
			(:tr (:td "Wednesday") (:td "4"))
			(:tr (:td "Thursday") (:td  "5"))
			(:tr (:td "Friday") (:td "6"))
			(:tr (:td "Saturday") (:td "7"))))))
			
  )


```

---

## define-basic.lisp - define-basic-image
Source: gornschool-training/t6/resources/source/define-basic.lisp
Type: tutorial

```
(define-object define-basic-image (base-ajax-sheet)
  :computed-slots
  ((additional-header-content (with-cl-who-string()
				(:link :rel "stylesheet" :href "/my-style.css")))
   (main-sheet-body (with-cl-who-string ()
		      (:p "The Genworks International Logo")
		      (:img :src "/images/logo.png"))))
			
  )

```

---

## define-basic.lisp - define-basic-image-links
Source: gornschool-training/t6/resources/source/define-basic.lisp
Type: tutorial

```
(define-object define-basic-image-links (base-ajax-sheet)
  :computed-slots
  ((additional-header-content (with-cl-who-string()
				(:link :rel "stylesheet" :href "/my-style.css")))
   (main-sheet-body (with-cl-who-string ()
		      (when gwl:*developing?* (str (the development-links)))
		      (:p "The Genworks International Logo")
		      (:img :src "/images/logo.png"))))
			
  )
(publish-gwl-app "/basic" "gwl-user::define-basic")
(publish-gwl-app "/basic-lhtml" "gwl-user::define-basic-lhtml")
(publish-gwl-app "/basic-styled" "gwl-user::define-basic-styled")
(publish-gwl-app "/basic-css" "gwl-user::define-basic-css")
(publish-gwl-app "/basic-image" "gwl-user::define-basic-image")
(publish-gwl-app "/basic-image-links" "gwl-user::define-basic-image-links") 

;; this parameter plus the following publishing assumes a directory structure as follows
;; some-path/source - LISP source files
;; some-path/css - CSS files
;; some-path/images - image files
;; the actual value of some-patch doesn't matter providing the 3 folder above have the same root path

(defparameter *home*
  (merge-pathnames "../" (make-pathname :name nil :type nil :defaults (glisp:source-pathname))))

(gwl::publish-file :path "/my-style.css"
		   :file (namestring (merge-pathnames "css/my-style.css" *home*)))

(gwl::publish-directory :prefix "/images"
			:destination (namestring (merge-pathnames "images/"  *home*)))

```

---

## building-bom-input-output.lisp - header
Source: gornschool-training/t6/resources/source/building-bom-input-output.lisp
Type: tutorial

```
(in-package :gwl-user)


```

---

## building-bom-input-output.lisp - read-file
Source: gornschool-training/t6/resources/source/building-bom-input-output.lisp
Type: tutorial

```
(defun read-file (file )
  (let ((result))
    (with-open-file (str file :direction :input)
      (do ((line (read-line str nil 'eof)
		 (read-line str nil 'eof)))
	  ((eql line 'eof) result)
	(setq result (append result (list line)))))))


```

---

## building-bom-input-output.lisp - import-building-data
Source: gornschool-training/t6/resources/source/building-bom-input-output.lisp
Type: tutorial

```
(defun import-building-data (file)
  (let* ((raw-data (read-file file))
	 (res (mapcar #'(lambda(a) (glisp:split-regexp "\\s+" a)) raw-data)))
    (mapcan #'(lambda(a) (list
			  (make-keyword (first a))
			  (read-safe-string (second a)))) res)))
       


```

---

## building-bom-input-output.lisp - building-bom
Source: gornschool-training/t6/resources/source/building-bom-input-output.lisp
Type: tutorial

```
(defun building-bom (&key (nominal-height nil) 
		       (nominal-width nil)
		       (nominal-length nil)
		       (roof-angle nil)
		       (input-filename nil)
		       (output-filename nil))
  (let ((obj (make-object 'building
			  :function-nominal-height nominal-height
			  :function-nominal-width nominal-width
			  :function-nominal-length nominal-length
			  :function-truss-angle roof-angle
			  :output-filename output-filename
			  :input-filename input-filename)))
    (if output-filename (theo obj write-bom-file!)
	(theo obj bom-formatted))))


```

---

## building-bom-input-output.lisp - building
Source: gornschool-training/t6/resources/source/building-bom-input-output.lisp
Type: tutorial

```
(define-object building (box)
  :input-slots
  ((function-nominal-height nil)
   (function-nominal-width nil)
   (function-nominal-length nil)
   (function-truss-angle  nil)
   (input-filename nil)
   (output-filename nil)
   
   (brick-height (or (getf (the file-inputs) :brick-height) 45))
   (brick-length (or (getf (the file-inputs) :brick-length) 180))
   (brick-width (or (getf (the file-inputs) :brick-width) 90))
   (mortar-joint-width (or (getf (the file-inputs) :mortar-joint-width) 10))
   (beam-width (or (getf (the file-inputs) :beam-width) 40))
   (beam-height (or (getf (the file-inputs) :beam-height) 50))
   (wall-thickness (or (getf (the file-inputs) :wall-thickness) 3))
   (material-density (or (getf (the file-inputs) :material-density) 7800))
   (roof-overhang (or (getf (the file-inputs) :roof-overhang) 50))
   (cladding-thickness (or (getf (the file-inputs) :cladding-thickness) 10))
   (max-beam-spacing (or (getf (the file-inputs) :max-beam-spacing) 1500))
   )

  :computed-slots
  ((file-inputs (when (the input-filename) (import-building-data (the input-filename))))
   (nominal-height (or (the function-nominal-height)
		       (getf (the file-inputs) :nominal-height)
		       3000))
   (nominal-width (or (the function-nominal-width)
		      (getf (the file-inputs) :nominal-width)
		      3000))
   (nominal-length (or (the function-nominal-length)
		       (getf (the file-inputs) :nominal-length)
		       4000))
   (truss-angle (or (the function-truss-angle)
		    (getf (the file-inputs) :truss-angle)
		    30))

   
   
   (length (the left-wall length))
   (width (the rear-wall length))
   (height (+ (the left-wall height) (the (roof-truss 0) height)))

   (number-of-roof-trusses (let ((trusses (ceiling (the left-wall length) 1500)))
			     (max trusses 2)))

   (truss-spacing (div (- (the left-wall length) (the beam-width))
		       (- (the number-of-roof-trusses) 1)))
   (truss-offsets (let ((res nil))
		    (dotimes (n (the number-of-roof-trusses) (nreverse res))
		      (push (+ (half (the beam-width))
			       (* n (the truss-spacing))) res))))

   (roof-length (+ (the left-wall length) (twice (the roof-overhang))))
   (roof-width (the cladding-thickness))
   (roof-height (let ((apex (the (roof-truss 0) apex-point))
		      (gutter (the (roof-truss 0) front-gutter-point)))
		  (+ (3d-distance apex gutter) (the roof-overhang))))

   ;; building properties
   (walls (remove nil (mapcar #'(lambda(a) (when (typep a 'wall) a)) (the children))))
   (full-bricks (apply '+ (mapsend (the walls) :full-bricks)))
   (half-bricks (apply '+ (mapsend (the walls) :half-bricks)))
   (mortar-volume (apply '+ (mapsend (the walls) :mortar-volume)))
   (cladding-dimensions (list :length (the roof-length)
			      :width (the roof-height)))
   (beam-properties (the (roof-truss 0) beam-properties))
   (beam-qty-by-size (let ((res nil))
		       (dolist (plis (the beam-properties) )
			 (let* ((trusses (the number-of-roof-trusses))
				(l (getf plis :length-mm))
				(p (position l res :key #'(lambda(a) (getf a :length-mm))))
				(qty (when p (getf (nth p res) :qty))))
			   (if p (setf (getf (nth p res) :qty) (+ qty trusses))
			       (setq res (append (list (list :length-mm l :qty trusses)) res)))))
		       (safe-sort res '< :key #'(lambda(a) (getf a :length-mm)))))
		       

   (roof-truss-mass (* (apply '+ (mapcar #'(lambda(a) (getf a :mass-kg))
					 (the beam-properties)))
		       (the number-of-roof-trusses)))

   (building-materials (list :full-bricks (the full-bricks)
			     :half-bricks (the half-bricks)
			     :mortar-volume-m3 (div (the mortar-volume) 1000000000)
			     :beams (the beam-qty-by-size)
			     :roof-cladding (append (the cladding-dimensions) (list :qty 2))))

  (bom-formatted (let* ((bom (the building-materials))
			(cladding (getf bom :roof-cladding))
			(bricks (format nil "Bricks~%======~%  Full Bricks ~a~%  Half Bricks ~a~%" 
					(getf bom :full-bricks) 
					(getf bom :half-bricks)))
			(mortar (format nil "Mortar~%======~%  Volume ~,3f m^3~%" 
					(getf bom :mortar-volume-m3)))
			(l (round-to-nearest (getf cladding :length) 1))
			(w (round-to-nearest (getf cladding :width) 1))
			(roof (format nil "Roof Cladding~%======~%  Qty ~a~%  Dimensions (L x W x T) ~d x ~d x ~d~%" 
				      (getf cladding :qty)
				      l w (the cladding-thickness)))
			(beams (getf (the building-materials) :beams))
			(beams-list (flatten
				     (mapcar #'(lambda(a)
						 (list (getf a :qty) (round-to-nearest (getf a :length-mm) 1)))
					     beams)))
			
			(beams-header (format nil "Beams~%=====~%  Section (H x W x T) ~a x ~a x ~a~%"
						 (the beam-height) (the beam-width) (the wall-thickness)))
			(beam-lengths (format nil "~{  Qty ~a Length ~a~%~}" beams-list)))
		   (format nil "~@{~a~}" bricks mortar roof beams-header beam-lengths))) 
   
		
   )

  :functions
  ((write-bom-file! ()
		    (with-open-file (s (the output-filename) :direction :output
						:if-exists :supersede
						:if-does-not-exist :create)
		      (format t "Exporting the BOM to ~a~%" (the output-filename))
		      (format s "~a" (the bom-formatted))
		      (format t "Exporting complete~%")))



   (get-roof-mid-point! (first-gutter last-gutter last-index)
		       (let*((mid-gutter (midpoint first-gutter last-gutter))
			     (first-apex (the (roof-truss 0) apex-point))
			     (last-apex (the (roof-truss last-index) apex-point))
			     (mid-apex (midpoint first-apex last-apex))
			     (vec (subtract-vectors mid-gutter mid-apex))
			     (mid-edge (translate-along-vector mid-gutter vec (the roof-overhang))))
			 (midpoint mid-apex mid-edge))) )
  
  :objects
  ((left-wall :type 'wall
	      :pass-down (brick-height
			  brick-length
			  brick-width
			  mortar-joint-width)
	      :center (translate-along-vector
		       (translate-along-vector (the (edge-center :bottom :left))
					       (the (face-normal-vector :right))
					       (half (the-child width)))
		       (the (face-normal-vector :top))
		       (half (the-child height)))
	      :wall-length (the nominal-length)
	      :wall-height (the nominal-height))

   (right-wall :type 'wall
	       :pass-down (brick-height
			   brick-length
			   brick-width
			   mortar-joint-width)
	       :center (translate-along-vector
			(translate-along-vector (the (edge-center :bottom :right))
						(the (face-normal-vector :left))
						(half (the-child width)))
			(the (face-normal-vector :top))
			(half (the-child height)))
	       :wall-length (the nominal-length)
	       :wall-height (the nominal-height))

   (rear-wall :type 'wall
	      :pass-down (brick-height
			  brick-length
			  brick-width
			  mortar-joint-width)
	      :center (translate-along-vector
			(translate-along-vector (the (edge-center :bottom :rear))
						(the (face-normal-vector :front))
						(half (the-child width)))
			(the (face-normal-vector :top))
			(half (the-child height)))
	      :orientation (alignment :rear (the (face-normal-vector :right)))
	      :wall-length (the nominal-width)
	      :wall-height (the nominal-height))

   (roof-truss :type 'truss
	       :sequence (:size (the number-of-roof-trusses))
	       :truss-length (the rear-wall length)
	       :center (translate-along-vector
			(translate-along-vector (the (edge-center :front :top))
						(the (face-normal-vector :bottom))
						(half (the-child height)))
			(the (face-normal-vector :rear))
			(nth (the-child index) (the truss-offsets)))
	       :orientation (alignment :rear (the (face-normal-vector :right))
				       )		
	       :pass-down (truss-angle
			   beam-width
			   beam-height
			   wall-thickness
			   material-density))
   
   (roof-cladding-left
    :type 'box
    :length (the roof-length)
    :height (the roof-height)
    :width (the cladding-thickness)
    :orientation (alignment :left (the (roof-truss 0) front-slope-normal))
    :center (let* ((last-index (- (the number-of-roof-trusses) 1))
		   (first-gutter (the (roof-truss 0) front-gutter-point))
		   (last-gutter (the (roof-truss last-index) front-gutter-point))
		   (mid-ctr (the (get-roof-mid-point! first-gutter last-gutter last-index))))
	      (translate-along-vector mid-ctr
				      (the (roof-truss 0) front-slope-normal)
				      (half (the cladding-thickness)))))
   
   (roof-cladding-right :type 'box
			:length (the roof-length)
			:height (the roof-height)
			:width (the cladding-thickness)
			:orientation (alignment :left (the (roof-truss 0) rear-slope-normal))
			 :center (let* ((last-index (- (the number-of-roof-trusses) 1))
					(first-gutter (the (roof-truss 0) rear-gutter-point))
					(last-gutter (the (roof-truss last-index) rear-gutter-point))
					(mid-ctr (the (get-roof-mid-point! first-gutter last-gutter last-index))))
				 (translate-along-vector mid-ctr
							 (the (roof-truss 0) rear-slope-normal)
							 (half (the cladding-thickness)))))
   )

  
		       
  )



```

---

## building-bom-input-output.lisp - wall
Source: gornschool-training/t6/resources/source/building-bom-input-output.lisp
Type: tutorial

```
(define-object wall(box)
  :input-slots
  ((brick-height 45)
   (brick-length 180)
   (brick-width 90)
   (mortar-joint-width 10)
   (wall-length 3700)
   (wall-height 900)
   (first-row :start-full)
   (front-edge :full)
   (rear-edge :full))

  :computed-slots
  ((row-height (+ (the brick-height) (the mortar-joint-width)))
   (number-of-rows (round-to-nearest (div (the wall-height) (the row-height)) 1))
   (actual-wall-height (* (the row-height) (the number-of-rows)))
   
   ;; for the wall-length we need the number of full bricks
   ;; if there are n full bricks then there will be (n-1) mortar joints
   ;; so n*brick-length + n-1*mortar-joint-width = overall-length
   ;; or n(brick-length + mortar-join-width) - mortar-joint-width = overall-length
   ;; or n = (overall-length - mortar-joint-width)/(brick-length + mortar-joint-width)
   (number-of-bricks (round-to-nearest (div (- (the wall-length) (the mortar-joint-width))
					    (+ (the brick-length) (the mortar-joint-width)))
				       1))
   (actual-wall-length (+ (* (the number-of-bricks) (the brick-length))
			  (* (- (the number-of-bricks) 1) (the mortar-joint-width))))

   ;; box inputs - gives the wall bounding box
   (height (the actual-wall-height))
   (width (the brick-width))
   (length (the actual-wall-length))

   (full-bricks (apply '+ (list-elements (the row) (the-element full-bricks))))
   (half-bricks (apply '+ (list-elements (the row) (the-element half-bricks))))
   (mortar-volume (apply '+ (list-elements (the row) (the-element mortar-volume))))
   (mortar-density 2162)
   (mortar-mass (* (the mortar-density) (div (the mortar-volume) 1000000000)))
   )
   
     
  :objects
  ((row :type 'row
     :sequence (:size (the number-of-rows))
     :center (translate-along-vector (the (face-center :bottom))
				     (the (face-normal-vector :top))
				     (+ (half (the-child height))
					(* (the-child index) (the-child height))))
     :height (+ (the brick-height) (the mortar-joint-width))
     :full-bricks-per-row (the number-of-bricks)
     :pass-down (brick-height
		 brick-length
		 brick-width
		 mortar-joint-width
		 length
		 width
		 first-row
		 front-edge
		 rear-edge))))


```

---

## building-bom-input-output.lisp - row
Source: gornschool-training/t6/resources/source/building-bom-input-output.lisp
Type: tutorial

```
(define-object row (box)
  :input-slots
  (full-bricks-per-row
   brick-height
   brick-length
   brick-width
   mortar-joint-width
   first-row
   front-edge
   rear-edge )

  :computed-slots
  ((full-brick-row? (cond ((eq (the first-row) :start-full)
			   (or (zerop (the index)) (evenp (the index))))
			  ((eq (the first-row) :start-half)
			   (not (or (zerop (the index)) (evenp (the index)))))))
		    
   (full-bricks (the bricks-and-mortar full-bricks))
   (half-bricks (the bricks-and-mortar half-bricks))
   (mortar-volume (+ (the bricks-and-mortar mortar-joint-volume)
		     (the mortar-bed volume))))

  :objects
  ((bricks-and-mortar :type 'bricks-and-mortar
		      :height (the brick-height)
		      :center (translate-along-vector (the mortar-bed (face-center :top))
						      (the mortar-bed (face-normal-vector :top))
						      (half (the-child height)))
		      :pass-down (width
				  length
				  full-brick-row?
				  brick-height
				  brick-length
				  brick-width
				  mortar-joint-width
				  full-bricks-per-row
				  front-edge
				  rear-edge))
   (mortar-bed :type 'box
	       :height (the mortar-joint-width)
	       :center (translate-along-vector (the (face-center :bottom))
					       (the (face-normal-vector :top))
					       (half (the-child height)))
	       :pass-down (width
			   length))))


```

---

## building-bom-input-output.lisp - bricks-and-mortar
Source: gornschool-training/t6/resources/source/building-bom-input-output.lisp
Type: tutorial

```
(define-object bricks-and-mortar (box)
  :input-slots
  (full-brick-row?
   full-bricks-per-row
   brick-height
   brick-length
   brick-width
   mortar-joint-width
   front-edge
   rear-edge)

  :computed-slots
  ((first-full-brick-start-point (if (the full-brick-row?)
				     (the (face-center :front))
				     (the (mortar-joint 0) (face-center :rear))))
   
   (first-mortar-joint-start-point (cond ((the full-brick-row?) (the (full-brick 0) (face-center :rear)))
					 ((eq (the front-edge) :full) (the (half-brick 0) (face-center :rear)))
					 ((eq (the front-edge) :keyed) (translate-along-vector (the (face-center :front))
											       (the (face-normal-vector :rear))
											       (half (the brick-length))))))

   (number-of-full-bricks (if (the full-brick-row?)
			      (the full-bricks-per-row)
			      (- (the full-bricks-per-row) 1)))

   (number-of-half-bricks (cond ((the full-brick-row?) 0)
				((and (eq (the front-edge) :full)(eq (the rear-edge) :full)) 2)
				((and (eq (the front-edge) :keyed) (eq (the rear-edge) :full)) 1)
				((and (eq (the front-edge) :full) (eq (the rear-edge) :keyed)) 1)
				((and (eq (the front-edge) :keyed) (eq (the rear-edge) :keyed)) 0)))

   ;; whether or not the ends are :full or :keyed, the number of mortar joints remains the same since the mortar joint
   ;; when it is :keyed is used to connect to the full brick of the other wall
   (number-of-mortar-joints (if (the full-brick-row?)
				(- (the number-of-full-bricks) 1)
				(+ (the number-of-full-bricks) 1)))

   ;; if it isn't a full brick row then there will be an extra joint because one
   ;; full brick is replaced with 2 half bricks so without correcting the
   ;; mortar-joint-width the ends of a full brick row and one starting and
   ;; finishing with half bricks won't align. So we need to correct
   ;; the mortar-joint-width
   (corrected-joint-width (if (the full-brick-row?)
				     (the mortar-joint-width)
				     (let ((total-gap (* (- (the number-of-mortar-joints) 1)
									(the mortar-joint-width))))
				       (div total-gap (the number-of-mortar-joints)))))
   ;; collating the output. We could do this analytically, but for this example we'll use the geometry
   (full-bricks (length (list-elements (the full-brick))))
   (half-bricks (length (list-elements (the half-brick))))
   (mortar-joint-volume (apply '+ (list-elements (the mortar-joint)
						 (the-element volume))))
   )
  
  :functions
  ((first-full-brick-center!
    ()
    (translate-along-vector (the first-full-brick-start-point)
			    (the (face-normal-vector :rear))
			    (half (the brick-length))))
   
   (other-full-brick-center!
    (index)
    ;; if its a full brick row, (full-brick 1) is positioned relative to (joint 0)
    ;; if its a half brick row, (full-brick 1) is positioned relative to (joint 1)
    (let ((ind (if (the full-brick-row?) (- index 1) index)))
	  (translate-along-vector (the (mortar-joint ind) (face-center :rear))
				  (the (face-normal-vector :rear))
				  (half (the brick-length)))))
   
    (first-joint-center!
     ()
     (translate-along-vector (the first-mortar-joint-start-point)
			     (the (face-normal-vector :rear))
			     (half (the corrected-joint-width))))
   (other-joint-center!
    (index)
    ;; if its a full brick row, (joint 1) is positioned relative to (full-brick 1)
    ;; if its a half brick row, (joint 1) is positioned relative to (full-brick 0)
    (let ((ind (if (the full-brick-row?) index (- index 1))))
      (translate-along-vector (the (full-brick ind) (face-center :rear))
			      (the (face-normal-vector :rear))
			      (half (the corrected-joint-width)))))

    (first-half-brick-center!
     ()
     (translate-along-vector (the (face-center :front))
			     (the (face-normal-vector :rear))
			     (half (half (the brick-length)))))
   
   (last-half-brick-center!
     ()
     (translate-along-vector (theo (the mortar-joint last) (face-center :rear))
			     (the (face-normal-vector :rear))
			     (half (half (the brick-length))))))
 			     
  
  :objects
  ((full-brick :type 'box
	       :sequence (:size (the number-of-full-bricks))
	       :center (if (= (the-child index) 0)
			   (the first-full-brick-center!)
			   (the (other-full-brick-center! (the-child index))))
	       :length (the brick-length)
	       :height (the brick-height)
	       :width (the brick-width))

   (half-brick :type 'box
	       :sequence (:size (the number-of-half-bricks))
	       :center (cond ((and (= (the-child index) 0)
				   (eq (the front-edge) :full)) (the first-half-brick-center!))
			     ((and (= (the-child index) 0)
				   (eq (the front-edge) :keyed)
				   (eq (the rear-edge) :full)) (the last-half-brick-center!))
			     ((eq (the rear-edge) :full) (the last-half-brick-center!)))
	       :length (half (the brick-length))
	       :height (the brick-height)
	       :width (the brick-width))

   (mortar-joint :type 'box
		 :sequence (:size (the number-of-mortar-joints))
		 :center (if (= (the-child index) 0)
			     (the first-joint-center!)
			  (the (other-joint-center! (the-child index))))   
		 :height (the brick-height)
		 :width (the brick-width)
		 :length (the corrected-joint-width))))



```

---

## building-bom-input-output.lisp - degrees-to-radians
Source: gornschool-training/t6/resources/source/building-bom-input-output.lisp
Type: tutorial

```
(defun degrees-to-radians (degrees)
  (div (* degrees pi) 180))



```

---

## building-bom-input-output.lisp - truss
Source: gornschool-training/t6/resources/source/building-bom-input-output.lisp
Type: tutorial

```
(define-object truss (box)
  :input-slots
  ((truss-length 2000)
   (truss-height nil)
   (truss-angle 30)

   (beam-width 30)
   (beam-height 50)
   (wall-thickness 3)

   (material-density 7800)
   (tonne-rate 450))

  :computed-slots
  ((length (the truss-length))
   (height (cond ((the truss-height)(the truss-height))
		 ((the truss-angle) (+ (* (half (the truss-length))
					  (tan (degrees-to-radians (the truss-angle))))
				       (the beam-height)))))
   (width (the beam-width))
  
   (truss-front-slope-vector (the (get-slope-vector! :front)))
   (truss-rear-slope-vector (the (get-slope-vector! :rear)))

   (front-slope-length (the (get-slope-length! :front)))
   (rear-slope-length (the (get-slope-length! :rear)))
   
   (front-slope-center (the (get-slope-center! :front)))
   (rear-slope-center (the (get-slope-center! :rear)))
   

   (beam-properties (mapsend (the children) :beam-properties))
   (total-mass (round-to-nearest
		(apply '+
		       (mapcar #'(lambda(a) (getf a :mass-kg))
			       (the beam-properties)))
		0.001))
   (total-cost (round-to-nearest
		(apply '+
		       (mapcar #'(lambda(a) (getf a :cost-gbp))
			       (the beam-properties)))
		0.01))

   ;; messages to support roof cladding sizing and positioning
   (apex-point (inter-line-plane (the rear-slope-construction-line end)
			   (the truss-rear-slope-vector)
			   (the lower-beam center)
				 (the (face-normal-vector :rear))))
   (front-gutter-point (the front-slope-construction-line start))
   (rear-gutter-point (the rear-slope-construction-line start))
   (front-slope-normal (the front-slope-beam (face-normal-vector :top)))
   (rear-slope-normal (the rear-slope-beam (face-normal-vector :top)))
   )

  :functions
  ((get-slope-vector! (beam-side)
		      (let ((v-key (the (get-v-key! beam-side)))
			    (l-key (the (get-l-key! beam-side))))
		      (subtract-vectors (the vertical-beam (edge-center :rear v-key))
					(the lower-beam (edge-center l-key :top)))))
   (get-slope-length! (beam-side)
		      (let ((v-key (the (get-v-key! beam-side)))
			    (l-key (the (get-l-key! beam-side))))
			(3d-distance (the vertical-beam (edge-center :rear v-key))
				     (the lower-beam (edge-center l-key :top)))))
   (get-slope-center!
    (beam-side)
    (let ((pt (case beam-side
		(:front (the front-slope-construction-line center))
		(:rear  (the rear-slope-construction-line center))))
	  (norm-vector (case beam-side
			 (:front (the front-slope-beam (face-normal-vector :bottom)))
			 (:rear (the rear-slope-beam (face-normal-vector :bottom))))))
      (translate-along-vector pt
			      norm-vector
			      (half (the beam-height)))))
   (get-v-key! (beam-side)
	       (case beam-side
		 (:front :top)
		 (:rear :bottom)))
   (get-l-key! (beam-side)
	       (case beam-side
		 (:front :front)
		 (:rear :rear)))

   
   )
   
  
  
  :objects
  ((lower-beam :type 'beam
	       :pass-down (:beam-height :beam-width :wall-thickness :material-density :tonne-rate)
	       :beam-length (the truss-length)
	       :center (translate-along-vector (the (face-center :bottom))
					       (the (face-normal-vector :top))
					       (half (the beam-height))))
   (vertical-beam :type 'beam
		  :pass-down (:beam-height :beam-width :wall-thickness :material-density :tonne-rate)
		  
		  :beam-length (- (the height) (the beam-height))
		  :orientation (alignment :rear (the (face-normal-vector :top))
					  :right (the (face-normal-vector :right)))  
		  :center (translate-along-vector (the lower-beam (face-center :top))
						  (the lower-beam (face-normal-vector :top))
						  (half (the-child beam-length))))
   (front-slope-beam :type 'beam
		     :pass-down (:beam-height :beam-width :wall-thickness :material-density :tonne-rate)
		     :beam-length (the front-slope-length)
		     :center (the front-slope-center)
		     :orientation (alignment :rear (the truss-front-slope-vector)
					     :right (the (face-normal-vector :right))))

   (rear-slope-beam :type 'beam
		    :pass-down (:beam-height :beam-width :wall-thickness :material-density :tonne-rate)
		    :beam-length (the rear-slope-length)
		    :center (the rear-slope-center)
		    :orientation (alignment :rear (the truss-rear-slope-vector)
					    :left (the (face-normal-vector :right))))
   
   )

  :hidden-objects
  ((apex-pt :type 'sphere
	    :radius 5
	    :display-controls (list :color :green)
	    :center (the apex-point))
   (front-slope-construction-line :type 'line
				  :start (the lower-beam (edge-center :front :top))
				  :end (the vertical-beam (edge-center :rear :top)))
   (rear-slope-construction-line :type 'line
				 :start (the lower-beam (edge-center :rear :top))
				 :end (the vertical-beam (edge-center :rear :bottom)))
   
   (front-mid-pt :type 'sphere
		 :display-controls (list :color :blue)
		 :radius 5
		 :center (the front-slope-construction-line center))
   (rear-mid-pt :type 'sphere
		:display-controls (list :color :blue)
		:center (the rear-slope-construction-line center)
		:radius 5)
   (pt-1 :type 'sphere
	 :radius 5
	 :display-controls (list :color :green)
	 :center (the lower-beam (edge-center :rear :top)))
   (pt-2 :type 'sphere
	 :radius 5
	 :display-controls (list :color :red)
	 :center (the vertical-beam (edge-center :rear :bottom)) )

   

   (vector-line :type 'vector-line
		:start-point (the rear-slope-construction-line center)
		:vector (the rear-slope-beam (face-normal-vector :bottom))
		:length 150)
   
   ))


```

---

## building-bom-input-output.lisp - vector-line
Source: gornschool-training/t6/resources/source/building-bom-input-output.lisp
Type: tutorial

```
(define-object vector-line (box)
  :input-slots
  ((start-point (make-point 0 0 0))
   (vector (make-vector 1 0 1))
   (length 50)
   (width 1))
  :computed-slots
  ((height (div (the length) 5)))
  :objects
  ((v-line :type 'line
	   :start (the start-point)
	   :display-controls (list :color :red)
	   :end (translate-along-vector (the start-point)
					(the vector)
					(the length)))
   (arrow :type 'cone
	  :radius-1 0
	  :radius-2 (div (the length) 50)
	  :length (div (the length) 5)
	  :display-controls (list :color :red)
	  :center (translate-along-vector (the v-line end)
					  (the vector)
					  (half (the-child length)))
	  :orientation (alignment :front (the vector)))))

			 
		  

```

---

## building-bom-input-output.lisp - beam
Source: gornschool-training/t6/resources/source/building-bom-input-output.lisp
Type: tutorial

```
(define-object beam (box)
  :input-slots
  ((beam-length 1000)
   (beam-width 40)
   (beam-height 50)
   (wall-thickness 2)
   (material-density 7800)
   (tonne-rate 500))

  :computed-slots
  ((length (the beam-length))
   (width (the beam-width))
   (height (the beam-height))

   (beam-volume (- (the outer volume) (the inner volume)))
   (beam-mass (* (div (the beam-volume) 1000000000) (the material-density)))
   (beam-cost (* (the tonne-rate) (div (the beam-mass) 1000)))
   (beam-properties (list :volume-mm3 (the beam-volume)
			  :mass-kg (round-to-nearest (the beam-mass) 0.01)
			  :cost-gbp (round-to-nearest (the beam-cost) 0.01)
			  :length-mm (the beam-length)
			  :width-mm (the beam-width)
			  :height-mm (the beam-height)
			  :thickness-mm (the wall-thickness))))
   
  :objects
  ((outer :type 'box
	  :length (the beam-length)
	  :width (the beam-width)
	  :height (the beam-height))

   (inner :type 'box
	  :length (the beam-length)
	  :width (- (the beam-width) (twice (the wall-thickness)))
	  :height (- (the beam-height) (twice (the wall-thickness)))))
  )










```

---

## wall.lisp - header
Source: gornschool-training/t6/resources/source/wall.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## wall.lisp - wall
Source: gornschool-training/t6/resources/source/wall.lisp
Type: tutorial

```
(define-object wall(box)
  :input-slots
  ((brick-height 45)
   (brick-length 180)
   (brick-width 90)
   (mortar-joint-width 10)
   (wall-length 3700)
   (wall-height 900))

  :computed-slots
  ((row-height (+ (the brick-height) (the mortar-joint-width)))
   (number-of-rows (round-to-nearest (div (the wall-height) (the row-height)) 1))
   (actual-wall-height (* (the row-height) (the number-of-rows)))
   
   ;; for the wall-length we need the number of full bricks
   ;; if there are n full bricks then there will be (n-1) mortar joints
   ;; so n*brick-length + n-1*mortar-joint-width = overall-length
   ;; or n(brick-length + mortar-join-width) - mortar-joint-width = overall-length
   ;; or n = (overall-length - mortar-joint-width)/(brick-length + mortar-joint-width)
   (number-of-bricks (round-to-nearest (div (- (the wall-length) (the mortar-joint-width))
					    (+ (the brick-length) (the mortar-joint-width)))
				       1))
   (actual-wall-length (+ (* (the number-of-bricks) (the brick-length))
			  (* (- (the number-of-bricks) 1) (the mortar-joint-width))))

   ;; box inputs - gives the wall bounding box
   (height (the actual-wall-height))
   (width (the brick-width))
   (length (the actual-wall-length))

   (full-bricks (apply '+ (list-elements (the row) (the-element full-bricks))))
   (half-bricks (apply '+ (list-elements (the row) (the-element half-bricks))))
   (mortar-volume (apply '+ (list-elements (the row) (the-element mortar-volume))))
   (mortar-density 2162)
   (mortar-mass (* (the mortar-density) (div (the mortar-volume) 1000000000)))
   )
   
     
  :objects
  ((row :type 'row
     :sequence (:size (the number-of-rows))
     :center (translate-along-vector (the (face-center :bottom))
				     (the (face-normal-vector :top))
				     (+ (half (the-child height))
					(* (the-child index) (the-child height))))
     :height (+ (the brick-height) (the mortar-joint-width))
     :full-bricks-per-row (the number-of-bricks)
     :pass-down (brick-height
		 brick-length
		 brick-width
		 mortar-joint-width
		 length
		 width))))


```

---

## wall.lisp - row
Source: gornschool-training/t6/resources/source/wall.lisp
Type: tutorial

```
(define-object row (box)
  :input-slots
  (full-bricks-per-row
   brick-height
   brick-length
   brick-width
   mortar-joint-width)

  :computed-slots
  ((full-brick-row? (or (zerop (the index)) (evenp (the index))))
   (full-bricks (the bricks-and-mortar full-bricks))
   (half-bricks (the bricks-and-mortar half-bricks))
   (mortar-volume (+ (the bricks-and-mortar mortar-joint-volume)
		     (the mortar-bed volume))))

  :objects
  ((bricks-and-mortar :type 'bricks-and-mortar
		      :height (the brick-height)
		      :center (translate-along-vector (the mortar-bed (face-center :top))
						      (the mortar-bed (face-normal-vector :top))
						      (half (the-child height)))
		      :pass-down (width
				  length
				  full-brick-row?
				  brick-height
				  brick-length
				  brick-width
				  mortar-joint-width
				  full-bricks-per-row))
   (mortar-bed :type 'box
	       :height (the mortar-joint-width)
	       :center (translate-along-vector (the (face-center :bottom))
					       (the (face-normal-vector :top))
					       (half (the-child height)))
	       :pass-down (width
			   length))))


```

---

## wall.lisp - bricks-and-mortar
Source: gornschool-training/t6/resources/source/wall.lisp
Type: tutorial

```
(define-object bricks-and-mortar (box)
  :input-slots
  (full-brick-row?
   full-bricks-per-row
   brick-height
   brick-length
   brick-width
   mortar-joint-width)

  :computed-slots
  ((first-full-brick-start-point (if (the full-brick-row?)
				     (the (face-center :front))
				     (the (mortar-joint 0) (face-center :rear))))
   
   (first-mortar-joint-start-point (if (the full-brick-row?)
				       (the (full-brick 0) (face-center :rear))
				       (the (half-brick 0) (face-center :rear))))

   (number-of-full-bricks (if (the full-brick-row?)
			      (the full-bricks-per-row)
			      (- (the full-bricks-per-row) 1)))

   (number-of-mortar-joints (if (the full-brick-row?)
				(- (the number-of-full-bricks) 1)
				(+ (the number-of-full-bricks) 1)))

   ;; if it isn't a full brick row then there will be an extra joint because one
   ;; full brick is replaced with 2 half bricks so without correcting the
   ;; mortar-joint-width the ends of a full brick row and one starting and
   ;; finishing with half bricks won't align. So we need to correct
   ;; the mortar-joint-width
   (corrected-joint-width (if (the full-brick-row?)
				     (the mortar-joint-width)
				     (let ((total-gap (* (- (the number-of-mortar-joints) 1)
									(the mortar-joint-width))))
				       (div total-gap (the number-of-mortar-joints)))))
   ;; collating the output. We could do this analytically, but for this example we'll use the geometry
   (full-bricks (length (list-elements (the full-brick))))
   (half-bricks (length (list-elements (the half-brick))))
   (mortar-joint-volume (apply '+ (list-elements (the mortar-joint)
						 (the-element volume))))
   )
  
  :functions
  ((first-full-brick-center!
    ()
    (translate-along-vector (the first-full-brick-start-point)
			    (the (face-normal-vector :rear))
			    (half (the brick-length))))
   
   (other-full-brick-center!
    (index)
    ;; if its a full brick row, (full-brick 1) is positioned relative to (joint 0)
    ;; if its a half brick row, (full-brick 1) is positioned relative to (joint 1)
    (let ((ind (if (the full-brick-row?) (- index 1) index)))
	  (translate-along-vector (the (mortar-joint ind) (face-center :rear))
				  (the (face-normal-vector :rear))
				  (half (the brick-length)))))
   
    (first-joint-center!
     ()
     (translate-along-vector (the first-mortar-joint-start-point)
			     (the (face-normal-vector :rear))
			     (half (the corrected-joint-width))))
   (other-joint-center!
    (index)
    ;; if its a full brick row, (joint 1) is positioned relative to (full-brick 1)
    ;; if its a half brick row, (joint 1) is positioned relative to (full-brick 0)
    (let ((ind (if (the full-brick-row?) index (- index 1))))
      (translate-along-vector (the (full-brick ind) (face-center :rear))
			      (the (face-normal-vector :rear))
			      (half (the corrected-joint-width)))))

    (first-half-brick-center!
     ()
     (translate-along-vector (the (face-center :front))
			     (the (face-normal-vector :rear))
			     (half (half (the brick-length)))))
   
   (last-half-brick-center!
     ()
     (translate-along-vector (theo (the mortar-joint last) (face-center :rear))
			     (the (face-normal-vector :rear))
			     (half (half (the brick-length))))))
 			     
  
  :objects
  ((full-brick :type 'box
	       :sequence (:size (the number-of-full-bricks))
	       :center (if (= (the-child index) 0)
			   (the first-full-brick-center!)
			   (the (other-full-brick-center! (the-child index))))
	       :length (the brick-length)
	       :height (the brick-height)
	       :width (the brick-width))

   (half-brick :type 'box
	       :sequence (:size (if (the full-brick-row?) 0 2))
	       :center (if (= (the-child index) 0)
			   (the first-half-brick-center!)
			   (the last-half-brick-center!))
	       :length (half (the brick-length))
	       :height (the brick-height)
	       :width (the brick-width))

   (mortar-joint :type 'box
		 :sequence (:size (the number-of-mortar-joints))
		 :center (if (= (the-child index) 0)
			     (the first-joint-center!)
			  (the (other-joint-center! (the-child index))))   
		 :height (the brick-height)
		 :width (the brick-width)
		 :length (the corrected-joint-width))))







```

---

## file-upload-3.lisp - header
Source: gornschool-training/t6/resources/source/file-upload-3.lisp
Type: tutorial

```
(in-package :gwl-user)


```

---

## file-upload-3.lisp - file-upload-3
Source: gornschool-training/t6/resources/source/file-upload-3.lisp
Type: tutorial

```
(define-object file-upload-3 (base-html-page)
  :computed-slots
  ((uploaded-path "" :settable)
   (file-content (let ((lines (read-file (the uploaded-path))))
		   (mapcar #'(lambda(a) (glisp::split-regexp "," a)) lines)))
   (body
    (with-lhtml-string ()
       (str (the development-links))
       (when (= (length (the uploaded-path)) 0)
	 (htm (str (with-form-string (:enctype "multipart/form-data")
		(:table
		    (:tr (:td (:input :type "file" :name :uploaded-path :value (the uploaded-path))))
		  (:tr (:td (:input :type "submit" :name "upload" :value "Upload"))))))))

      (when (> (length (the uploaded-path)) 0)
	(htm (str (fmt "The file has been uploaded to ~a" (the uploaded-path)))
	     (:p "The file contents are")
	     (:table :border 1 (:tr (:td :colspan 2 (str (first (first (the file-content))))))
	       (dolist (line (cdr (the file-content)))
		 (htm (:tr (:td (str (first line)))
			   (:td (str (second line)))))))
	     
	(str (with-form-string (:enctype "multipart/form-data")
	       (:input :type "submit" :name "reset" :value "Reset Form"))))))))

  :functions
  ((after-set! ()
	       (when (member "Reset Form" (the query-plist) :test 'equalp)
		 
		 (when (probe-file (the uploaded-path)) (delete-file (the uploaded-path)))
		 (the (restore-slot-default! :uploaded-path ))))))


```

---

## file-upload-3.lisp - read-file
Source: gornschool-training/t6/resources/source/file-upload-3.lisp
Type: tutorial

```
(defun read-file (file )
  (let ((result))
    (with-open-file (str file :direction :input)
      (do ((line (read-line str nil 'eof)
		 (read-line str nil 'eof)))
	  ((eql line 'eof) result)
	(setq result (append result (list line)))))))

(publish-gwl-app "/file-upload-3" "gwl-user::file-upload-3")

```

---

## file-upload-1.lisp - header
Source: gornschool-training/t6/resources/source/file-upload-1.lisp
Type: tutorial

```
(in-package :gwl-user)


```

---

## file-upload-1.lisp - file-upload-1
Source: gornschool-training/t6/resources/source/file-upload-1.lisp
Type: tutorial

```
(define-object file-upload-1 (base-html-page)
  :computed-slots
  ((uploaded-path "" :settable)
   (body
    (with-lhtml-string () (str (the development-links))
      (str
       (with-form-string (:enctype "multipart/form-data")
         (:table (:tr (:td (:input :type "file" :name :uploaded-path :value (the uploaded-path))))
           (:tr (:td (:input :type "submit" :name "upload" :value "Upload"))
                (:td (str (the uploaded-path)))))))))))

(publish-gwl-app "/file-upload-1" "gwl-user::file-upload-1")

```

---

## using-form-controls.lisp - header
Source: gornschool-training/t6/resources/source/using-form-controls.lisp
Type: tutorial

```
(in-package :gwl-user)


```

---

## using-form-controls.lisp - form-control-layout
Source: gornschool-training/t6/resources/source/using-form-controls.lisp
Type: tutorial

```
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


```

---

## using-form-controls.lisp - form-control-validation
Source: gornschool-training/t6/resources/source/using-form-controls.lisp
Type: tutorial

```
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

```

---

## wall-example-form.lisp - header
Source: gornschool-training/t6/resources/source/wall-example-form.lisp
Type: tutorial

```
(in-package :gwl-user)


```

---

## wall-example-form.lisp - wall-example-form
Source: gornschool-training/t6/resources/source/wall-example-form.lisp
Type: tutorial

```
(define-object wall-example-form (base-html-page)

  :computed-slots
  ((body (with-lhtml-string ()
		      (:table (:tr (:td (str (the form-controls-section div)))
			       (:td (str (the report-section div)))))))
   
   )

  :objects
  ((wall :type 'gdl-user::wall
	 :brick-height (the brick-height-fc value)
	 :brick-length (the brick-length-fc value)
	 :brick-width (the brick-width-fc value)
	 :mortar-joint-width (the mortar-width-fc value)
	 :wall-length (the wall-length value)
	 :wall-height  (the wall-height value))

   (form-controls-section :type 'base-html-div
			  :inner-html (with-lhtml-string ()
					(:table
					    (dolist (obj (the form-controls))
					      (htm (:tr (:td (str (theo obj prompt)))
							(:td (str (theo obj form-control)))))))))
   (report-section :type 'base-html-div
		   :inner-html (with-lhtml-string ()
				 (:table
				     (:tr (:td "Actual Wall Length")
				      (:td (str (the wall actual-wall-length))))
				   (:tr (:td "Actual Wall Height")
					(:td (str (the wall actual-wall-height))))
				   (:tr (:td "Number of Full Bricks")
					(:td (str (the wall full-bricks))))
				   (:tr (:td "Number of Half Bricks")
					(:td (str (the wall half-bricks))))
				   (:tr (:td "Mortar Mass")
					(:td (fmt "~,1f" (the wall mortar-mass)))))))

			  
   (brick-height-fc :type 'number-form-control
		    :prompt "Brick Height (mm)"
		    :default 45
		    :ajax-submit-on-change? t)
   (brick-length-fc :type 'number-form-control
		    :default 180
		    :prompt "Brick Length (mm)"
		    :ajax-submit-on-change? t)
   (brick-width-fc :type 'number-form-control
		   :default 90
		   :prompt "Brick Width (mm)"
		   :ajax-submit-on-change? t)
   (mortar-width-fc :type 'number-form-control
		    :default 10
		    :prompt "Mortar Joint Width (mm)"
		    :ajax-submit-on-change? t)
   (wall-length :type 'number-form-control
		:default 3700
		:prompt "Nominal Wall Length (mm)"
		:ajax-submit-on-change? t)
   
   (wall-height :type 'number-form-control
		:default 3700
		:prompt "Nominal Wall Height (mm)"
		:ajax-submit-on-change? t)))
		
(publish-gwl-app "/wall-example" "gwl-user::wall-example-form")

```

---

## basic-define-and-publish.lisp - header
Source: gornschool-training/t6/resources/source/basic-define-and-publish.lisp
Type: tutorial

```
(in-package :cl-user)

(load (compile-file (merge-pathnames "gwl-patches.lisp" (glisp:source-pathname))))

(in-package :gwl-user)


(setq *developing?* t)




```

---

## basic-define-and-publish.lisp - sample-page
Source: gornschool-training/t6/resources/source/basic-define-and-publish.lisp
Type: tutorial

```
(define-object sample-page (base-html-page)
  :computed-slots
  ((title "sample page")
   (body "My sample page")))

(publish-gwl-app "/sample-page" 'sample-page)



```

---

## basic-define-and-publish.lisp - sample-html-page
Source: gornschool-training/t6/resources/source/basic-define-and-publish.lisp
Type: tutorial

```
(define-object sample-html-page (base-html-page)
  :computed-slots
  ((title "sample html page")
   (body (with-lhtml-string ()
           "My sample html page"
           (:table
               (:tr (:th "Day") (:th "Number"))
             (:tr (:td "Sunday") (:td "1"))
             (:tr (:td "Monday") (:td "2"))
             (:tr (:td "Tuesday") (:td "3"))
             (:tr (:td "Wednesday") (:td "4"))
             (:tr (:td "Thursday") (:td "5"))
             (:tr (:td "Friday") (:td "6"))
             (:tr (:td "Saturday") (:td "7")))))))

(publish-gwl-app "/sample-html-page" 'sample-html-page)


```

---

## basic-define-and-publish.lisp - sample-inline-styled-html-page
Source: gornschool-training/t6/resources/source/basic-define-and-publish.lisp
Type: tutorial

```
(define-object sample-inline-styled-html-page (base-html-page)
  :computed-slots
  ((title "sample inline styled html page")
   (body (with-lhtml-string ()
           "My sample inline styled html page"
           (:table :style "border-style: solid;"
               (:tr (:th "Day") (:th "Number"))
             (:tr (:td "Sunday") (:td "1"))
             (:tr (:td "Monday") (:td "2"))
             (:tr (:td "Tuesday") (:td "3"))
             (:tr (:td "Wednesday") (:td "4"))
             (:tr (:td "Thursday") (:td "5"))
             (:tr (:td "Friday") (:td "6"))
             (:tr (:td "Saturday") (:td "7")))))))

(publish-gwl-app "/sample-inline-styled-html-page" 'sample-inline-styled-html-page)

;; for the css and image files we assume a directory structure as follows
;; <home>/source - location of this file
;; <home>/css - location of css example files
;; <home>/images - location of image files

(defparameter *home*(merge-pathnames "../" (make-pathname :name nil :type nil :defaults (glisp:source-pathname))))

(gwl::publish-file :path "/my-style.css" :file (namestring (merge-pathnames "css/my-style.css" *home*)))


```

---

## basic-define-and-publish.lisp - sample-external-styled-html-page
Source: gornschool-training/t6/resources/source/basic-define-and-publish.lisp
Type: tutorial

```
(define-object sample-external-styled-html-page (base-html-page)
  :computed-slots
  ((additional-header-content (with-lhtml-string ()
                                ((:link :rel "stylesheet" :href "/my-style.css"))))
   (title "sample external styled html page")
   (body (with-lhtml-string ()
           "My sample external styled html page"
           (:table 
               (:tr (:th "Day") (:th "Number"))
             (:tr (:td "Sunday") (:td "1"))
             (:tr (:td "Monday") (:td "2"))
             (:tr (:td "Tuesday") (:td "3"))
             (:tr (:td "Wednesday") (:td "4"))
             (:tr (:td "Thursday") (:td "5"))
             (:tr (:td "Friday") (:td "6"))
             (:tr (:td "Saturday") (:td "7")))))))

(publish-gwl-app "/sample-external-styled-html-page" 'sample-external-styled-html-page)

(gwl::publish-directory :prefix "/images" :destination (namestring (merge-pathnames "images/" *home*)))


```

---

## basic-define-and-publish.lisp - sample-image-html-page
Source: gornschool-training/t6/resources/source/basic-define-and-publish.lisp
Type: tutorial

```
(define-object sample-image-html-page (base-html-page)
  :computed-slots
  ((css-published-path *css-published-path*)
   (images-published-prefix *images-published-prefix*)
   (additional-header-content (with-lhtml-string ()
                                (:link :rel "stylesheet" :href "/my-style.css")))
   (body (with-lhtml-string ()
           (:p "Gorn Struggle")
           (:img :src (string-append (the images-published-prefix) "/star-trek-gorn.webp"))))))

(publish-gwl-app "/sample-image-html-page" 'sample-image-html-page)



```

---

## basic-define-and-publish.lisp - sample-image-development-links
Source: gornschool-training/t6/resources/source/basic-define-and-publish.lisp
Type: tutorial

```
(define-object sample-image-development-links (base-html-page)

  :computed-slots
  ((css-published-path *css-published-path*)
   (images-published-prefix *images-published-prefix*)
   (additional-header-content (with-lhtml-string ()
                                (:link :rel "stylesheet" :href (the css-published-path))))
   (body (with-lhtml-string ()
           (when *developing?* (str (the development-links)))
           (:p "Gorn Struggle")
           (:img :src (string-append (the images-published-prefix) "/star-trek-gorn.webp"))))))
(publish-gwl-app "/sample-image-development-links" 'sample-image-development-links)


```

---

## simple-file-output.lisp - header
Source: gornschool-training/t6/resources/source/simple-file-output.lisp
Type: tutorial

```
(in-package :gwl-user)


```

---

## simple-file-output.lisp - simple-file-output
Source: gornschool-training/t6/resources/source/simple-file-output.lisp
Type: tutorial

```
(define-object simple-file-output (base-html-page)
  :computed-slots
  ((body (with-lhtml-string ()
           (str (the development-links))
	   (str (the export-section div))))

   (file-contents  (let ((line (list 1 2 3 4 5 6 7)))
		     (format nil "~{Line ~a of my file~^~%~}" line)))

   (text-physical-file-url (let ((url (format nil "/file-output-~a.txt" (get-current-date-time)))
                                 (file-path (make-pathname :defaults (glisp:temporary-file)
                                                                    :type "txt")))
                             (with-open-file (f file-path :direction :output :if-exists :supersede)
                               (write-string (the file-contents) f))
                             (publish-file :path url
                                           :content-type "text/plain"
                                           :file file-path)
                             url))

   (text-file-url (let ((url (format nil "/stream-output-~a.txt" (get-current-date-time))))
                    (publish :path url
                             :content-type "text/plain"
                             :function #'(lambda(req ent)
                                           (with-http-response (req ent)
                                             (with-http-body (req ent)
                                               (write-string (the file-contents) *html-stream*)))))
                    url)))
  :objects
  ((export-section
    :type 'page-section
    :inner-html (with-lhtml-string ()
                  (:p "Click "
                      (:a :href (the text-file-url) :download "virtual-text-file.txt" "Here")
                      " to download a virtual text file.")
                  (:p "Click "
                      (:a :href (the text-physical-file-url) :download "physical-text-file.txt" "Here")
                      " to download a physical text file.")))))


```

---

## simple-file-output.lisp - get-current-date-time
Source: gornschool-training/t6/resources/source/simple-file-output.lisp
Type: tutorial

```
(defun get-current-date-time ()
  (let* ((d (multiple-value-bind (s m h d mo y da)
		(get-decoded-time)
	      (declare (ignore da))
	      (list d mo y h m s)))
	 (day (first d))
	 (month (second d))
	 (year (third d))
	 (hour (fourth d))
	 (mins (fifth d))
	 (sec (lastcar d)))
    (format nil "~2,,,'0@a-~2,,,'0@a-~a-~2,,,'0@a-~2,,,'0@a-~2,,,'0@a" day month year hour mins sec)))

(publish-gwl-app "/simple-file-output" "gwl-user::simple-file-output")

```

---

## using-page-sections.lisp - header
Source: gornschool-training/t6/resources/source/using-page-sections.lisp
Type: tutorial

```
(in-package :gwl-user)


```

---

## using-page-sections.lisp - simple-page-with-section
Source: gornschool-training/t6/resources/source/using-page-sections.lisp
Type: tutorial

```
(define-object simple-page-with-section (base-html-page)
  :computed-slots
  ((body
    (with-lhtml-string ()
      (when gwl:*developing?* (str (the development-links)))
      (:h2 "Basic Page Sections")
      (str (the section-1 div)))))
  :objects
  ((section-1 :type 'base-html-div
              :inner-html (with-lhtml-string ()
                            (:p "Using page sections to provide content")))))

(publish-gwl-app "/simple-page-with-section" 'simple-page-with-section)


```

---

## using-page-sections.lisp - simple-page-with-custom-section
Source: gornschool-training/t6/resources/source/using-page-sections.lisp
Type: tutorial

```
(define-object simple-page-with-custom-section (base-html-page)
  :computed-slots
  ((input-list (list 1 2 3))
   (body (with-lhtml-string ()
           (when gwl:*developing?* (str (the development-links)))
           (:h2 "Basic Page Sections")
           (str (the section-1 div)))))
  :objects
  ((section-1 :type 'base-html-div-1
              :input-list (the input-list))))



```

---

## using-page-sections.lisp - base-html-div-1
Source: gornschool-training/t6/resources/source/using-page-sections.lisp
Type: tutorial

```
(define-object base-html-div-1 (base-html-div)
  :input-slots
  (input-list)
  :computed-slots
  ((inner-html (with-cl-who-string ()
                 (:table :border 1
                   (:tr (:th "Content"))
                   (dolist (c (the input-list))
                     (htm (:tr (:td (fmt "Cell ~a content" c))))))))))

(publish-gwl-app "/simple-page-with-custom-section" 'simple-page-with-custom-section)

```

---

## basic-form-controls.lisp - header
Source: gornschool-training/t6/resources/source/basic-form-controls.lisp
Type: tutorial

```
(in-package :gwl-user)


```

---

## basic-form-controls.lisp - basic-form-controls
Source: gornschool-training/t6/resources/source/basic-form-controls.lisp
Type: tutorial

```
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

```

---

## building-application-file.lisp - header
Source: gornschool-training/t6/resources/source/building-application-file.lisp
Type: tutorial

```
(in-package :gwl-user)


```

---

## building-application-file.lisp - building-application-file
Source: gornschool-training/t6/resources/source/building-application-file.lisp
Type: tutorial

```
(define-object building-application-file (base-html-page)

  :computed-slots
  ((uploaded-path "" :settable)
   
   (body (with-lhtml-string ()
	   (str (the development-links))
	   (when (= (length (the uploaded-path)) 0)
	     (str
	      (with-form-string (:enctype "multipart/form-data")
		(:table (:tr (:td (:input :type "file" :name :uploaded-path :value (the uploaded-path))))
		  (:tr (:td (:input :type "submit" :name "upload" :value "Upload")))))))
	   (when (> (length (the uploaded-path)) 0)
		    (htm (:p "Click " (:a :href (the output-url) :download "bom.txt" "Here") " to download the BoM")
		    (str (with-form-string (:enctype "multipart/form-data")
			   (:input :type "submit" :name "reset" :value "Reset Form")))))))
	   

   (output-path (merge-pathnames "building-output.txt" (the uploaded-path)))

   (output-url (let ((url "/output.txt"))
		 (the building write-bom-file!)
		 (gwl::unpublish url)
		 (gwl::publish-file :path url
				    :file (the output-path))
		 url)))

  :functions
  ((after-set! ()
	       (cond ((member "Reset Form" (the query-plist) :test 'equalp)
		      (when (probe-file (the uploaded-path)) (delete-file (the uploaded-path)))
		      (the (restore-slot-default! :uploaded-path ))))))
		      

  :objects
  ((building :type 'building
	     :input-filename (when (> (length (the uploaded-path)) 1) (the uploaded-path))
	     :output-filename (when (> (length (the uploaded-path)) 1) (the output-path)) )))

(publish-gwl-app "/building-example" "gwl-user::building-application-file")

```

---

## file-upload-2.lisp - header
Source: gornschool-training/t6/resources/source/file-upload-2.lisp
Type: tutorial

```
(in-package :gwl-user)


```

---

## file-upload-2.lisp - file-upload-2
Source: gornschool-training/t6/resources/source/file-upload-2.lisp
Type: tutorial

```
(define-object file-upload-2 (base-html-page)
  :computed-slots
  ((uploaded-path "" :settable)
   (body
    (with-lhtml-string ()
       (str (the development-links))
       (when (= (length (the uploaded-path)) 0)
	 (htm (str (with-form-string (:enctype "multipart/form-data")
		(:table
		    (:tr (:td (:input :type "file" :name :uploaded-path :value (the uploaded-path))))
		  (:tr (:td (:input :type "submit" :name "upload" :value "Upload"))))))))

      (when (> (length (the uploaded-path)) 0)
	(htm (str (format nil "The file has been uploaded to ~a" (the uploaded-path))))
	(str (with-form-string (:enctype "multipart/form-data")
	       (:input :type "submit" :name "reset" :value "Reset Form"))))))

	 )

  :functions
  ((after-set! ()
	       (when (member "Reset Form" (the query-plist) :test 'equalp)
		 
		 (when (probe-file (the uploaded-path)) (delete-file (the uploaded-path)))
		(the (restore-slot-default! :uploaded-path ))))))

(publish-gwl-app "/file-upload-2" "gwl-user::file-upload-2")

```

---

## gendl-graphics.lisp - header
Source: gornschool-training/t6/resources/source/gendl-graphics.lisp
Type: tutorial

```

(in-package :gwl-user)


```

---

## gendl-graphics.lisp - single-viewport-sheet
Source: gornschool-training/t6/resources/source/gendl-graphics.lisp
Type: tutorial

```
(define-object single-viewport-sheet (geysr:assembly)

  :input-slots
  ((background-color :cyan))
  
  :computed-slots
  ((body-onpageshow nil )
   (body-onresize nil)
   (body-onpageload nil)



   (x3dom? (eql (the viewport image-format-selector value) :x3dom))
   (viewport-dimensions (list :length 500 :width 500) :settable)

   (main-sheet-body
    (with-cl-who-string ()
      (when gwl:*developing?* (str (the development-links)))
      (:p "Sample Graphics Viewport")
      (:p
       (str (the menu main-div))
       (str (the menu-script main-div))
       (str (the viewport main-div))
       ;;(str (the viewport viewport-script))
       ))))

  
  :objects
  ((wall :type 'gdl-user::wall)

   (viewport :type 'geysr:viewport
             :view-direction-default :trimetric
	     :image-format-default :svg
	     :image-format-plist (list :x3dom "Shaded"
				       :svg "Wireframe"
                                       ;;:a-frame "VR"
                                       )
	     :empty-display-list-message (with-cl-who-string ()
					   (:p (:h2 (fmt "Viewport Display-List is currently empty."))))
	     :div-style "background: green; height: 100%; width: 100%"	     
	     :geysr self
	     :div-class "viewport-wrapper"
	     :root-object self
	     :length (if (the x3dom?) "100%" (getf (the viewport-dimensions) :length))
	     :width (if (the x3dom?) "100%" (getf (the viewport-dimensions) :width))
	     :onclick-function nil
             :display-list-object-roots (list (the wall))
	     :background-color (the background-color)  ;;was  "#d3d3d3"
             )))


(publish-gwl-app "/wall" 'single-viewport-sheet)


(in-package :gdl-user)


```

---

## gendl-graphics.lisp - wall
Source: gornschool-training/t6/resources/source/gendl-graphics.lisp
Type: tutorial

```
(define-object wall (box)
  :input-slots
  ((brick-height 45)
   (brick-length 180)
   (brick-width 90)
   (mortar-joint-width 10)
   (wall-length 3700)
   (wall-height 900))

  :computed-slots
  ((row-height (+ (the brick-height) (the mortar-joint-width)))
   (number-of-rows (round-to-nearest (div (the wall-height) (the row-height)) 1))
   (actual-wall-height (* (the row-height) (the number-of-rows)))
   
   ;; for the wall-length we need the number of full bricks
   ;; if there are n full bricks then there will be (n-1) mortar joints
   ;; so n*brick-length + n-1*mortar-joint-width = overall-length
   ;; or n(brick-length + mortar-join-width) - mortar-joint-width = overall-length
   ;; or n = (overall-length - mortar-joint-width)/(brick-length + mortar-joint-width)
   (number-of-bricks (round-to-nearest (div (- (the wall-length) (the mortar-joint-width))
					    (+ (the brick-length) (the mortar-joint-width)))
				       1))
   (actual-wall-length (+ (* (the number-of-bricks) (the brick-length))
			  (* (- (the number-of-bricks) 1) (the mortar-joint-width))))

   ;; box inputs - gives the wall bounding box
   (height (the actual-wall-height))
   (width (the brick-width))
   (length (the actual-wall-length))

   (full-bricks (apply '+ (list-elements (the row) (the-element full-bricks))))
   (half-bricks (apply '+ (list-elements (the row) (the-element half-bricks))))
   (mortar-volume (apply '+ (list-elements (the row) (the-element mortar-volume))))
   (mortar-density 2162)
   (mortar-mass (* (the mortar-density) (div (the mortar-volume) 1000000000)))
   )
   
     
  :objects
  ((row :type 'row
     :sequence (:size (the number-of-rows))
     :center (translate-along-vector (the (face-center :bottom))
				     (the (face-normal-vector :top))
				     (+ (half (the-child height))
					(* (the-child index) (the-child height))))
     :height (+ (the brick-height) (the mortar-joint-width))
     :full-bricks-per-row (the number-of-bricks)
     :pass-down (brick-height
		 brick-length
		 brick-width
		 mortar-joint-width
		 length
		 width))))


```

---

## gendl-graphics.lisp - row
Source: gornschool-training/t6/resources/source/gendl-graphics.lisp
Type: tutorial

```
(define-object row (box)
  :input-slots
  (full-bricks-per-row
   brick-height
   brick-length
   brick-width
   mortar-joint-width)

  :computed-slots
  ((full-brick-row? (or (zerop (the index)) (evenp (the index))))
   (full-bricks (the bricks-and-mortar full-bricks))
   (half-bricks (the bricks-and-mortar half-bricks))
   (mortar-volume (+ (the bricks-and-mortar mortar-joint-volume)
		     (the mortar-bed volume))))

  :objects
  ((bricks-and-mortar :type 'bricks-and-mortar
		      :height (the brick-height)
		      :center (translate-along-vector (the mortar-bed (face-center :top))
						      (the mortar-bed (face-normal-vector :top))
						      (half (the-child height)))
		      :pass-down (width
				  length
				  full-brick-row?
				  brick-height
				  brick-length
				  brick-width
				  mortar-joint-width
				  full-bricks-per-row))
   (mortar-bed :type 'box
	       :height (the mortar-joint-width)
	       :center (translate-along-vector (the (face-center :bottom))
					       (the (face-normal-vector :top))
					       (half (the-child height)))
	       :pass-down (width
			   length))))


```

---

## gendl-graphics.lisp - bricks-and-mortar
Source: gornschool-training/t6/resources/source/gendl-graphics.lisp
Type: tutorial

```
(define-object bricks-and-mortar (box)
  :input-slots
  (full-brick-row?
   full-bricks-per-row
   brick-height
   brick-length
   brick-width
   mortar-joint-width)

  :computed-slots
  ((first-full-brick-start-point (if (the full-brick-row?)
				     (the (face-center :front))
				     (the (mortar-joint 0) (face-center :rear))))
   
   (first-mortar-joint-start-point (if (the full-brick-row?)
				       (the (full-brick 0) (face-center :rear))
				       (the (half-brick 0) (face-center :rear))))

   (number-of-full-bricks (if (the full-brick-row?)
			      (the full-bricks-per-row)
			      (- (the full-bricks-per-row) 1)))

   (number-of-mortar-joints (if (the full-brick-row?)
				(- (the number-of-full-bricks) 1)
				(+ (the number-of-full-bricks) 1)))

   ;; if it isn't a full brick row then there will be an extra joint because one
   ;; full brick is replaced with 2 half bricks so without correcting the
   ;; mortar-joint-width the ends of a full brick row and one starting and
   ;; finishing with half bricks won't align. So we need to correct
   ;; the mortar-joint-width
   (corrected-joint-width (if (the full-brick-row?)
				     (the mortar-joint-width)
				     (let ((total-gap (* (- (the number-of-mortar-joints) 1)
									(the mortar-joint-width))))
				       (div total-gap (the number-of-mortar-joints)))))
   ;; collating the output. We could do this analytically, but for this example we'll use the geometry
   (full-bricks (length (list-elements (the full-brick))))
   (half-bricks (length (list-elements (the half-brick))))
   (mortar-joint-volume (apply '+ (list-elements (the mortar-joint)
						 (the-element volume))))
   )
  
  :functions
  ((first-full-brick-center!
    ()
    (translate-along-vector (the first-full-brick-start-point)
			    (the (face-normal-vector :rear))
			    (half (the brick-length))))
   
   (other-full-brick-center!
    (index)
    ;; if its a full brick row, (full-brick 1) is positioned relative to (joint 0)
    ;; if its a half brick row, (full-brick 1) is positioned relative to (joint 1)
    (let ((ind (if (the full-brick-row?) (- index 1) index)))
	  (translate-along-vector (the (mortar-joint ind) (face-center :rear))
				  (the (face-normal-vector :rear))
				  (half (the brick-length)))))
   
    (first-joint-center!
     ()
     (translate-along-vector (the first-mortar-joint-start-point)
			     (the (face-normal-vector :rear))
			     (half (the corrected-joint-width))))
   (other-joint-center!
    (index)
    ;; if its a full brick row, (joint 1) is positioned relative to (full-brick 1)
    ;; if its a half brick row, (joint 1) is positioned relative to (full-brick 0)
    (let ((ind (if (the full-brick-row?) index (- index 1))))
      (translate-along-vector (the (full-brick ind) (face-center :rear))
			      (the (face-normal-vector :rear))
			      (half (the corrected-joint-width)))))

    (first-half-brick-center!
     ()
     (translate-along-vector (the (face-center :front))
			     (the (face-normal-vector :rear))
			     (half (half (the brick-length)))))
   
   (last-half-brick-center!
     ()
     (translate-along-vector (theo (the mortar-joint last) (face-center :rear))
			     (the (face-normal-vector :rear))
			     (half (half (the brick-length))))))
 			     
  
  :objects
  ((full-brick :type 'box
	       :sequence (:size (the number-of-full-bricks))
	       :center (if (= (the-child index) 0)
			   (the first-full-brick-center!)
			   (the (other-full-brick-center! (the-child index))))
	       :length (the brick-length)
	       :height (the brick-height)
	       :width (the brick-width))

   (half-brick :type 'box
	       :sequence (:size (if (the full-brick-row?) 0 2))
	       :center (if (= (the-child index) 0)
			   (the first-half-brick-center!)
			   (the last-half-brick-center!))
	       :length (half (the brick-length))
	       :height (the brick-height)
	       :width (the brick-width))

   (mortar-joint :type 'box
		 :sequence (:size (the number-of-mortar-joints))
		 :center (if (= (the-child index) 0)
			     (the first-joint-center!)
			  (the (other-joint-center! (the-child index))))   
		 :height (the brick-height)
		 :width (the brick-width)
		 :length (the corrected-joint-width))))







```

---

