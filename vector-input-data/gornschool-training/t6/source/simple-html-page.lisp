(in-package :training-6)

(define-object simple-html-page (base-training-sheet)

  :computed-slots
  ((index-words (list "with-lhtml-string" "gwl:*delevoping?*" "publish-gwl-app" "base-html-page" "merge-pathnames"
		      "gwl::publish-file" "publish-file" "gwl::publish-directory" "publish-directory" "CSS" "images"
		      "body" "additional-header-content" "development-links"))
		
   (code-1 (list"(in-package :gwl-user)"
		""
		"(define-object sample-page (base-html-page)"
		"  :computed-slots"
		"   ((body \"My first web page\")))"
		""
		"(publish-gwl-app \"/sample-page\" 'sample-page)"))

   (code-2 (list "(define-object sample-html-page (base-html-page)"
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

   (code-3 (list "(define-object sample-inline-styled-html-page  (base-html-page)"
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
   
   (code-5 (list "(define-object sample-external-styled-html-page (base-html-page)"
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

   
   (code-7 (list "(define-object sample-image-html-page (base-html-page)"
		 " :computed-slots"
		 " ((additional-header-content (with-lhtml-string()"
		 "                              (:link :rel \"stylesheet\" :href \"/my-style.css\")))"
		 "  (body (with-lhtml-string ()"
		 "              (:p \"The Genworks International Logo\")"
		 "              (:img :src \"/images/logo.png\")))))"
		 ""
		 "(publish-gwl-app \"/sample-image-html-page\" 'sample-image-html-page)"))
   
   (code-8 (list "(define-object sample-image-development-links (base-html-page)"
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


