(in-package :training-3)

(define-object working-with-packages (base-training-sheet)
  :input-slots
  (getting-started-url)

  :computed-slots
  ((index-words (list "define-package" "in-package" ":use" ":export" "Fully qualified"))

   (code-1 (list "(in-package :gdl-user)"
		 ""
		 "(define-package :my-app)"))
   
   (code-2 (list "(in-package :gdl-user)"
		 ""
		 "(define-package :my-app"
		 "    (:export #:my-slot))"))

   (code-3 (list "(in-package :functions)"
		 "(eval-when (load eval) (export 'average))"
		 "(defun average (lis)"
		 "  (/ (apply '+ lis) (length lis)))"))
   
   (code-4 (list "(in-package :gdl-user)"
		 ""
		 "(define-package :my-other-app"
		 "    (:use :my-app))"))
   

   
   (repl-1 (list (list :command "(in-package :my-app)"
		       :output "#<Package \"MY-APP\">")
		 (list :prompt "MY-APP> ")))

   (repl-2 (list (list 
		       :command "(setq my-slot 2)"
		       :output 2)
		 (list :prompt "MY-APP> "
		       :command "(in-package :my-other-app)"
		       :output "#<Package \"MY-OTHER-APP\">")
		 (list :prompt "MY-OTHER-APP> "
		       :command "my-slot"
		       :error "; Evaluation aborted on #<UNBOUND-VARIABLE #x2103BF155D>.")))

   (repl-3 (list (list :command (list "(define-package package-a"
				      "   (:export x))")
		       :output "#<Package \"PACKAGE-A\">")
		 (list :command "(define-package package-b)"
		       :output "#<Package \"PACKAGE-B\">")
		 (list :command (list "(define-package package-c"
				      "   (:use a))")
		       :output "#<Package \"PACKAGE-C\">")
		 (list :command "(in-package package-a)"
		       :output "#<Package \"PACKAGE-A\">")
		 (list :prompt "PACKAGE-A>"
		       :command "(setq x 1)"
		       :output 1)
		 (list :prompt "PACKAGE-A>"
		       :command "(setq y 2)"
		       :output 1)
		 (list :prompt "PACKAGE-A>"
		       :command "(in-package package-b)"
		       :output "#<Package \"B\">")
		 (list :prompt "PACKAGE-B>"
		       :command "x"
		       :error "; Evaluation aborted on #<UNBOUND-VARIABLE #x2103CD8FCD>.")
		 (list :prompt "PACKAGE-B>"
		       :command "y"
		       :error "; Evaluation aborted on #<UNBOUND-VARIABLE #x2103CD8FCD>.")
		 (list :prompt "PACKAGE-B>"
			:command "package-a:x"
		       :output 1)
		 (list :prompt "PACKAGE-B>"
			:command "package-a:y"
		       :error "; Evaluation aborted on #<SIMPLE-ERROR #x2103CA28BD>.")
		 (list :prompt "PACKAGE-B>"
		       :command "package-a::y"
		       :output 2)
		 (list :prompt "PACKAGE-B>"
		       :command "(in-package c)"
		       :output "#<Package \"C\">")
		 (list :prompt "PACKAGE-C>"
		       :command "x"
		       :output 1)
		 (list :prompt "PACKAGE-C>"
		       :command "y"
		       :output "; Evaluation aborted on #<UNBOUND-VARIABLE #x2103CD8FCD>.")
		 (list :prompt "PACKAGE-C>"
		       :command "package-a:y"
		       :output "; Evaluation aborted on #<SIMPLE-ERROR #x2103CA28BD>.")
		 (list :prompt "PACKAGE-C>"
		       :command "package-a::y"
		       :output 2)))

   (body-content (with-cl-who-string()
		   ((:div :class "main-page-container" :style "grid-template-columns: 650px auto;")
		    ((:div :class "main-page-item")
		     (:p "Packages represent "(:em "namespaces") " for Lisp symbols. You can think of them like an \"area-code\" for symbols.")
		     (:p "In the " (if (the getting-started-url) (htm ((:a :href (the getting-started-url))"Getting Started with GendL")) "Getting Started with GendL ")
			 " tutorial, all of the example code that was provided started with the "
			 (:em (:b "(in-package :gdl-user)"))" statement. All of the symbols created when objects were defined, belonged to the "
			 ((:span :class "package-name")":gdl-user")" package. Because all source code files stared with the "
			 (:em (:b "(in-package :gdl-user)"))" statement we don't need to qualify any of the symbols, we just use their name. But if we wanted to use a symbol from another package that wasn't used by "
			 ((:span :class "package-name")":gdl-user")" we would need to qualify that symbol with a package name. ")
		     (:p "To create a package, we"
			 (:ul (:li "Call the Gendl macro "((:span :class "macro")"define-package"))
			      (:li "Define the "(:em (:b "name"))" of the new package")
			      (:li "Optionally provide a list of packages it will "
				   ((:span :class "object-keyword")":use")". Note that "
				   ((:span :class "macro")"define-package")" automatically includes the following packages: "
				   (:ul (:li ((:span :class "package-name")":common-lisp"))
					(:li ((:span :class "package-name")":gdl"))
					(:li ((:span :class "package-name")":geom-base"))
					(:li ((:span :class "package-name")":surf"))))
			      (:li "Optionally define a list of symbols that the package will "((:span :class "object-keyword")":export")))
			
			 "In most cases we create a lisp file, normally called package.lisp "
			 (:em "(this is just a convention, not a requirement)")" to do this. We need to set a working pakage and for this the "
			 ((:span :class "package-name")":gdl-user")" package is suitable. The we call the GendL "
			 ((:span :class "macro")"define-package")" macro. So if we define some package code like this")
		     (str (code-example (the code-1)))
		     (:p "Then in the REPL we can use the "((:span :class "function")"in-package")" function to switch to out new package")
		     (str (repl-example (the repl-1)))
		     (:h3 "The :export option")
		     (:p "When we define a new package we can "
			 ((:span :class "object-keyword")":export")" symbols from that package for external use. Whilst symbols from that package can still be accessed even if the aren't "
			 ((:span :class "object-keyword")"exported")", it isn't considered to be good practice to do this, it's up to the package owner to choose to "
			 ((:span :class "object-keyword")"export")" or "
			 (:em "expose")" which symbols are for external use. A good watchword is "
			 (:ul (:li (:em (:b "\"Not Exported = Unsupported\"")))))
		     (:p "Extending our "
			 ((:span :class "macro")"define-package")" example to "
			 ((:span :class "object-keyword")":export")" a symbol "
			 ((:span :class "slot")"my-slot")" from our package would look like this")
		     (str (code-example (the code-2)))
		     (:p "In some cases it is convenient to "
			 ((:span :class "object-keyword")":export")" a symbol after "
			 ((:span :class "macro")"define-package")" has been evaluated. This may be the case, for example, where we have a number of functions to be "
			 ((:span :class "object-keyword")":exported")" and it is easier to manage the "
			 ((:span :class "object-keyword")":export")" from within the functions source code file. To do this we can use the Common Lisp Special Operator "
			 ((:span :class "special-operator")"eval-when")" as shown in the example below")
		     (str (code-example (the code-3)))
		     (:p "In this example the function "
			 ((:span :class "function")"average")" is exported from the "
			 ((:span :class "object-keyword")":functions")" package when either a top-level form in a compiled file is being loaded ("
			 (:em (:b "load"))") or when the expression would be evaluated anyway ("
			 (:em (:b "eval"))")")
		     (:h3 "The :use option")
		     (:p "To access a symbol which has been "
			 ((:span :class "object-keyword")"exported")" from a package we need to use the fully qualified name for that symbol - in other words its package and symbol seperated by a colon. However, if we "
			 ((:span :class "object-keyword")":use")" a package when we create a new package, we can access "
			 ((:span :class "object-keyword")"exported")" symbols using their symbol only. Creating a new package which  "
			 ((:span :class "object-keyword")"uses")" the package created earlier would look like this")
		
		     (str (code-example (the code-4)))
		     (:h3 "Bringing it all together")
		     (:p "Let's consider 3 packages, "
			 ((:span :class "package-name")"package-a")", "
			 ((:span :class "package-name")"package-b")" and "
			 ((:span :class "package-name")"package-c")". Package "
			 ((:span :class "package-name")"package-a")" "
			 ((:span :class "object-keyword")"exports")" a symbol "
			 ((:span :class "slot")"x")" and also defines another symbol "
			 ((:span :class "slot")"y")". Package "
			 ((:span :class "package-name")"package-b")" doesn't "
			 ((:span :class "object-keyword")":export")" any symbols and doesn't "
			 ((:span :class "object-keyword")":use")" any other packages. Package "
			 ((:span :class "package-name")"package-c")" "
			 ((:span :class "object-keyword")":uses")" package "
			 ((:span :class "package-name")"package-a"))
		     (str (repl-example (the repl-3)))
		     (:p "When we are in package "
			 ((:span :class "package-name")"package-b")", which does not "
			 ((:span :class "object-keyword")":use")" package "
			 ((:span :class "package-name")"package-a")
			 (:ul (:li "we can access exported symbols from package "
				   ((:span :class "package-name")"package-a")" but need to qualify the symbol with the "
				   (:em (:b "package name and a single colon")))
			      (:li "we can access unexported symbols from package "
				   ((:span :class "package-name")"package-a")" but need to qualify the symbol with the "
				   (:em (:b "package name and double colon"))))
			 (:p "When we are in package "
			     ((:span :class "package-name")"package-c")", which does "
			     ((:span :class "object-keyword")":use")" package "
			     ((:span :class "package-name")"package-a")
			     (:ul (:li "we can access exported symbols from package "
				       ((:span :class "package-name")"package-a")" just by their name")
				  (:li "we can access unexported symbols from package "
				       ((:span :class "package-name")"package-a")" but need to qualify the symbol with the "(:em (:b "package name and a double colon")))))))
		    ((:div :class "main-page-item")
		     (:h2 "Resources")
		     (str (the resource-links))))))))

			

			
