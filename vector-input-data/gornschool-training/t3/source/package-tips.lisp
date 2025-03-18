(in-package :training-3)

(define-object package-tips (base-training-sheet)

  :computed-slots
  ((index-words (list "define-package" "in-package" ":use" ":export" "Interned"))

   (body-content (with-cl-who-string()
		   ((:div :class "main-page-container" :style "grid-template-columns: 600px auto;")
		    ((:div :class "main-page-item")
		     (:h3 "When to use packages")
		     (:ul (:li "If you are starting a new application or project")
			  (:li "If you find yourself copying and pasting from another project/application (create a new package that would be shared between the 2 projects/applications, put the code in that new package and export the symbol")
			  (:li "If you are writing some general purpose functions which you intend to share"))
			
		     (:h3 "Some package guidelines")
		     (:p "When working with packages theres a few things you need to know to make it all work smoothly")
		     (:ol (:li (:em (:b "Packages must be defined before you can Load or Compile a file containing an in-package statement refering to that package"))
			       (:p "This means there is an order in which files must be loaded, in turn implying that the code defining a package should be in a separate file to code that uses those packages. We'll get onto both of these topics later"))
			  (:li (:em (:b "Packages must be defined before other packages can refer to them"))
			       (:p "Practically this means avoiding circularities in the "
				   ((:span :class "object-keyword")":use")" statement, but again implies form form of load order which again means keeping code defining packages in separate files"))
			  (:li (:em (:b "Code files should contain one, "
					(:em "and only one")", "
					((:span :class "function")"in-package")" statement and this should be the first line of code in that file"))
			       (:p "As far as Common Lisp is concerned you can put multiple in-package statements in a file. There are however 2 reasons why this approach would be considered good practice"
				   (:ul (:li "It avoids confusing humand who might not notice a second or third in-package statement")
					(:li "The emacs/SLIME development environment look for the in-package statement to determine the package to be used when communicationg with Common Lisp and multiple in-package statement may confuse these tools"))))
			  (:li (:em (:b "A final consideration is naming of Packages and particularly avoiding package name clashes."))
			       (:p "This isn't generally a problem when you are only using packages that you have developed, but if you are sharing them with other programmers, or using third party packages this needs to be considered, with some kind of naming convention. As a package name is just a text string you can adopt any convention you choose. A trend seems to be a reverse internet domain name followed by a description, for example com.genworks.my-app")))

		     (:h3 "Some package gotcha's")
		     (:p "Generally once you've got working with packages you won't need to think about them that much, they just do what they're supposed to do. But occasionally, and mainly when setting up new packages, there can be a few 'odd' errors. Perhaps the most common is symbol conflicts. Consider the following case"
			 (:ul (:li "You create a package, say "
				   ((:span :class "package-name")"my-package")", which you expect to use a function, say "
				   ((:span :class "function")"test-function")", which is "
				   ((:span :class "object-keyword")":exported")" from another package "
				   ((:span :class "package-name")"my-functions"))
			      (:li "In the source code for the app using "
				   ((:span :class "package-name")"my-package")", you refer to "
				   ((:span :class "function")"test-function")" by its symbol only - which is fine because its "
				   ((:span :class "object-keyword")"exported"))
			      (:li "But you've forgotten to "
				   ((:span :class "object-keyword")":use")" "
				   ((:span :class "package-name")"my-functions")" when defining the package "
				   ((:span :class "package-name")"my-package"))
			      "So when you try to run the compiled code, you'll get an "
			      (:em (:b "'undefined function'"))" error. No problem, you return to the "
			      ((:span :class "macro")"define-package")" statement for "
			      ((:span :class "package-name")"my-package")", add "
			      (:em (:b "(:use my-functions)"))" and then try to run again. This time you get a "
			      (:em (:b "conflicting symbols"))" error! Whats happened here is that the first time you run the code the the symbol "
			      ((:span :class "function")"test-function")" is interned in "
			      ((:span :class "package-name")"my-package")". Then you "
			      ((:span :class "object-keyword")":use")" "
			      ((:span :class "package-name")"my-functions")" from which "
			      ((:span :class "function")"test-function")" is exported so you've got a "
			      (:em (:b "conflicting symbol"))" clash! The solution is to accept the restart to unintern "
			      ((:span :class "function")"test-function")" from "
			      ((:span :class "package-name")"my-package")" and then recompile the "
			      ((:span :class "package-name")"my-package")" package"))
		     (:p "Another issue potential issue is"
			 (:ul (:li "You create a package, say "
				   ((:span :class "package-name")"my-package")", which "
				   ((:span :class "object-keyword")":uses")" "
				   ((:span :class "package-name")"my-functions"))
			      (:li ((:span :class "package-name")"my-functions")" "
				   ((:span :class "object-keyword")":exports")" a number of functions")
			      (:li "You then define a function in "
				   ((:span :class "package-name")"my-package")" which has the same symbol as a function "
				   ((:span :class "object-keyword")":exported")" from "
				   ((:span :class "package")"my-functions")))
			 "What will happen here is that your function in "
			 ((:span :class "package-name")"my-package")" will effectively overwrite the function of the same name in "
			 ((:span :class "package-name")"my-functions")". In most Lisps you'll get a warning that a symbol is being redefined, but its not classed as an error")
		     (:p "A final package-related issue is to do with dropping out of Geysr into the REPL to do some debugging. If the debugging is referring to slots defined in a package other than the REPL's current package, when you try to evaluate the slot you'll get an 'undefined function' error. The simple solution to this is to change the REPL's package, using the in-package function, to the package of the code that you're working with and evaluate the expression again"))))))
  )
