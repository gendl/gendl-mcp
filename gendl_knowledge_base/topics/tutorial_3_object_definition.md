# Gendl Documentation - tutorial_3_object_definition

## assembly.lisp - header
Source: gornschool-training/t3/source/assembly.lisp
Type: tutorial

```
(in-package :training-3)

(defparameter *publish-prefix* "t3")  


```

---

## assembly.lisp - assembly
Source: gornschool-training/t3/source/assembly.lisp
Type: tutorial

```
(define-object assembly (base-tutorial-sheet)
  :input-slots
  ((getting-started-url nil)
   (tutorial-name "Organising your code"))

  :computed-slots
  ((introduction (with-cl-who-string ()
		   (:p "Now that you have completed the Getting Started with GendL tutorial, its worth giving some thought to the way you may want to organise and load the code you will be writing. This tutorial covers some basic principles and provides a few guidelines that we would consider to be best practice.")
		   (:p "The best time to get organised is right at the start as it will avoid getting locked in to less then optimal working practices, or needing to go through all your code at a later date to retro-fit it to your chosen organisation structure or methodology"))))

  :objects
  ((working-with-packages :type 'working-with-packages
			  :pass-down (page-objects)
			  :publish-prefix *publish-prefix*
			  :page 1
			  :page-title "Working with Packages"
			  :getting-started-url (the getting-started-url)
			  :resources (list "packages.lisp"))
   
   (package-tips :type 'package-tips
		 :pass-down (page-objects)
		 :publish-prefix *publish-prefix*
		 :page 2
		 :page-title "Package Tips")
   
   (code-structuring :type 'code-structuring
		     :pass-down (page-objects)
		     :publish-prefix *publish-prefix*
		     :page 3
		     :page-title "Code Structuring")
   
   (loading-code :type 'loading-code
		 :pass-down (page-objects)
		 :publish-prefix *publish-prefix*
		 :page 4
		 :page-title "Loading Code with cl-lite"))

  )









```

---

## working-with-packages.lisp - header
Source: gornschool-training/t3/source/working-with-packages.lisp
Type: tutorial

```
(in-package :training-3)


```

---

## working-with-packages.lisp - working-with-packages
Source: gornschool-training/t3/source/working-with-packages.lisp
Type: tutorial

```
(define-object working-with-packages (base-training-sheet)
  :input-slots
  (getting-started-url)

  :computed-slots
  ((index-words (list "define-package" "in-package" ":use" ":export" "Fully qualified"))

   (code-1 (list "(in-package :gdl-user)"
		 ""
		 "
```

---

## working-with-packages.lisp - :my-app
Source: gornschool-training/t3/source/working-with-packages.lisp
Type: tutorial

```
(define-package :my-app)"))
   
   (code-2 (list "(in-package :gdl-user)"
		 ""
		 "
```

---

## working-with-packages.lisp - :my-app"
Source: gornschool-training/t3/source/working-with-packages.lisp
Type: tutorial

```
(define-package :my-app"
		 "    (:export #:my-slot))"))

   (code-3 (list "(in-package :functions)"
		 "(eval-when (load eval) (export 'average))"
		 "
```

---

## working-with-packages.lisp - average
Source: gornschool-training/t3/source/working-with-packages.lisp
Type: tutorial

```
(defun average (lis)"
		 "  (/ (apply '+ lis) (length lis)))"))
   
   (code-4 (list "(in-package :gdl-user)"
		 ""
		 "
```

---

## working-with-packages.lisp - :my-other-app"
Source: gornschool-training/t3/source/working-with-packages.lisp
Type: tutorial

```
(define-package :my-other-app"
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

   (repl-3 (list (list :command (list "
```

---

## working-with-packages.lisp - package-a"
Source: gornschool-training/t3/source/working-with-packages.lisp
Type: tutorial

```
(define-package package-a"
				      "   (:export x))")
		       :output "#<Package \"PACKAGE-A\">")
		 (list :command "
```

---

## working-with-packages.lisp - package-b
Source: gornschool-training/t3/source/working-with-packages.lisp
Type: tutorial

```
(define-package package-b)"
		       :output "#<Package \"PACKAGE-B\">")
		 (list :command (list "
```

---

## working-with-packages.lisp - package-c"
Source: gornschool-training/t3/source/working-with-packages.lisp
Type: tutorial

```
(define-package package-c"
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

			

			

```

---

## loading-code.lisp - header
Source: gornschool-training/t3/source/loading-code.lisp
Type: tutorial

```
(in-package :training-3)


```

---

## loading-code.lisp - loading-code
Source: gornschool-training/t3/source/loading-code.lisp
Type: tutorial

```
(define-object loading-code (base-training-sheet)
  :computed-slots
  ((code-1 (list "(cl-lite \"/codebase/airplane\")"))
   (code-2 (list "(\"common\" \"wing\" \"fuselage\" \"engine\" \"source\")"))
   (code-3 (list "(\"package\")"))
   (code-4 (list "(\"engine\")"))
   (body-content (with-cl-who-string ()
		   ((:div :class "main-page-container" :style "grid-template-columns: 600px auto;")
		    ((:div :class "main-page-item")
		     (:p "In previous topics, we have seen that there is some requirement to manage the load order. With small numbers of source files this is manageable manually, but once the number of source files exceeds maybe 5 or 6, this becomes an onerous and somewhat tedious task")
		     (:p "GendL has a utility to support this called "
			 (:span :class "function" "cl-lite")". It can be called from either the REPL or it can be initiated from the init file (gdlinit.cl). "
			 (:span :class "function" "cl-lite")" supports the following features"
			 (:ul (:li "It will load any .lisp file found in any "
				   (:em (:b "source"))" directory below the target directory. So in the example codebase on the right, if "
				   (:span :class "function" "cl-lite")" was applied to the "
				   (:em (:b "airplane"))" directory it would load .lisp files from "
				   (:em (:b "source"))", "
				   (:em (:b "common/source"))", "
				   (:em (:b "engine/source"))", "
				   (:em (:b "fuselage/source"))" and "
				   (:em (:b "wing/source")))
			      (:li "The order in which directories are loaded may be specified at any level using a "
				   (:b "system-ordering.isc")" file")
			      (:li "The order in which files in a source folder are loaded may be specified for that "
				   (:em (:b "source"))" directory in a "
				   (:b "file-ordering.isc")" file")
			      (:li "Directories or files to be ignored can be specified in an  "
				   (:b "ignore-list.isc")" file")))
		     (:p "The structure of the "
			 (:b "system-ordering.isc")", "
			 (:b "file-ordering.isc")" and "
			 (:b "ignore-list.isc")" is identical; it is a literal list of strings identifying target folders or files. Note that "
			 (:b "ignore-list.isc")" may apply to either directories or files whilst "
			 (:b "system-ordering.isc")" only applies to directories and "
			 (:b "file-ordering.isc")" only applies to files"))
		    ((:div :class "main-page-item")
		     ((:img :src (format nil "/~a-images/codebase.png" *publish-prefix*) :style "width: auto; height: 300px; margin: 0 0 0 0 ;")))
		    ((:div :class "main-page-item")
		     (:h3 "Executing cl-lite")
		     (:p (:span :class "function" "cl-lite")" takes a pathname argument. The pathname may be a string or a logical pathname")
		     (str (code-example (the code-1)))
		     (:p "If we wanted to enforce a "
			 (:em "load order")" for the directories we would use the "
			 (:b "system-ordering.isc file")". In this example, assuming multiple packages which mirror the directory structure, we would probably want to load "
			 (:em (:b "common"))" first, then "
			 (:em (:b "engine"))", "
			 (:em (:b "wing"))" and "
			 (:em (:b "fuselage"))" (since they would "
			 (:span :class "object-keyword" ":use")" the "
			 (:span :class "package-name" ":airplane-common")" package) and lastly the "
			 (:em (:b "source"))" directory since it would "
			 (:span :class "object-keyword" ":use")" all of the previously defined packages. So "
			 (:b "system-ordering.isc")" would look like this")
		     (str (code-example (the code-2)))
		     (:p "Assuming each of the folders has a package file, we would want to load that as the first file when the directory is loaded, so each "
			 (:em  (:b"source"))" folder would have a "
			 (:b "file-ordering.isc")" file like this")
		     (str (code-example (the code-3)))
		     (:p "2 things to note here"
			 (:ul (:li "There is no need to specify the file extension")
			      (:li "There is only a requirement to specify files which have special load order, any other files in the folder are loaded by default in alphabetical order")))
		     (:p "Finally, if for some reason we wished to suppress the load of the engine module, we would use the "
			 (:b "ignore-list.isc")" file in the "
			 (:em  (:b "airplane"))" directory as shown below")
		     (str (code-example (the code-4)))))))))

			

```

---

## code-structuring.lisp - header
Source: gornschool-training/t3/source/code-structuring.lisp
Type: tutorial

```
(in-package :training-3)


```

---

## code-structuring.lisp - code-structuring
Source: gornschool-training/t3/source/code-structuring.lisp
Type: tutorial

```
(define-object code-structuring (base-training-sheet)
  :computed-slots
  ((body-content (with-cl-who-string ()
		   ((:div :class "main-page-container" :style "grid-template-columns: 500px auto;")
		    ((:div :class "main-page-item")
		     (:p "These are just recomendations based on experience and convenience")
		     (:h3 "Source Code Files")
		     (:ul (:li "If the code is in a discrete package, ensure there is a separate package.lisp file to define the package")
			  (:li "Where possible or feasible one object/function per file. ("(:em "Whilst you can open the file containing an object or function code from in emacs by positioning the cursor in the object/function name and using "(:b "Meta-.")", the one object/function per file goal does make life easier)"))))
		    ((:div :class "main-page-item"))
		    ((:div :class "main-page-item")
		     (:h3 "Directory organisation")
		     (:ul (:li "Where possible and practical arrange the directory structure so that it mirrors the applications object structure")
			  (:li "Strike a balance between granularity - some logical separation is good, but avoid separating too much")
			  (:li "For each module create a "(:em (:b "source"))" directory for the source code files")
			  (:li "In larger projects define a "(:em (:b "common"))" folder for code that is shared between the other modules"))
		     (:p "In the example on the right, the airplane/source directory would be used for the source code assembling the different modules into an airplane. The common directory would be used for shared objects/functions and the main application code would be stored in the source directories of each module - engine, wing, fuselage. Some considertion ought to be given at this stage to the package design; a single :airplane package may be adequate or the packages could be broken down by module, each of the sub-modules using airplane-common and the top level airplane package using all of the sub-module packages.A bit of time spent at the outset of a project is usuually time well spent, although the result doesn't need to be perfect since it is easy enough to change as the project/application evolves")
		     (:p "The suggested codebase structure is also compatible with cl-lite, GendL's code loading utility which is discussed in the next topic"))
		    ((:div :class "main-page-item")
		     ((:img :src (format nil "/~a-images/codebase.png" *publish-prefix*) :style "width: auto; height: 300px; margin: 1em 0 1em 3% ;"))))))))
  

```

---

## package-tips.lisp - header
Source: gornschool-training/t3/source/package-tips.lisp
Type: tutorial

```
(in-package :training-3)


```

---

## package-tips.lisp - package-tips
Source: gornschool-training/t3/source/package-tips.lisp
Type: tutorial

```
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

```

---

