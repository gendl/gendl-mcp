(in-package :training-2)

(define-object tutorial-intro (base-training-sheet)
  :input-slots
  (index-url)
  
  :computed-slots
  ((setq-repl (list (list :command "(setq a 1)"
			  :output 1)
		    (list :command "(setq b 2 c \"three\" d nil)"
			  :output "NIL")
		    (list :command "b"
			  :output 2)
		    (list :command "c"
			  :output "three")
		    (list :command "d"
			  :output "NIL")))

   (code-1 (list
	    "(define-object my-box-1 (box)"
	    " :input-slots"
	    " ((length 2)"
	    "  (width 3)"
	    "  (height 4)))"))


   (body-content (with-cl-who-string ()
		
                   (wmd "This tutorial aims to get you started with GendL.
It assumes a standard GendL installation and setup which provides
Emacs and will start the GendL web server on localhost port 9000.

The tutorial makes use of GendL's built-in wireframe geometry (the
`geom-base` package) to demonstrate some of the basic concepts of
GendL. It also covers most of the Common Lisp (CL) basics that you
will need to master in order to work with GendL effectively. What you
will _not_ need is to become a CL expert or specialist in order to
excel as a GendL user, as GendL does much of the language \"heavy
lifting\" for you.

Most of the topics covered in this tutorial have either `.lisp` or PDF
files which you can download and/or use. These are presented in the
References section on the right-hand sidebar of each page. Any `.lisp`
files with valid ANSI Common Lisp (CL) code sould be compileable in GendL")


                   (:p "This tutorial uses the following color-coding and font convention for
identifying categories of Lisp symbols when they occur in the narrative portion of the text (don't worry if
you don't understand the meaning of all these terms yet):"
                       (:ul (:li (:span :class "function" "Function") " - a CL or GendL Function name")
			    (:li (:span :class "macro" "Macro") " - a CL or GendL Macro name")
                            (:li (:span :class "object" "Object Definition name")
                                 " - a symbol being used to name a Gendl object definition")
			    (:li (:span :class "object-keyword" "Object Definition Keyword")
                                 " - one of the valid Keywords which can be used in any GendL object definitions")
			    (:li (:span :class "slot" "Slot Name")
                                 " - a name of a slot specified in a particular GendL object definition")
			    (:li (:span :class "special-operator" "Special Operator") " - CL special operators")
                            (:li (:span :class "variable-name" "Variable Name") " - symbol being used as a variable name")
                            (:li (:span :class "package-name" "Package Name") " - symbol being used to name a CL Package")
                            (:li (:span :class "general-keyword" "General Keyword") " - keyword symbol used for its name only")
                            (:li (:span :class "value" "Value")
                                 " Any Lisp data value, e.g. a Number, String, List, or Gendl Object
instance. Such values are typically obtained by calling a function or
sending a message to an object.")))

                        
		   (:p "Examples in the CL Read-Eval-Print-Loop are shown in sections of the screen as shown below")
		      
		       
		   (str (repl-example (the setq-repl)))
		      
		   (:p "Code examples are shown in sections of the screen as shown below and will generally be downloadable from the References, as shown on this page")

		   (str (code-example (the code-1)))
		      
		   (:p "Each topic page has Previous and Next navigation links as well as a link back to the Home page which contains a list of all topics in this tutorial. Note that most pages topics build on what was covered in previous topics so its recomended to work through the pages sequentially")
		   (:p "Finally an index is provided with links to indexed words reference in each topic. The index may be accessed from the home page. To view the index now click "
		       ((:a :href (the index-url)) "here"))
                   
		 (:div :class "main-page-item"
		       (:h2 "Resources")
		       (str (the resource-links)))))))

  
