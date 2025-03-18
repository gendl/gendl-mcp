(in-package :training-2)

(define-object object-definition (base-training-sheet)
  
  :computed-slots
  (
   (index-words (list "define-object" "mixin" ":input-slots" ":computed-slots" ":objects" ":functions" "mixin precedence" "the"))

   (code-1 (list
	    "(define-object my-box-1a (box)"
	    " :input-slots"
	    " ((length 2)"
	    "  (width 3)"
	    "  (height 4)))"))
   (code-2 (list
	    "(define-object my-box-1b ()"
	    " :input-slots"
	    " ((length 2)"
	    "  (width 3)"
	    "  (height 4)))"
	    ""
	    "(define-object my-box-2 (my-box-1b box))"
	    " "
	    "(define-object my-box-3 (box my-box-1b))"
	    " "
	    ))
   (code-3 (list
	    "(define-object my-box-4 (box)"
	    " :input-slots"
	    " (length"
	    "  (width 4)"
	    "  (height 4)))"))
   (code-4 (list
	    "(define-object my-box-4 (box)"
	    " :input-slots"
	    " (length"
	    "  (width 4)"
	    "  (height 4)))"
	    " :computed-slots"
	    " ((density 7800)"
	    "  (mass (* (div (the volume) 1000000000) (the density)))))"))

   (code-5 (list
	    "(define-object assembly-1 (base-object)"
	    " :objects"
	    " ((my-box :type 'my-box-4"
	    "          :length 10)"
	    "  (my-sphere :type 'sphere"
	    "             :radius (the my-box width))))"))
   
   (repl-1 (list (list :command "(setq self (make-object 'box)"
		       :output "#<BOX #x210348242D>")
		 (list :command "(the length)"
		       :output 0)
		 (list :command "(the width)"
		       :output 0)
		 (list :command "(the height)"
		       :output 0)
		 (list :command "(the volume)"
		       :output 0)
		 (list :command "(setq self (make-object 'my-box-1a)"
		       :output "#<BOX #x210348242D>")
		 (list :command "(the length)"
		       :output 2)
		 (list :command "(the width)"
		       :output 3)
		 (list :command "(the height)"
		       :output 4)
		 (list :command "(the volume)"
		       :output 24)))
   (repl-2 (list 
	    (list :command "(setq self (make-object 'my-box-2)"
		  :output "#<MY-BOX-2 #x210346742D>")
	    (list :command "(the length)"
		  :output 2)
	    (list :command "(setq self (make-object 'my-box-3)"
		  :output "#<MY-BOX-3 #x210349442D>")
	    (list :command "(the length)"
		  :output 0)))

   (repl-3 (list 
	    (list :command "(setq self (make-object 'my-box-4))"
		  :output "#<MY-BOX-4 #x2103467C4D>")
	    (list :command "(the length)"
		  :output "Invoking restart: Return to SLIME's top level."
		  :error "; Evaluation aborted on #<SIMPLE-ERROR #x21056CE7DD>.")
	    (list :command "(the volume)"
		  :output "Invoking restart: Return to SLIME's top level."
		  :error "; Evaluation aborted on #<SIMPLE-ERROR #x21056B76DD>.")
	    (list :command "(setq self (make-object 'my-box-4 :length 3))"
		  :output "#<MY-BOX-4 #x21034F4C4D>")
	    (list :command "(the length)"
		  :output 3)
	    (list :command "(the volume)"
		  :output 48)))

   (repl-4 (list (list :command "(setq self (make-object 'my-box-4 :length 3 :width 10))"
		       :output "#<MY-BOX-4 #x21034F4C4D>")
		 (list :command "(the volume)"
		       :output 120)))
   
   (body-content (with-cl-who-string()
		   (:p "Recall on the slide about "
                       ((:a :href (the instantiate-repl url)) "Instantiating Objects in the REPL")
                       ", we defined our own object using the Gendl "
		       (:span :class "macro" "define-object") " macro similarly to the following:")
		   (str (code-example (the code-1)))
		   (:p "In this section we will look a bit deeper into " (:span :class "macro" "define-object") ".")
		   (:p "The syntax for " (:span :class "macro" "define-object") " is:")
		   (:p (str (code-example (list "(define-object definition-name ([mixins*]) [specifications*])"))))
                        
		   (:p "where:"
                       (:dl
                        (:dt "definition-name")
                        (:dd "is a symbol naming the object being defined.")
                        (:dt "mixins*")
                        (:dd "means zero or more symbols naming other object definitions,
from which this object type will inherit characteristics.")
                        (:dt "specifications*")
                        (:dd "comprise the body of the object definition and consist of a set
of keyword-value pairs which describe the characteristics and behaviors of any given instance of the object type.")))
	                                      
		   (:h2 "mixins")
		   (:p "In the above example, "
		       (:span :class "object" "my-box-1a") " specifies "
		       (:span :class "object" "box") " as a mixin. Because "
		       (:span :class "object" "box") " includes a built-in slot "
		       (:span :class "slot" "volume") " , "
		       (:span :class "object" "my-box-1a") " will also include a "
                       (:span :class "slot" "volume") " slot. The primitive "
		       (:span :class "object" "box") " also includes "
		       (:span :class "slot" "length") ", "
		       (:span :class "slot" "width") " and "
		       (:span :class "slot" "height")", but specifies their default values each to be "
		       (:span :class "value" "0") ". In the example above, because "
		       (:span :class "object" "my-box-1a") " also includes "
		       (:span :class "slot" "length") ", "
		       (:span :class "slot" "width") ", and "
		       (:span :class "slot" "height")", and assigns default values to them ("
                       (:span :class "value" "2") ", " (:span :class "value" "3") " and "
                       (:span :class "value" "4") ", respectively), 
these values override the default "
		       (:span :class "value" "0") " values in any instances we may make of "
                       (:span :class "object" "my-box-1a")
		       " as long as we do not feed in any other specific values at
instantiation-time.")

		   (str (repl-example (the repl-1)))
                        
		   (:p "If you specify multiple mixins, precedence on the slots is left to right (and depth-first).")

                   (str (code-example (the code-2)))
                   :br 
		   (str (repl-example (the repl-2)))
                        
		   (:h2 "specifications")
		   (:p "The specifications section is what really defines the object.
 This can be thought of as its computational DNA.")
		   (:p "Each section of the specification is identified by one of a few supported keyword symbols.
The most common ones are:"
		       (:ul :class "list-disc list-outside ml-3 mr-8 bg-zinc-100"
                            (:li (:span :class "object-keyword" ":input-slots"))
			    (:li (:span :class "object-keyword" ":computed-slots"))
			    (:li (:span :class "object-keyword" ":objects"))
			    (:li (:span :class "object-keyword" ":functions"))))
		   (:h3 (:span :class "object-keyword" ":input-slots"))
		   (:p  (:span :class "object-keyword" ":input-slots") " specify any required and/or optional
inputs to the object. Each input-slot may be "

                        (:dl (:dt "a symbol")
                             (:dd "In this case it is a required input for the object.")
                             (:dt "a symbol-value pair enclosed in parentheses")
                             (:dd "in this case the slot is provided with a default value which may be overridden by "
                                  (:em "passing in")
                                  " a different value, either from the parent object or from a toplevel call to the "
                                  (:span :class "function" "make-object") " function.")))
		   (:p "In the example below, "
		       (:span :class "slot" "length") " is required, but "
		       (:span :class "slot" "width") " and "
		       (:span :class "slot" "height") " each default to "
		       (:span :class "value" "4") ".")
                                        
		   (str (code-example (the code-3)))
                                        
		   (:p "If "
		       (:span :class "object" "my-box-4") " is instantiated, and "
		       (:span :class "slot" "length")" (or any attribute which depends on "
		       (:span :class "slot" "length") ") is evaluated, then an error 
will result, as "
		       (:span :class "slot" "length")
		       " does not have a value. In other words, to be of any use,
 this object must be instantiated with " (:span :class "slot" "length")" passed in explicitly
as an input.")
		   (str (repl-example (the repl-3)))
		   (:p (:span :class "object-keyword" ":input-slots")" which have default 
values may have those values over-ridden when the object is instantiated")
                                              
		   ;;(str (repl-example (the repl-4) :class "repl-example-wide"))

                   (str (repl-example (the repl-4) :class "repl-example"))
                                              
		   (:h3 (:span :class "object-keyword"  ":computed-slots"))
		   (:p  (:span :class "object-keyword" ":computed-slots") " can represent 
known values, intermediate results, or final outputs which may be computed by an object")
		   (:p  "They are defined as symbol-value pairs enclosed in parentheses. 
The value can be any Common Lisp value or expression.")
		   (:p  (:span :class "object-keyword" ":computed-slots")
			" can refer to the return values of other "
			(:span :class "object-keyword" ":computed-slots")
			" using the GendL macro " (:span :class "macro""the"))
		   (str (code-example (the code-4)))
		   (:p "In the example above, a computed-slot " (:span :class "slot" "density")
                       " has been created and set to " (:span :class "value" "7800")
                       " (the density of steel in kg/m" (:sup "3") "). A further slot has 
been created, " (:span :class "slot" "mass") ", which divides the "
		       (:span :class "slot" "volume") " by "
                       (:span :class "value" "1000000") " and then multiplies that 
result by the value of "
                       (:span :class "slot" "density"))
		   (:p (:em "A point to note here is the Gendl is dimensionless. 
It is the responsibility of the programmer to ensure units are correct when performing 
calculations. Implicit in the example here is that length, width and height are specified 
in mm and the resultant mass will be in kg"))
		   (:p "We will cover the use of functions to perform calculations in 
more detail later in this tutorial")
		   (:h3 (:span :class "object-keyword" ":objects"))
		   (:p "The " (:span :class "object-keyword" ":objects")
		       " section is where child objects are specified. 
This specification includes:"
		       (:dl (:dt "The object name")
                            (:dd "i.e. the name of the slot which will contain this child object instance")
                            (:dt "The object type")
                            (:dd "This is expected to correspond to an object definition name specified in another "
				 (:span :class "function" "define-object") ". This can be a literal (quoted) symbol or
an expression which yields a symbol when evaluated (that is, object type can be determined dynamically at runtime).")
                            (:dt "The object input values")
                            (:dd "these are keyword-tagged expressions which specify the values to be passed (when and if demanded)
into the child object instance. They keywords should match existing "
                                 (:span :class "slot" "input-slots")
                                 " in the child object's type, otherwise a compiler warning will be generated.")))
		   (:p "In the definition below, the parent object ("
		       (:span :class "object" "assembly-1")
		       ") has two child objects: one called "
		       (:span :class "object" "my-box")" based on "
		       (:span :class "object" "my-box-4") " with "
		       (:span :class "slot" "length") " set to "
		       (:span :class "value" "10") ", and another called "
		       (:span :class "object" "my-sphere") ", based on the GendL "
		       (:span :class "object" "sphere") " object, with its "
		       (:span :class "slot" "radius")" specified as being equal to the "
		       (:span :class "slot" "width")" of "
		       (:span :class "slot" "my-box"))
		   (:p "Note that when specifying the inputs to an object"
		       (:ul :class "list-disc list-outside ml-3 mr-8 bg-zinc-100"
                            (:li "the name must match one of the symbols in the object's "
				 (:span :class "object-keyword" ":input-slots") ".")
			    (:li "the name must be specified as a keyword (ie preceeded by a "
				 (:b (:code ":")) ") so an "
				 (:span :class "object-keyword" ":input-slot") " "
				 (:span :class "slot" "length") " is specified as "
				 (:b ":length") " when specifying the inputs.")))
		   (str (code-example (the code-5)))
		   (:p "Instantiating assembly-1 in Geysr and drawing the leaves will 
look like this")
		   (:img :src (format nil "/~a-images/assembly-1.png" (the publish-prefix)) :style "width: auto; height: 200px;" )
		   (:h3 (:span :class "object-keyword" ":functions"))
		   (:p "The "
		       (:span :class "object-keyword" ":functions") " section is where 
you put the names, argument lists and bodies for "
		       (:span :class "object-keyword" ":functions") " which can operate 
within the context of the Gendl object they are defined in. "
		       "They shouldn't be confused with Common Lisp functions which are 
defined with "
		       (:em (:b"defun"))", although the syntax is very similar. The biggest 
difference is that a Gendl "
		       (:span :class "object-keyword" ":function") " can access messages 
within the object it is defined in by using the Gendl "
		       (:span :class "macro" "the") " macro")
		   (:p "We will cover more on "
		       (:span :class "object-keyword" ":functions") " in a later part of this tutorial")
		       
			
                   ((:div :class "main-page-item")
		    (:h2 "Resources")
		    (str (the resource-links)))))))






