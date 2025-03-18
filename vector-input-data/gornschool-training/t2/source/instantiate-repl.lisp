(in-package :training-2)

(define-object instantiate-repl (base-training-sheet)
  :computed-slots
  ((index-words (list "make-object" "the-object" "theo" "setq" "self" ":input-slots" "define-object" "REPL" "compile code"))

   (repl-1 (list (list :command "(setq my-box (make-object 'box :length 2 :width 3 :height 4))"
		       :output "#<BOX #x210348222D>")
		 (list :command "(the-object my-box length)"
		       :output 2)
		 (list :command "(the-object my-box width)"
		       :output 3)
		 (list :command "(the-object my-box height)"
		       :output 4)))

   (repl-2 (list (list :command "(the-object my-box volume)"
		       :output 24)))

   (repl-3 (list (list :command "(theo my-box volume)"
		       :output 24)))
   
   (repl-4 (list (list :command "(setq self (make-object 'box :length 2 :width 3 :height 4))"
		       :output "#<BOX #x210348242D>")
		 (list :command "(the volume)"
		       :output 24)
                 (list :command "(the-object self volume)"
		       :output 24)
                 (list :command "(theo self volume)"
		       :output 24)))

   (repl-4b (list (list :command "(make-self 'box :length 1 :width 2 :height 3)"
		        :output "#<BOX #x210348242D>")
		  (list :command "(the volume)"
		        :output 6)
                  (list :command "(the-object self volume)"
		        :output 6)
                  (list :command "(theo self volume)"
		        :output 6)))

   (repl-5 (list (list :command "(setq self (make-object 'my-box-1))"
		       :output "#<BOX #x210344342D>")
		 (list :command "(the volume)"
		       :output 24)))

   (repl-6 (list (list :command "(setq self (make-object 'my-box-1 :length 8))"
		       :output "#<BOX #x210478242D>")
		 (list :command "(the volume)"
		       :output 96)))

   (setq-repl (list (list :command "(setq a 1)"
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

   (body-content (with-cl-who-string()
		   (:p "GendL includes a collection of object definitions which you can use
directly or customise/enhance to suit particular needs.")
		   (:p "To work with instances of these objects you may choose to "
                       (:em "instantiate")" an object in the REPL. For example, to instantiate a GendL object of type "
                       (:span :class "object" "box") " in the REPL, we call GendL's general-purpose "
                       (:span :class "function" "make-object")
                       " constructor function, which yields a freshly instantiated object which we assign to a symbol ("
                       (:span :class "variable-name"  "my-box") ") in the below example), using the Lisp special operator "
                       (:span :class "special-operator" "setq") ". The "
                       (:span :class "function" "make-object")
                       " function takes an object name as its required argument, followed by a
\"spliced-in\" list of alternating slot-names and corresponding
desired initial values. This type of list which is made up from
alternating key-value pairs is called a " (:i "plist") ". You will learn more
about plists later.
                            
In the example below we are instantiating a wireframe volumetric " (:span :class "object" "box") " and initializing its "
                       (:span :class "slot" "length")", "
                       (:span :class "slot" "width") " and "
                       (:span :class "slot" "height") " slots to "
                       (:span :class "value" "2") ", " (:span :class "value" "3") ", and "
                       (:span :class "value" "4") ", respectively.")
		   (:p "Using the macro "
		       (:span :class "macro" "the-object") 
                       ", you can then \"send a message\" to this object, which yields the current
value of the requested slot.")
		      
		   (str (repl-example (the repl-1)))
		   (:p "Note that " (:span :class "special-operator" "setq")
                       " is a standard CL special operator which takes one or more symbol-value
pairs, assigns the respective values to the symbols, and finally returns the last value.")
		   (str (repl-example (the setq-repl)))

		   (:p "As with most built-in primitives, The GendL " (:span :class "object" "box")
                       " supports a number of built-in messages, for example "
		       (:span :class "slot" "volume")
                       ". To send the " (:span :class "slot" "volume") " message to our "
                       (:span :class "variable-name" "my-box")
                       " instance and get its value returned, we will need to use a "
                       (:i "reference chain") ". A reference chain consists of a reference operator such as "
                       (:span :class "macro" "the-object") ", followed by an expression which is expected
to return an object instance (the example below this expression is simply that "
                       (:span :class "variable-name" "my-box") " symbol).")
                        
		   (str (repl-example (the repl-2)))

		   (:p "You can use the abbreviation "
		       ((:span :class "macro")"theo") " rather than "
		       ((:span :class "macro")"the-object") ". It does the same thing with less typing.")
		   (str (repl-example (the repl-3)))
		       
		   (:p "As an added convenience, you can assign the object to the special variable "
                       (:span :class "variable-name" "self")
                       " rather than to an arbitrarily-named variable. If you take this approach, then rather than using "
		       (:span :class "macro" "theo") " or "
		       (:span :class "macro" "the-object") " to refer to an object expression explicitly when
sending a message, you now have the option of using the GendL macro "
		       (:span :class "macro" "the") " to send a message to the object, and automatically "
                       (:span :class "variable-name" "self") " will be assumed as the object to receive the message. ")

                   (:p "In the following REPL interaction, the last three expressions are equivalent with each other:")

                   (str (repl-example (the repl-4)))

                        
		   (:p "While " (:span :class "variable-name" "self")
                       " is a normal symbol, it has a special meaning within the context of GendL.
It is fine to use "
                       (:span :class "variable-name") "self" " in the REPL, you should never bind or set/modify "
                       (:span :class "variable-name") "self" " within the body of a GendL object definition (created using the "
                       (:span :class "macro")"define-object" " macro)")


                   (:p "As yet an additional convenience, you can use the built-in GendL function "
                       (:span :class "function" "make-self")
                       " to instantiate an object and set it to the toplevel "
                       (:span :class "variable-name" "self")
                       ", all in one fell swoop:")

                   (str (repl-example (the repl-4b)))
                        
                        
		   (:p "If you'd like to create a named \"blueprint\"
of an object instance, e.g. for being able to instantiate customized
boxes without needing to feed in customized slot values, then you may
use GendL's "
                       (:span :class "macro" "define-object") " macro. In the example below we are
defining a new object "
                       (:span :class "object" "my-box")
                            
                       ",  which is effectively a customisation of the default Gendl "
		       (:span :class "object" "box") " object, by specifying "
		       (:span :class "object" "box") " as part of its "
                       (:i "mixin list") ". More about "
		       (:span :class "macro""define-object") " later.")
                        
		   (str (code-example (the code-1)))
                        
		   (:p "After this object definition has been compiled & loaded into the
running system (in Emacs/Slime this can be done with the cursor in the object and pressing C-c C-c),
then you can instantiate it in the REPL. Note that because the new object "
		       (:span :class "object" "my-box") " specifies default values for the "
		       (:span :class "object-keyword" ":input-slots")" , you do not need to feed them in when making instances of "
                       (:span :class "object" "my-box") " in the REPL")
		   (str (repl-example (the repl-5)))

		   (:p "However, you continue to have the ability to feed in alternative values of "
		       (:span :class "object-keyword" ":input-slots") " for "
		       (:span :class "object" "my-box"))
		   (str (repl-example (the repl-6)))
  
		   (str (the resource-links))))))
