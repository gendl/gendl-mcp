(in-package :training-2)

(define-object functions-and-functions (base-training-sheet)
  :computed-slots
  ((index-words (list "Named Functions" ":functions" "GendL object functions"
                      "&optional" "&key" "&rest"))

   (repl-1 (list (list :command "(defun add2 (x) (+ x 2))"
		       :output "ADD2")
		 (list :command "(add2 4)"
		       :output 6)
		 (list :command (list "(defun kinetic-energy (mass velocity)"
				      "   (div (* mass velocity velocity) 2))")
		       :output "KINETIC-ENERGY")
		 (list :command "(kinetic-energy 5 12)"
		       :output "360.0")))

   (repl-2 (list (list  :command (list "(defun kinetic-energy (&optional (mass 5) (velocity 12)"
				       "   (div (* mass velocity velocity) 2))")
			:output "KINETIC-ENERGY")
		 (list :command "(kinetic-energy)"
		       :output "360.0")
		 (list :command "(kinetic-energy 10)"
		       :output "720.0")
		 (list :command "(kinetic-energy 10 24)"
		       :output "2880.0")))

   (repl-3 (list (list  :command (list "(defun kinetic-energy (&key (mass 5) (velocity 12)"
				       "   (div (* mass velocity velocity) 2))")
			:output "KINETIC-ENERGY")
		 (list :command "(kinetic-energy)"
		       :output "360.0")
		 (list :command "(kinetic-energy :mass 10)"
		       :output "720.0")
		 (list :command "(kinetic-energy :velocity 24)"
		       :output "1440.0")
		 (list :command "(kinetic-energy :velocity 24 :mass 10)"
		       :output "2880.0") ))

   (code-1 (list "(defun kinetic-energy (&key (mass 5) (velocity 12)"
		 "	 (div (* mass velocity velocity) 2)))"
		 ""
		 "(define-object function-example(base-object)"
		 ""
		 ":computed-slots"
		 "((mass 5)"
		 " (velocity 12)"
		 " (ke-1 (the kinetic-energy-1))"
		 " (ke-2 (the kinetic-energy-2))"
		 " (ke-3 (the (kinetic-energy-2 :mass 10)))"
		 " (ke-4 (the (kinetic-energy-2 :velocity 24)))"
		 " (ke-5 (the (kinetic-energy-2 :velocity 24 :mass 10)))"
		 ""
		 " (ke-6 (kinetic-energy :mass 10 :velocity 24))"
		 " )"
		 ""
		 ":functions"
		 "((kinetic-energy-1 () (div (* (the mass) (the velocity) (the velocity)) 2))"
		 ""
		 " (kinetic-energy-2 (&key (mass (the mass)) (velocity (the velocity)))"
		 "		      (div (* mass velocity velocity) 2))"
		 " )"
		 ")"))

   (repl-4 (list (list :command "(setq self (make-object 'function-example))"
		       :output "#<GDL-USER::FUNCTION-EXAMPLE #x210397F77D>")
		 (list :command "(the ke-1)"
		       :output "360.0")
		 (list :command "(the ke-2)"
		       :output "360.0")
		 (list :command "(the ke-3)"
		       :output "720.0")
		 (list :command "(the ke-4)"
		       :output "1440.0")
		 (list :command "(the ke-5)"
		       :output "2880.0")
		 (list :command "(the ke-6)"
		       :output "2880.0"))) 

   

   (body-content (with-cl-who-string()
		   (:div
                    :class "main-page-container"
		    (:div
                     :class "main-page-item"
                     (:h2 (str (the page-header)))
		     (:div
                      :class "grid-container-2-650px"
		      (:div
                       :class "grid-item"
		       (:p (wmd "Functions are Lisp objects which can be _called_, or invoked, to
accept some _inputs_ (or \"arguments\") and yield corresponding _outputs_ or \"return values.\"

Typically, functions will simply return new values without modifying
any of the inputs. If you program mostly with such a non-modifying
style, you are doing _Functional Programming."))
                       (:p "GendL offers two kinds of functions:"
			   (:ul (:li "Functions defined using the "
				     ((:span :class "macro")"defun")" macro, Sometimes called "
				     (:em (:b "Named Functions"))
                                     ", such functions become objects in memory which have a symbol for a name. Usually, variables mentioned within the body of such functions
are \"passed in\" to the function call as arguments, Variables with
Global and Dynamic Scope can also be accessed within function bodies,
but we will cover those types of variables later.")
				(:li "Functions defined within the "
				     ((:span :class "object-keyword")":functions")
                                     " section of a GendL object definition. As with toplevel "
                                     (:span :class "macro" "defun")
                                     " functions, variables may be passed into "
                                     (:span :class "object-keyword" ":functions")
                                     " as arguments and the function bodies may access
global data. But additionally, "
                                     (:span :class "object-keyword" ":functions")

                                     " can also access any other "(:em (:b "messages"))" defined within
the same GendL object type where they occur. " (:em (:b "Messages"))" can be any of "
                                     (:ul
                                      (:li (:span :class "object-keyword" ":input-slots"))
                                      (:li (:span :class "object-keyword" ":computed-slots"))
                                      (:li (:span :class "object-keyword" ":objects")))
                                     "... and other "
                                     (:span :class "object-keyword" ":functions")
                                     " also count as messages!

When you want to access the value of other mesages within the same object instance, you can use the "
                                     (:span :class "macro" "the") (:em " referencing macro")".")))
		       (:h3 "Named functions")
		       (:p "We have already encountered a number of Named Functions, for example"
			   (:ul (:li (:span :class "function" "+")" which returns the sum of
the numbers supplied as arguments")
				(:li (:span :class "function" "length")" which returns
the length of a list supplied as an argument")))
                       (:p "Named functions defined using the "
                           (:span :class "macro" "defun")
                           " macro use "
			   (:em (:b "Prefix Notation")) ". In other words, the function
name is specified first, followed by its arguments. Named functions can accept "
			   (:ul (:li "Specific standard arguments, which immediately follow
the function name")
				(:li "Specific optional arguments, identified using "
				     (:em (:b "&optional")) " in the argument list")
				(:li "Specific keyword arguments, identified using "
				     (:em (:b "&key")) " in the argument list")
				(:li "Any number of remaining arguments, identified using "
				     (:em (:b "&rest")) " in the argument list"))
			   "We'll discuss standard, "
			   (:em (:b "&optional")) " and "
			   (:em (:b "&keyword")) " arguments in this tutorial and come back to "
			   (:em (:b "&rest arguments")) " in a later tutorial")
		       (:h4 "Standard arguments")
		       (:p "Here we define two functions with one and two arguments respectively.
In the second function, the ordering of the arguments is what determines which input value ends
up in which variable in the body of the function.")
		       (str (repl-example (the repl-1)))
		       (:h4 "Optional arguments")
		       (:p "The syntax for optional arguments is"
			   (:ul (:li (:code "defun function-name (&optional (arg1-name arg1-default) (arg2-name arg2-default))"))
				(:li "In principle there is no limit to the number of optional arguments you can specify")
				(:li "Optional arguments are order dependant as with Standard ones, so if "
                                     (:span :class "general-keyword" "arg2")
                                     " is to be specified, "
                                     (:span :class "general-keyword" "arg1")
                                     " must be specified as well.")))
		       (str (repl-example (the repl-2)))
		       (:p "The first call to "

			   ((:span :class "function")"kinetic-energy")
                           " passes in no values for the arguments and so it ends up using both default optional values.
The second call uses a supplied value for "
			   ((:span :class "slot") "mass")" but the default value for "
			   ((:span :class "slot") "velocity")". And the third call to "
			   ((:span :class "function") "kinetic-energy")" uses supplied values for "
			   ((:span :class "slot") "mass")" and "
			   ((:span :class "slot")"velocity")
                           ". Note that when defined in this way, in order to used a specified "
			   ((:span :class "slot") "velocity") ", then "
			   ((:span :class "slot") "mass")" must be supplied, even if given simply 
as its default value")

			    
		       (:h4 "Keyword arguments")	      
		       (:p "The syntax for keyword arguments is"
			   (:ul (:li (:code "defun function-name (&key (arg1-key arg1-default) (arg2-key arg2-deefault))"))
				(:li "As with Optional arguments, there is no limit to the number of
keyword arguments you can specify")
                                (:li "Contrasted with Standard and Optional arguments, you can call a function with
Keyword arguments with those argument values specified in any order (because each one is identified explicitly
by a keyword symbol in the definition of the function. This is a significant advantage over Optional and Standard
arguments, especially when you want to extend a function to accept new arguments without breaking existing code
which is calling it.")
			        (:li "As with Optional arguments, you only need to specify the keyword arguments you want to use
when calling the function, and any arguments which you do not include in the call will take on their default values when
the body of the function is being evaluated.")))
		       (str (repl-example (the repl-3)))
		       (:p "Above we can see how the order-dependency is removed")

		       (:p "Some general recomendations and observations:"
			   (:ul (:li "If there is only one argument then it's reasonable to use a Standard argument")
				(:li "Optional arguments will be easy to work with if there is only one of
them, a bit less easy if there are two, and will start becoming
unwieldy to work with if you use more than 2 Optional
arguments.

Additionally, thought needs to be given to the order in
which they are defined to avoid having to specify optional arguments
just to occupy a space in the argument list (e.g. if you have two optional arguments,
list first the one which is more likely to be passed in as a non-default value in calls to that function.")
				(:li "Keyword arguments work well when the function has more than 1 argument, and help with readability")
				(:li "Optional and Keyword arguments are relatively 'expensive' at runtime
compared with Standard arguments, so particularly with Keyword
arguments you may be trading Convenience for Speed in some cases (such cases can be shaken out
with Runtime Profiling)")))
                        
		       (:h3 "GendL object "((:span :class "object-keyword")":functions"))
		       (:p "A GendL object function is defined in the "
			   ((:span :class "object-keyword")":functions")" section of an object definition made with the "
                           (:span :class "macro" "define-object")
                           " macro. The function only \"exists\" in the context of the object
definition itself, but it can reference all of the slots within the
object directly, rather than you having to pass in values as
arguments. But of course it can also accept passed-in arguments. The
argument syntax is identical to that for Named Functions (this style
of argument list is called a \"lambda list\". The Named
Function "
			   ((:span :class "function")"kinetic-energy")" shown above,
when defined as a GendL object function, may look like this:")))
		     ((:div :class "main-page-container")
		      ((:div :class "main-page-item")

		       (str (code-example (the code-1))))
		      (:div :class "main-page-item"
			    (str (repl-example (the repl-4)))))
		     (:div :class "grid-container-2-650px"
			   (:div :class "grid-item"
			         (:p "Some points to note from the above example"
			             (:ul (:li "The "
				               (:span :class "object-keyword" ":function")" "
				               (:span :class "function" "kinetic-energy-1")" has no arguments but it references the "
				               (:span :class "object-keyword" ":computed-slots")" "
				               (:span :class "slot" "mass")" and "
				               (:span :class "slot" "velocity")" directly using the "
				               (:span :class "macro" "the")" macro")
				          (:li "The "

				               (:span :class "object-keyword" ":function") " "
				               (:span :class "function" "kinetic-energy-2")
                                               " defines keyword arguments for "
				               (:span :class "slot" "mass")" and "
				               (:span :class "slot" "veocity")
                                               " and sets their default values to the values of the "
				               (:span :class "object-keyword" ":computed-slots")" "
				               (:span :class "slot" "mass")" and "
				               (:span :class "slot" "velocity") ".")
				          (:li "When a "
				               (:span :class "object-keyword" ":function")" is called without arguments, the "
				               (:span :class "macro" "the")" referencing macro is still used but the "
				               (:span :class "object-keyword" ":function")" does not need enclosing in parentheses (so
such a reference would look just like a reference to a "

                                               (:span :class "object-keyword" ":computed-slot") " (see "
                                               (:span :class "object-keyword" ":computed-slots") " above)")

				          (:li "When a "
                                               (:span :class "object-keyword" ":function")" is called with arguments, whilst the "
				               (:span :class "macro" "the")"
referencing macro is again used, you must wrap the function name and
arguments with parentheses, similar to calling a normal Named function
with the only difference being an outer wrapping referencing macro e.g. "
                                               (:code "(the ... )")
                                               " (see the "
				               (:span :class "object-keyword"":computed-slots")" "
				               (:span :class "slot" "ke-3")", "
				               (:span :class "slot" "ke-4")" and "
				               (:span :class "slot" "ke-5"))
				          (:li "Compare and contrast these "
				               (:span :class "object-keyword" ":computed-slots")" with the way in which the "
				               (:span :class "function" "kinetic-energy")" Named Function is used for "
				               (:span :class "slot" "ke-6")))))))
			
		    (:div :class "main-page-item"
			  (:h2 "Resources")
			  (str (the resource-links))))))))



