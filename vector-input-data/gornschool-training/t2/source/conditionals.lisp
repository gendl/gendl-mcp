(in-package :training-2)

(define-object conditionals (base-training-sheet)
  :computed-slots
  ((index-words (list "if" "when" "case" "cond" "unless"))

   (repl-1 (list (list :command "(if (> 1 0) (+ 3 4))"
		       :output 7)
		 (list :command "(if (< 1 0) (+ 3 4))"
		       :output "NIL")
		 (list :command "(if (< 1 0) (+ 3 4)(+ 5 6))"
		       :output 7)))
   (repl-2 (list (list :command "(defparameter *evaluated* NIL)"
		       :output "*EVALUATED*")
		 (list :command (list "(if (> 1 0)"
				      "    (progn (+ 1 2)"
				      "           (setf *evaluated* (not *evaluated*))"
				      "           (+ 4 5))"
				      "    (+ 3 2))")
		       :output 9)
		 (list :command "*evaluated*"
		       :output "T")
		(list :command (list "(if (> 1 0)"
				      "    (prog1 (+ 1 2)"
				      "           (setf *evaluated*  (not *evaluated*))"
				      "           (+ 4 5))"
				      "    (+ 3 2))")
		      :output 3)
		 (list :command "*evaluated*"
		       :output "NIL")))
   
   (repl-3 (list (list :command "(defparameter *evaluated* NIL)"
		       :output "*EVALUATED*")
		 (list :command (list "(when (> 1 0)"
				      "  (+ 1 2)"
				      "  (setf *evaluated* (not *evaluated*))"
				      "  (+ 4 5))")
		       :output 9)
		 (list :command "*evaluated*"
		       :output "T")
		 (list :command (list "(when (< 1 0)"
				      "  (+ 1 2)"
				      "  (setf *evaluated* (not *evaluated*))"
				      "  (+ 4 5))")
		       :output nil)))
   (repl-4 (list (list :command (list "(cond ((> 1 2) (+ 3 4))"
				      "((string= \"peter\" \"paul\") (+ 5 6))"
				      "((equalp (list 1 2) (list 1 2)) (setq a (+ 7 8))"
				      "                                (oddp a))"
				      "(T 0))")
		       :output T)))

   (repl-5 (list (list :command (list "(defun month-days (month &key (leap-year nil))"
				      "(case month"
				      " ((1 3 5 7 8 10 12) 31)"
				      " ((4 6 9 11) 30)"
				      " (2 (if leap-year 29 28))"
				      " (otherwise \"month must be between 1 and 12\"))) ")
		       :output "MONTH-DAYS")
		 (list :command	"(month-days 3)"
		       :output 31)
		 (list :command "(month-days 2 :leap-year t)"
		       :output 29)
		 (list :command "(month-days 13)"
		       :output "month must be between 1 and 12")))

   (repl-6 (list (list :command "(setq self (make-object 'assembly-8))"
		 :output "#<ASSEMBLY-8 #x2103F266ED>")
	   (list :command "(the box-centers)"
		 :output "(#(0.0 0.0 0.0) #(6.0 0.0 0.0) #(12.0 0.0 0.0))")
	   (list :command "(setq self (make-object 'assembly-8 :box-lengths (list 3 8)))"
		 :output "#<ASSEMBLY-8 #x2103F25B2D>")
	   (list :command "(the box-centers)"
		 :output "(#(0.0 0.0 0.0) #(6.0 0.0 0.0))")))

   (code-1 (list "(define-object assembly-8 (base-object)"
		 ":input-slots"
		 "((box-lengths (list 2 5 8 12)))"
		 ":computed-slots"
		 "((number-of-boxes (if (> (length (the box-lengths)) 3)"
		 "			3"
		 "			(length (the box-lengths))))"
		 ""
		 " (box-centers (case (the number-of-boxes)"
		 "		  (1 (list (make-point 0 0 0)))"
		 "		  (2 (list (make-point 0 0 0)"
		 "			   (make-point 6 0 0)))"
		 "		  (3 (list (make-point 0 0 0)"
		 "			    (make-point 6 0 0)"
		 "			    (make-point 12 0 0)))))"
		 "   "
		 " (box-volumes (list-elements (the my-box) (the-element volume)))"
		 " (box-1-volume (nth 0 (the box-volumes))))"
		 ""
		 ":objects"
		 "((my-box :type 'box"
		 "	   :sequence (:size (the number-of-boxes))"
		 "	   :length (nth (the-child index) (the box-lengths))"
		 "	   :width 2"
		 "	   :height 1"
		 "	   :center (nth (the-child index) (the box-centers)))))"))

   (body-content
    (with-cl-who-string()
      (:p "We use conditionals mainly to implement logic in our computer programs. Common Lisp provides the following conditionals"
	  (:ul (:li (:span :class "special-operator" "if"))
	       (:li (:span :class "macro" "when"))
	       (:li (:span :class "macro" "unless"))
	       (:li (:span :class "macro" "cond"))
	       (:li (:span :class "macro" "case"))))
      (:h3 (:span :class "special-operator" "if"))
      (:p (:span :class "special-operator" "if")" is a special operator.")
      (:p "The syntax is " (:span :class "general-keyword"  "(if test then [else])")
	  ", where"
	  (:ul (:li (:span :class "general-keyword" "test")
		    " is an expression that will evaluate to T or NIL")
	       (:li (:span :class "general-keyword" "then")
		    " is an expression that is evaluated and its value returned if "(:em "test") " is T")
	       (:li (:span :class "general-keyword" "else")
		    " is an optional argument which is an expression to be evalued and its value returned if "
		    (:span :class "general-keyword" "test") " is "
		    (:span :class "value" "NIL") ".")
	       (:li "if "(:span :class "general-keyword" "else")" is not provided, but "
		    (:span :class "general-keyword" "test")
		    " returns "
		    (:span :class "value" "NIL") ", then "
		    (:span :class "special-operator" "if") " also returns "
		    (:span :class "value" "NIL"))))
      (str (repl-example (the repl-1)))

      (:p (:span :class "special-operator" "if") " only allows single expressions for the "
	  (:span :class "general-keyword" "then") " and "
	  (:span :class "general-keyword" "else") " arguments; if multiple expressions needs to be evaluated then the must be wrapped in a "
	  (:span :class "special-operator" "progn") " (which returns the last evaluated value in its body) or "
	  (:span :class "macro"  "prog1") " (which evaluates all expressions in its body in order and finally returns the value of the first). "
	  "Not that it matters much to the routine user, but note that "
	  (:span :class "special-operator" "progn") " is a special operator, whilst "
	  (:span :class "macro" "prog1") " is a macro (which you might imaging utilizes the former in its implementation).")
      (str (repl-example (the repl-2)))
      (:h3 (:span :class "macro" "when"))
      (:p (:span :class "macro" "when")" is a macro.")
      (:p "The syntax is " (:span :class "general-keyword" "(when test body)") ", where "
	  (:ul (:li (:span :class "general-keyword" "test") "is an expression that will evaluate to "
		    (:span :class "value" "T") " or " (:span :class "value" "NIL") ".")
	       (:li (:span :class "general-keyword" "body")
		    " is one or more expressions, an implicit "
		    (:span :class "special-operator" "progn")
		    " such that each expression will be evaluated in order, and finally the value of the last one returned.")
	       (:li "if " (:span :class "general-keyword" "test") " returns "
		    (:span :class "value" "NIL") ", then the "
		    (:span :class "macro" "when") " expression likewise returns "
		    (:span :class "value" "NIL"))))
      (str (repl-example (the repl-3)))
      (:h3 (:span :class "macro" "unless"))
      (:p (:span :class "macro" "unless")" is a macro and acts as the opposite of "
	  (:span :class "macro" "when")". It takes the same arguments, but "
	  (:span :class "general-keyword" "body")" is only evaluated when "
	  (:span :class "general-keyword" "test")" returns "
	  (:span :class "value" "NIL") ")")
      (:h3 (:span :class "macro" "cond"))
      (:pr  (:span :class "macro" "cond")
	    " is a macro which allows multiple conditions to be tested.
The code associated with each condition is wrapped in an implicit
progn allowing multiple expressions to be evaluated per
condition. Each "
	    (:span :class "general-keyword" "test") " and "
	    (:span :class "general-keyword"  "body") " of expressions is specified as a list and the "
	    (:span :class "general-keyword"  "body") " of expressions associated with the first "
	    (:span :class "general-keyword"  "test") " to evaluate to "
	    (:span :class "value" "T") " is evaluated and the value of the last expression in that "
	    (:span :class "general-keyword"  "body") " returned. Once a "
	    (:span :class "general-keyword"  "test") " has evaluated to "
	    (:span :class "value" "T") ", no further "
	    (:span :class "general-keyword"  "tests") " are evaluated.")
      (str (repl-example (the repl-4)))
      (:p "In the example above, the final "
	  (:span :class "general-keyword"  "test")" is always T so this is effectively the default if none of the prior "
	  (:span :class "general-keyword"  "test")" expressions return T")
      (:h3 (:span :class "macro" "case"))
      (:p (:span :class "macro" "case")" is a macro which takes an "
	  (:em "object")" as a first argument, followd by lists of "
	  (:em "key")"-"(:em "expression")" pairs. The "
	  (:em "key")-(:em "expression")" pairs are then tested in order, and where the "
	  (:em "object")" is "
	  (:span :class "function" "eql") " to the "
	  (:em "key")", or a member of the "
	  (:em "key")", then the corresponding "
	  (:em "expression")" is evaluated and returned. Optionally "
	  (:span :class "general-keyword" "T") " or "
	  (:span :class "general-keyword" "otherwise") " may be specified as a catchall "
	  (:em "key")" at the end of the "
	  (:span :class "macro" "case") " expression, meaning that its corresponsing "
	  (:em "expression")" will be evauated and returned if and only if "
	  (:em "object")" is not "
	  (:span :class "function" "eql") " to, or a member of, any of the "
	  (:em "keys") ". Without such a " (:span :class "general-keyword" "otherwise") " or "
	  (:span :class "general-keyword" "T") ", "
	  (:span :class "value" "NIL") " would be returned in such a non-matching situation.")
      (str (repl-example (the repl-5)))
      (:p)
      (:p "We could extend "
	  (:span :class "object" "assembly-8")
	  ", used in the Lists tutorial as follows to make it a bit more flexible, ensuring that the number of box centers defined will always match the number of boxes to be created")
      (str (code-example (the code-1)))
      (str (repl-example (the repl-6)))

      (:h2 "Resources")
      (str (the resource-links))))))

