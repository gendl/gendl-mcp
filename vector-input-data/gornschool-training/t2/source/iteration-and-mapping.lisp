(in-package :training-2)

(define-object iteration-and-mapping (base-training-sheet)

  :computed-slots
  ((index-words (list "dolist" "dotimes" "mapcar" "mapcan" "mapc" "lambda function" "anonymous function"))
   (repl-1 (list (list :command "(setq a (list 1 2 3 4 5 6))"
		       :output "1 2 3 4 5 6)")
		 (list :command (list "(setq b (let ((res nil))"
				      "                 (dolist (var a res)"
				      "                    (push (* var 5) res))))")
		       :output "(30 25 20 15 10 5)")
		 (list :command (list "(setq b (let ((res nil))"
				      "                 (dolist (var a (nreverse res))"
				      "                    (push (* var 5) res))))")
		       :output "(5 10 15 20 25 30)")))

   (repl-2 (list (list :command (list "(setq c (let ((len 0))"
				      "                  (dolist (var a)"
				      "                     (setf len (+ len 1)))"
				      "                   len))")
		       :output 6)
		 (list :command (list "(setq c (let ((len 0))"
				      "                  (dolist (var a)"
				      "                     (incf len)"
				      "                  len))")
		       :output 6)))

   (repl-3 (list (list :command "(setq a (list 1 2 3 4 5))"
		       :output "(1 2 3 4 5)")
		 (list :command "(setq b (list \"a\" \"b\" \"c\" \"d\" \"e\"))"
		       :output "(\"a\" \"b\" \"c\" \"d\" \"e\")")
		 (list :command (list "(let ((res nil))"
				      "       (dotimes (n (length a) (nreverse res))"
				      "         (push (nth n a) res)"
				      "         (push (nth n b) res)))")
		       :output "(1 \"a\" 2 \"b\" 3 \"c\" 4 \"d\" 5 \"e\")")

		 (list :command (list "(setq b (let ((res nil))"
				      "               (dotimes (n (length a) (nreverse res))"
				      "                 (push (* (nth n a) 5) res))))))")
		       :output "(5 10 15 20 25)")))
   
   (body-content (with-cl-who-string()
		   (:p "Common Lisp provides a number of iteration and mapping macros and functions which enable us to process the
contents of lists, the most commonly used being"
		       (:ul (:li "Iteration - the macros "
				 ((:span :class "macro")"dolist")" and "
				 ((:span :class "macro")"dotimes"))
			    (:li "Mapping - the functions "
				 ((:span :class "function")"mapcar")", "
				 ((:span :class "function")"mapcan")" and "
				 ((:span :class "function")"mapc")))
		       "Common Lisp also defines 2 powerful and more general iteration macros, "
		       ((:span :class "macro")"do/do*")" and "
		       ((:span :class "macro")"loop")", but they are outside the scope of this tutorial")
		   (:h3 "Iteration Macros")
		   (:ul ((:div :class "grid-container-2")
			 ((:div :class "main-page-item")
			  (:li ((:span :class "macro")"dolist")" - takes a list of a "
			       (:em "variable")" and an "
			       (:em "expression")" which returns a "
			       (:em "list")", followed by a "
			       (:em "body of expressions")". Optionally "
			       ((:span :class "macro")"dolist")
                               " can take a third element for its list, which is an expression which
will be evaluated and its value returned from the "
                               (:span :class "macro" "dolist")
                               " expression once the "
			       ((:span :class "macro") "dolist") " iteration has completed. The "
			       (:em "body of expressions") " is evaluated with the "
			       (:em "variable")" bound to successive elements in the list."
			       (:br) "The example on the right first sets a return variable  "
			       (:em "res") " to "
                               (:span :class "value" "nil") ". It then iterates through the list "
			       (:em "a")", multiplying each value by "
                               (:span :class "value" "5")
                               " and pushing the result onto "
			       (:em "res") ". Finally "
			       (:em "res") " is returned from the "
			       (:span :class "macro" "dolist") " and set to "
			       (:em "b")"."
			       (:br) "If you wanted to return the resultant list values in the same order as they
occur in the input list, you would need to wrap the dolist return value "
			       (:em "res")" in a call to the function "
			       ((:span :class "function")"reverse") ". But because "
			       (:em "res")" is locally bound and will never be used after "
			       (:em "b") " has been evaluated, you could use the destructive function "
			       ((:span :class "function") "nreverse") ", which is slightly more efficient than "
			       ((:span :class "function")"reverse" )))
			 ((:div :class "main-page-item")
			  (str (repl-example (the repl-1))))
			 ((:div :class "main-page-item")
			  (:p "It is also possible to use "
			      ((:span :class "macro") "dolist")" without any return value argument, in which case you'd be relying on "
			      (:em (:b "side-effecting") " within the body. The two examples shown are different ways of returning the length of a list")))
			 ((:div :class "main-page-item")
			  (str (repl-example (the repl-2))))
			 ((:div :class "main-page-item")
			  (:li (:span :class "macro" "dotimes") " - takes two required inputs, a "
			       (:em "variable")" and an "
			       (:em "integer")", followed by a "
			       (:em "body of expressions")". As with "
                               (:span :class "macro" "dolist") ", the list at the start of "
                               (:span :class "macro" "dotimes") "'s argument list can optionally "
			       ((:span :class "macro")"dotimes")" have a third element, an expression which will "
			       " be evaluated and the resulting value returned once the "
			       ((:span :class "macro")"dotimes")" has completed. The "
			       (:em "body of expressions")" are evaluated with the "
			       (:em "variable")" bound to successive integers between 0 and "(:em "integer") " minus 1"
			       (:p "In contrast to "
				   ((:span :class "macro")"dolist")" which is generally used to iterate through a single list, using "
				   ((:span :class "macro")"dotimes")" with the function "
				   ((:span :class "function") "nth")" permits iteration on multiple lists")
			       (:p "The first example shows "((:span :class "macro")"dotimes")" being used to splice together 2 lists. The second example is the first "
				   ((:span :class "macro")"dolist")" example converted to a "
				   ((:span :class "macro")"dotimes")" implementation")))
			 ((:div :class "main-page-item")
			  (str (repl-example (the repl-3))))))
		   (:h3 "Mapping Functions")
		   (:ul ((:div :class "grid-container-2")
			 ((:div :class "main-page-item")
			  (:li ((:span :class "function") "mapcar")" - one of the most heavily used mapping functions, it takes a "
			       (:em "function")" and one or more "
			       (:em "lists")" and calls the "
			       (:em "function")" on successive elements of the "
			       (:em "lists")))
			 ((:div :class "main-page-item")
			  (str (repl-example (the repl-4))))
			 ((:div :class "main-page-item")
			  (:p ((:span :class "function")"mapcar")" is very commonly used with a "
			      (:em (:b "lambda"))" or anonymous function. This effectively allows functions to be defined on the fly and they can be used
to map across multiple lists. The first example is an alternative to the "
			      ((:span :class "macro")"dotimes")" example for splicing 2 lists together. Successive elements from the 2 input lists are bound to the "
			      (:em (:b "lambda"))" vailables "
			      (:em "x")" and "
			      (:em "y")" and the return value from the "
			      (:em (:b "lambda"))" expression appended to the return list. The second example replaces the "
			      ((:span :class "function")"LIST-PRODUCT")" function with a "
			      (:em (:b "lambda")" function to achieve the same result")
			      (:p ((:span :class "function")"mapcar")" can often be used to accomplish the same thing as "
				  ((:span :class "macro")"dolist")" and may be less verbose. However, when the list processing becomes more complex "
				  ((:span :class "macro")"dolist")" may have some advantage in clarity and debugging")))
			 ((:div :class "main-page-item")
			  (str (repl-example (the repl-5))))
			 ((:div :class "main-page-item")
			  (:li ((:span :class "function")"mapcan")" - takes a "
			       (:em "function")" and one or more "
			       (:em"lists")" like "
			       ((:span :class "function")"mapcar")" but splices together the values returned by the " (:em"function")))
			 ((:div :class "main-page-item")
			  (str (repl-example (the repl-6))))
			 ((:div :class "main-page-item")
			  (:li ((:span :class "function")"mapc")" is like "
			       ((:span :class "function")"mapcar")" but it doesn't accumulate any data to return so the only reason to use it is for side-effecting. "
			       ((:span :class "function")"mapc")" always returns is second argument (the first list provided). When only side-effecting is required "
			       ((:span :class "function")"mapc")" may be a better option than "((:span :class "macro")"dolist")" because is can traverse multiple lists in parallel"))
			 ((:div :class "main-page-item")
			  (str (repl-example (the repl-7))))))))
			     

   (repl-4 (list (list :command (list "(defun plus2(i)"
				      "        (+ i 2))")
		       :output "PLUS2")
		 (list :command "(mapcar #'plus2 (list 1 2 3))"
		       :output "(3 4 5)")
		 (list :command (list "(defun list-product (a b)"
				      "        (* a b))")
		      :output "LIST-PRODUCT")
		 (list :command "(mapcar #'list-product (list 1 2 3) (list 4 5 6))"
		       :output "(4 10 18)")))
   
   (repl-5 (list (list :command (list "(flatten" 
				      "       (mapcar"
				      "         #'(lambda(x y) (list x y))"
				      "            (list 1 2 3 4 5)"
				      "            (list \"a\" \"b\" \"c\" \"d\" \"e\")))")
		       :output "(1 \"a\" 2 \"b\" 3 \"c\" 4 \"d\" 5 \"e\")")
		 (list :command "mapcar #'(lambda(x y) (* x y) (list 1 2 3) (list 4 5 6))"
		       :output "(4 10 18)")))

   (repl-6 (list (list :command "(mapcan #'list (list 1 2 3 4 5) (list \"a\" \"b\" \"c\" \"d\" \"e\"))"
		 :output "(1 \"a\" 2 \"b\" 3 \"c\" 4 \"d\" 5 \"e\")")))

   (repl-7 (list (list :command (list "(let ((x 0))"
				      "          (mapc #'(lambda(a) (setf x (+ x a))) (list 1 2 3))"
				      "       x)")
		       :output 6)
		 (list :command (list "(let ((x 0))"
				      "         (mapc #'(lambda(a b c) (setf x (+ x a b c)))" 
				      "             (list 1 2 3) "
				      "             (list 4 5 6)" 
				      "             (list 7 8 9))"
				      "       x)")
		       :output 45)))))
		      

