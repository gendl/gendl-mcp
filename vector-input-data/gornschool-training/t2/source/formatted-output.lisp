(in-package :training-2)

(define-object formatted-output (base-training-sheet)
  :computed-slots
  ((index-words (list "~a" "~d" "~f" "~$" "~p" "~r" "~( ~)" "~{ ~}" "~[ ~]" "format" "format directive" "format directive modifier"
		      "format prefix parameter" "format conditional" "format iteration" "integer directive (format)"
		      "floating point directive (format)" "english language directive (format)" "case control (format)"
		      "iteration (format)" "conditional control (format)"))
   
   (repl-1 (list (list :command "(setq text \"Hello\")"
		       :output "\"Hello\"")
		 (list :command "(setq number 123.456)"
		       :output 123.456)
		 (list :command "(setq my-list (list 1 2 3))"
		       :output "(1 2 3)")
		 (list :command "(format nil \"~a\" text)"
		       :output "\"Hello\"")
		 (list :command "(format nil \"~a\" number)"
		        :output "\"123.456\"")
		 (list :command "(format nil \"~a\" my-list)"
		       :output "\"(1 2 3)\"")
		 (list :command "(format nil \"Line 1~%Line2\")"
		       :output (list "\"Line 1" "Line 2\""))
		 (list :command "(format nil \"Line 1~%Line2\")"
		       :output (list "\"Line 1" "" "Line 2\""))))
   
   (repl-2 (list (list :command "(format nil \"~d\" 1234567)"
		       :output "\"1234567\"")
		 (list :command "(format nil \"~:d\" 1234567)"
		       :output "\"1,234,567\"")
		 (list :command "(format nil \"~@d\" 1234567)"
		       :output "\"+1234567\"")
		 (list :command "(format nil \"~:@d\" 1234567)"
		       :output "\"+1,234,567\"")
		 (list :command "(format nil \"~12d\" 1234567)"
		       :output "\"     1234567\"")
		 (list :command "(format nil \"~12,'0d\" 1234567)"
		       :output "\"000001234567\"")
		 (list :command "(format nil \"~2,'0d-~2,'0d-~d\" 31 7 2022)"
		       :output "\"31-07-2022\"")))

   (repl-3 (list (list :command "(format nil \"~f\" pi)"
		       :output "\"3.141592653589793\"")
		 (list :command "(format nil \"~,3f\" pi)"
		       :output "3.142")
		 (list :command "(format nil \"~e\" pi)"
		       :output "\"3.141592653589793E+0\"")
		 (list :command "(format nil \"~$\" pi)"
		       :output "3.14")))

   (repl-4 (list (list :command "(format nil \"computer~p\" 1)"
		       :output "\"computer\"")
		 (list :command "(format nil \"~r computer~:p\" 1)"
		       :output "\"one computer\"")
		 (list :command "(format nil \"~r computer~:p\" 2)"
		       :output "\"two computers\"")
		 (list :command "(format nil \"~r fl~:@p\" 1)"
		       :output "\"one fly\"")
		 (list :command "(format nil \"~r fl~:@p\" 2)"
		       :output "\"two flies\"")))
   
   (repl-5 (list (list :command "(setq txt \"sOme RANdom TexT\")"
		       :output "\"sOme RANdom TexT\"")
		 (list :command "(format nil \"~(~a~)\" txt)"
		       :output "\"some random text\"")
		 (list :command "(format nil \"~@(~a~)\" txt)"
		       :output "\"Some random text\"")
		 (list :command "(format nil \"~:(~a~)\" txt)"
		       :output "\"Some Random Text\"")
		 (list :command "(format nil \"~:@(~a~)\" txt)"
		       :output "\"SOME RANDOM TEXT\"")))

   (repl-6 (list (list :command "(format nil \"~{~a, ~}\" (list 1 2 3))"
		       :output "\"1 2 3 \"")
		 (list :command "(format nil \"~{~a ~}\" (list 1 2 3))"
		       :output "\"1, 2, 3, \"")
		 (list :command "(format nil \"~{~a~^,~}\" (list 1 2 3))"
		       :output "\"1, 2, 3\"")
		 (list :command "(format nil \"~@{~a~^,~}\" 1 2 3)"
		       :output "\"1, 2, 3\"")))
   
   (repl-7 (list (list :command "(format nil \"~[Peter~;Paul~;John~]\" 0)"
		       :output "\"Peter\"")
		 (list :command "(format nil \"~[Peter~;Paul~;John~]\" 4)"
		       :output "\"\"")
		 (list :command "(format nil \"~[Peter~;Paul~;;John~]\" 4)"
		       :output "\"John\"")
		 (list :command "(format nil \"~@[~r cat~:p ~]~@[~r dog~:p~]\" nil 2)"
		       :output "\"two dogs\"")
		 (list :command "(format nil \"~@[~r cat~:p ~]~@[~r dog~:p~]\" 3 nil)"
		       :output "\"three cats \"")
		 (list :command "(format nil \"~@[~r cat~:p ~]~@[~r dog~:p~]\" 3 1)"
		       :output "\"three cats one dog\"")))
   
   (body-content (with-cl-who-string()
		   ((:div :class "main-page-container")
		    ((:div :class "main-page-item")
		     (:div :class "grid-container-2-650px"
			   (:div :class "grid-item"
		      
			         (:p "When we want more control, flexibility and sophistication with text output we use the "
			             ((:span :class "function")"format")" function")
			         (:p "below are some of the most commonly used and useful features of "((:span :class "function")"format")
                                     ", but it can sometimes be difficult to understand or read, due to its compactness and its 
non-Lispy syntax. A great resource on the Internet is an article by Peter Seibel, called a few FORMAT recipes
which provides a detailed discussion on the features and uses of "
                                     ((:span :class "function") "format")
                                     " and some very useful examples. See the link in the resources section")
			         (:p ((:span :class "function")"Format")" takes 2 required arguments"
			             (:ul (:li "A "(:em "destination")" for its output")
				          (:li "A "(:em "control string")" that will generally contain literal text, but will always contain format"
				               (:em (:b "directives")))) ". "
			             ((:span :class "function")"Format")" will also generally be provided with extra arguments which are used by the "
			             (:em (:i "directives")) " in the control string.")
		                 (:h3 "Output Destination")
		                 (:p "The "(:em (:b "output destination"))" is the first argument to format. There are 4 types of output destination, but we'll discuss just the first 3"
			             (:ul (:li "If the value is "
				               (:em (:span :class "general-keyword"  "T"))", the output destination is the "
				               (:span :class "variable-name"  "*standard-output*")" stream")
				          (:li "If the value is "
				               (:em (:span :class "value"  "NIL"))", the output is generated as a string and returned by "
				               ((:span :class "function") "format"))
				          (:li "If the destination is a stream, the output will be written to that stream")))
			         (:h3 "Control String")
			         (:p "The "(:em "control string")" may look complex because it is based on characters, not s-expressions,
and is optimised for compactness. In many ways it is another 
mini-programming language within Lisp. As well as containing literal text,
the control string contains format "
			             (:em (:b"directives")) ". "
			             (:ul (:li "All "
				               (:em (:b"directives"))" start with a tilde (~) character, and end with a single character.
This character can be either upper of lower case")
				          (:li "Some "
				               (:em (:b"directives"))" take "
				               (:em "prefix parameters")", between the tilde and the character. If more than one "
				               (:em "prefix parameter")" is used they are separated by commas (,). "
				               (:em "Prefix parameters")" give additional output control")
				          (:li "Some "
				               (:em (:b "directives") " use either the colon (:) or at-sign (@) "
					            (:em (:b "modifiers")) " which change the behavior of the "
					            (:em (:b"directive"))" in small ways"))
				          (:li "There are also some special "
				               (:em (:b"directives"))" which are used in pairs and can be wrapped around other "
				               (:em (:b"directives"))", e.g. to control capitalisation ( "
				               (:span :class "format-directive"  "~( ~)")" ), conditional formatting ("
				               (:span :class "format-directive"  "~[ ~]")") or iteration ("
				               (:span :class "format-directive"  "~{ ~}")")"))
			             "This list isn't exhaustive, but contains the most frequently used principles.")
			         (:h3 "Basic Formatting")
			
			         (:p "The most basic "
			             (:em (:b "directive"))" is "
			             ((:span :class "format-directive") "~a") ". It consumes one of the "
                                     ((:span :class "function") "format")
                                     " arguments and outputs it in human-readable form. Note that because we are using "
                                     (:span :class "value" "NIL") " as the "
			             (:em (:b "output destination"))", "
			             ((:span :class "function")"format") " is returning a string as return-value and not outputting anywhere.")
			         "Another basic "(:em (:b "directive"))" is "
			         (:span :class "format-directive" "~%")", which causes a newline to be emitted. It optionally takes a single "

			         (:em "prefix parameter")" that defines how many newlines to be emitted"
                         
			         (str (repl-example (the repl-1)))
			
			         (:h3 "Integer Directives")
			
			         (:p "Whist the "
			             (:em (:b"directive"))" "
			             ((:span :class "format-directive")"~a")" can be used to output numbers, the "
			             (:em (:b"directive"))" "
			             ((:span :class "format-directive")"~d")
			             " offers more control for outputting integers (d standing for decimal, or base 10). There are 2 "
			             (:em "modifiers")":"
			             (:ul (:li "A colon ("
                                               (:span :class "format-directive" ":")
                                               ") adds commas separating the number into groups of 3 integers")
				          (:li "An at-sign ("
                                               (:span :class "format-directive" "@")
                                               ") always prints a sign before the number")
				          (:li "These 2 modifiers may be combined"))
				     "It can also take 2 "
				     (:em "prefix parameters")":"
				     (:ul (:li "The first "
					       (:em "prefix parameter")" specifies the minimum width for the output")
					  (:li "The second "
					       (:em "prefix parameter")" specifies the padding character. By default this is a space. Padding characters must be quoted are always inserted before the number")))
			         (str (repl-example (the repl-2)))
			         (:h3 "Floating Point Directives")
			
			         (:p "The 2 principle "
			             (:em (:b"directives"))" handling floating point numbers are "
			             ((:span :class "format-directive")"~f")" and "
			             ((:span :class "format-directive")"~e")". The difference is "
			             ((:span :class "format-directive")"~f")" is alowed to use scientific notation if the number is large enough or small enough, whilst "
			             ((:span :class "format-directive")"~e")" will always emit the number argument in scientific notation. ")
			         (:p" There are a number of "
			            (:em "prefix parameters")", but the only one of real significance is the second, which specifies the  number of digits to output after the decimal point. Note that, if this "
			            (:em "prefix parameter")" is less than the number of decimal digits in the argument, the argument will be mathematically rounded.")
			         (:p "A third floating point number "
				     (:em (:b"directive"))" is "
				     ((:span :class "format-directive")"~$")" which is a monetary directive. It is basically equivalent to "
				     ((:span :class "format-directive")"~f")" with the second prefix parameter dafaulting to 2")
			         (str (repl-example (the repl-3)))

			         (:h3 "English Language Directives")
				
				 (:p "These directives are useful for converting number to english language, outputting plurals and performing case conversions"
				     (:ul (:li ((:span :class "format-directive")"~r") " prints numbers as english words. With the "
					       (:span :class "format-directive"  ":")" "
					       (:em "modifier")" it prints the number as an ordinal")
					  (:li ((:span :class "format-directive")"~p") " pluralises a word, emitting an "
					       (:em (:span :class "general-keyword"  "s"))" character when the argument is anything but 1. It is often used with the "
					       (:em (:span :class "format-directive"  ":"))" "
					       (:em "modifier")" which makes it reprocess the previous format argument. Using the "
					       (:em (:span :class "format-directive"  "@"))" "
					       (:em "modifier")" causes a "
					       (:em (:span :class "general-keyword"  "y"))" character to be emitted when the format argument is 1, or "

					       (:em (:span :class "general-keyword"  "ies"))" to be emitted for other values")))
				 (str (repl-example (the repl-4)))
				 

				 (:p "To control case we use the "
				     ((:span :class "format-directive")"~(")" directive paired with a "
				     ((:span :class "format-directive")"~)")" in conjunction with the 2 "
				     (:em "modifiers")" "
				     (:em (:span :class "format-directive"  ":"))" and "
				     (:em (:span :class "format-directive"  "@"))
				     (:ul (:li "Without either modifier the output is all lower case")
					  (:li "With the "
					       (:em (:span :class "format-directive"  "@"))" "
					       (:em "modifier")" the first word is the string between the "
					       ((:span :class "format-directive")"~(")" and "
					       ((:span :class "format-directive")"~)")" directives is capitalised")
					  (:li "With the "
					       (:em (:span :class "format-directive"  ":"))" "
					       (:em "modifier")" all words is the string between the "
					       ((:span :class "format-directive")"~(")" and "
					       ((:span :class "format-directive")"~)")" directives are capitalised")
					  (:li "With both "

					       (:em "modifiers")" the output is all upper case")))
				 (str (repl-example (the repl-5)))
			         (:h3 "Iteration")
			         (:p "The "
			             ((:span :class "format-directive")"~{")" directive paired with"
			             ((:span :class "format-directive")"~}")" makes "
			             ((:span :class "function")"format")" itterate over the elements of a list. The control string between these 2 "
			             (:em (:b "directives"))" will be repeatedly processed as long as there are elements left in the list.")
			         (:p "In many cases we may wish to add some text seperators, for example a comma between the elements in the list, but avoid a final seperator. To do this we use the "
			             ((:span :class "format-directive")"~^")" directive before the seperator. By using the "
			             (:em (:span :class "format-directive"  "~@"))" modifier, any remaining "

			             ((:span :class "function")"format")" arguments are treated as a list")
			         (str (repl-example (the repl-6)))
			
		                 (:h3 "Conditionals")
		                 (:p "The "
			             ((:span :class "format-directive")"~[")" directive paired with"
			             ((:span :class "format-directive")"~]")" provides a simple control construct. Inbetween these directive are a number of clauses seperated by "
			             (:em (:span :class "format-directive"  "~;"))" and the argument supplied represents the index number (0 based) of the clasue to be used. If the index number is bigger then the number of clauses nothing is printed, unless the last clause is seperated by "(:em (:span :class "format-directive"  "~:;"))" in which case this clause is used as the default if no match is found"
			             (:p "With the "
			                 (:em (:span :class "format-directive" "~@"))" modifier the control string between "
			                 ((:span :class "format-directive")"~[")" and "
			                 ((:span :class "format-directive")"~]")" is only emitted if the "
			                 ((:span :class "function")"format")" argument is non-NIL"))
			         (str (repl-example (the repl-7))))))
				 
		    ((:div :class "main-page-item")
		     (:h2 "Resources")
		     (str (the resource-links))))))))
  

