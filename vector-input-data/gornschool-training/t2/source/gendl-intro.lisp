(in-package :training-2)

(define-object gendl-intro (base-training-sheet)
  
  :computed-slots
  ((index-words (list "Emacs" "REPL" "YADD" "Geysr"))
  
   (body-content
    (with-cl-who-string()
      (:h3 "What is GendL?")
       "
GendL is a dynamic, declarative, object-oriented language environment
embedded in ANSI Common Lisp (CL).

It consists of a collection of predefined objects, code-expansion
macros, and functions which you, the GendL application developer, may
either use directly or extend, to solve problems of any level of
complexity decomposed into manageable units.

GendL includes geometric primitives (wireframe, surfaces and solids)
and has a built-in web server to facilitate cross-platform deployment.

Unlike many other programming languages which execute procedurally
line by line, GendL only evaluates expressions when they are demanded,
and the object, slot, and function specifications within an object
definition can be written in any order. Another useful and
distinguishing feature is GendL's built-in runtime dependency
tracking. This allows your applicaton code to make modifications to
the value of any slot at runtime, and the GendL runtime system will
immediately mark any downstream dependent slots as \"Unbound\",
forcing them to be re-computed with fresh data the next time they are
demanded."

      (:h3 "GendL development tools")
	    
      (:h4 "Emacs IDE")
      "

GendL is provided with a configuration for the Emacs Integrated
Development Environment (IDE) called \"Slime\", or the \"Superior Lisp
Interaction Mode for Emacs.\" Although any text editor can be used,
the Emacs/Slime combination provides a rich and evolving set of
features for developing GendL and CL code.

Emacs/Slime includes a Read-Eval-Print-Loop (REPL), which is an
interactive, editable command prompt connected to a live GendL
session (which itself is hosted in a running CL process). You will
find that the REPL proves useful for testing and debugging code as
you are writing it.
                          
Emacs also boasts a wide range of open-source plugins for
customization and automation, including plugins for the emerging AI
coding assistants such as Github Copilot.

"


    (:img :src (format nil "/~a-images/emacs.png" (the publish-prefix)) :style "width: auto; height: 200px;" )
      
    (:h4 "YADD")
      "
      Hyperlinked documentation for the objects, macros, functions, and
global parameters provided with GendL is available through any web
browser using YADD, a documentation generator built
using... GendL. Assuming you have a running session which is using
port 9000 for its webserver listener, then reference documentation for
the developer will be accessible at
[http://localhost:9000/yadd](http://localhost:9000/yadd).
		       
YADD may also be used to document your own user-defined objects,
macros, functions, and global parameters. The resulting documentation
available on the main YADD home page side by side with that for the
built-in operators.

In the picture on the right, you can see documentation links for
built-in GendL packages such as CL-LITE, GENDL, GEOM-BASE, GEYSR, GWL
and YADD, as well as user-defined packages such as PURSUIT-ANALYSIS,
WEB-PARTS, and WIND-TUNNEL.

From time to time, supplemental documentation may be published at [the
Genworks
Website](https://genworks.com/documentation){target=\"_new\"}."
                         
      (:img :src (format nil "/~a-images/yadd.png" (the publish-prefix)) :style "width: auto; height: 200px;" )
      (:div :class "grid-item"
	    (:h4 "Geysr")
            "
As code is developed it may be evaluated in the CL
Read-Eval-Print-Loop (REPL).

Additionally, GendL provides a model and object browser called
Geysr. This is a web-based UI which allows objects to be instantiated,
navigated and inspected. Assuming you have a running session which is
using port 9000 for its webserver listener, then Geysr will be
available
at [http://localhost:9000/geysr](http://localhost:9000/geysr).

Geysr is also integrated with the REPL. By invoking Geysr's \"Set
Self!\" action on any object in the object instance tree displayed
Geysr, that object will become bound to a global variable `self`,
allowing you to work with that object (including access to its
ancestor and descendant instances). As with YADD, Geysr is also built
using GendL itself."
                         
	    (:img :src (format nil "/~a-images/geysr.png" (the publish-prefix)) :style "width: auto; height: 200px;")
            (:div :class "main-page-item"
	          (:h2 "Resources")
	          (str (the resource-links))))))))

