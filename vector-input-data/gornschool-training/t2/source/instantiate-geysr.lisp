(in-package :training-2)

(define-object instantiate-geysr (base-training-sheet)
  
  :computed-slots
  ((body-content (with-cl-who-string()
		   (:p "As a companion to the REPL interaction described on the previous slide, you can use the built-in
Geysr object browser.")
		   (:p "Geysr is particularly helpful when working with geometry,
as it includes a viewport for rendering the geometry.")
		   (:p "Assuming the Gendl web server is running on port 9000, Geysr may be accessed at "
                       ((:a :href "http://localhost:9000/geysr") "http://localhost:9000/geysr") ".")
		   ((:div :class "grid-container-2")
		    ((:div :class "grid-item")
		     (:p "When Geysr is opened, a splash screen is displayed:"))
		    ((:div :class "grid-item")
		     (:img :src (format nil "/~a-images/geysr-splash.png" (the publish-prefix)) :style "width: auto; height: 200px;" ))
		    ((:div :class "grid-item")
		     (:p "To instantiate a compiled object, select:")
		     (:p (:b "File..New") ", and in the text field enter the desired package name and the object
type name separated by " (:b "::"))
                     (:p "For the example code in the Resources section, this will be " (:code "gdl-user::my-box-1") ".")
		     (:p "Press " (:b "Enter") " to instantiate the object."))
		    ((:div :class "grid-item")
		     (:img :src (format nil "/~a-images/geysr-file-new.png" (the publish-prefix)) :style "width: auto; height: 100px;" ) (:br)
		     (:img :src (format nil "/~a-images/geysr-package-object.png" (the publish-prefix)) :style "width: auto; height: 100px;" ))
		    ((:div :class "grid-item")
		     (:p "The object will be instantiated, and all of its available slots will
be displayed in the "
                         (:i "Inspector") " pane in the lower Left-hand section of the browser window.")
		     (:p "Note that most slots will initially be shown as " (:em "unbound") ".")
		     (:p "Clicking on the " (:em "unbound") " link will cause the value of the associated
slot to be evaluated. In the lower screenshot the slot "
			 ((:span :class "slot") "length") " has been evaluated and returns "
                         ((:span :class "value") "4") "."))
		    ((:div :class "grid-item")
		     (:img :src (format nil "/~a-images/geysr-instantiate.png" (the publish-prefix)) :style "width: auto; height: 200px;" ) (:br)
		     (:img :src (format nil "/~a-images/geysr-eval.png" (the publish-prefix)) :style "width: auto; height: 200px;" ))
		    ((:div :class "grid-item")
		     (:p "To draw the geometry: first click or tap ")
		     (:p (:b "Mode") " and ensure " (:b "Add Leaves") " is selected with a checkmark"))
		    ((:div :class "grid-item")
		     (:img :src (format nil "/~a-images/geysr-add-leaves.png" (the publish-prefix)):style "width: auto; height: 100px;" ))
		    ((:div :class "grid-item")
		     (:p "Then left mouse click or tap on the object name just below the menu bar (in this case "
			 (:code "GDL-USER::MY-BOX-1") "), and the geometry will be displayed in the main viewport")
		     (:p "The geometry camera view may be changed by selecting "
			 (:b "View..Perspective") " and picking any of the pre-defined orientations such a top or left.")
		     (:p "The geometry viewport may be cleared by selecting "
			 (:b "View..Clear!")))
		    ((:div :class "grid-item")
		     (:img :src (format nil "/~a-images/geysr-geometry.png" (the publish-prefix)):style "width: auto; height: 400px;" ))
		    ((:div :class "grid-item")
		     (:p "By selecting " (:b "Mode..Set Self...") " then clicking on any object in the tree pane at upper-left,
the toplevel value of "
                         ((:span :class "variable-name") "self") " will be set to that object, to similar effect
as typing "
                         (:code "(setq self (make-object 'my-box-1))") " at the REPL. You may then evaluate any slot in
my-box-1 at the REPL"))
		    ((:div :class "grid-item")
		     (str (repl-example (the repl-set-self)))))
		   (:h3 "Creating Fresh Instances")
		   (:p 

                    (wmd "__Note__: __File..New__ is currently __only__ supported for a *Fresh Web Browser Tab*.
It is not currently supported to make a fresh instance in an existing Geysr browser tab or window.

So, if you'd like to make a new, fresh instance (of the same or a different type),
then follow these steps:

1. Open a *new* Web Browser tab or window.
1. Visit the toplevel  [Geysr url](/geysr).
1. Click or tap __File..New__ and proceed as in the __File..New__ section above.


"))
                   (:h3 "Updating Existing Instances after a Code Change")
                   (:p "If you want to see the results of a change you've made to the code
and loaded into the running session (don't worry, you will learn how
compile & load code changes later), then select the "
                       (:b "Update") " mode (click or tap " (:b "Mode..Update...") "),
and finally click in the tree on the object for which you would like
to unload any cached results it may be holding, and let the system
recompute fresh everything based on your latest code (typically the
node you click to "
                       (:b "Update")
                       " will simply be the root object in the tree).")
                        
                   (:p (wmd "
This recomputation forms one of the steps of a five-step iterative
process you'll typically follow when developing using Geysr:

1. Author your desired code (new code or changes to existing code).
1. Compile the code and load it into the running session (compiling is optional but recommended).
1. Update the instance in the running Geysr session to reflect the new code.
1. Inspect and poke around with the results in the various panes of Geysr
1. Based on said results, return to Step 1 above and repeat.

Your typical development session will include many iterations of the
above five steps. Depending on the situation at hand it is possible to
complete a single iteration very quickly, sometimes in less than one
minute. This rapid iteration potential is one of the upsides of
becoming proficient with GendL combined with Geysr."))

                   (:h3 "A Note on Symbol Names")
                   (:p (wmd "Note that object type names in Gendl (as in Common Lisp) have two parts:

1. a Package name
2. a Symbol name

These can be written together separated by a double colon (`::`).")

                       " For example, the symbol naming our " ((:span :class "object") "my-box-1")
                       " definition was introduced into the system while the "
                       ((:span :class "package-name") "gdl-user") " package was active, or
\"current\" &mdash; therefore you can refer to this definition with the fully qualified symbol
name: "
                       ((:span :class "package-name") "gdl-user")
                       (:code "::")
                       ((:span :class "object") "my-box-1")
                       (wmd
                        "Sometimes it is possible to use a single colon (`:`) rather than a
double colon (`::`), and often it is possible to omit the package name
entirely when referring to a symbol.

That's enough on Packages and Symbols for now. You will learn more about Packages and Symbols later."))
			    
		   (:h2 "Resources")
		   (str (the resource-links))))

   (repl-set-self (list (list :comment "Self is now set to to #<MY-BOX-1 #x210500D41D>, you may use command-line interaction....")
			(list :comment "127.0.0.1 - - [Thu, 02 Jun 2022 09:00:48 GMT] \"POST /gdlAjax HTTP/1.1\" 200 222")
			(list :command "(the)"
			      :output "#<MY-BOX-1 #x210500D41D>")
			(list :command "(the length)"
			      :output 2)
			(list :command "(the height)"
			      :output 4)
			(list :command "(the width)"
			      :output 3)
			(list :command "(the volume)"
			     :output 24))))

   )
