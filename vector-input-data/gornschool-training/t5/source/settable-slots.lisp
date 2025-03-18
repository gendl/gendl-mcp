(in-package :training-5)

(define-object settable-slots (base-training-sheet)
  :input-slots
  (getting-started-url)
  
  :computed-slots
  ((index-words (list ":settable" "set-slot!" "restore-slot-default" "restore-slot-defaults!" "dependancy tracker" "demand driven"))
		
   (code-1 (list "(define-object settable-slots (base-object)"
		 ""
		 "  :computed-slots"
		 "  ((speed 25 :settable)"
		 "   (time 15 :settable)"
		 "   (distance (* (the speed) (the time)) :settable))"
		 ""
		 "  :functions"
		 "  ((set-speed! (&key (value 20)) (the (set-slot! :speed value)))"
		 "   (set-time! (&key (value 10)) (the (set-slot! :time value)))"
		 "   (set-distance! () (the (set-slot! :distance 100)))"
		 "   (reset-distance! () (the (restore-slot-default! :distance)))"
		 "   (reset-all! () (the (restore-slot-defaults! (list :speed :time :distance))))))"))

   (repl-1 (list (list :command "(make-self 'settable-slots)"
		       :output "#<SETTABLE-SLOTS #x2104DC5DFD>")
		 (list :command "(the distance)"
		       :output 375)))
   (repl-2 (list (list :command "(the set-speed!)"
		       :output "NIL")
		 (list :command "(the distance)"
		       :output 300)))
   (repl-3 (list (list :command "(the set-time!)"
		       :output "NIL")
		 (list :command "(the distance)"
		       :output 200)))
   
   (repl-4 (list (list :command "(the set-distance!)"
		       :output "NIL")
		 (list :command "(the distance)"
		       :output 100)
		 (list :command "(the (set-speed! :value 30))"
		       :output "NIL")
		 (list :command "(the distance)"
		       :output 100)))
   (repl-5 (list (list :command "(the reset-distance!)"
		       :output ":DISTANCE")
		  (list :command "(the distance)"
		       :output 300)))
   (repl-6 (list (list :command "(the reset-all!)"
		       :output "(:SPEED :TIME :DISTANCE)")
		 (list :command "(the speed)"
		       :output 25)
		 (list :command "(the time)"
		       :output 15)
		 (list :command "(the distance)"
		       :output 375)))
   

   (body-content (with-cl-who-string ()
		   (:div :class "grid-container-2-650px"
			 (:div :class "grid-item"
			       (:p "In the "
				   (if (the getting-started-url)
				       (htm (:a :href (the getting-started-url) "Getting Started with GendL"))
				       (htm "Getting Started with GendL "))
				   " tutorial, we identified on of the biggest differences between GendL and other programming languages was that slot evaluation was always demand driven and the inbuilt dependency tracker will always ensure slot values are current whenever any input changes. This removes a big burdon from the developer and also ensures run times are fast, as we are never calculating any values we don't need to use.")
		               (:p "However, there may be times when we need to make programatic changes to values an over-ride this default behaviour. To do this, we identify either slots as "(:span :class "general-keyword" ":settable")" and then we use the "(:span :class "function" "set-slot!")" function to programaically alter the value of the selected slot.")
		               (:p "When we make a change to a slot value by programatically setting it, the dependency tracker is aware of this and any changes which would ordinarily cause the value to be updated are suspended. If we want to reverse this behaviour (back to default) we can use either of the "(:span :class "function" "restore-slot-default!")" or "(:span :class "function" "restore-slot-defaults!")" functions")
		               (:p "As with a lot of different techniques, programatically altering slot values and over-riding the default behaviour has its place and is a useful additoon to the tools and techniques available to the programmer. However, we need to be aware that at the point we do this, and until we revert to default behaviour, it becomes to responsibility of the developer to manage dependencies. For this reason it is recommended to use this technique sparingly and only when a solution using the default behaviour will not give the desired results")
		               (:p "Consider the following code")
		               (str (code-example (the code-1)))
		               (:p "3 "(:span :class "object-keyword" ":computed slots")", "(:span :class "slot" "speed")", "
			           (:span :class "slot" "time")" and "
			           (:span :class "slot" "distance")" have been tagged as "
			           (:span :class "general-keyword" ":settable")". 5 functions have been defined, 3 which set new values (using "
			           (:span :class "function" "set-slot!")") for the respective slots and 2 which will restore default values and behaviours (using "
			           (:span :class "function" ":restore-slot-default!")" and "
			           (:span :class "function" ":restore-slot-defaults!")"). If we make the object and evaluate distance we get the following"
			           (str (repl-example (the repl-1)))
			           "We can now programatically change the value of speed by running the "(:span :class "function" "set-speed!")" function. If we evaluate "
			           (:span :class "slot" "distance")", we can see that"
			           (:ul (:li "The value has changed")
			                (:li "But the underlying calculation is being performed as specified"))
			           (str (repl-example (the repl-2)))
			           "We can do the same with "(:span :class "slot" "time")", getting the same results"
			           (str (repl-example (the repl-3)))
			           "If we now modify the value of "(:span :class "slot" "distance")", we can see the change in that value, but then going back and changing the value of "
			           (:span :class "slot" "speed")" again, we can see that "(:span :class "slot" "distance")" has not been changed. This is because "(:span :class "slot" "distance")" has had its value set and any dependency tracking associated with it, which would ordinarily cause a change to its value, has been disabled"
			           (str (repl-example (the repl-4)))
			           "However, if we now reset the value of "(:span :class "slot" "distance")" only, we can see that dependency tracking has picked up the values of "(:span :class "slot" "speed")" and "(:span :class "slot" "time")" and has updated the value for "(:span :class "slot" "distance")
			           (str (repl-example (the repl-5)))
			           "Finally, if we reset all slots, all are set back to default"
			           (str (repl-example (the repl-6)))))
			 (:div :class "grid-item"
			       (:h2 "Resources")
			       (str (the resource-links))))))))

