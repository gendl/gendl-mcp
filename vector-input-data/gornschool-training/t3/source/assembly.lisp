(in-package :training-3)

(defparameter *publish-prefix* "t3")  

(define-object assembly (base-tutorial-sheet)
  :input-slots
  ((getting-started-url nil)
   (tutorial-name "Organising your code"))

  :computed-slots
  ((introduction (with-cl-who-string ()
		   (:p "Now that you have completed the Getting Started with GendL tutorial, its worth giving some thought to the way you may want to organise and load the code you will be writing. This tutorial covers some basic principles and provides a few guidelines that we would consider to be best practice.")
		   (:p "The best time to get organised is right at the start as it will avoid getting locked in to less then optimal working practices, or needing to go through all your code at a later date to retro-fit it to your chosen organisation structure or methodology"))))

  :objects
  ((working-with-packages :type 'working-with-packages
			  :pass-down (page-objects)
			  :publish-prefix *publish-prefix*
			  :page 1
			  :page-title "Working with Packages"
			  :getting-started-url (the getting-started-url)
			  :resources (list "packages.lisp"))
   
   (package-tips :type 'package-tips
		 :pass-down (page-objects)
		 :publish-prefix *publish-prefix*
		 :page 2
		 :page-title "Package Tips")
   
   (code-structuring :type 'code-structuring
		     :pass-down (page-objects)
		     :publish-prefix *publish-prefix*
		     :page 3
		     :page-title "Code Structuring")
   
   (loading-code :type 'loading-code
		 :pass-down (page-objects)
		 :publish-prefix *publish-prefix*
		 :page 4
		 :page-title "Loading Code with cl-lite"))

  )








