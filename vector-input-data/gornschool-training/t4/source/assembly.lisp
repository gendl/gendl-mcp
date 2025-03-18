(in-package :training-4)

(defparameter *publish-prefix* "t4")  

(define-object assembly (base-tutorial-sheet)
  :input-slots
  ((getting-started-url nil)
   (tutorial-name "File I/O"))

  :computed-slots
  ((introduction (with-cl-who-string ()
		   (:p "A basic requirement for most applications is to be able to interface to the outside world. This may be done with a User Interface, but it may also be done by reading inputs from a file and/or writing outputs to a file"
		       (:p "This tutorial covers the basics of file i/o")))))
  

  :objects
  ((file-io-basics :type 'file-io-basics
		   :pass-down (page-objects)
		   :publish-prefix *publish-prefix*
		   :page 1
		   :getting-started-url (the getting-started-url)
		   :page-title "File IO Basics")
   
   (writing-to-a-file :type 'writing-to-a-file
		      :pass-down (page-objects)
		      :publish-prefix *publish-prefix*
		      :page 2
		      :page-title "Writing to a file"
		      :resources (list "file-output.lisp"))
   
   (reading-from-a-file :type 'reading-from-a-file
			:pass-down (page-objects)
			:publish-prefix *publish-prefix*
			:page 3
			:page-title "Reading from a file"
			:resources (list "read-input.lisp" "report.txt"))
   
   (example-1 :type 'file-io-example-1
	      :pass-down (page-objects)
	      :publish-prefix *publish-prefix*
	      :page 4
	      :getting-started-url (the getting-started-url)
	      :page-title "Example - outputting the Tutorial 2 Building BoM to a file"
	      :resources (list "building-bom-output.lisp") )

   (example-2 :type 'file-io-example-2
	      :pass-down (page-objects)
	      :publish-prefix *publish-prefix*
	      :page 5
	      :getting-started-url (the getting-started-url)
	      :read-from-file-url (the reading-from-a-file url)
	      :page-title "Example - instantiating the Tutorial 2 Building example from a file input"
	      :resources (list "building-bom-input-output.lisp"))

   )
  )









  
  
  
  
  
