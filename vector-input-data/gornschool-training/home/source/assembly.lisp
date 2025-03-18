(in-package :training-home)

(define-object assembly (training-common:base-site-mixin)

  :input-slots ((email-address nil :settable) (restored? nil :settable) (opt-in? nil :settable))

  :computed-slots
  (

   (title "GendL Self-start Tutorials")
   
   (page-header nil)

   (body-content
    (with-cl-who-string()
      (:h2 "GendL Self Start Tutorials")
		  
      (:p "Welcome"
          (str (when (the restored?) " back"))
          (str (if (the email-address) (format nil ", ~a" (the email-address)) ""))
          " to the GendL Self-Start Tutorials, which are a set of training materials aimed at getting you up and running with GendL and introducing you to some of the development tools and techniques.")
                  
      (:h3 "So what is GendL?")
                  
      (:p "GendL is a dynamic, declarative, object-oriented language environment embedded in ANSI Common Lisp (CL).")
                  
      (:p "It consists of a collection of predefined objects, code-expansion macros, and functions which you, the GendL application developer, may either use directly or extend, to solve problems of any level of complexity decomposed into manageable units.")

      (:p "GendL includes geometric primitives and has a built-in web server to  facilitate cross-platform deployment.")

      (:p "GendL is open source (under the Gnu AGPL license) and has been ported to several ANSI Common Lisp implementations, including Allegro CL, LispWorks, Clozure CL (CCL), Steel Bank Common Lisp (SBCL), and Clasp. There is also a commercial version offering proprietary/closed-source distribution rights, full levels of technical support and with some extra features like surface and solid modelling. The commercial version is packaged with Allegro CL from Franz Inc.")

      (:h3 "About These tutorials")

      (:p "The tutorials are designed to be self-paced learning resources and where appropriate  contain example code files which may be downloaded. It is recommended that the first  two tutorials, "
          (:a :href (the installing-gendl url) "Installing GendL")
          " and "
          (:a :href (the getting-started url) "Getting Started with GendL")
          " are completed in that  order - after that we move into more specific or advanced subjects which may be undertaken as required. Within each tutorial there are a number of topics,  which we would suggest are undertaken in the order presented.")

      (:h3 "Tutorials")
      ((:div :class "ml-3")
       ((:ol :class "list-decimal")
	(dolist (plis (the tutorial-objects))
	  (htm (:li ((:a :href (getf plis :url)) (str (getf plis :tutorial-name))) (:br))))))))
    
   (tutorial-objects (safe-sort
		      (remove nil (mapcar #'(lambda(obj)
					      (when (theo obj  tutorial-index)
						(list :object obj :tutorial-index (theo obj tutorial-index)
						      :tutorial-name (theo obj tutorial-name) :url (theo obj url))))
					  (remove-if-not #'(lambda(child)(typep child 'base-tutorial-sheet))
							 (the children))))
		      #'< :key #'(lambda(a) (getf a :tutorial-index)))))


  ;;
  ;; FLAG - make the below be a quantified set so we can navigate them more easily.
  ;;
  :objects
  ((installing-gendl :type 'training-1:assembly
		     :tutorial-index 1
		     :tutorial-name "Installing GendL"
                     :previous-page nil :next-page (the getting-started)
                     )
   
   (getting-started :type 'training-2:assembly
		    :tutorial-index 2
		    :tutorial-name "Getting started with GendL"
                    :previous-page (the installing-gendl) :next-page (the code-structuring))

   
   (code-structuring :type 'training-3:assembly
		     :tutorial-index 3
		     :tutorial-name "Structuring and Organising your code"
                     :previous-page (the getting-started) :next-page (the file-io))


   (file-io :type 'training-4:assembly
	    :tutorial-index 4
	    :tutorial-name "Reading from and Writing to Files"
            :previous-page (the code-structuring) :next-page (the more-objects))


   (more-objects :type 'training-5:assembly
		 :tutorial-name "More on Objects"
		 :tutorial-index 5
		 :getting-started-url (the getting-started url)
		 :define-object-url (the getting-started object-definition url)
		 :wall-example-url (the getting-started wall-example url)
		 :truss-example-url (the getting-started truss-example url)
                 :previous-page (the file-io) :next-page (the websites))

   (websites :type 'training-6:assembly
             :tutorial-name "Websites and Web Applications"
             :tutorial-index 6
	     :getting-started-tutorial (the getting-started)
	     :io-tutorial (the file-io)
             :previous-page (the more-objects) :next-page nil)))



