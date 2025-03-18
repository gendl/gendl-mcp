(in-package :training-1)

(defparameter *publish-prefix* "t1")

(define-object assembly (base-tutorial-sheet)
  :input-slots
  (tutorial-index
  (tutorial-name "Installation and Setup"))




  :objects
  ((installation :type 'installation
                 :pass-down (page-objects)
                 :page 1
                 :page-title "Installation of your GendL-based System"
                 :publish-prefix *publish-prefix*
                 :index-url (the index-page url))
   
   (learning-emacs :type 'learning-emacs
                   :pass-down (page-objects)
                   :page 2
                   :page-title "Learning Emacs"
                   :publish-prefix *publish-prefix*
                   :index-url (the index-page url))
   
   (learning-slime :type 'learning-slime
                   :pass-down (page-objects)
                   :page 3
                   :page-title "Learning Slime"
                   :publish-prefix *publish-prefix*
                   :index-url (the index-page url))
   
   ))

  







