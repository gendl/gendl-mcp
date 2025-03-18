(in-package :training-1)

(define-object learning-emacs (base-training-sheet)
  :input-slots
  (index-url)

  :computed-slots
  ((body-content (with-cl-who-string ()
                   (:p
                       "Regardless of which installation method you choose, you will need to become
familar with Gnu Emacs as a first step. Every installation of Gnu Emacs contains a built-in tutorial
which you can access by typing "
                       (:span :class "general-keyword" "C-h t")
                       " from inside Emacs (that's hold Control, type h, then release Control
and type t.)")


                      (:p "Note on CapsLock: Because the Control key
is used extensively in Emacs, you may wish to remap your CapsLock to
act as a Control key. On Windows, this can be done with the
Microsoft-supported tool Power Toys (available from Microsoft
Store). On Linux desktops and MacOS, it is possible to map the
CapsLock key to the Control key using the Settings or Preferences app.")))))

                      

                          


