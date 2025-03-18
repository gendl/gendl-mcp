(in-package :training-1)

(define-object learning-slime (base-training-sheet)
  :input-slots
  (index-url)

  :computed-slots
  ((body-content (with-cl-who-string ()
                   (:p
                    (:a :href "https://slime.common-lisp.dev" "Slime")
                    " is the Superior Lisp Interaction Mode for Emacs. If you've successfully installed
Gendl or Genworks GDL according to the "
                    (:a :href (the installation url) "Installation")
                    " section, you will be presented with a Slime REPL prompt where you
can type Common Lisp and GendL commands. Although we don't teach all
of Slime in this tutorial, we do provide examples of working at the REPL.")

                      (:p "An excellent video overview of Slime can be found  "
                          (:a :href "https://www.youtube.com/watch?v=_B_4vhsmRRI" "here") ". (you can skip the beginning about installation).")
                      (:p "And you don't need to know much to get started. Here are a few tips to get started:"
                          (:ul (:li "If you get thrown into the debugger, type "
                                    (:span :class "general-keyword" "a") " to get out of it.")
                               (:li "At the REPL you can type "
                                    (:span :class "general-keyword" "M-p") " to bring back previous command from history.")
                               (:li "At the REPL you can type "
                                    (:span :class "general-keyword" "M-n") " to bring up the next command from history.")))))))


                      

                          



