(in-package :training-1)

(define-object web-based-ide (base-training-sheet)
  :input-slots
  (index-url)

  :computed-slots
  ((main-sheet-body (with-cl-who-string ()
                      (:h2 (str (the page-header)))
                      (:p "Genworks or one of its parters/VARs will be offering a no-install
option to develop with GendL and Genworks GDL based on running emacs
inside a web terminal. Please check this space for links once the service is live.")))))

                          


