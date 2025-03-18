(in-package :gwl-user)

(define-object simple-page-with-section (base-html-page)
  :computed-slots
  ((body
    (with-lhtml-string ()
      (when gwl:*developing?* (str (the development-links)))
      (:h2 "Basic Page Sections")
      (str (the section-1 div)))))
  :objects
  ((section-1 :type 'base-html-div
              :inner-html (with-lhtml-string ()
                            (:p "Using page sections to provide content")))))

(publish-gwl-app "/simple-page-with-section" 'simple-page-with-section)

(define-object simple-page-with-custom-section (base-html-page)
  :computed-slots
  ((input-list (list 1 2 3))
   (body (with-lhtml-string ()
           (when gwl:*developing?* (str (the development-links)))
           (:h2 "Basic Page Sections")
           (str (the section-1 div)))))
  :objects
  ((section-1 :type 'base-html-div-1
              :input-list (the input-list))))


(define-object base-html-div-1 (base-html-div)
  :input-slots
  (input-list)
  :computed-slots
  ((inner-html (with-cl-who-string ()
                 (:table :border 1
                   (:tr (:th "Content"))
                   (dolist (c (the input-list))
                     (htm (:tr (:td (fmt "Cell ~a content" c))))))))))

(publish-gwl-app "/simple-page-with-custom-section" 'simple-page-with-custom-section)
