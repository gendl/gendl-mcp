(in-package :cl-user)

(load (compile-file (merge-pathnames "gwl-patches.lisp" (glisp:source-pathname))))

(in-package :gwl-user)


(setq *developing?* t)



(define-object sample-page (base-html-page)
  :computed-slots
  ((title "sample page")
   (body "My sample page")))

(publish-gwl-app "/sample-page" 'sample-page)


(define-object sample-html-page (base-html-page)
  :computed-slots
  ((title "sample html page")
   (body (with-lhtml-string ()
           "My sample html page"
           (:table
               (:tr (:th "Day") (:th "Number"))
             (:tr (:td "Sunday") (:td "1"))
             (:tr (:td "Monday") (:td "2"))
             (:tr (:td "Tuesday") (:td "3"))
             (:tr (:td "Wednesday") (:td "4"))
             (:tr (:td "Thursday") (:td "5"))
             (:tr (:td "Friday") (:td "6"))
             (:tr (:td "Saturday") (:td "7")))))))

(publish-gwl-app "/sample-html-page" 'sample-html-page)

(define-object sample-inline-styled-html-page (base-html-page)
  :computed-slots
  ((title "sample inline styled html page")
   (body (with-lhtml-string ()
           "My sample inline styled html page"
           (:table :style "border-style: solid;"
               (:tr (:th "Day") (:th "Number"))
             (:tr (:td "Sunday") (:td "1"))
             (:tr (:td "Monday") (:td "2"))
             (:tr (:td "Tuesday") (:td "3"))
             (:tr (:td "Wednesday") (:td "4"))
             (:tr (:td "Thursday") (:td "5"))
             (:tr (:td "Friday") (:td "6"))
             (:tr (:td "Saturday") (:td "7")))))))

(publish-gwl-app "/sample-inline-styled-html-page" 'sample-inline-styled-html-page)

;; for the css and image files we assume a directory structure as follows
;; <home>/source - location of this file
;; <home>/css - location of css example files
;; <home>/images - location of image files

(defparameter *home*(merge-pathnames "../" (make-pathname :name nil :type nil :defaults (glisp:source-pathname))))

(gwl::publish-file :path "/my-style.css" :file (namestring (merge-pathnames "css/my-style.css" *home*)))

(define-object sample-external-styled-html-page (base-html-page)
  :computed-slots
  ((additional-header-content (with-lhtml-string ()
                                ((:link :rel "stylesheet" :href "/my-style.css"))))
   (title "sample external styled html page")
   (body (with-lhtml-string ()
           "My sample external styled html page"
           (:table 
               (:tr (:th "Day") (:th "Number"))
             (:tr (:td "Sunday") (:td "1"))
             (:tr (:td "Monday") (:td "2"))
             (:tr (:td "Tuesday") (:td "3"))
             (:tr (:td "Wednesday") (:td "4"))
             (:tr (:td "Thursday") (:td "5"))
             (:tr (:td "Friday") (:td "6"))
             (:tr (:td "Saturday") (:td "7")))))))

(publish-gwl-app "/sample-external-styled-html-page" 'sample-external-styled-html-page)

(gwl::publish-directory :prefix "/images" :destination (namestring (merge-pathnames "images/" *home*)))

(define-object sample-image-html-page (base-html-page)
  :computed-slots
  ((css-published-path *css-published-path*)
   (images-published-prefix *images-published-prefix*)
   (additional-header-content (with-lhtml-string ()
                                (:link :rel "stylesheet" :href "/my-style.css")))
   (body (with-lhtml-string ()
           (:p "Gorn Struggle")
           (:img :src (string-append (the images-published-prefix) "/star-trek-gorn.webp"))))))

(publish-gwl-app "/sample-image-html-page" 'sample-image-html-page)


(define-object sample-image-development-links (base-html-page)

  :computed-slots
  ((css-published-path *css-published-path*)
   (images-published-prefix *images-published-prefix*)
   (additional-header-content (with-lhtml-string ()
                                (:link :rel "stylesheet" :href (the css-published-path))))
   (body (with-lhtml-string ()
           (when *developing?* (str (the development-links)))
           (:p "Gorn Struggle")
           (:img :src (string-append (the images-published-prefix) "/star-trek-gorn.webp"))))))
(publish-gwl-app "/sample-image-development-links" 'sample-image-development-links)

