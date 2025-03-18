(in-package :gwl-user)

(define-object define-basic (base-html-page)
  :computed-slots
  ((body "My first web page"))
  )

(define-object define-basic-lhtml (base-html-page)
  :computed-slots
  ((body (with-lhtml-string ()
		      (:table
			  (:tr (:th "Day") (:th "Number"))
			(:tr (:td "Sunday") (:td "1"))
			(:tr (:td "Monday") (:td "2"))
			(:tr (:td "Tuesday") (:td "3"))
			(:tr (:td "Wednesday") (:td "4"))
			(:tr (:td "Thursday") (:td "5"))
			(:tr (:td "Friday") (:td "6"))
			(:tr (:td "Saturday") (:td "7"))))))
			
  )

(define-object define-basic-styled (base-html-page)
  :computed-slots
  ((body (with-lhtml-string ()
		      (:table :style "border-style: solid;"
			  (:tr (:th "Day") (:th "Number"))
			(:tr (:td "Sunday") (:td :style "text-align: center;" "1"))
			(:tr (:td "Monday") (:td :style "text-align: center;" "2"))
			(:tr (:td "Tuesday") (:td :style "text-align: center;" "3"))
			(:tr (:td "Wednesday") (:td :style "text-align: center;" "4"))
			(:tr (:td "Thursday") (:td :style "text-align: center;" "5"))
			(:tr (:td "Friday") (:td :style "text-align: center;" "6"))
			(:tr (:td "Saturday") (:td :style "text-align: center;" "7"))))))
			
  )


(define-object define-basic-css (base-ajax-sheet)
  :computed-slots
  ((additional-header-content (with-cl-who-string()
				(:link :rel "stylesheet" :href "/my-style.css")))
   (main-sheet-body (with-cl-who-string ()
		      (:table
			  (:tr (:th "Day") (:th "Number"))
			(:tr (:td "Sunday") (:td "1"))
			(:tr (:td "Monday") (:td "2"))
			(:tr (:td "Tuesday") (:td "3"))
			(:tr (:td "Wednesday") (:td "4"))
			(:tr (:td "Thursday") (:td  "5"))
			(:tr (:td "Friday") (:td "6"))
			(:tr (:td "Saturday") (:td "7"))))))
			
  )

(define-object define-basic-image (base-ajax-sheet)
  :computed-slots
  ((additional-header-content (with-cl-who-string()
				(:link :rel "stylesheet" :href "/my-style.css")))
   (main-sheet-body (with-cl-who-string ()
		      (:p "The Genworks International Logo")
		      (:img :src "/images/logo.png"))))
			
  )
(define-object define-basic-image-links (base-ajax-sheet)
  :computed-slots
  ((additional-header-content (with-cl-who-string()
				(:link :rel "stylesheet" :href "/my-style.css")))
   (main-sheet-body (with-cl-who-string ()
		      (when gwl:*developing?* (str (the development-links)))
		      (:p "The Genworks International Logo")
		      (:img :src "/images/logo.png"))))
			
  )
(publish-gwl-app "/basic" "gwl-user::define-basic")
(publish-gwl-app "/basic-lhtml" "gwl-user::define-basic-lhtml")
(publish-gwl-app "/basic-styled" "gwl-user::define-basic-styled")
(publish-gwl-app "/basic-css" "gwl-user::define-basic-css")
(publish-gwl-app "/basic-image" "gwl-user::define-basic-image")
(publish-gwl-app "/basic-image-links" "gwl-user::define-basic-image-links") 

;; this parameter plus the following publishing assumes a directory structure as follows
;; some-path/source - LISP source files
;; some-path/css - CSS files
;; some-path/images - image files
;; the actual value of some-patch doesn't matter providing the 3 folder above have the same root path

(defparameter *home*
  (merge-pathnames "../" (make-pathname :name nil :type nil :defaults (glisp:source-pathname))))

(gwl::publish-file :path "/my-style.css"
		   :file (namestring (merge-pathnames "css/my-style.css" *home*)))

(gwl::publish-directory :prefix "/images"
			:destination (namestring (merge-pathnames "images/"  *home*)))
