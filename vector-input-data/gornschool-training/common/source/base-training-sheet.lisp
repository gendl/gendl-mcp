(in-package :training-common)

(define-object base-training-sheet (base-site-mixin)

  :input-slots
  (publish-prefix page page-title page-objects (resources nil) (body-content nil))
                  

  :computed-slots
  ((title (the page-title)) ;; for standard inclusion of title in header by base-ajax-sheet.

   (index-words nil)



   ;;
   ;; FLAG -- make following 3 generic so they can be in base-site-mixin. 
   ;;
   (current-page-ind (position (the page) (the page-objects) :key #'(lambda(a) (getf a :page))))

   (next-page (let ((next-index (1+ (the current-page-ind))))
                (when (< next-index (length (the page-objects)))
                  (nth next-index (the page-objects)))))

   (previous-page (when (> (the current-page-ind) 0)
                    (nth (1- (the current-page-ind)) (the page-objects))))

   ;;
   ;; FLAG make this generic so it can be in base-site-mixin.
   ;;
   (page-header (with-lhtml-string ()
                  (:h2 (str (the page-title)))
                  (:p
                   (when (the previous-page) (htm ((:a :class "text-red-500" :href (getf (the previous-page) :url)) "&lt;-Previous")))
                   (:span :class "font-bold" " | ")
                   ((:a :class "text-blue-500" :href (the parent url)) "^UP^")
                   (:span :class "font-bold" " | ")
                   (when (the next-page) (htm ((:a :class "text-green-500" :href (getf (the next-page) :url)) "Next-&gt;"))))))



   (resource-links (with-lhtml-string()

		     (:table
			 (dolist (resource (the resources))
			   (let* ((fname (if (listp resource) (getf resource :url) resource))
				  (href (if (listp resource) fname (format nil "/~a-resources/~a" (the publish-prefix) fname)))
				  (target (when (listp resource) "_new"))
				  (ftype (pathname-type fname))
				  (label (if (listp resource) (getf resource :title) resource))
				  (icon (cond ((string-equal ftype "html") "/common-images/html-file.png")
					      ((string-equal ftype "htm") "/common-images/html-file.png")
					      ((string-equal ftype "lisp") "/common-images/lisp-file.png")
                                              ((string-equal ftype "gendl") "/common-images/lisp-file.png")
                                              ((string-equal ftype "gdl") "/common-images/lisp-file.png")
					      ((string-equal ftype "pdf") "/common-images/pdf-file.png")
					      ((string-equal ftype "txt") "/common-images/txt-file.png")
					      ((string-equal ftype "css") "/common-images/css-file.png")
					      ((string-equal ftype "png") "/common-images/png-file.png")
					      (t nil))))
			     (htm (:tr (when icon
					 (htm (:td ((:a :href href :target target) ((:img :src icon :style "width: 40px; height: auto;"))))))
				       (:td ((:a :href href :target target) (str label))))))))))))


