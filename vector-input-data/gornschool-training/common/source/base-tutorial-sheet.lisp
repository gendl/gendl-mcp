(in-package :training-common)


(define-object base-tutorial-sheet (base-site-mixin)

  :documentation
  (:author "Mike Twelves"  :description "Mixin to be used for tutorial assembly. Assumes all child
objects are tutorial topics with the exception of the index
page, and that each topic has a page number")
  
  :input-slots
  (tutorial-index
   tutorial-name
   previous-page
   next-page)


  :computed-slots
  ((title (the tutorial-name))  ;; for standard inclusion of title in header by base-ajax-sheet.

   (introduction nil)

   (page-header (with-lhtml-string ()
                  (:h2 :class "text-red-900"  "Tutorial: " (str (the tutorial-name)))
                  (when (the previous-page)
                    (htm (:a :class "text-red-500" :href (the previous-page url) "<-Previous")))
                  " | "
                  (when (the parent)
                    (htm  (:a :class "text-neutral-800" :href (the parent url) "^UP^")))
                  " | "
                  (when (the next-page)
                    (htm  (:a :class "text-green-500" :href (the next-page url) "Next->")))))
   
   
   
   (body-content (with-lhtml-string ()
		   (when (the introduction) (str (the introduction)))
                   ((:div :class "pl-5")
                    ((:ol :class "list-decimal")
		     (dolist (plis (the page-objects))
		       (htm (:li ((:a :href (getf plis :url)) (str (getf plis :title))) (:br))))))))

   (page-objects (safe-sort
		  (remove nil (mapcar #'(lambda(obj)
                                          (when (theo obj page)
                                            (list :object obj :page (theo obj page) :index-words (theo obj index-words)
                                                  :title (theo obj page-title) :url (theo obj url))))
                                      (remove-if-not #'(lambda(child)(or (typep child 'base-training-sheet)
                                                                         (typep child 'index-page)))
                                                     (the children)))) #'< :key #'(lambda(a) (getf a :page))))

   (index-list-hash (let ((hash (make-hash-table :test #'equalp)))
		      (dolist (page (the page-objects) hash)
                        (destructuring-bind (&key url title index-words &allow-other-keys) page
                          (let ((link (with-lhtml-string () ((:a :href url) (str title)))))
		            (dolist (word index-words)
		              (setf (gethash word hash) (append (gethash word hash) (list link))))))))))

  :objects
  ((index-page :type 'index-page :pass-down (index-list-hash) :page 99 :page-title "Index")))



