(in-package :training-common)

(define-object base-site-mixin (base-html-page)

  :input-slots
  ((title "The Gorn School")
   (body-content nil) (page-header nil)
   (dont-write-snap (not (search "sessions" (the url))))
   ;;(dont-write-snap t)
   )
  
  :computed-slots
  ((additional-header-content (with-lhtml-string ()
                                (:meta :name "viewport"
                                       :content "width=device-width, initial-scale=1.0")
                                (:link :rel "stylesheet" :href "/css/training-style.css")))

   (favicon-type "image/png")(favicon-path "/gorn.png")
   (include-default-favicon? nil)

   (body (with-lhtml-string()
	   (when gwl:*developing?* (str (the development-links)))
	   (:div :class "container mx-auto"
		 (:div :class "main-page-item"
                       (:div :class "mt-1 ml-2 mr-5"
                             (str (the page-header))
                             (str (the body-content))))))))

  :functions
  (;;
   ;; FLAG - style this a bit differently if the section is open vs closed. 
   ;;
   (hint-button
    (&key function-key arguments)
    (with-lhtml-string ()
      ((:div :class "flex space-y-2")
       (:input :type :button :class "gs-button-blue" :name "Hint" :value "Hint"
	       :onclick (the (gdl-ajax-call :bashee (the) ;; FLAG why is this needed?
					    :function-key function-key
                                            :arguments arguments))))))
   (after-present! () (unless (the dont-write-snap)
			(gwl::write-snap self :snap-home gwl::*snap-home*)))))


   



