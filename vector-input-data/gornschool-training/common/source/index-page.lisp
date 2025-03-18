(in-package :training-common)

(define-object index-page (base-ajax-sheet)
  :input-slots
  (index-list-hash page-title page)

  :computed-slots
  ((index-words nil)
   (additional-header-content (with-cl-who-string()
				((:link :rel "stylesheet" :href "/tw-css-build/training-style-tw.css"))
				(:title (str (the page-title)))))
   (index-list (let ((res nil)
		     (specials (list "*" "+" "-" "/" "'" "\<" "\<=" "=" ">" ">=" "~a" "~f" "~d" "~r" "~p" "~$" "~{ ~}" "~[ ~]" "~( ~)")))
		 (maphash  #'(lambda(k v) (setq res (append res (let* ((key (glisp::replace-regexp (string-downcase k) ":|&" ""))
								       (index (if (member key specials :test 'string-equal)
										  "Special Characters"
										  (string-upcase (aref key 0)))))
								  (list (list :key key
									      :index index
									      :word k
									      :links v)))))) (the index-list-hash))
		 (safe-sort res
			    #'string<
			    :key #'(lambda(a) (getf a :key)))))
			     
   (main-sheet-body (with-cl-who-string()
		      (when gwl:*developing?* (str (the development-links)))
		      (:h2 (str (the page-title)))
		      
		      ((:a :href (the root url)) "Home")
		      (str (the index-items main-div)))))

  :objects
  ((index-select-fc :type 'menu-form-control
		    :size (length (the-child choice-list))
		    :choice-list (list "Special Characters" "A" " B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z" "All Index")
		    :default "Special Characters"
		    :ajax-submit-on-change? t)

   (index-items :type 'index-items
	
		:menu-fc (the index-select-fc)
		:index-value (the index-select-fc value)
		:index-list (the index-list))))


(define-object index-items (sheet-section)
  :input-slots
  (index-value
   index-list
   menu-fc)

  :computed-slots
  ((inner-html (with-cl-who-string ()
		 ((:div :class "main-page-container")
		  ((:div :class "main-page-item")
		   (str (theo (the menu-fc) form-control)))
		  ((:div :class "main-page-item")
		   (:table (let ((index (the index-value))
				 (index-values nil))
			     (dolist (lis (the index-list))
			       (let ((index-val (getf lis :index)))
				 (when (or (string-equal index "All Index")
					   (string-equal index index-val))
				   (setq index-values T)
				   (let ((links (getf lis :links)))
				     (htm (:tr 
					   (:td (str (getf lis :word)))
					   (:td (str (car (getf lis :links)))))
					  (if (> (length links) 1)
					      (dolist (link (cdr links))
						(htm (:tr (:td)
							  (:td (str link)))))))))))
			     (unless index-values (htm (:tr (:td "No content for this value"))))))))))))
