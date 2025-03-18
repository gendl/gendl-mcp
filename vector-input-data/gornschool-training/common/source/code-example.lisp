(in-package :training-common)

(defun code-example (code-strings &key (class "code-example"))
  (with-cl-who-string()
    ((:div :class class)
     (:pre (dolist (code-str code-strings)
	     (htm (str (indented-html code-str ))))))))


#+nil
(let* ((str-len (length code-str))
	      (trimmed-str (glisp::replace-regexp code-str "^[ /t]+" ""))
	      (trimmed-str-len (length trimmed-str))
	      (spaces (- str-len trimmed-str-len))
	      (style (format nil "margin-left: ~apx;" (* spaces 7))))
	 (htm ((:span :class "code" :style style) (str trimmed-str))))

(defun indented-html (code-str &key (class "code") (prefix nil) (prompt "GDL-USER> "))
  (with-cl-who-string()
    (let* ((str-len (length code-str))
	   (trimmed-str (glisp::replace-regexp code-str "^[ \\t]+" ""))
	   (trimmed-str-len (length trimmed-str))
	   (class-val (cond ((string-equal class "code")
				  (if (glisp::match-regexp "^;" trimmed-str) "code-comment" "code"))
			    ((not prefix) (format nil "~a-no-prefix" class))
			    (T class)))
	   (offset (if (or (string-equal class "code") prefix) 0 9))
	   (spaces (- str-len trimmed-str-len))
	   (style (format nil "margin-left: ~apx;" (+ (* offset 7) (* spaces 7)))))
      (when prefix
	(htm ((:span :class "prompt" :style "display: inline;") (str (string-append prompt "&nbsp;")))))
      (htm ((:span :class class-val :style style) (str trimmed-str))))))
