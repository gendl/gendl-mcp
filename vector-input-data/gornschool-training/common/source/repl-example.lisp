(in-package :training-common)


;;
;; 
;; command-no-prefix  -- class name listed here for purgeCSS to keep it in our CSS.
;;

(defun repl-example (command-and-output &key (class "repl-example"))
  (with-cl-who-string()
    ((:div :class class)
     (:pre (dolist (plis command-and-output)
             (destructuring-bind (&key prompt comment command output error) plis
	       (let ((prompt (if prompt (string-append prompt "&nbsp;") "GDL-USER>&nbsp;"))
		     (out (if (stringp output) (glisp::replace-regexp output "<" "&#60;") output))
		     (cmt (if (stringp comment) (glisp::replace-regexp comment "<" "&#60;") comment))
		     (err (if (stringp error) (glisp::replace-regexp error "<" "&#60;") error)))
	         (when cmt (htm ((:span :class "comment") (str cmt))))
                 (when command (if (listp command)
                                   (let ((prefix t))
                                     (dolist (string command)
                                       (htm (str (indented-html string :class "command" :prefix prefix :prompt prompt)))
                                       (setq prefix nil)))
                                   (htm  ((:p :style "display: inline;")
				          ((:span :class "prompt" :style "display: inline;") (str prompt))
				          ((:span :class "command" :style "display: inline;") (str command))))))
	         (when out  (if (listp out) (dolist (str out) (htm ((:span :class "output") (str (glisp::replace-regexp str "<" "&#60;")))))
			        (htm ((:span :class "output") (str out)))))
	         (when err (htm ((:span :class "error") (str err)))))))))))



