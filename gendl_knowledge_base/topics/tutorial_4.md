# Gendl Documentation - tutorial_4

## read-input.lisp - header
Source: gornschool-training/t4/resources/source/read-input.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## read-input.lisp - read-file
Source: gornschool-training/t4/resources/source/read-input.lisp
Type: tutorial

```
(defun read-file (file )
  (let ((result))
    (with-open-file (str file :direction :input)
      (do ((line (read-line str nil 'eof)
		 (read-line str nil 'eof)))
	  ((eql line 'eof) result)
	(setq result (append result (list line)))))))


```

---

## read-input.lisp - import-data
Source: gornschool-training/t4/resources/source/read-input.lisp
Type: tutorial

```
(defun import-data (file)
  (let*
      ((raw-data (read-file file))
       (res (mapcar #'(lambda(a) (glisp:split-regexp "\\s+" a)) raw-data))
       (res-1 (mapcan #'(lambda(a) (list (make-keyword (second a)) (third a))) res))
       (keywords (remove nil res-1 :key #'(lambda(a) (keywordp a))))
       (r nil))
    (dolist (k keywords r)
      (cond ((or (eq k :width) (eq k :length) (eq k :height))
	     (setq r (append r (list k (read-safe-string (getf res-1 k))))))
	    ((eq k :center)
	     (let ((co-ord (glisp:split-regexp "," (getf res-1 k))))
	       (setq r (append
			r
			(list k (make-point (read-safe-string (first co-ord))
					    (read-safe-string (second co-ord))
					    (read-safe-string (third co-ord))))))))))))
	

```

---

## report.txt (chunk 1/1)
Source: gornschool-training/t4/resources/source/report.txt
Type: tutorial

```
Box Width 3
Box Length 6
Box Height 4
Box Center 0.0,0.0,0.0
Box Volume 72

```

---

