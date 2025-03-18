(in-package :gdl-user)

(defun subseq-safe (sequence start &optional end)
  ;; if either the start or end inputs to subseq are more than the length of the list, then subseq will error
  ;; also subseq is only valid for sequences, so strings and lists
  ;; if any of the cases that would cause subseq to error are encountered, subseq-safe returns nil, otherwise it
  ;; returns the value subseq would normally return
  (when (or (stringp sequence) (listp sequence))
    (cond ((>= start (length sequence)) nil)
	  ((and end (> end (length sequence))) (subseq sequence start))
	  (t (subseq sequence start end)))))

(define-object more-on-lists (base-object)

  :computed-slots
  ((my-number-list (list 1 2 3 4))
   (my-number-list-1 (list 1 2 3 4))
   (my-string-list (list "peter" "paul" "mike" "john"))
   (my-plist (list :uk "London" :france "Paris" :belgium "Brussels"))
   
   ;; Length of a list
   (length-1 (length (the my-number-list)))    ;;; 4
   (length-2 (length nil))                     ;;; 0
   


   ;; member of a list
   (member-1 (member 2 (the my-number-list)))                           ;;; (2 3 4)
   (member-2 (member "paul" (the my-string-list)))                      ;;; returns nil because the default test for equality is eql
                                                                        ;;; use the :test input to specify what test for equality is to be used
   (member-2a (member "paul" (the my-string-list) :test 'string-equal)) ;; ("paul" "mike" "john") (or 'string=)

   ;; parts of lists
   (part-1 (subseq (the my-number-list) 1))    ;;; (2 3 4) anything from the first element onwards is retained
   (part-2 (subseq (the my-number-list) 0 3))  ;;; (1 2 3) anything beyond the 3rd element is removed
   ;; if you trim beyond the end of the list an error is generated
   (part-3 (ignore-errors (subseq (the my-number-list) 0 10))) ;;; NIL
					                       ;;; #<SIMPLE-ERROR #x210376DFFD>



   ;; removing elements from lists
   (remove-1 (remove 3 (the my-number-list)))                           ;;; (1 2 4)
   (remove-2 (remove "mike" (the my-string-list)))                      ;;; ("peter" "paul" "mike" "john") same as member, we have to specify the correct :test
   (remove-3 (remove "mike" (the my-string-list) :test 'string-equal))  ;;; ("peter" "paul" "john")

   ;; removing duplicates
   (remove-duplicates-1 (remove-duplicates (list 1 2 3 4 2 3)))                              ;;; (1 4 2 3)

   ;; we can also define what we mean by a duplicate, for example when working on float numbers
   (remove-duplicates-2 (remove-duplicates (list 15.0 112.0 5.1 77.8 15.1)
					       :test #'(lambda (a b) (near-to? a b 0.5))))  ;;; (112.0 5.1 77.8 15.1)
   
   (list-remove-duplicates-3 (remove-duplicates (list "string-1" "string-2" "string-3" "string-1" "string-3")))
                                                                                            ;;; ("string-1" "string-2" "string-3" "string-1" "string-3")
   
   (list-remove-duplicates-4 (remove-duplicates (list "string-1" "string-2" "string-3" "string-1" "string-3")
						:test #'(lambda (a b) (string-equal a b)))) ;; ("string-2" "string-1" "string-3")

   ;; sorting
   (sort-1 (sort  (the my-number-list-1) #'>)) ;;; (4 3 2 1) BUT if we then evaluate (the my-number-list-1) is returns (1).
	                                      ;;; This is because sort is DESTRUCTIVE - it modifies the supplied list
	  
   (sort-2 (safe-sort (the my-number-list) #'>))              ;;; (4 3 2 1) and (the my-number-list) is unchanged	   
   (sort-3 (safe-sort (the my-string-list) #'string-lessp))   ;;; ("john" "mike" "paul" "peter")	   
   (sort-4 (safe-sort (the my-string-list) #'string<))	      ;;; ("john" "mike" "paul" "peter")

   
   ;; flattening lists
   (flatten-1 (flatten (list 1 2 (list 3 4 (list 5 6))))) ;;; (1 2 3 4 5 6) - conceptually it just removes all of the parens, no matter how deeply nested a list is
   (flatten-2 (flatten (list 1 2 3 nil 4 5)))             ;;; (1 2 3 4 5) - flatten removes nil because nil is an empty list

  

   
   
   )

  
   
)


