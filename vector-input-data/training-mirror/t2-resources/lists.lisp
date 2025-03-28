(in-package :gdl-user)

(define-object lists (base-object)

  :computed-slots
  ((my-number-list (list 1 2 3 4))
   (my-plist (list :uk "London" :france "Paris" :belgium "Brussels"))
   
   ;; Length of a list
   (length-1 (length (the my-number-list)))    ;;; 4
   (length-2 (length nil))                     ;;; 0
   

   ;; referencing in a list (from welcome-to-lisp)
    ;; accessing the first element of a list
   (access-first-1 (first (the my-number-list)))    ;;; 1
   (access-first-2 (car (the my-number-list)))      ;;; 1
   (access-first-3 (nth 0 (the my-number-list)))    ;;; 1

   ;; accessing the second element of a list
   (access-second-1 (second (the my-number-list)))     ;;; 2
   (access-second-2 (car (cdr (the my-number-list))))  ;;; 2
   (access-second-3 (cadr (the my-number-list)))       ;;; 2
   (access-second-4 (nth 1 (the my-number-list)))      ;;; 2

   ;; accessing all but the first element in a list
   (access-rest-1 (cdr (the my-number-list)))          ;;; (2 3 4))
   (access-rest-2 (rest (the my-number-list)))         ;;; (2 3 4))

   ;; accesing the last element of a list
   (access-last-1 (car (last (the my-number-list))))                               ;;; 4 !! caution - last returns a list !!
   (access-last-2 (nth (- (length (the my-number-list)) 1) (the my-number-list)))  ;;; 4
   (access-last-3 (car (reverse (the my-number-list))))                            ;;; 4
   (access-last-4 (lastcar (the my-number-list)))                                  ;;; 4

   ;; access all but the last element of the list
   (access-but-last (butlast (the my-number-list)))   ;;; (1 2 3 )

   ;; appending lists
   (add-1 (append  (the my-number-list) (the my-string-list))) ;;; (1 2 3 4 "peter" "paul" "mike" "john")

   (add-2 (append (the my-number-list) (list 1)))	      ;;; (1 2 3 4 1)
   (add-3 (append (the my-number-list) 5))                    ;;; (1 2 3 4 . 5) if the first argument is a list this generates a dotted list.
   (add-4 (ignore-errors (append 0 (the my-number-list))))    ;;; if the first argument isn't a list append will error
                                                              ;;; NIL
                                                              ;;; #<TYPE-ERROR #x210376AEED>
   
   (add-5 (cons 0 (the my-number-list))) ;; (0 1 2 3 4) if the second argument is a list cons will return a list
   (add-6 (cons (the my-number-list) 5)) ;; ((1 2 3 4) . 5) note the different behaviour to append

   ;; plists
   (plist-1 (getf (the my-plist) :uk))                ;;; "London"
   )
  )
