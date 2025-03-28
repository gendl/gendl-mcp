;;
;; Copyright 2012 Genworks International
;;
;; This source file is part of the General-purpose Declarative
;; Language project (GDL).
;;
;; This source file contains free software: you can redistribute it
;; and/or modify it under the terms of the GNU Affero General Public
;; License as published by the Free Software Foundation, either
;; version 3 of the License, or (at your option) any later version.
;; 
;; This source file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Affero General Public License for more details.
;; 
;; You should have received a copy of the GNU Affero General Public
;; License along with this source file.  If not, see
;; <http://www.gnu.org/licenses/>.
;; 

(in-package :gdl)

(defmacro define-object-amendment (name mixin-list &rest spec-plist)
  "Supplements or alters an existing GDL object definition. Syntax is similar to that for <tt>define-object</tt>.
Note that there is currently no way to undefine messages defined with this macro, other than redefining the 
original object or restarting the GDL session. Support for surgically removing messages will be added in a
future GenDL release."  
  `(%define-object-amendment% ,name ,mixin-list ,@(merge-common-keys spec-plist)))


(defmacro %define-object-amendment% (name mixin-list 
                                     &key input-slots 
                                       computed-slots
                                       objects
                                       hidden-objects
                                       functions
                                       methods
                                       documentation
                                       trickle-down-slots
                                       query-slots)

  (check-syntax name input-slots computed-slots objects hidden-objects 
                functions methods documentation trickle-down-slots query-slots)

  (clear-metaclass-caches)

  (with-gdl-message-symbols
      ()
      (let ((message-ht (make-hash-table :size (length messages))))
        (dolist (message messages)
          (push message (gethash message message-ht)))
        (let ((duplicates (let (duplicates)
                            (maphash #'(lambda(key val)
                                         (if (consp (rest val)) 
                                             (push key duplicates))) message-ht) duplicates)))
          (let ((duplicates (remove-if #'(lambda(dup) (member dup method-syms)) duplicates)))
            (when duplicates
              (error "duplicate slot name~p: ~{~a~^, ~}" 
                     (length duplicates) (sort duplicates #'string-lessp :key #'symbol-name))))))


      (let* ((class (find-class name))
             (new-messages (remove-duplicates (append (messages class) messages)))
             (new-computed-slots (remove-duplicates (append (computed-slots class) computed-slot-syms)))
             (new-required-input-slots (remove-duplicates (append (required-input-slots class) required-input-slot-syms)))
             (new-optional-input-slots (remove-duplicates (append (optional-input-slots class) optional-input-slot-syms)))
             (new-defaulted-input-slots (remove-duplicates (append (defaulted-input-slots class) defaulted-input-slot-syms)))
             (new-query-slots (remove-duplicates (append (query-slots class) query-slot-syms)))
             (new-settable-computed-slots 
               (remove-duplicates (append (settable-computed-slots class) settable-computed-slot-syms)))
             (new-uncached-computed-slots 
               (remove-duplicates (append (uncached-computed-slots class) uncached-computed-slot-syms)))
             (new-settable-optional-input-slots
               (remove-duplicates (append (settable-optional-input-slots class) settable-input-slot-syms)))
             (new-settable-defaulted-input-slots 
               (remove-duplicates (append (settable-defaulted-input-slots class) settable-defaulted-input-slot-syms)))
             (new-functions (remove-duplicates (append (functions class) function-syms)))
             (new-methods (remove-duplicates (append (methods class) method-syms)))
             (new-cached-methods (remove-duplicates (append (cached-methods class) cached-method-syms)))
             (new-cached-functions (remove-duplicates (append (cached-functions class) cached-function-syms)))
             (new-objects (remove-duplicates (append (objects class) object-syms)))
             (new-quantified-objects (remove-duplicates (append (quantified-objects class) quantified-object-syms)))
             (new-hidden-objects (remove-duplicates (append (hidden-objects class) hidden-object-syms)))
             (new-quantified-hidden-objects 
               (remove-duplicates (append (quantified-hidden-objects class) quantified-hidden-object-syms)))
             (new-trickle-down-slots
               (remove-duplicates 
                (append (trickle-down-slots class)
                        (append object-syms quantified-object-syms hidden-object-syms 
                                quantified-hidden-object-syms trickle-down-slot-syms))))
             (new-settable-slots
               (remove-duplicates 
	        (append (settable-slots class)
		        (append required-input-slot-syms settable-input-slot-syms 
			        settable-defaulted-input-slot-syms settable-computed-slot-syms))))
           
             (supers (remove-duplicates (append (glisp:direct-superclass-names class) mixin-list)
                                        :from-end t))

	     (class (gensym)))
           
      
        (reserved-word-warning-or-error name messages supers)

        (print-variables new-computed-slots)

        `(let ((,class (find-class ',name)))
	   ,(when (and *compile-documentation-database?* documentation)
              `(when *load-documentation-database?*
	         (setf (gdl-documentation ,class) ',documentation)))
           
           (setf (messages ,class) ',new-messages)
           (setf (required-input-slots ,class)  ',new-required-input-slots)
           (setf (optional-input-slots ,class)  ',new-optional-input-slots)
           (setf (defaulted-input-slots ,class)  ',new-defaulted-input-slots)
           (setf (computed-slots ,class) ',new-computed-slots)
           (setf (query-slots ,class) ',new-query-slots)
           (setf (settable-computed-slots ,class) ',new-settable-computed-slots)
           (setf (uncached-computed-slots ,class) ',new-uncached-computed-slots)
           (setf (settable-optional-input-slots ,class) ',new-settable-optional-input-slots)
           (setf (settable-defaulted-input-slots ,class) ',new-settable-defaulted-input-slots)
           (setf (functions ,class)  ',new-functions)
           (setf (methods ,class)  ',new-methods)
           (setf (cached-functions ,class) ',new-cached-functions)
           (setf (cached-methods ,class) ',new-cached-methods)
           (setf (objects ,class)  ',new-objects)
           (setf (quantified-objects ,class) ',new-quantified-objects)
           (setf (hidden-objects ,class)  ',new-hidden-objects)
           (setf (quantified-hidden-objects ,class) ',new-quantified-hidden-objects)
           (setf (trickle-down-slots ,class) ',new-trickle-down-slots)
           (setf (settable-slots ,class) ',new-settable-slots)
         
           (eval-when (:compile-toplevel :load-toplevel :execute) (glisp:begin-redefinitions-ok))

           ,(let ((slots-body (remove-duplicates 
                               (append (make-standard-slots)
                                       (make-accessible-slots new-computed-slots)
                                       (make-accessible-slots new-query-slots)
                                       (make-accessible-slots new-settable-computed-slots)
                                       (make-accessible-slots new-uncached-computed-slots)
                                       (make-accessible-slots new-objects)
                                       (make-accessible-slots new-quantified-objects)
                                       (make-accessible-slots new-hidden-objects)
                                       (make-accessible-slots new-quantified-hidden-objects)
                                       (make-accessible-slots new-cached-functions)
                                       (make-accessible-slots new-cached-methods)
                                       (make-accessible-slots new-required-input-slots :inputs? t)
                                       (make-accessible-slots new-optional-input-slots :inputs? t)
                                       (make-accessible-slots new-settable-optional-input-slots :inputs? t)
                                       (make-accessible-slots new-defaulted-input-slots :inputs? t)
                                       (make-accessible-slots new-settable-defaulted-input-slots :inputs? t)) :key #'first)))

              `(defclass ,name ,supers (,@slots-body) (:metaclass gdl-class)))
           

           (eval-when (:compile-toplevel  :load-toplevel :execute) (glisp:end-redefinitions-ok))
       
           
           ;;
           ;; FLAG -- make sure these next two are ok. 
           ;;
           ,@(message-generics (set-difference messages (append method-syms cached-method-syms)))
                 
           ,(input-slots-generics (append (group-remark-strings (remove-if-not #'(lambda(item)
                                                                                   (or (symbolp item) (stringp item)))
                                                                               input-slots))
                                          (remove-if-not #'consp input-slots)))


           ;; FLAG -- consider pre-cooking these expression lists
           ;;
       
           ,@(input-slots-section name (group-remark-strings (remove-if-not #'(lambda(item)
                                                                                (or (symbolp item) (stringp item)))
                                                                            input-slots)))

       
           ,@(optional-input-slots-section 
              name (remove-if-not #'(lambda(slot) (and (consp slot) (null (rest (rest (strip-strings slot))))))
                                  input-slots))

           ,@(optional-input-slots-section 
              name (remove-if-not 
                    #'(lambda(slot) 
                        (and (consp slot) 
                             (member :settable (rest (rest (strip-strings slot))))
                             (not (member :defaulting (rest (rest (strip-strings slot)))))))
                    input-slots))

           ,@(optional-input-slots-section 
              name (remove-if-not #'(lambda(slot) 
                                      (and (consp slot) 
                                           (member :defaulting (rest (rest (strip-strings slot))))
                                           (not (member :settable (rest (rest (strip-strings slot)))))))
                                  input-slots) t)
       
           ,@(optional-input-slots-section
              name (remove-if-not 
                    #'(lambda(slot) 
                        (and (consp slot) 
                             (member :settable (rest (rest (strip-strings slot))))
                             (member :defaulting (rest (rest (strip-strings slot))))))
                    input-slots) t)
              
           ,@(computed-slots-section 
              name (remove-if-not #'(lambda(slot) (and (consp slot) (null (rest (rest (strip-strings slot))))))
                                  computed-slots))
           
           ,@(functions-section 
              name (mapcar #'(lambda(slot)
                               (if (stringp (first slot))
                                   (list (first slot) (second slot) nil (third slot))
                                   (list (first slot) nil (second slot))))
                           (remove-if-not #'(lambda(slot) (and (consp slot) (eql (first (rest (rest (strip-strings slot))))
                                                                                 :uncached)))
                                          computed-slots)))
           
           ,@(computed-slots-section 
              name (remove-if-not #'(lambda(slot)
                                      (and (consp slot) (eql (first (rest (rest (strip-strings slot)))) 
                                                             :settable)))
                                  computed-slots))
       
           ,@(computed-slots-section name query-slots :query? t)
       
           ,@(objects-section name objects) ,@(objects-section name hidden-objects) 
       
           ,@(functions-section name functions)
       
           ,@(methods-section name methods)
       
           ,@(trickle-down-slots-section 
              (append object-syms quantified-object-syms hidden-object-syms 
                      quantified-hidden-object-syms trickle-down-slot-syms))

	   #+nil
           (defmethod gdl-rule::%object-keywords%
               ((,self-arg ,name))
             (remove-duplicates
              (append ',object-syms ',quantified-object-syms
                      (when (next-method-p) (call-next-method)))))

           
	   (eval-when (:load-toplevel)
	     (let ((class (find-class ',name)))
	       (dolist (super (cons class (all-superclasses class)))
	         (let ((ht (or (gethash super *class-used-by-hash*)
			       (setf (gethash super *class-used-by-hash*)
				     (glisp:make-sans-value-hash-table)))))
		   (setf (gethash class ht) t)))
	       (maphash #'(lambda(key value)
			    (declare (ignore value))
			    (setf (gethash key *message-list-hashes*) nil))
		        (gethash class *class-used-by-hash*))))


	   (clear-metaclass-caches)))))

