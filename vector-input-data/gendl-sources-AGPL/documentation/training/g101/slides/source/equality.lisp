;;
;; Copyright 2002, 2009 Genworks International
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

(in-package :training-g101)

(define-object equality (slide-show-leaf)
  
  :computed-slots
  ((strings-for-display "Equality and Comparison" :uncached)
  
   (slide-data 
    `((:title 
       "Generic equality predicates"
       :bullet-points
       ((:description
	 "Common Lisp defines 4 generic equality predicates - EQ, EQL, EQUAL and EQUALP. They return True if 2 objects being compared are equivalent, False outherwise")))
       
	(:title 
	 "EQ equality predicate"
	 :bullet-points
	 ((:description
	 "EQ test for object identity, but the result can be dependant on how the particular LISP is implemented, and may give unpredictable results"
	 :examples
	 ((:code (eq 1 1)
	   :return-value T)
	  (:code (eq 1 1.0)
	   :return-value NIL)
	  (:code (setq x "a"))
	  (:code (eq x x)
	   :return-value T)
	  (:code (eq "a" "a")
	   :return-value NIL)
	  (:code (eq (list 1 2) (list 1 2))
	   :return-value NIL))))
	(:description
	 "Best advice is probably to steer clear of EQ"))
       
	(:title 
	 "EQL equality predicate"
	 :bullet-points
	 ((:description
	 "EQL is like EQ except it guarantees to consider 2 objects of the same class representing the same numeric or character value to be the same"
	 :examples
	 ((:code (eql 1 1)
	  :return-value T)
	  (:code (eql 1 1.0)
	  :return-value NIL)
	  (:code (eql x x)
	  :return-value T)
	  (:code (eql "a" "a")
	  :return-value NIL)
	  (:code (eql (list 1 2) (list 1 2))
	  :return-value NIL)))))

	(:title 
	 "EQUAL equality predicate"
	 :bullet-points
	 ((:description
	 "EQUAL has a less discriminating notion of equivalence than EQL, which may allow different objects to be considered equivalent. Particulary useful it can be used to consider equivalence of lists and strings"
	 :examples
	((:code (equal 1 1)
	   :return-value T)
	  (:code (equal 1 1.0)
	   :return-value  NIL)
	  (:code (equal x x)
	   :return-value  T)
	  (:code (equal "a" "a")
	   :return-value  T)
	  (:code (equal (list 1 2) (list 1 2))
	   :return-value  T)
	  (:code (equal (list 1 2) (list 1 2.0))
	   :return-value  NIL)))))
	
	(:title 
	 "EQUALP equality predicate"
	 :bullet-points
	 ((:description
	 "EQUALP is similar to EQUAL, but even less discriminating. Numbers are EQUALP if they represent the same mathematical value. Characters are equivalent if they differ only in case. Lists which have EQUALP elements are considered equivalent"
	 :examples
	 ((:code (equalp 1 1)
	   :return-value T)
	  (:code (equalp 1 1.0)
	   :return-value  T)
	  (:code (equalp x x)
	   :return-value  T)
	  (:code (equalp "a" "a")
	   :return-value  T)
	  (:code (equapl (list 1 2) (list 1 2))
	   :return-value  T)
	  (:code (equalp (list 1 2) (list 1 2.0))
	   :return-value  T)))))

	(:title 
       "Generic equality predicates Summary"
       :bullet-points
       ((:description "4 predicates, EQ, EQL, EQUAL, EQUALP")
	(:description "EQ can be different depending on LISP implementations, some of the return values from EQL may be surprising")
	(:description "EQUAL and EQUALP differ from EQL in their level of discrimination on what is considered to be equivalent"
	(:description
	 "The takaway message is be careful with equality predicates and ensure you use the one appropriate for the equality comparison you are making" ))))
	 
       
	

     
      (:title "Numeric Comparison: ``=''"
	      :bullet-points 
	      ((:description "Any number of arguments")
	       (:description "Returns <i>T</i> if difference among arguments is Zero")
	       (:description "More than two args same as conjunction"
			     :examples ((:code (= 3 3.0 3.00))
					(:code (and (= 3 3.0) (= 3.0 3.00)))))
	       (:description "Do not use ``='' for floating-point numbers:"
			     :examples ((:code (= 1.599999999 1.6)
					       :return-value nil)))
	       (:description "Check the magnitude of the difference or GDL defines ``near-to?'' for this purpose:"
			     :examples ((:code (near-to? 1.555555559 1.6)
					       :return-value t)))))
     

      (:title "Numeric Comparison, cont'd: ``&gt;,'' ``&lt;,'' ``&lt;=,'' ``&gt;=,'' and ``/=''"
	      :bullet-points
	      ((:description "Any number of arguments")
	       (:description "More than two args same as conjunction"
			     :examples ((:code (> 3 4 5))
					(:code (and (> 3 4) (> 4 5)))))))

	
	(:title "Character Comparison: ``char=''"
	 :bullet-points 
		((:description "Case sensitive equivalent to the numeric =")
		 (:description "As with =, can take any number of arguments and returns true if they are all the same character")
		 (:description "Case-insensitive version is char-equal")
		 (:description "Obvious, but character comparison is for single characters!")))

	(:title "String Comparison: ``string=''"
	 :bullet-points
		((:description "String comparison follows same naming convention as character comparison")
		 (:description "Unlike numeric an character comparison, string comparison is only for 2 strings")
		 (:description "string= is the case ensitive version, string-equal is case insensitive")
		 (:description "Applied to a string, string-equal and equalp will give the same result")
		 (:description "However string= and string-equal allow the comparison to be restricted to sub-strings by specifying :start1, :end1, start2 and end2 keyword arguments"
		  :examples ((:code (string= "genworks" "GenworkS")
			     :return-value NIL)
			     (:code(string-equal "genworks" "GenworkS")
			     :return-value T)
			     (:code (string= "genworks" "GenworkS" :start1 1 :end1 6 :start2 1 :end2 6)
			     :return-value T)))))))))

