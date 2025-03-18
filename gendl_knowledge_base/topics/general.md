# Gendl Documentation - general

## usage.txt (chunk 1/34)
Source: usage.txt
Type: usage_guide

```
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


         Basic GDL Overview and Syntax
         =============================

This file contains basic information and usage instructions for the
```

---

## usage.txt (chunk 2/34)
Source: usage.txt
Type: usage_guide

```
;; <http://www.gnu.org/licenses/>.
;; 


         Basic GDL Overview and Syntax
         =============================

This file contains basic information and usage instructions for the
base General-purpose Declarative Language System.

For further documentation and updates, please visit
http://www.genworks.com or contact us at info@genworks.com
or 248-910-0912.

The GDL product is a commercially available KBE system, and the core
GDL language is a proposed standard for a vendor-neutral KBE
language. 


Core GDL Syntax
==================

GDL is based on and is a superset of ANSI Common Lisp.

1 define-package
================

The macro gdl:define-package is used to set up a new working package
in GDL.

Example:

   (gdl:define-package :gdl-user)

The :gdl-user package is an empty, pre-defined package for your use if
you do not wish to make a new package just for scratch work.

For real projects it is recommended that you make and work in your own
GDL package.
```

---

## usage.txt (chunk 3/34)
Source: usage.txt
Type: usage_guide

```
-defined package for your use if
you do not wish to make a new package just for scratch work.

For real projects it is recommended that you make and work in your own
GDL package.


Notes for advanced users:

  Packages defined with gdl:define-package will implicitly :use the
  GDL package and the Common-Lisp package, so you will have access to
  all exported symbols in these packages without prefixing them with
  their package name.

  You may extend this behavior, by calling gdl:define-package and
  adding additional packages to use with (:use ...).  For example, if
  you want to work in a package with access to GDL exported symbols,
  Common Lisp exported symbols, and symbols from the Surf (NURBS
  surfaces and brep solids) package, you could set it up as follows:

    (gdl:define-package :my-gdl-user (:use :gdl :surf))


2 define-object
===============

define-object is the basic macro for defining objects (i.e. creating
classes) in GDL.
```

---

## usage.txt (chunk 4/34)
Source: usage.txt
Type: usage_guide

```
e :my-gdl-user (:use :gdl :surf))


2 define-object
===============

define-object is the basic macro for defining objects (i.e. creating
classes) in GDL. A GDL object definition is a superset of a CLOS
Standard Class.

Basic syntax of Define-Object is 

 (define-object <class-name> <mixin-list> <specification-plist>)

<class-name> is any non-keyword symbol. A CLOS Standard Class will be
generated for this symbol, so any name you use will override a
defclass if one is already defined with the same name.

<mixin-list> is a list of other class-names from which this object
will inherit. It maps directly into the CLOS mixin list.

Note that the standard mixin GDL:Vanilla-Mixin gets mixed in
automatically with any GDL object and carries some of the basic GDL
functionality (messages).

<spec-plist> is a plist made up of pairs made from special keywords
and expression lists. The special keywords currently supported are the
following, and each is documented in the respective section of this
```

---

## usage.txt (chunk 5/34)
Source: usage.txt
Type: usage_guide

```
st> is a plist made up of pairs made from special keywords
and expression lists. The special keywords currently supported are the
following, and each is documented in the respective section of this
file: :input-slots, :computed-slots, :trickle-down-slots, :objects,
:hidden-objects, :functions, and :methods.


2.1 :input-slots
================

:input-slots are made up of a list, each of whose elements is either a
symbol or a list expression whose first element is a symbol. In either
case, the symbol represents a value which can be supplied either

 (a) into the toplevel object of an object hierarchy, at object instantiation
     (see (1.5), make-object, below)

 (b) into a child object, using a :objects specification (see (1.6), :objects,
     below)

Inputs are specified either as a simple symbol (which may, but need
not be, a keyword symbol), or as an expression whose first is a symbol
and whose second is an expression returning a value which will be the
```

---

## usage.txt (chunk 6/34)
Source: usage.txt
Type: usage_guide

```
e symbol (which may, but need
not be, a keyword symbol), or as an expression whose first is a symbol
and whose second is an expression returning a value which will be the
default value for the input slot.

Optionally, a third item can be supplied, the keyword :defaulting,
which indicates that if a slot by this name is contained in any
ancestor object's list of :trickle-down-slots, the value from the
ancestor will take precedence over the local default expression.

Example 1:

 (define-object person (base-object)

   :input-slots
   (first-name last-name age image-url))

In this example, the slots first-name, last-name, age, and image-url
are all defined, with no default expressions. This means that for the
object to answer these messages, these slots must be specified at the
time of object instantiation.

Example 2:

 (define-object person (base-object)

   :input-slots
   (first-name last-name age 
    (image-url "http://localhost:9000/images/")))
```

---

## usage.txt (chunk 7/34)
Source: usage.txt
Type: usage_guide

```
instantiation.

Example 2:

 (define-object person (base-object)

   :input-slots
   (first-name last-name age 
    (image-url "http://localhost:9000/images/")))

In this example, first-name, last-name, and age are all defined with
no default expressions, but image-url has the default expression
"http://localhost:9000/images/." This means that if nothing is
specified for image-url at object instantiation time, the image-url
message will return "http://localhost:9000/images/."


Example 3:

 (define-object person (base-object)

   :input-slots
   (first-name last-name age 
    (image-url "http://localhost:9000/images/" :defaulting)))


This example is the same as Example 2, with the exception that if
image-url is included in an ancestor object (see below for discussion
of object hierarchies) as a :trickle-down-slot, the slot's value from
that ancestor will take precedence over the local default expression
of "http://localhost:9000/images/."



2.2 computed-slots
==================
```

---

## usage.txt (chunk 8/34)
Source: usage.txt
Type: usage_guide

```
s) as a :trickle-down-slot, the slot's value from
that ancestor will take precedence over the local default expression
of "http://localhost:9000/images/."



2.2 computed-slots
==================

computed-slots are messages which are generally computed based on
their default expression.

computed-slots will only be computed when called ("demanded"), then
their values will be cached in memory. Only if another slot on which
they depend becomes modified will they become unbound, then their
values will be recomputed from their expressions when demanded.

The referencing macro ``the'' is used to refer to the values of
messages within the current object (named implicitly with the variable
"self"), or, through reference-chaining (see (1.6), :objects, below),
the values of messages in other object instances.

Notes for Advanced users: 

  In packages created with gdl:define-package, the Common Lisp symbol
  ``the'' is shadowed by gdl:the. If you wish to access
```

---

## usage.txt (chunk 9/34)
Source: usage.txt
Type: usage_guide

```
t instances.

Notes for Advanced users: 

  In packages created with gdl:define-package, the Common Lisp symbol
  ``the'' is shadowed by gdl:the. If you wish to access
  common-lisp:the, use the explicit package prefix, e.g. ``cl:the.''

Example 1:

  (define-object person (base-object)

   :input-slots
   (first-name
    last-name 
    age
    image-url)

   :computed-slots
   ((full-name (concatenate 'string (the first-name) " " (the  last-name)))))

In this example, the message full-name is always computed strictly
based on its default expression, which concatenates (the first-name)
and (the last-name).


Example 2:

  (define-object person (base-object)

   :input-slots
   (first-name
    last-name 
    age
    image-url)

   :computed-slots
   ((full-name (concatenate 'string (the first-name) " " (the  last-name)) :settable)))

In this example, the message full-name is by default computed based on
its default expression, which concatenates (the first-name) and (the
last-name).
```

---

## usage.txt (chunk 10/34)
Source: usage.txt
Type: usage_guide

```
st-name) " " (the  last-name)) :settable)))

In this example, the message full-name is by default computed based on
its default expression, which concatenates (the first-name) and (the
last-name). However, because it is :settable, its value may be altered
procedurally at runtime (see "setting slot values" below)

 

2.3 :objects and :hidden-objects
================================

:objects is used to specify a list of Instance specifications, where
each instance is considered to be a ``child'' object of the current
object :hidden-objects serves the same purpose and has the same syntax,
but hidden objects are considered ``hidden-children'' rather than
``children'' (so they are not returned by a call to (the children),
for example).

Inputs to each object are specified as a plist of inputs and
value expressions, spliced in after the objects's name and type
specification:

 Examples
 ========
> (define-object city (base-object)
   
   :computed-slots
```

---

## usage.txt (chunk 11/34)
Source: usage.txt
Type: usage_guide

```
and
value expressions, spliced in after the objects's name and type
specification:

 Examples
 ========
> (define-object city (base-object)
   
   :computed-slots
   ((total-water-usage (+ (the hotel water-usage)
                          (the bank water-usage))))
   :objects
   ((hotel :type 'hotel
           :size :large)
    (bank  :type 'bank
           :size :medium)))

--> CITY
     

>  (define-object hotel (base-object)
     :input-slots
     (size)

     :computed-slots
     ((water-usage (ecase (the size)
                     (:small 10)
                     (:medium 20)
                     (:large 30)))))
--> HOTEL


>  (define-object bank (base-object)
     :input-slots
     (size)
  
     :computed-slots
     ((water-usage (ecase (the size)
                     (:small 2)
                     (:medium 3)
                     (:large 4)))))

--> BANK

  > (setq self (make-object 'city))
--> #<CITY @ #x20933922>

  > (the total-water-usage)
--> 33
```

---

## usage.txt (chunk 12/34)
Source: usage.txt
Type: usage_guide

```
(:medium 3)
                     (:large 4)))))

--> BANK

  > (setq self (make-object 'city))
--> #<CITY @ #x20933922>

  > (the total-water-usage)
--> 33

The special message children will return a list of all the child
instances in a object:

  > (the children)
--> (#<HOTEL @ #x209350ca> #<BANK @ #x2093b62a>)



2.4 Sequences of Objects
========================

2.4.1 Fixed-size Sequences
==========================

Objects may be specified as a fixed-length sequence, analogous to a
single-dimensional array. Although we call this a fixed-length
sequence, the length can change if something it depends on becomes
modified. But if this happens, the entire sequence will have to be
recomputed.

Each member of the sequence will automatically answer an :index
message, which starts at 0 goes up to one less than the total number
of elements in the sequence.

Note that the referencing macro ``the-child'' may be used to reference
```

---

## usage.txt (chunk 13/34)
Source: usage.txt
Type: usage_guide

```
arts at 0 goes up to one less than the total number
of elements in the sequence.

Note that the referencing macro ``the-child'' may be used to reference
into the current child objects (in sequenced objects as well as in
normal non-sequenced objects). This can be useful for sequenced
objects, in order to access the :index of the current member.

Example

(defparameter *presidents-data*
    '((:name 
       "Carter"
       :term 1976)
      (:name "Reagan"
       :term 1980)
      (:name "Clinton"
       :term 1990)))
       
(define-object presidents-container (base-object)
  :input-slots 
  ((data *presidents-data*))
  
  :objects
  ((presidents :type 'president
	       :sequence (:size (length (the data)))
	       :name (getf (nth (the-child index)
				(the data)) 
			   :name)
	       :term (getf (nth (the-child index)
				(the data)) 
			   :term))))


(define-object president (base-object)
 :input-slots
 (name term))
```

---

## usage.txt (chunk 14/34)
Source: usage.txt
Type: usage_guide

```
erm (getf (nth (the-child index)
				(the data)) 
			   :term))))


(define-object president (base-object)
 :input-slots
 (name term))


For convenience, the special objects keyword :Parameters may be used to
pass an actual plist into a child instance instead of having to refer
to the individual parameters.

Example:

(define-object presidents-container (base-object)
  :input-slots
  ((data *presidents-data*))
  
  :objects
  ((presidents :type 'president
	       :sequence (:size (length (the data)))
    	       :parameters (nth (the-child index)
		  		(the data)))))


The members of quantified set are accessed like functions, by wrapping
extra parentheses and including the index number as the argument.

Example:

>   (setq self (make-object 'presidents-container))
--> #<PRESIDENTS-CONTAINER @ #x207441e2>

>   (the (presidents 0) name)
--> "Carter"


The quantified set can handle certain pre-defined messages,
including  last and first.

Example:

>   (the (presidents last))
```

---

## usage.txt (chunk 15/34)
Source: usage.txt
Type: usage_guide

```
ER @ #x207441e2>

>   (the (presidents 0) name)
--> "Carter"


The quantified set can handle certain pre-defined messages,
including  last and first.

Example:

>   (the (presidents last))
--> #<PRESIDENT @ #x2075061a>


Members of a quantified set can also handle the messages
 previous,  next, first?, and last?.


The types of a quantified set can also be quantified, by
supplying them as a list and using the keyword :sequence
in the :type specification, e.g.

(define-object stuff (base-object)
  :computed-slots
  ((child-types (list 'boy 'girl 'man 'woman)))

  :objects
  ((people :type (:sequence (the child-types))
           :sequence (:size (length (the child-types))))))


If the expression returning the :sequence of types, or of the :size,
of a fixed-size sequence becomes modified, or anything they depend on
becomes modified, then the entire sequence will become unbound and
will have to be recomputed the next time it is demanded.

2.4.2 Variable-size Sequences
```

---

## usage.txt (chunk 16/34)
Source: usage.txt
Type: usage_guide

```
anything they depend on
becomes modified, then the entire sequence will become unbound and
will have to be recomputed the next time it is demanded.

2.4.2 Variable-size Sequences
=============================

Objects may be specified as a variable-length sequence, analogous to a
list. These are similar to fixed-length sequences, but the syntax is:

  :sequence (:indices <list-of-indices>)

where the <list-of-indices> is an initial list of indices. The indices
are usually integers, but can be any object which matches with eql
(e.g. keyword symbols).

For inserting and deleting members of a variable-length sequence,
please see the reference documentation on variable-sequence.


2.5 :functions
==============

Functions are uncached methods on the object, which discriminate only
on the type of the object. They are defined with a normal
(non-specialized) lambda list, so they do not discriminate on the
types of their arguments other than the implicit ``self'' argument.
```

---

## usage.txt (chunk 17/34)
Source: usage.txt
Type: usage_guide

```
of the object. They are defined with a normal
(non-specialized) lambda list, so they do not discriminate on the
types of their arguments other than the implicit ``self'' argument.

Functions are called in a normal reference chain but their name is
wrapped in parentheses and the lambda-list is spliced on after the
name, within the parentheses.

Example:
=======

(define-object hotel (base-object)
  :input-slots
  (room-rate)
  
  :functions
  ((total-cost
    (number-of-nights)
    (* (the room-rate) number-of-nights))))


>   (setq self (make-object 'hotel :room-rate 100))
--> #<HOTEL @ #x2094f502> 

>   (the (total-cost 7))
--> 700

>   (the (total-cost 10))
--> 1000


2.5 :methods
==============

Methods are identical to GDL Functions, with the additional capability
of specializing on their argument signature (i.e. the combination of
types of the arguments) in addition to the implicit ``self'' argument
(as with standard CLOS methods).


2.6 :trickle-down-slots
```

---

## usage.txt (chunk 18/34)
Source: usage.txt
Type: usage_guide

```
eir argument signature (i.e. the combination of
types of the arguments) in addition to the implicit ``self'' argument
(as with standard CLOS methods).


2.6 :trickle-down-slots
=======================

:trickle-down-slots are a list of symbols naming other messages
(:input-slots, :computed-slots, etc.) in the object which will
automatically be available in any descendant (e.g. child, grandchild,
etc.) instances, unless overridden in the descendant instance (e.g. by
being defined as an :input-slot, :computed-slot, etc, in the
descendant instance).

Example:

(define-object person (base-object)
  :input-slots
  (social-security-number)
  
  :trickle-down-slots
  (social-security-number)
  
  :objects
  ((irs-records       :type 'irs-records)
   (state-tax-returns :type 'state-tax-returns)
   (fbi-file          :type 'fbi-file)
   (interpol-file     :type 'interpol-file)))

In the above object definition, the message social-security-number
```

---

## usage.txt (chunk 19/34)
Source: usage.txt
Type: usage_guide

```
(fbi-file          :type 'fbi-file)
   (interpol-file     :type 'interpol-file)))

In the above object definition, the message social-security-number
will be automatically available in the instances irs-records,
state-tax-returns, fbi-file, and interpol-file, unless otherwise
defined in those respective objects.

NOTE: :objects and :hidden-objects are automatically trickle-down.


2.7 Settable Slots
==================

Settable slots are just like normal slots, but their values can be
programmatically modified using the special object function :set-slot!.

Any other slots depending on them (directly or indirectly) will then
become unbound and be recomputed the next time they are demanded.

Example:

> (define-object container (base-object)
    :computed-slots
    ((name "Pristine" :settable)
     (full-name (string-append (the :name) " Container") :settable)))

>   (setq self (make-object 'container))
--> #<CONTAINER @ #x209495c2>

>   (the full-name)
--> "Pristine Container"
```

---

## usage.txt (chunk 20/34)
Source: usage.txt
Type: usage_guide

```
le)
     (full-name (string-append (the :name) " Container") :settable)))

>   (setq self (make-object 'container))
--> #<CONTAINER @ #x209495c2>

>   (the full-name)
--> "Pristine Container"

>   (the (set-slot! name "Tainted"))
--> "Tainted"

>   (the full-name)
--> "Tainted Container"

Both :computed-slots and :input-slots may be specified as :settable
(this includes :input-slots which are also specified as :defaulting).



3 Make-Object
=============

The basic constructor for GDL objects is ``make-object.''

This maps into a call to the Common Lisp function ``make-instance,''
with some extra operations to support the GDL machinery.

Keyword symbols are used to tag input values when passed into an
object in a call to make-object:

  Example 1:
   
   > (setq myobject (make-object 'person :first-name "Albert" :last-name "Einstein"))

 --> #<PERSON @ #x209274ee>

Toplevel inputs can also be specified by applying #'make-object to a
plist containing the inputs:

  Example 2:
```

---

## usage.txt (chunk 21/34)
Source: usage.txt
Type: usage_guide

```
on :first-name "Albert" :last-name "Einstein"))

 --> #<PERSON @ #x209274ee>

Toplevel inputs can also be specified by applying #'make-object to a
plist containing the inputs:

  Example 2:

   > (setq myobject (apply #'make-object 'person 
                           (list :first-name "Albert" 
                                 :last-name "Einstein")))
 --> #<PERSON @ #x209274fa>


4 the-object
============

You can send messages to individual object instances using the macro
``the-object:''

Example:

   > (the-object myobject full-name)
 --> "Albert Einstein"

The-object takes as its first argument an expression which returns an
object (i.e. instance), followed by a symbol naming a message returned
by that object. The symbol is immune to Lisp package, so a keyword
symbol may be used, but this is not a requirement. As we will see
later, the-object actually can take any number of symbols,
representing a reference chain down through an object hierarcy (see
"object hierarchies" below).
```

---

## usage.txt (chunk 22/34)
Source: usage.txt
Type: usage_guide

```
this is not a requirement. As we will see
later, the-object actually can take any number of symbols,
representing a reference chain down through an object hierarcy (see
"object hierarchies" below).

(The <instance>) expands to (the-object self <instance>), so you can
conveniently bind a variable named ``self'' to the result of a
make-object, then use a simple ``the'' to do referencing:

Example:

   > (setq self (apply #'make-object 'person 
                        (list :first-name "Albert" 
                              :last-name "Einstein")))
--> #<PERSON @ #x2092cc8a>


   > (the full-name)
 --> "Albert Einstein"



5 Evaluating Slot Names at Runtime
==================================

The ``evaluate'' macro can be used in cases where the message name is
not known until runtime -- it is wrapped around an expression which
returns a symbol naming a message. The symbol is immune to package, so
it may be a keyword or non-keyword symbol.

Example:
========
```

---

## usage.txt (chunk 23/34)
Source: usage.txt
Type: usage_guide

```
is wrapped around an expression which
returns a symbol naming a message. The symbol is immune to package, so
it may be a keyword or non-keyword symbol.

Example:
========

>   (setq my-key :full-name)
--> :FULL-NAME

>   (setq self (make-object 'container))
--> #<CONTAINER @ #x209495c2>

>   (the (evaluate my-key))
--> "Pristine Container"


6 Formats and Views
===================

6.1 Overview
============

The basic idea behind Formats and Views is that of providing different
perspectives on an object for the purposes of output. This concept is
something more than ``presentation methods'' as defined by CLIM. It is
more like ``presentation objects'' which contain ``presentation
methods.''

Core GDL follows the message-passing paradigm of object
orientation. You have objects which have slots, sub-objects,
functions, etc. These are all actually methods, or messages, ``on''
the object, i.e. the message passing paradigm.

Another way to look at message passing is to think that any given
```

---

## usage.txt (chunk 24/34)
Source: usage.txt
Type: usage_guide

```
ts, sub-objects,
functions, etc. These are all actually methods, or messages, ``on''
the object, i.e. the message passing paradigm.

Another way to look at message passing is to think that any given
method dispatches, or is specialized, only on a single argument, which
is the object to which it ``belongs.'' Formats and Views extend upon
this notion by allowing methods to dispatch on two arguments. The
first argument is a ``Format'' object, and the second argument is the
normal object just as with straight GDL.

Format objects are defined with ``define-format'' and instantiated
only when needed, inside the body of a ``with-format'' macro.

Methods which apply to a particular object and from the perspective of
a particular format are defined as :output-methods with
``define-lens''

6.2 define-format
=================

As its name implies, Define-Format is used to define new
formats. GDL/GWL comes with several pre-defined formats, so it is
```

---

## usage.txt (chunk 25/34)
Source: usage.txt
Type: usage_guide

```
e-format
=================

As its name implies, Define-Format is used to define new
formats. GDL/GWL comes with several pre-defined formats, so it is
likely that you will not need to define your own formats initially.

The syntax is

 (Define-Format <format-name> <mixin-list> <spec-plist>)

<Format-name> is any non-keyword symbol. A defclass will be generated
for this symbol, so any name you use will override a defclass if one
is already defined with the same name.

<Mixin-list> is a list of other format-names from which this format
will inherit. It maps directly into a CLOS mixin list.

<spec-plist> is a plist made up of pairs made from special keywords
and expression lists. Define-format in GDL currently only supports the
``:Functions'' section keyword.

6.2.1 functions
===============

Functions of a format are actual uncached methods on the format
object. They are defined with a normal (non-specialized) lambda
list. There is a variable ``stream'' dynamically bound within the body
```

---

## usage.txt (chunk 26/34)
Source: usage.txt
Type: usage_guide

```
Functions of a format are actual uncached methods on the format
object. They are defined with a normal (non-specialized) lambda
list. There is a variable ``stream'' dynamically bound within the body
of these functions, to which output is expected to be written.

Example:
========

(define-format base-format ()
  :functions
  ((a 
    (expression)
    (format stream "~a" expression))
   
   (newline-out
    ()
    (format stream "~%"))))


6.3 define-lens
===============

As its name implies, the define-lens macro is used to define a
``lens'' to a object, from the perspective of a given format. A lens
is a way of defining methods which apply to a object when ``viewed''
through the ``lens'' of a particular format. Therefore, views are
defined (and named) according to a particular object type, and a
particular format.

The Syntax is:

 (define-lens (<format-name> <object-type>) (<mixin-lists>) <spec-plist>)

<format-name> is the name of a format which must already be defined
```

---

## usage.txt (chunk 27/34)
Source: usage.txt
Type: usage_guide

```
e, and a
particular format.

The Syntax is:

 (define-lens (<format-name> <object-type>) (<mixin-lists>) <spec-plist>)

<format-name> is the name of a format which must already be defined
with define-format. <object-type> is the name of an object type which
must already be defined with define-object.

<mixin-lists> is currently unused. Inheritance for define-lens in GDL
currently simply follows the inheritance of the particular format and
object named in the define-lens. At some point more explicit inheritance
control might be added using these <mixin-lists>.

<spec-plist> is a plist made up of pairs made from special keywords
and expression lists define-lens in GDL currently only supports the
``:output-functions'' section keyword.

Output-functions are defined like normal :functions on an object,
however, in addition to sending messages to the object with normal
``the'' referencing macro, the ``write-env'' macro may also be used to
```

---

## usage.txt (chunk 28/34)
Source: usage.txt
Type: usage_guide

```
object,
however, in addition to sending messages to the object with normal
``the'' referencing macro, the ``write-env'' macro may also be used to
call :functions which are known to be defined for the associated
format.

Example:
========

(define-lens (base-format try)()
  :output-functions
  ((:summary
    ()
    (write-env (a "The value is: ") (a (the value))
	       (newline-out)
	       (a "The color of ``this'' is: ") (a (the this color))
	       (newline-out)
	       (a "The color of ``that'' is: ") (a (the (these 0) color))
	       (newline-out)))))

6.4 with-format
===============

The with-format macro sets up an environment for calling :functions of
formats (using ``write-env'' -- see below) and :output-functions of
views (using ``write-the'' and ``write-the-object'').

The syntax is:

 (With-format (<format-name> <stream-or-pathname>) 
    <body>)


<format-name> is the name of a format which has been defined with
``define-format''
```

---

## usage.txt (chunk 29/34)
Source: usage.txt
Type: usage_guide

```
ax is:

 (With-format (<format-name> <stream-or-pathname>) 
    <body>)


<format-name> is the name of a format which has been defined with
``define-format''

<stream-or-pathname> is a variable or expression which evaluates to a
stream which can accept output or to a string or pathname which can be
opened to accept output.

<body> can contain any normal Lisp expressions as well as the format
and view reference macros ``write-env,'' ``write-the-object,'' and
``write-the'' (see below).

Within <body>, the parameter ``stream'' will be dynamically bound to
the stream specified by <stream-or-pathname>, or to a file stream
opened to stream-or-pathname, if it is a string or pathname. Because
it is dynamically bound, this means any other functions or methods
called within <body> will also see the correct value of ``stream.''

6.4.1 Write-Env
===============

Write-env must be called either within the (dynamic) body of a
``with-format'' or within an :output-function of a view, and is used
```

---

## usage.txt (chunk 30/34)
Source: usage.txt
Type: usage_guide

```
correct value of ``stream.''

6.4.1 Write-Env
===============

Write-env must be called either within the (dynamic) body of a
``with-format'' or within an :output-function of a view, and is used
to invoke :functions defined on the specified format

Examples:

(define-lens (base-format try)()
  :output-functions
  ((summary
    ()
    (write-env (a "The value is: ") (a (the value))
	       (newline-out)
	       (a "The color of ``this'' is: ") (a (the this color))
	       (newline-out)
	       (a "The color of ``that'' is: ") (a (the (these 0) color))
	       (newline-out)))))

(with-format (base-writer "/tmp/try.txt")
  (write-env (a "This is a test")))


6.4.2 Write-The-Object
======================

Syntax:

(write-the-object <object> <reference-chain>)

``write-the-object'' works in similar fashion to ``the-object'' in the
sense that it handles reference chains, but the last element in the
reference chain must name a :output-function defined in a relevant
view.
```

---

## usage.txt (chunk 31/34)
Source: usage.txt
Type: usage_guide

```
ilar fashion to ``the-object'' in the
sense that it handles reference chains, but the last element in the
reference chain must name a :output-function defined in a relevant
view. ``Write-the-object'' must be called inside the (dynamic) body of
a ``with-format'' so that the effective format and stream will be
known. The :output-function indicated by the last element of the
reference chain will be invoked, which presumably will write some
output to the specified stream.

Currently the ``evaluate'' macro is not implemented in GDL to resolve
the :write-method name at runtime, so this name must be given as a
literal symbol in the compiled source.


Example:
========

(with-writer (base-format *standard-output*)
  (write-the-object (make-object 'try) (summary)))

2.4.3 Write-The
===============

Syntax:

(write-the <reference-chain>)

``Write-the'' is similar to ``Write-the-object,'' but it assumes
``self'' as the object, so it is not necessary to pass the object
```

---

## usage.txt (chunk 32/34)
Source: usage.txt
Type: usage_guide

```
Syntax:

(write-the <reference-chain>)

``Write-the'' is similar to ``Write-the-object,'' but it assumes
``self'' as the object, so it is not necessary to pass the object
explicitly to ``write-the'' as is necessary with ``write-the-object.''

Example:
========

(with-writer (base-format *standard-output*)
  (let ((self (make-object 'try)))
    (write-the (summary))))


For further examples and a listing of built-in formats currently
shipping with GDL/GWL, please see output-formats.txt.



7 Object Amendments
===================

The macro define-object-amendment can be used to extend and/or
redefine both user-defined objects and built-in GDL objects. The
syntax for define-object-amendment is identical to that for
define-object. Any additional elements will be added to the
definition, and any elements with the same names as existing elements
will overwrite the existing elements currently loaded into the system.



8 Extensions and Implementations
================================
```

---

## usage.txt (chunk 33/34)
Source: usage.txt
Type: usage_guide

```
any elements with the same names as existing elements
will overwrite the existing elements currently loaded into the system.



8 Extensions and Implementations
================================

Genworks also provides a large set of built-in primitives and
interfaces for our GDL product, written in the GDL language. 

Although Genworks currently produces the only available full-featured
implementation of the GDL language specification, this core language
specification also represents something of a de-facto standard for KBE
languages based in ANSI Common Lisp. If new implementations emerge, we
encourage them to adopt this standard as well, and communicate with
Genworks regarding refinements and further extensions, so that the
Industry can move toward a true vendor-neutral Standard KBE language
specification.
```

---

## usage.txt (chunk 34/34)
Source: usage.txt
Type: usage_guide

```
uage
specification.
```

---

## make.lisp - header
Source: gornschool-training/make.lisp
Type: tutorial

```
(in-package :gdl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :ql) (load-quicklisp)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *training-home*
    (make-pathname :name nil :type nil :defaults (glisp:source-pathname)))
  ;;(unless (find-package :ql) (load-quicklisp))
  (pushnew (probe-file *training-home*) ql:*local-project-directories* :test #'equalp))
  

(defparameter *ci-commit-branch* (let ((branch (or (uiop:getenv "CI_COMMIT_BRANCH")
                                                   (uiop:run-program "git branch --show-current"
                                                                     :output :string
                                                                     :error :string
                                                                     :ignore-error-status t))))
                                   (if branch (make-keyword branch) :unspecified)))
```

---

## make.lisp - header
Source: gornschool-training/make.lisp
Type: tutorial

```
:ignore-error-status t))))
                                   (if branch (make-keyword branch) :unspecified)))

(defparameter *ci-commit-sha* (uiop:getenv "CI_COMMIT_SHA"))

(defvar *ci-info* (list :branch *ci-commit-branch* :sha *ci-commit-sha*))
```

---

## make.lisp - training
Source: gornschool-training/make.lisp
Type: tutorial

```
(define-object training (gdl-app)

  :input-slots
  ((deploy? nil))


  :computed-slots
  ((application-name (case *ci-commit-branch*
                       (:master "training")
                       (:devo "training-devo")
                       (otherwise "training-test")))

   (target-host "gornschool.com")

   ;;
   ;; FLAG Much of what's below consider to become generic for all
   ;;      deployments. extract "training" anything from below.
   ;;

   (fullchain-pem (format nil "/etc/keys/~a/fullchain.pem" (the target-host)))
   (privkey-pem (format nil "/etc/keys/~a/privkey.pem" (the target-host)))

   (build-level :gwl-graphics)

   (generate-scripts? t)   

   (overwrite? t)

   (overwrite-validation-function #'(lambda(pathname)
                                      (search (the application-name)
                                              (namestring pathname))))

   (pre-make-function (lambda()
                        (ql:quickload :training)
                        (ql:quickload :cl-smtp)))

   (post-load-form (progn

		     (print-variables (the fullchain-pem) (the privkey-pem))
		     
		     (remove
                      nil
                      `(progn
			 ,(when *ci-commit-branch*
                            `(setf (symbol-value (read-from-string "training-home::*ci-commit-branch*"))
                                   ,*ci-commit-branch*))
			 ,(when *ci-commit-sha*
                            `(setf (symbol-value (read-from-string "training-home::*ci-commit-shea*"))
                                   ,*ci-commit-sha*))))))


   (application-fasls
    (mapcar #'(lambda(system)
                (asdf:operate 'asdf:monolithic-compile-bundle-op system)
                (asdf:output-file 'asdf:monolithic-compile-bundle-op system))
            (list :training :cl-smtp)))

   (http-port (case *ci-commit-branch*
                ((:master :main) 80)
                (:devo 9080)
                (otherwise 9090)))

   (https-port (let ((branch *ci-commit-branch*))
		 (print-variables branch)
		 (let ((port
			 (case branch
			   ((:master :main) 443)
			   (:devo 9443)
			   (otherwise 9443))))
		   (print-variables port)
		   port)))

   (swank-port (case *ci-commit-branch*
                 ((:master :main) 1088)
                 (:devo 10088)
                 (otherwise 10098)))

   (restart-init-function

    (progn

      (format t "Building restart-init-function~%")
      (print-variables (the privkey-pem) (the fullchain-pem)
		       (the http-port) (the https-port) (the swank-port))
    
      `(lambda()

	 (format t "Now Starting training app...~%")

	 (setq gwl:*privkey-pem-path* ,(the privkey-pem))
	 (setq gwl:*fullchain-pem-path* ,(the fullchain-pem))

	 (gwl::reset-settings)

	 (print-variables gwl:*privkey-pem-path* gwl:*fullchain-pem-path*)

	 (if (and gwl:*privkey-pem-path*
                  (probe-file gwl:*privkey-pem-path*)
                  gwl:*fullchain-pem-path*
                  (probe-file gwl::*fullchain-pem-path*))
	     (progn
	       (setq gwl:*http-port* ,(the http-port)
		     gwl:*start-https?* t
		     gwl:*https-port* ,(the https-port))
	       (gendl:start-gendl!))
             (warn "Cannot find SSL certificate files: ~a and/or ~a"
                   gwl::*privkey-pem-path* gwl::*fullchain-pem-path*))

	 ;; #+zacl (setq cl+ssl:*default-cipher-list* "@SECLEVEL=0:kEECDH+ECDSA:kEECDH:kEDH:HIGH:MEDIUM:+3DES:+SHA:!RC4:!aNULL:!eNULL:!LOW:!MD5:!EXP")

	 ;;
	 ;; Demote this service  process to a normal user
	 ;;
	 (when (zerop (glisp:getuid))
           (format t "~&~%Demoting process to normal user ...~%")
           ;;
           ;; FLAG -- figure out how to not hardcode the gid/uid below.
           ;;
           (glisp:setgid 997) (glisp:setuid 1000))
	 
	 ;;
	 ;; Try to start swank listener after user is set correctly.
	 ;;
	 ;; FLAG -- may have to open permissions on /root/.slime/ because
	 ;; setuid does not change the effective home directory. Figure
	 ;; out how to change effective home directory.
	 ;;
	 (swank:create-server :port ,(the swank-port) :dont-close t)

	 ;;
	 ;; Set the main asset home directory then call all initializers in order.
	 ;;
	 (setf (symbol-value (read-from-string "training-common:*home*"))
	       (merge-pathnames "training-source/" (glisp:executable-homedir-pathname)))

         ;;
         ;; Application-level init
         ;;
	 (funcall (read-from-string "training-common:initialize-all"))
	 ;;
	 ;; FLAG -- extract this to another repo and load quiz as a  module only if needed.
	 ;;
	 (when (find-package :quiz)
           (setf (symbol-value (read-from-string "quiz:*vocab-pathname*"))
		 (merge-pathnames "sanskrit/vocab/" (symbol-value (read-from-string "training-common:*home*"))))
           (funcall (read-from-string "quiz:publish-endpoints")))

	 (setq gwl:*developing?* ,(case *ci-commit-branch*
				    ((:master :main :devo) nil)
				    (otherwise t)))

	 (format t "~&~%Sleeping the main thread to become a webserver...~%")

	 (force-output)

	 (do () () (sleep 10000)))))))





```

---

## make.lisp - training-app
Source: gornschool-training/make.lisp
Type: tutorial

```
(defun training-app (&key deploy?)
  (let* ((self (make-object 'training :deploy? deploy?))
         ;;(*default-pathname-defaults* (the destination-directory))
         )
    (the make!)))


;; backups of DNS configuration entries:
#|

TXT record

default._domainkey

v=DKIM1; h=sha256; k=rsa; p=MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAhRL3xx1EyaUJjUYSaqSUC+FwIFOX84VPq7X/Y2Fcp3vq5XjdBNE4OUW/XNklLbzQ+ms6E7v3D9EbgDoVKXePba07QbeDL+0afv+YHm14IJZYlrpTniQLhsAHKxfKmlDhgEDLqHfIr0p+Pl8sXX6UPApHbTzr8Uo3z7RXLZSi/wnn0vkc5lYrvlUzuAsPuqapQwzebPnCyh/0BEWTJY5u+43vNtqEj/902NLED9cdFCBGOTTCm9v+Nuo6hMTWyO3HAXGt4UThFj4tWapBW7RVmb17QlZLbgmXZeJDTInecYPmiPIrkBTbLjzrA4100JXtpd5n/ZLl+qtrgwtqtsLBXQIDAQAB0;173;54M0;173;54m

Automatic


|#

```

---

## load.lisp - header
Source: gornschool-training/load.lisp
Type: tutorial

```
(in-package :gdl-user)

(load-quicklisp)

(push (truename "./") ql:*local-project-directories*)
(load "./make.lisp")

(let ((hostname (uiop:hostname)))
  (training-app :deploy? (string-equal hostname "gornschool.com")))

```

---

## initialize.lisp - header
Source: gornschool-training/zzend/source/initialize.lisp
Type: tutorial

```
(in-package :gdl-user)

(unless (uiop:getenv "CI_COMMIT_SHA") (training-common:initialize-all))

```

---

## package.lisp - header
Source: gornschool-training/home/source/package.lisp
Type: tutorial

```
(in-package :gdl-user)

(gwl:define-package :training-home
    (:use :training-common)
  (:shadow #:*home*)
  (:export #:assembly))


```

---

## assembly.lisp - header
Source: gornschool-training/home/source/assembly.lisp
Type: tutorial

```
(in-package :training-home)


```

---

## assembly.lisp - assembly
Source: gornschool-training/home/source/assembly.lisp
Type: tutorial

```
(define-object assembly (training-common:base-site-mixin)

  :input-slots ((email-address nil :settable) (restored? nil :settable) (opt-in? nil :settable))

  :computed-slots
  (

   (title "GendL Self-start Tutorials")
   
   (page-header nil)

   (body-content
    (with-cl-who-string()
      (:h2 "GendL Self Start Tutorials")
		  
      (:p "Welcome"
          (str (when (the restored?) " back"))
          (str (if (the email-address) (format nil ", ~a" (the email-address)) ""))
          " to the GendL Self-Start Tutorials, which are a set of training materials aimed at getting you up and running with GendL and introducing you to some of the development tools and techniques.")
                  
      (:h3 "So what is GendL?")
                  
      (:p "GendL is a dynamic, declarative, object-oriented language environment embedded in ANSI Common Lisp (CL).")
                  
      (:p "It consists of a collection of predefined objects, code-expansion macros, and functions which you, the GendL application developer, may either use directly or extend, to solve problems of any level of complexity decomposed into manageable units.")

      (:p "GendL includes geometric primitives and has a built-in web server to  facilitate cross-platform deployment.")

      (:p "GendL is open source (under the Gnu AGPL license) and has been ported to several ANSI Common Lisp implementations, including Allegro CL, LispWorks, Clozure CL (CCL), Steel Bank Common Lisp (SBCL), and Clasp. There is also a commercial version offering proprietary/closed-source distribution rights, full levels of technical support and with some extra features like surface and solid modelling. The commercial version is packaged with Allegro CL from Franz Inc.")

      (:h3 "About These tutorials")

      (:p "The tutorials are designed to be self-paced learning resources and where appropriate  contain example code files which may be downloaded. It is recommended that the first  two tutorials, "
          (:a :href (the installing-gendl url) "Installing GendL")
          " and "
          (:a :href (the getting-started url) "Getting Started with GendL")
          " are completed in that  order - after that we move into more specific or advanced subjects which may be undertaken as required. Within each tutorial there are a number of topics,  which we would suggest are undertaken in the order presented.")

      (:h3 "Tutorials")
      ((:div :class "ml-3")
       ((:ol :class "list-decimal")
	(dolist (plis (the tutorial-objects))
	  (htm (:li ((:a :href (getf plis :url)) (str (getf plis :tutorial-name))) (:br))))))))
    
   (tutorial-objects (safe-sort
		      (remove nil (mapcar #'(lambda(obj)
					      (when (theo obj  tutorial-index)
						(list :object obj :tutorial-index (theo obj tutorial-index)
						      :tutorial-name (theo obj tutorial-name) :url (theo obj url))))
					  (remove-if-not #'(lambda(child)(typep child 'base-tutorial-sheet))
							 (the children))))
		      #'< :key #'(lambda(a) (getf a :tutorial-index)))))


  ;;
  ;; FLAG - make the below be a quantified set so we can navigate them more easily.
  ;;
  :objects
  ((installing-gendl :type 'training-1:assembly
		     :tutorial-index 1
		     :tutorial-name "Installing GendL"
                     :previous-page nil :next-page (the getting-started)
                     )
   
   (getting-started :type 'training-2:assembly
		    :tutorial-index 2
		    :tutorial-name "Getting started with GendL"
                    :previous-page (the installing-gendl) :next-page (the code-structuring))

   
   (code-structuring :type 'training-3:assembly
		     :tutorial-index 3
		     :tutorial-name "Structuring and Organising your code"
                     :previous-page (the getting-started) :next-page (the file-io))


   (file-io :type 'training-4:assembly
	    :tutorial-index 4
	    :tutorial-name "Reading from and Writing to Files"
            :previous-page (the code-structuring) :next-page (the more-objects))


   (more-objects :type 'training-5:assembly
		 :tutorial-name "More on Objects"
		 :tutorial-index 5
		 :getting-started-url (the getting-started url)
		 :define-object-url (the getting-started object-definition url)
		 :wall-example-url (the getting-started wall-example url)
		 :truss-example-url (the getting-started truss-example url)
                 :previous-page (the file-io) :next-page (the websites))

   (websites :type 'training-6:assembly
             :tutorial-name "Websites and Web Applications"
             :tutorial-index 6
	     :getting-started-tutorial (the getting-started)
	     :io-tutorial (the file-io)
             :previous-page (the more-objects) :next-page nil)))




```

---

## utilities.lisp - header
Source: gornschool-training/quiz/source/utilities.lisp
Type: tutorial

```
(in-package :quiz)

;;
;; Following was generated by ChatGPT (after much prompting):
;;

(defparameter *bad-chars* (mapcar #'code-char
				  '(#x093e #x093f #x0940 #x0941 #x0942 #x0943 #x0944 #x0900
				    #x0945 #x0946 #x0947 #x0948 #x0949 #x094a #x094b #x094c
				    #x094d #x094e #x094f #x0955 #x0956 #x0957 #x0962 #x0963
				    #x0901 #x0902 #x0903
				    #x093a #x093b #x093c #x093e #x093f #x0940 #x0941)))


```

---

## utilities.lisp - scramble-string
Source: gornschool-training/quiz/source/utilities.lisp
Type: tutorial

```
(defun scramble-string (string)
  (let* ((string-list (coerce string 'list))
         (string-length (length string-list)))
    (dotimes (i (1- string-length))
      (let* ((j (+ i (random (- string-length i))))
             (scramble-prob (/ i (1- string-length))))
        (when (and (/= i j)
                   (not (char= (nth i string-list)(nth j string-list)))
                   (not (char= #\Space (nth i string-list)))
                   (not (char= #\Space (nth j string-list)))
                   (< (random 1.0) scramble-prob))
          (rotatef (nth i string-list)
                   (nth j string-list)))))

    (setq string-list (remove-if #'(lambda(char) (member char *bad-chars*)) string-list))
    
    (let ((result (coerce string-list 'string)))
      (if (string= string result)
          (subseq string 0 (max 0 (1- (length string))))
          result))))


```

---

## package.lisp - header
Source: gornschool-training/quiz/source/package.lisp
Type: tutorial

```
(in-package :cl-user)

(gwl:define-package :quiz (:export #:*vocab-pathname* #:publish-endpoints))


```

---

## defparameters.lisp - header
Source: gornschool-training/quiz/source/defparameters.lisp
Type: tutorial

```
(in-package :quiz)

(defparameter *vocab-pathname*
  (make-pathname
   :type nil :name nil :defaults (merge-pathnames "../../sanskrit/vocab/" (glisp:source-pathname))))

```

---

## package.lisp - header
Source: gornschool-training/common/source/package.lisp
Type: tutorial

```
(in-package :gdl-user)

(gwl:define-package :training-common
    (:export #:*home*
             #:*initializers*
             #:initialize-all
             #:base-site-mixin
             #:repl-example
	     #:code-example
	     #:index-page
	     #:base-training-sheet
	     #:base-tutorial-sheet))


(load (compile-file (merge-pathnames "../../t6/resources/source/gwl-patches.lisp" (glisp:source-pathname))))

```

---

## patches.lisp - header
Source: gornschool-training/common/source/patches.lisp
Type: tutorial

```
(in-package :gwl)


(defvar *snap-home* nil)



```

---

## patches.lisp - find-lost-session
Source: gornschool-training/common/source/patches.lisp
Type: tutorial

```
(defun find-lost-session (req ent &key (snap-home *snap-home*))
  (let (url)
    (with-http-response (req ent) (setq url (lost-session-finder req :snap-home snap-home)))
    (if url (with-http-response (req ent :response *response-found*)
              (setf (reply-header-slot-value req :cache-control) "no-cache")
              (setf (reply-header-slot-value req :pragma) "no-cache")
              (setf (reply-header-slot-value req :location) (format nil "~a" url))
              (setf (reply-header-slot-value req :response) *response-found*)
              (with-http-body (req ent)))
        (with-http-response (req ent)
          (with-http-body (req ent)
            (format *html-stream* "Sorry, session was not found.~%"))))))



```

---

## patches.lisp - lost-session-finder
Source: gornschool-training/common/source/patches.lisp
Type: tutorial

```
(defun  lost-session-finder (req &key (snap-home *snap-home*))
  (let* ((components (glisp:split-regexp "/" (uri-path (request-uri req))))
         (root-path (gwl::compute-root-path (reverse (rest (rest (rest components))))))
         (session-id (nth (1+ (position "sessions" components :test #'string-equal)) components))
         (new-session (restore-session! session-id :snap-home snap-home)))
    (if new-session (theo new-session (follow-root-path root-path) url))))


```

---

## patches.lisp - restore-session!
Source: gornschool-training/common/source/patches.lisp
Type: tutorial

```
(defun restore-session! (session-id &key (snap-home *snap-home*))
  (when snap-home
    (let ((filename (merge-pathnames (format nil "~a.snap" session-id) snap-home)))
      (when (probe-file filename)
        (read-snapshot :filename filename)))))


```

---

## patches.lisp - write-snap
Source: gornschool-training/common/source/patches.lisp
Type: tutorial

```
(defun write-snap (self &key (snap-home *snap-home*))
  (when snap-home
    (let ((filename (merge-pathnames (format nil "~a.snap" (the instance-id)) snap-home)))
      (the root (write-snapshot :filename filename)))))


;; FLAG -make this work only for requests from the local machine.
;;
#+nil

```

---

## patches.lisp - activate-poison-pill
Source: gornschool-training/common/source/patches.lisp
Type: tutorial

```
(defun activate-poison-pill ()
  (publish :path "/terminate-code-108"
           :function #'(lambda(req ent)
                         (bt:make-thread #'(lambda()
                                             (sleep 5) (net.aserve:shutdown)
                                             #+ccl (ccl:quit)
                                             #+allegro (excl:exit 0 :no-unwind t)
                                             #-(or ccl allegro) (error "Need quit function for ~a.~%" (lisp-implementation-type)))
                                         :name "terminate-code-108")
                         (with-http-response (req ent)
                           (with-http-body (req ent)
                             (format *html-stream* "Terminating in five seconds..."))))))



```

---

## patches.lisp - clear-all-instances
Source: gornschool-training/common/source/patches.lisp
Type: tutorial

```
(defun clear-all-instances ()
  "Void. Clears all instances from GWL's master table of root-level instances.
The instance IDs are the numbers you see in published GWL URIs, and are available
as the \"instance-id\" message within each GWL object which inherit from base-html-sheet.

Clearing all the instances makes available for garbage collection all memory used by
the object hierarchies rooted at the instances, as well as all associated published URIs.

:example <pre>
  (clear-all-instance)
  </pre>"
  (maphash #'(lambda(key val)
               (declare (ignore val))
               (unless (member key gwl::*keys-to-preserve*)
                 (clear-instance key))) *instance-hash-table*)
  
  (maphash #'(lambda(key val)
               (declare (ignore val))
               (unless (member key gwl::*keys-to-preserve*)
                 (gwl::unpublish-instance-urls key))) *url-hash-table*))

```

---

## patches.lisp - base-html-sheet
Source: gornschool-training/common/source/patches.lisp
Type: tutorial

```
(define-object-amendment base-html-sheet ()
  :computed-slots
  ((compute-urls (let ((url-base (format nil "~{~(~a~)~^/~}"
					 (mapcar #'(lambda(component)
						     (if (listp component)
							 (format nil "~a/~a" (first component) (second component))
							 component))
						 (reverse (the root-path))))))
		   (append (mapcar #'(lambda(extension)
				       (string-append url-base (if (zerop (length url-base)) "" "/")  extension))
				   (the url-extensions))
			   (list url-base (the descriptive-url)))))))

;;
;; FLAG -- merge with  GDL 1598 
;;

(#+allegro excl:without-package-locks #-allegro progn
 (#+allegro excl:without-redefinition-warnings #-allegro progn
  
```

---

## patches.lisp - crawl-anchor
Source: gornschool-training/common/source/patches.lisp
Type: tutorial

```
(defun crawl-anchor (anchor host port output-root visited-urls)
    (let ((uri (net.uri:parse-uri (getf (rest (first anchor)) :href))))
      (let ((scheme (net.uri:uri-scheme uri))
	    (uri-host (net.uri:uri-host uri))
	    (uri-port (net.uri:uri-port uri))
	    (path (net.uri:uri-path uri)))
        (unless (or (null path) ;; empty path - likely hash mark anchor.
                    (not (string-equal (pathname-type path) "html")) ;; punt on non-html urls
		    scheme uri-host uri-port ;; this is external - don't crawl.
		    (gethash path visited-urls)) ;; already crawled - don't crawl
	  (crawl-url path host port output-root visited-urls)))))))



```

---

## patches.lisp - publish-gwl-app
Source: gornschool-training/common/source/patches.lisp
Type: tutorial

```
(defun publish-gwl-app (path string-or-symbol &key publish-args make-object-args host headers (server *http-server*))
  "Void. Publishes an application, optionally with some initial arguments to be passed in as input-slots.

:arguments (path \"String. The URL pathname component to be published.\"
            string-or-symbol \"String or symbol. The object type to insantiate.\")

:&key (make-object-args \"Plist. Extra arguments to pass to make-object.\")
"

  (apply #'publish
	 :path path
	 :server server
         :host host
         :headers headers
	 :function #'(lambda(req ent)
		       (gwl-make-object req ent 
					(format nil (if (stringp string-or-symbol) "~a" "~s")
						string-or-symbol)
					:make-object-args make-object-args))
	 publish-args))





```

---

## patches.lisp - skeleton-form-control
Source: gornschool-training/common/source/patches.lisp
Type: tutorial

```
(define-object-amendment skeleton-form-control ()
  :input-slots
  ((required? nil) (autocomplete nil)))



```

---

## patches.lisp - with-expanded-html-output
Source: gornschool-training/common/source/patches.lisp
Type: tutorial

```
(defmacro with-expanded-html-output ((var &key stream prologue) &body body)
  (let ((new-body
          (mapcar #'(lambda(form)
                      (cond ((and (listp form)
                                  (listp (first form))
                                  (member (first (first form)) '(:input :select :textarea :button)))
                             (cons (append (first form)
                                           `(:required
                                             (the required?)
                                             :autocomplete (the autocomplete)


                                             :disabled 
                                             (if (the disabled?) t nil)
                                             ;;
                                             ;; The new HTML5 stuff:
                                             ;;
                                             :placeholder (the placeholder)
                                            
                                            
                                             :class (the class)
                                             :readonly (if (the readonly?) t nil)
                                             :ismap (if (the ismap?) t nil)
                                             :size  (the size)
                                             :maxlength (the maxlength)
                                             :src (the src)
                                             :alt (the alt)
                                             :usemap (the usemap)
                                             :tabindex (the tabindex)
                                             :accesskey (the accesskey)
                                             :onfocus (the onfocus)
                                             :onblur (the onblur)
                                             :onselect (the onselect)
                                             :onchange (the onchange)
                                             :ondblclick (the ondblclick)
                                             :onclick (the onclick)
                                             :onmousedown (the onmousedown)
                                             :onmouseup (the onmouseup)
                                             :onmouseover (the onmouseover)
                                             :onmousemove (the onmousemove)
                                             :onmouseout (the onmouseout)
                                             :onkeypress (the onkeypress)
                                             :onkeydown (the onkeydown)
                                             :onkeyup (the onkeyup)
                                             :accept (the accept)
                                             :lang (the lang)
                                             :title (the title)
                                             :style (the style)
                                             :align (the align)))
                                   (rest form)))
                            (t form))) body)))
    `(with-html-output (,var ,stream :prologue ,prologue :indent :indent)
       ,@new-body)))



;;
;; Just to add the max-id-value input
;;

```

---

## patches.lisp - %gwl-make-object%
Source: gornschool-training/common/source/patches.lisp
Type: tutorial

```
(defun %gwl-make-object% (part &key make-object-args share? skin (max-id-value 9999999999999999999999999))
  (let* ((instance-id (if share? "share" (make-new-instance-id :max-value max-id-value)))
         (current (gethash (make-keyword-sensitive instance-id) *instance-hash-table*))
         (skin (if skin (make-instance skin) t))
         (root-part-and-version 
          (if (or (not share?) (not current))
              (list (apply #'make-object (read-safe-string part)
                           :instance-id instance-id
                           make-object-args) *dummy-version*) current)))
    (setf (gethash (first root-part-and-version) *weak-objects*) t)
    (setq root-part-and-version (append root-part-and-version (list skin)))
    (when (or (not share?) (not current))
      (setf (gethash (make-keyword-sensitive instance-id) *instance-hash-table*) root-part-and-version))
    (first root-part-and-version)))



(defparameter *user-name* nil)



```

---

## patches.lisp - set-user-name
Source: gornschool-training/common/source/patches.lisp
Type: tutorial

```
(defun set-user-name ()
  (setq *user-name*
	#+linux
        (multiple-value-bind (name error code)
            (uiop:run-program (format nil "id -nu ~a" (glisp:getuid))
                              :output :string
                              :ignore-error-status t
                              :error-output :string)
          (if (zerop code) (glisp:replace-regexp name "\\s" "")
              (error "setting username resulted in code ~a with error: ~a~%" code error)))
	#+windows
	(lastcar (pathname-directory (user-homedir-pathname))))
  *user-name*)



```

---

## patches.lisp - publish-sessions-catchall
Source: gornschool-training/common/source/patches.lisp
Type: tutorial

```
(defun publish-sessions-catchall (&key (server *https-server*) host snap-home)
  (publish-prefix  :server server :host host :prefix "/sessions"
                   :function #'(lambda(req ent) (find-lost-session  req ent :snap-home snap-home))))


```

---

## patches.lisp - publish-http-catchall
Source: gornschool-training/common/source/patches.lisp
Type: tutorial

```
(defun publish-http-catchall (&key (server *http-server*) host)
  "Redirect all requests to the equivalent request on the *https-server*"
  (publish-prefix :server server :prefix "/"
                  :host host
                  :function #'(lambda(req ent) (redirect-to-https req ent))))


```

---

## patches.lisp - redirect-to-https
Source: gornschool-training/common/source/patches.lisp
Type: tutorial

```
(defun redirect-to-https (req ent)
  (let ((https-host-port (or (uiop:getenv "HTTPS_HOST_PORT") *https-port*)))
    (unless (and *https-server* https-host-port)
      (error "redirect-to-https: *https-server* and environment HTTPS_HOST_PORT must be set"))
    (let* ((port (let ((try (parse-integer (format nil "~a" https-host-port))))
                   (unless (= try 443) try)))
           (scheme "https")
           (uri (slot-value req 'uri))
           (host (uri-host uri))
           (path (uri-path uri)))
      (with-http-response (req ent :response *response-found*)
        (setf (reply-header-slot-value req :cache-control) "no-cache")
        (setf (reply-header-slot-value req :pragma) "no-cache")
        (let ((target (format nil "~a://~a~a~a"
                              scheme host (if port (format nil ":~a" port) "") path)))
          (format t "Redirecting to: ~a~%" target)
          (setf (reply-header-slot-value req :location) target))
        (with-http-body (req ent))))))




```

---

## repl-example.lisp - header
Source: gornschool-training/common/source/repl-example.lisp
Type: tutorial

```
(in-package :training-common)


;;
;; 
;; command-no-prefix  -- class name listed here for purgeCSS to keep it in our CSS.
;;


```

---

## repl-example.lisp - repl-example
Source: gornschool-training/common/source/repl-example.lisp
Type: tutorial

```
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




```

---

## base-site-mixin.lisp - header
Source: gornschool-training/common/source/base-site-mixin.lisp
Type: tutorial

```
(in-package :training-common)


```

---

## base-site-mixin.lisp - base-site-mixin
Source: gornschool-training/common/source/base-site-mixin.lisp
Type: tutorial

```
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


   




```

---

## index-page.lisp - header
Source: gornschool-training/common/source/index-page.lisp
Type: tutorial

```
(in-package :training-common)


```

---

## index-page.lisp - index-page
Source: gornschool-training/common/source/index-page.lisp
Type: tutorial

```
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



```

---

## index-page.lisp - index-items
Source: gornschool-training/common/source/index-page.lisp
Type: tutorial

```
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

```

---

## base-tutorial-sheet.lisp - header
Source: gornschool-training/common/source/base-tutorial-sheet.lisp
Type: tutorial

```
(in-package :training-common)



```

---

## base-tutorial-sheet.lisp - base-tutorial-sheet
Source: gornschool-training/common/source/base-tutorial-sheet.lisp
Type: tutorial

```
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




```

---

## code-example.lisp - header
Source: gornschool-training/common/source/code-example.lisp
Type: tutorial

```
(in-package :training-common)


```

---

## code-example.lisp - code-example
Source: gornschool-training/common/source/code-example.lisp
Type: tutorial

```
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


```

---

## code-example.lisp - indented-html
Source: gornschool-training/common/source/code-example.lisp
Type: tutorial

```
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

```

---

## publish.lisp - header
Source: gornschool-training/common/source/publish.lisp
Type: tutorial

```
(in-package :training-common)

(defparameter *publish-prefix* "common")
(defparameter *initializers* nil)


(defparameter *home*
  (merge-pathnames "../../" (make-pathname :name nil :type nil :defaults (glisp:source-pathname))))



```

---

## publish.lisp - set-snap-home
Source: gornschool-training/common/source/publish.lisp
Type: tutorial

```
(defun set-snap-home (user-name)
  (setq gwl::*snap-home*
        (ensure-directories-exist
         (merge-pathnames "tutorial-sessions/"
                          (if user-name
                              #+linux
                              (merge-pathnames (format nil "~a/" user-name) "/home/")
                              #-linux
                              (user-homedir-pathname)
                              (user-homedir-pathname))))))



```

---

## publish.lisp - initialize
Source: gornschool-training/common/source/publish.lisp
Type: tutorial

```
(defun initialize ()


  (setq gwl:*bypass-security-check?* t)

  (let ((user-name (when (glisp:featurep :linux) (gwl::set-user-name)))) (set-snap-home user-name))

  (unless (probe-file *home*) (error "Common home not found at ~A,
please arrange to have it set properly before calling ~s." *home* 'initialize))


  (let ((hostname (uiop:hostname))
        (https-server *https-server*)
        (http-server *http-server*))

 
    (unless http-server (error "No *http-server* found in training-common::initialize.
You may want to do `(gdl::initialize)` or `(gdl:start-gdl!)` before loading this code.~%"))
    
    
    (gwl::publish-http-catchall :server *http-server*)
    
    (unless https-server (error "No *https-server* found in training-common::initialize.
You may want to do `(gdl::initialize)` or `(gdl:start-gdl!)` before loading this code.~%"))


    (setq *start-https?* t)(setq *start-http?* t)
    
    (dolist (host (if (string-equal hostname "gornschool.com")
                      (list hostname (format nil "www.~a" hostname)
                            "localhost" "127.0.0.1"
                            "school.genworks.com" "school.gen.works")
                      (list nil)))
      
      (let ((server https-server)) ;;with-all-servers (server)

        
        
        ;; a cute favicon (need to get it designed).
        (publish-file :path "/gorn.png" :server server :host host  :content-type "image/png"
                      :file (namestring
                             (merge-pathnames "images/gorn-icon.png" *home*)))

        ;; the email gateway
        (publish-gwl-app "/" "email-gateway:assembly" :server server :host host)

        ;;
        ;; site-specific GWL config:
        ;;

        (gwl::publish-sessions-catchall :server server :host host :snap-home gwl::*snap-home*)


        ;;
        ;; And now the Application-level publishings
        ;;
        (publish-gwl-app "/gendl-self-start-backdoor" "training-home:assembly" :server server :host host)

        (publish-shared "training-home:assembly" :path "/shared" :server server :host host)

        (publish-file :server server :host host
                      :path "/css/training-style.css"
                      :file (namestring (merge-pathnames "css/training-style.css" *home*)))


        (let ((common-images (merge-pathnames "images/" *home*)))
          (print-variables common-images)
          (publish-directory :server server :host host
                             :prefix "/common-images/"
                             :destination (namestring common-images)))

        (mapc #'(lambda (publish-prefix object-type-string)
                  (format t "** Publishing an App as follows:~%~%")
                  (let ((path (format nil "/~a-backdoor" publish-prefix)))
                    (print-variables server host path)
                    (publish-gwl-app  path object-type-string :server server :host host))
                  ;;
                  ;; FLAG -- the below function is a mess because of mismatches between stored file paths and published prefixes. Fix it.
                  ;;
                  (dolist (subdir (list "images" "resources" "resources/images"))
                    (let ((path (merge-pathnames (format nil "~a/~a/~a" publish-prefix subdir (if (string-equal subdir "resources") "source/" "")) *home*)))
                      (when (probe-file path)
                        (let ((prefix (if (string-equal subdir "/resources/images") "resource-images"
                                          (format nil "/~a-~a" publish-prefix subdir))))

                          (format t "** Publishing a Directory as follows:~%~%")
                          (print-variables prefix (namestring path))
                          (publish-directory :prefix prefix
                                             :destination (namestring path)
                                             :server server :host host))))))
              (list "home" "t1" "t2" "t3" "t4" "t5" "t6")
              (list "training-home:assembly" "training-1:assembly" "training-2:assembly" "training-3:assembly"
                    "training-4:assembly" "training-5:assembly" "training-6:assembly")))))

  (force-output))



(pushnew 'initialize *initializers*)


```

---

## publish.lisp - initialize-all
Source: gornschool-training/common/source/publish.lisp
Type: tutorial

```
(defun initialize-all () (mapc #'funcall (reverse *initializers*)))  ;; reverse is important because the above body must run first to set up e.g. *snap-home*.


;; (initialize)

```

---

## base-training-sheet.lisp - header
Source: gornschool-training/common/source/base-training-sheet.lisp
Type: tutorial

```
(in-package :training-common)


```

---

## base-training-sheet.lisp - base-training-sheet
Source: gornschool-training/common/source/base-training-sheet.lisp
Type: tutorial

```
(define-object base-training-sheet (base-site-mixin)

  :input-slots
  (publish-prefix page page-title page-objects (resources nil) (body-content nil))
                  

  :computed-slots
  ((title (the page-title)) ;; for standard inclusion of title in header by base-ajax-sheet.

   (index-words nil)



   ;;
   ;; FLAG -- make following 3 generic so they can be in base-site-mixin. 
   ;;
   (current-page-ind (position (the page) (the page-objects) :key #'(lambda(a) (getf a :page))))

   (next-page (let ((next-index (1+ (the current-page-ind))))
                (when (< next-index (length (the page-objects)))
                  (nth next-index (the page-objects)))))

   (previous-page (when (> (the current-page-ind) 0)
                    (nth (1- (the current-page-ind)) (the page-objects))))

   ;;
   ;; FLAG make this generic so it can be in base-site-mixin.
   ;;
   (page-header (with-lhtml-string ()
                  (:h2 (str (the page-title)))
                  (:p
                   (when (the previous-page) (htm ((:a :class "text-red-500" :href (getf (the previous-page) :url)) "&lt;-Previous")))
                   (:span :class "font-bold" " | ")
                   ((:a :class "text-blue-500" :href (the parent url)) "^UP^")
                   (:span :class "font-bold" " | ")
                   (when (the next-page) (htm ((:a :class "text-green-500" :href (getf (the next-page) :url)) "Next-&gt;"))))))



   (resource-links (with-lhtml-string()

		     (:table
			 (dolist (resource (the resources))
			   (let* ((fname (if (listp resource) (getf resource :url) resource))
				  (href (if (listp resource) fname (format nil "/~a-resources/~a" (the publish-prefix) fname)))
				  (target (when (listp resource) "_new"))
				  (ftype (pathname-type fname))
				  (label (if (listp resource) (getf resource :title) resource))
				  (icon (cond ((string-equal ftype "html") "/common-images/html-file.png")
					      ((string-equal ftype "htm") "/common-images/html-file.png")
					      ((string-equal ftype "lisp") "/common-images/lisp-file.png")
                                              ((string-equal ftype "gendl") "/common-images/lisp-file.png")
                                              ((string-equal ftype "gdl") "/common-images/lisp-file.png")
					      ((string-equal ftype "pdf") "/common-images/pdf-file.png")
					      ((string-equal ftype "txt") "/common-images/txt-file.png")
					      ((string-equal ftype "css") "/common-images/css-file.png")
					      ((string-equal ftype "png") "/common-images/png-file.png")
					      (t nil))))
			     (htm (:tr (when icon
					 (htm (:td ((:a :href href :target target) ((:img :src icon :style "width: 40px; height: auto;"))))))
				       (:td ((:a :href href :target target) (str label))))))))))))



```

---

## utilities.lisp - header
Source: gornschool-training/email-gateway/source/utilities.lisp
Type: tutorial

```
(in-package :email-gateway)

(defvar *user-hash* (make-hash-table :size 100 :test 'equalp))
(defvar *user-hash-path* nil)
(defvar *robot-log-path* nil)


```

---

## utilities.lisp - add-user
Source: gornschool-training/email-gateway/source/utilities.lisp
Type: tutorial

```
(defun add-user (email-address session-id)
  (setf (gethash email-address *user-hash*) session-id)
  (let ((filename *user-hash-path*))
    (with-open-file (out filename :direction :output :if-exists :supersede :if-does-not-exist :create)
      (print (list-hash *user-hash*) out))))


```

---

## utilities.lisp - retrieve-object
Source: gornschool-training/email-gateway/source/utilities.lisp
Type: tutorial

```
(defun retrieve-object (email-address)
  (let ((instance-id  (gethash email-address *user-hash*)))
    (when instance-id
      ;;
      ;; FLAG Applicaton name  bleeding through the authemtication process here. Needs to but is there a better way to do this?
      ;;
      (let ((object (read-snapshot :filename (merge-pathnames (format nil "~a.snap" instance-id) gwl::*snap-home*))))
        object))))



```

---

## utilities.lisp - restore-saved-users
Source: gornschool-training/email-gateway/source/utilities.lisp
Type: tutorial

```
(defun restore-saved-users  ()
  (let ((filename *user-hash-path*))
    (when (probe-file filename)
      (with-open-file (in filename)
        (let ((user-plist (read in))) ;; FLAG -- change strategy if more than 1,0000 users
          (mapc #'(lambda(key val) (setf (gethash key *user-hash*) val))
                (plist-keys user-plist) (plist-values user-plist)))))))


```

---

## utilities.lisp - set-user-hash-path
Source: gornschool-training/email-gateway/source/utilities.lisp
Type: tutorial

```
(defun set-user-hash-path ()
  (setq *user-hash-path*
        (ensure-directories-exist
         (merge-pathnames "user-hash.sexp" gwl::*snap-home*))))


```

---

## utilities.lisp - set-robot-log-path
Source: gornschool-training/email-gateway/source/utilities.lisp
Type: tutorial

```
(defun set-robot-log-path ()
  (setq *robot-log-path*
        (ensure-directories-exist
         (merge-pathnames "robot-log.sexps" gwl::*snap-home*))))


;;
;; FLAG Applicaton name  bleeding through the authemtication process here. Needs to but is there a better way to do this?
;;
(pushnew 'set-user-hash-path training-common:*initializers*)
(pushnew 'restore-saved-users training-common:*initializers*)
(pushnew 'set-robot-log-path training-common:*initializers*)

```

---

## package.lisp - header
Source: gornschool-training/email-gateway/source/package.lisp
Type: tutorial

```
(in-package :gdl-user)

(gwl:define-package :email-gateway
    (:export #:assembly))




```

---

## usage.txt (chunk 1/34)
Source: usage.txt
Type: usage_guide

```
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


         Basic GDL Overview and Syntax
         =============================

This file contains basic information and usage instructions for the
```

---

## usage.txt (chunk 2/34)
Source: usage.txt
Type: usage_guide

```
;; <http://www.gnu.org/licenses/>.
;; 


         Basic GDL Overview and Syntax
         =============================

This file contains basic information and usage instructions for the
base General-purpose Declarative Language System.

For further documentation and updates, please visit
http://www.genworks.com or contact us at info@genworks.com
or 248-910-0912.

The GDL product is a commercially available KBE system, and the core
GDL language is a proposed standard for a vendor-neutral KBE
language. 


Core GDL Syntax
==================

GDL is based on and is a superset of ANSI Common Lisp.

1 define-package
================

The macro gdl:define-package is used to set up a new working package
in GDL.

Example:

   (gdl:define-package :gdl-user)

The :gdl-user package is an empty, pre-defined package for your use if
you do not wish to make a new package just for scratch work.

For real projects it is recommended that you make and work in your own
GDL package.
```

---

## usage.txt (chunk 3/34)
Source: usage.txt
Type: usage_guide

```
-defined package for your use if
you do not wish to make a new package just for scratch work.

For real projects it is recommended that you make and work in your own
GDL package.


Notes for advanced users:

  Packages defined with gdl:define-package will implicitly :use the
  GDL package and the Common-Lisp package, so you will have access to
  all exported symbols in these packages without prefixing them with
  their package name.

  You may extend this behavior, by calling gdl:define-package and
  adding additional packages to use with (:use ...).  For example, if
  you want to work in a package with access to GDL exported symbols,
  Common Lisp exported symbols, and symbols from the Surf (NURBS
  surfaces and brep solids) package, you could set it up as follows:

    (gdl:define-package :my-gdl-user (:use :gdl :surf))


2 define-object
===============

define-object is the basic macro for defining objects (i.e. creating
classes) in GDL.
```

---

## usage.txt (chunk 4/34)
Source: usage.txt
Type: usage_guide

```
e :my-gdl-user (:use :gdl :surf))


2 define-object
===============

define-object is the basic macro for defining objects (i.e. creating
classes) in GDL. A GDL object definition is a superset of a CLOS
Standard Class.

Basic syntax of Define-Object is 

 (define-object <class-name> <mixin-list> <specification-plist>)

<class-name> is any non-keyword symbol. A CLOS Standard Class will be
generated for this symbol, so any name you use will override a
defclass if one is already defined with the same name.

<mixin-list> is a list of other class-names from which this object
will inherit. It maps directly into the CLOS mixin list.

Note that the standard mixin GDL:Vanilla-Mixin gets mixed in
automatically with any GDL object and carries some of the basic GDL
functionality (messages).

<spec-plist> is a plist made up of pairs made from special keywords
and expression lists. The special keywords currently supported are the
following, and each is documented in the respective section of this
```

---

## usage.txt (chunk 5/34)
Source: usage.txt
Type: usage_guide

```
st> is a plist made up of pairs made from special keywords
and expression lists. The special keywords currently supported are the
following, and each is documented in the respective section of this
file: :input-slots, :computed-slots, :trickle-down-slots, :objects,
:hidden-objects, :functions, and :methods.


2.1 :input-slots
================

:input-slots are made up of a list, each of whose elements is either a
symbol or a list expression whose first element is a symbol. In either
case, the symbol represents a value which can be supplied either

 (a) into the toplevel object of an object hierarchy, at object instantiation
     (see (1.5), make-object, below)

 (b) into a child object, using a :objects specification (see (1.6), :objects,
     below)

Inputs are specified either as a simple symbol (which may, but need
not be, a keyword symbol), or as an expression whose first is a symbol
and whose second is an expression returning a value which will be the
```

---

## usage.txt (chunk 6/34)
Source: usage.txt
Type: usage_guide

```
e symbol (which may, but need
not be, a keyword symbol), or as an expression whose first is a symbol
and whose second is an expression returning a value which will be the
default value for the input slot.

Optionally, a third item can be supplied, the keyword :defaulting,
which indicates that if a slot by this name is contained in any
ancestor object's list of :trickle-down-slots, the value from the
ancestor will take precedence over the local default expression.

Example 1:

 (define-object person (base-object)

   :input-slots
   (first-name last-name age image-url))

In this example, the slots first-name, last-name, age, and image-url
are all defined, with no default expressions. This means that for the
object to answer these messages, these slots must be specified at the
time of object instantiation.

Example 2:

 (define-object person (base-object)

   :input-slots
   (first-name last-name age 
    (image-url "http://localhost:9000/images/")))
```

---

## usage.txt (chunk 7/34)
Source: usage.txt
Type: usage_guide

```
instantiation.

Example 2:

 (define-object person (base-object)

   :input-slots
   (first-name last-name age 
    (image-url "http://localhost:9000/images/")))

In this example, first-name, last-name, and age are all defined with
no default expressions, but image-url has the default expression
"http://localhost:9000/images/." This means that if nothing is
specified for image-url at object instantiation time, the image-url
message will return "http://localhost:9000/images/."


Example 3:

 (define-object person (base-object)

   :input-slots
   (first-name last-name age 
    (image-url "http://localhost:9000/images/" :defaulting)))


This example is the same as Example 2, with the exception that if
image-url is included in an ancestor object (see below for discussion
of object hierarchies) as a :trickle-down-slot, the slot's value from
that ancestor will take precedence over the local default expression
of "http://localhost:9000/images/."



2.2 computed-slots
==================
```

---

## usage.txt (chunk 8/34)
Source: usage.txt
Type: usage_guide

```
s) as a :trickle-down-slot, the slot's value from
that ancestor will take precedence over the local default expression
of "http://localhost:9000/images/."



2.2 computed-slots
==================

computed-slots are messages which are generally computed based on
their default expression.

computed-slots will only be computed when called ("demanded"), then
their values will be cached in memory. Only if another slot on which
they depend becomes modified will they become unbound, then their
values will be recomputed from their expressions when demanded.

The referencing macro ``the'' is used to refer to the values of
messages within the current object (named implicitly with the variable
"self"), or, through reference-chaining (see (1.6), :objects, below),
the values of messages in other object instances.

Notes for Advanced users: 

  In packages created with gdl:define-package, the Common Lisp symbol
  ``the'' is shadowed by gdl:the. If you wish to access
```

---

## usage.txt (chunk 9/34)
Source: usage.txt
Type: usage_guide

```
t instances.

Notes for Advanced users: 

  In packages created with gdl:define-package, the Common Lisp symbol
  ``the'' is shadowed by gdl:the. If you wish to access
  common-lisp:the, use the explicit package prefix, e.g. ``cl:the.''

Example 1:

  (define-object person (base-object)

   :input-slots
   (first-name
    last-name 
    age
    image-url)

   :computed-slots
   ((full-name (concatenate 'string (the first-name) " " (the  last-name)))))

In this example, the message full-name is always computed strictly
based on its default expression, which concatenates (the first-name)
and (the last-name).


Example 2:

  (define-object person (base-object)

   :input-slots
   (first-name
    last-name 
    age
    image-url)

   :computed-slots
   ((full-name (concatenate 'string (the first-name) " " (the  last-name)) :settable)))

In this example, the message full-name is by default computed based on
its default expression, which concatenates (the first-name) and (the
last-name).
```

---

## usage.txt (chunk 10/34)
Source: usage.txt
Type: usage_guide

```
st-name) " " (the  last-name)) :settable)))

In this example, the message full-name is by default computed based on
its default expression, which concatenates (the first-name) and (the
last-name). However, because it is :settable, its value may be altered
procedurally at runtime (see "setting slot values" below)

 

2.3 :objects and :hidden-objects
================================

:objects is used to specify a list of Instance specifications, where
each instance is considered to be a ``child'' object of the current
object :hidden-objects serves the same purpose and has the same syntax,
but hidden objects are considered ``hidden-children'' rather than
``children'' (so they are not returned by a call to (the children),
for example).

Inputs to each object are specified as a plist of inputs and
value expressions, spliced in after the objects's name and type
specification:

 Examples
 ========
> (define-object city (base-object)
   
   :computed-slots
```

---

## usage.txt (chunk 11/34)
Source: usage.txt
Type: usage_guide

```
and
value expressions, spliced in after the objects's name and type
specification:

 Examples
 ========
> (define-object city (base-object)
   
   :computed-slots
   ((total-water-usage (+ (the hotel water-usage)
                          (the bank water-usage))))
   :objects
   ((hotel :type 'hotel
           :size :large)
    (bank  :type 'bank
           :size :medium)))

--> CITY
     

>  (define-object hotel (base-object)
     :input-slots
     (size)

     :computed-slots
     ((water-usage (ecase (the size)
                     (:small 10)
                     (:medium 20)
                     (:large 30)))))
--> HOTEL


>  (define-object bank (base-object)
     :input-slots
     (size)
  
     :computed-slots
     ((water-usage (ecase (the size)
                     (:small 2)
                     (:medium 3)
                     (:large 4)))))

--> BANK

  > (setq self (make-object 'city))
--> #<CITY @ #x20933922>

  > (the total-water-usage)
--> 33
```

---

## usage.txt (chunk 12/34)
Source: usage.txt
Type: usage_guide

```
(:medium 3)
                     (:large 4)))))

--> BANK

  > (setq self (make-object 'city))
--> #<CITY @ #x20933922>

  > (the total-water-usage)
--> 33

The special message children will return a list of all the child
instances in a object:

  > (the children)
--> (#<HOTEL @ #x209350ca> #<BANK @ #x2093b62a>)



2.4 Sequences of Objects
========================

2.4.1 Fixed-size Sequences
==========================

Objects may be specified as a fixed-length sequence, analogous to a
single-dimensional array. Although we call this a fixed-length
sequence, the length can change if something it depends on becomes
modified. But if this happens, the entire sequence will have to be
recomputed.

Each member of the sequence will automatically answer an :index
message, which starts at 0 goes up to one less than the total number
of elements in the sequence.

Note that the referencing macro ``the-child'' may be used to reference
```

---

## usage.txt (chunk 13/34)
Source: usage.txt
Type: usage_guide

```
arts at 0 goes up to one less than the total number
of elements in the sequence.

Note that the referencing macro ``the-child'' may be used to reference
into the current child objects (in sequenced objects as well as in
normal non-sequenced objects). This can be useful for sequenced
objects, in order to access the :index of the current member.

Example

(defparameter *presidents-data*
    '((:name 
       "Carter"
       :term 1976)
      (:name "Reagan"
       :term 1980)
      (:name "Clinton"
       :term 1990)))
       
(define-object presidents-container (base-object)
  :input-slots 
  ((data *presidents-data*))
  
  :objects
  ((presidents :type 'president
	       :sequence (:size (length (the data)))
	       :name (getf (nth (the-child index)
				(the data)) 
			   :name)
	       :term (getf (nth (the-child index)
				(the data)) 
			   :term))))


(define-object president (base-object)
 :input-slots
 (name term))
```

---

## usage.txt (chunk 14/34)
Source: usage.txt
Type: usage_guide

```
erm (getf (nth (the-child index)
				(the data)) 
			   :term))))


(define-object president (base-object)
 :input-slots
 (name term))


For convenience, the special objects keyword :Parameters may be used to
pass an actual plist into a child instance instead of having to refer
to the individual parameters.

Example:

(define-object presidents-container (base-object)
  :input-slots
  ((data *presidents-data*))
  
  :objects
  ((presidents :type 'president
	       :sequence (:size (length (the data)))
    	       :parameters (nth (the-child index)
		  		(the data)))))


The members of quantified set are accessed like functions, by wrapping
extra parentheses and including the index number as the argument.

Example:

>   (setq self (make-object 'presidents-container))
--> #<PRESIDENTS-CONTAINER @ #x207441e2>

>   (the (presidents 0) name)
--> "Carter"


The quantified set can handle certain pre-defined messages,
including  last and first.

Example:

>   (the (presidents last))
```

---

## usage.txt (chunk 15/34)
Source: usage.txt
Type: usage_guide

```
ER @ #x207441e2>

>   (the (presidents 0) name)
--> "Carter"


The quantified set can handle certain pre-defined messages,
including  last and first.

Example:

>   (the (presidents last))
--> #<PRESIDENT @ #x2075061a>


Members of a quantified set can also handle the messages
 previous,  next, first?, and last?.


The types of a quantified set can also be quantified, by
supplying them as a list and using the keyword :sequence
in the :type specification, e.g.

(define-object stuff (base-object)
  :computed-slots
  ((child-types (list 'boy 'girl 'man 'woman)))

  :objects
  ((people :type (:sequence (the child-types))
           :sequence (:size (length (the child-types))))))


If the expression returning the :sequence of types, or of the :size,
of a fixed-size sequence becomes modified, or anything they depend on
becomes modified, then the entire sequence will become unbound and
will have to be recomputed the next time it is demanded.

2.4.2 Variable-size Sequences
```

---

## usage.txt (chunk 16/34)
Source: usage.txt
Type: usage_guide

```
anything they depend on
becomes modified, then the entire sequence will become unbound and
will have to be recomputed the next time it is demanded.

2.4.2 Variable-size Sequences
=============================

Objects may be specified as a variable-length sequence, analogous to a
list. These are similar to fixed-length sequences, but the syntax is:

  :sequence (:indices <list-of-indices>)

where the <list-of-indices> is an initial list of indices. The indices
are usually integers, but can be any object which matches with eql
(e.g. keyword symbols).

For inserting and deleting members of a variable-length sequence,
please see the reference documentation on variable-sequence.


2.5 :functions
==============

Functions are uncached methods on the object, which discriminate only
on the type of the object. They are defined with a normal
(non-specialized) lambda list, so they do not discriminate on the
types of their arguments other than the implicit ``self'' argument.
```

---

## usage.txt (chunk 17/34)
Source: usage.txt
Type: usage_guide

```
of the object. They are defined with a normal
(non-specialized) lambda list, so they do not discriminate on the
types of their arguments other than the implicit ``self'' argument.

Functions are called in a normal reference chain but their name is
wrapped in parentheses and the lambda-list is spliced on after the
name, within the parentheses.

Example:
=======

(define-object hotel (base-object)
  :input-slots
  (room-rate)
  
  :functions
  ((total-cost
    (number-of-nights)
    (* (the room-rate) number-of-nights))))


>   (setq self (make-object 'hotel :room-rate 100))
--> #<HOTEL @ #x2094f502> 

>   (the (total-cost 7))
--> 700

>   (the (total-cost 10))
--> 1000


2.5 :methods
==============

Methods are identical to GDL Functions, with the additional capability
of specializing on their argument signature (i.e. the combination of
types of the arguments) in addition to the implicit ``self'' argument
(as with standard CLOS methods).


2.6 :trickle-down-slots
```

---

## usage.txt (chunk 18/34)
Source: usage.txt
Type: usage_guide

```
eir argument signature (i.e. the combination of
types of the arguments) in addition to the implicit ``self'' argument
(as with standard CLOS methods).


2.6 :trickle-down-slots
=======================

:trickle-down-slots are a list of symbols naming other messages
(:input-slots, :computed-slots, etc.) in the object which will
automatically be available in any descendant (e.g. child, grandchild,
etc.) instances, unless overridden in the descendant instance (e.g. by
being defined as an :input-slot, :computed-slot, etc, in the
descendant instance).

Example:

(define-object person (base-object)
  :input-slots
  (social-security-number)
  
  :trickle-down-slots
  (social-security-number)
  
  :objects
  ((irs-records       :type 'irs-records)
   (state-tax-returns :type 'state-tax-returns)
   (fbi-file          :type 'fbi-file)
   (interpol-file     :type 'interpol-file)))

In the above object definition, the message social-security-number
```

---

## usage.txt (chunk 19/34)
Source: usage.txt
Type: usage_guide

```
(fbi-file          :type 'fbi-file)
   (interpol-file     :type 'interpol-file)))

In the above object definition, the message social-security-number
will be automatically available in the instances irs-records,
state-tax-returns, fbi-file, and interpol-file, unless otherwise
defined in those respective objects.

NOTE: :objects and :hidden-objects are automatically trickle-down.


2.7 Settable Slots
==================

Settable slots are just like normal slots, but their values can be
programmatically modified using the special object function :set-slot!.

Any other slots depending on them (directly or indirectly) will then
become unbound and be recomputed the next time they are demanded.

Example:

> (define-object container (base-object)
    :computed-slots
    ((name "Pristine" :settable)
     (full-name (string-append (the :name) " Container") :settable)))

>   (setq self (make-object 'container))
--> #<CONTAINER @ #x209495c2>

>   (the full-name)
--> "Pristine Container"
```

---

## usage.txt (chunk 20/34)
Source: usage.txt
Type: usage_guide

```
le)
     (full-name (string-append (the :name) " Container") :settable)))

>   (setq self (make-object 'container))
--> #<CONTAINER @ #x209495c2>

>   (the full-name)
--> "Pristine Container"

>   (the (set-slot! name "Tainted"))
--> "Tainted"

>   (the full-name)
--> "Tainted Container"

Both :computed-slots and :input-slots may be specified as :settable
(this includes :input-slots which are also specified as :defaulting).



3 Make-Object
=============

The basic constructor for GDL objects is ``make-object.''

This maps into a call to the Common Lisp function ``make-instance,''
with some extra operations to support the GDL machinery.

Keyword symbols are used to tag input values when passed into an
object in a call to make-object:

  Example 1:
   
   > (setq myobject (make-object 'person :first-name "Albert" :last-name "Einstein"))

 --> #<PERSON @ #x209274ee>

Toplevel inputs can also be specified by applying #'make-object to a
plist containing the inputs:

  Example 2:
```

---

## usage.txt (chunk 21/34)
Source: usage.txt
Type: usage_guide

```
on :first-name "Albert" :last-name "Einstein"))

 --> #<PERSON @ #x209274ee>

Toplevel inputs can also be specified by applying #'make-object to a
plist containing the inputs:

  Example 2:

   > (setq myobject (apply #'make-object 'person 
                           (list :first-name "Albert" 
                                 :last-name "Einstein")))
 --> #<PERSON @ #x209274fa>


4 the-object
============

You can send messages to individual object instances using the macro
``the-object:''

Example:

   > (the-object myobject full-name)
 --> "Albert Einstein"

The-object takes as its first argument an expression which returns an
object (i.e. instance), followed by a symbol naming a message returned
by that object. The symbol is immune to Lisp package, so a keyword
symbol may be used, but this is not a requirement. As we will see
later, the-object actually can take any number of symbols,
representing a reference chain down through an object hierarcy (see
"object hierarchies" below).
```

---

## usage.txt (chunk 22/34)
Source: usage.txt
Type: usage_guide

```
this is not a requirement. As we will see
later, the-object actually can take any number of symbols,
representing a reference chain down through an object hierarcy (see
"object hierarchies" below).

(The <instance>) expands to (the-object self <instance>), so you can
conveniently bind a variable named ``self'' to the result of a
make-object, then use a simple ``the'' to do referencing:

Example:

   > (setq self (apply #'make-object 'person 
                        (list :first-name "Albert" 
                              :last-name "Einstein")))
--> #<PERSON @ #x2092cc8a>


   > (the full-name)
 --> "Albert Einstein"



5 Evaluating Slot Names at Runtime
==================================

The ``evaluate'' macro can be used in cases where the message name is
not known until runtime -- it is wrapped around an expression which
returns a symbol naming a message. The symbol is immune to package, so
it may be a keyword or non-keyword symbol.

Example:
========
```

---

## usage.txt (chunk 23/34)
Source: usage.txt
Type: usage_guide

```
is wrapped around an expression which
returns a symbol naming a message. The symbol is immune to package, so
it may be a keyword or non-keyword symbol.

Example:
========

>   (setq my-key :full-name)
--> :FULL-NAME

>   (setq self (make-object 'container))
--> #<CONTAINER @ #x209495c2>

>   (the (evaluate my-key))
--> "Pristine Container"


6 Formats and Views
===================

6.1 Overview
============

The basic idea behind Formats and Views is that of providing different
perspectives on an object for the purposes of output. This concept is
something more than ``presentation methods'' as defined by CLIM. It is
more like ``presentation objects'' which contain ``presentation
methods.''

Core GDL follows the message-passing paradigm of object
orientation. You have objects which have slots, sub-objects,
functions, etc. These are all actually methods, or messages, ``on''
the object, i.e. the message passing paradigm.

Another way to look at message passing is to think that any given
```

---

## usage.txt (chunk 24/34)
Source: usage.txt
Type: usage_guide

```
ts, sub-objects,
functions, etc. These are all actually methods, or messages, ``on''
the object, i.e. the message passing paradigm.

Another way to look at message passing is to think that any given
method dispatches, or is specialized, only on a single argument, which
is the object to which it ``belongs.'' Formats and Views extend upon
this notion by allowing methods to dispatch on two arguments. The
first argument is a ``Format'' object, and the second argument is the
normal object just as with straight GDL.

Format objects are defined with ``define-format'' and instantiated
only when needed, inside the body of a ``with-format'' macro.

Methods which apply to a particular object and from the perspective of
a particular format are defined as :output-methods with
``define-lens''

6.2 define-format
=================

As its name implies, Define-Format is used to define new
formats. GDL/GWL comes with several pre-defined formats, so it is
```

---

## usage.txt (chunk 25/34)
Source: usage.txt
Type: usage_guide

```
e-format
=================

As its name implies, Define-Format is used to define new
formats. GDL/GWL comes with several pre-defined formats, so it is
likely that you will not need to define your own formats initially.

The syntax is

 (Define-Format <format-name> <mixin-list> <spec-plist>)

<Format-name> is any non-keyword symbol. A defclass will be generated
for this symbol, so any name you use will override a defclass if one
is already defined with the same name.

<Mixin-list> is a list of other format-names from which this format
will inherit. It maps directly into a CLOS mixin list.

<spec-plist> is a plist made up of pairs made from special keywords
and expression lists. Define-format in GDL currently only supports the
``:Functions'' section keyword.

6.2.1 functions
===============

Functions of a format are actual uncached methods on the format
object. They are defined with a normal (non-specialized) lambda
list. There is a variable ``stream'' dynamically bound within the body
```

---

## usage.txt (chunk 26/34)
Source: usage.txt
Type: usage_guide

```
Functions of a format are actual uncached methods on the format
object. They are defined with a normal (non-specialized) lambda
list. There is a variable ``stream'' dynamically bound within the body
of these functions, to which output is expected to be written.

Example:
========

(define-format base-format ()
  :functions
  ((a 
    (expression)
    (format stream "~a" expression))
   
   (newline-out
    ()
    (format stream "~%"))))


6.3 define-lens
===============

As its name implies, the define-lens macro is used to define a
``lens'' to a object, from the perspective of a given format. A lens
is a way of defining methods which apply to a object when ``viewed''
through the ``lens'' of a particular format. Therefore, views are
defined (and named) according to a particular object type, and a
particular format.

The Syntax is:

 (define-lens (<format-name> <object-type>) (<mixin-lists>) <spec-plist>)

<format-name> is the name of a format which must already be defined
```

---

## usage.txt (chunk 27/34)
Source: usage.txt
Type: usage_guide

```
e, and a
particular format.

The Syntax is:

 (define-lens (<format-name> <object-type>) (<mixin-lists>) <spec-plist>)

<format-name> is the name of a format which must already be defined
with define-format. <object-type> is the name of an object type which
must already be defined with define-object.

<mixin-lists> is currently unused. Inheritance for define-lens in GDL
currently simply follows the inheritance of the particular format and
object named in the define-lens. At some point more explicit inheritance
control might be added using these <mixin-lists>.

<spec-plist> is a plist made up of pairs made from special keywords
and expression lists define-lens in GDL currently only supports the
``:output-functions'' section keyword.

Output-functions are defined like normal :functions on an object,
however, in addition to sending messages to the object with normal
``the'' referencing macro, the ``write-env'' macro may also be used to
```

---

## usage.txt (chunk 28/34)
Source: usage.txt
Type: usage_guide

```
object,
however, in addition to sending messages to the object with normal
``the'' referencing macro, the ``write-env'' macro may also be used to
call :functions which are known to be defined for the associated
format.

Example:
========

(define-lens (base-format try)()
  :output-functions
  ((:summary
    ()
    (write-env (a "The value is: ") (a (the value))
	       (newline-out)
	       (a "The color of ``this'' is: ") (a (the this color))
	       (newline-out)
	       (a "The color of ``that'' is: ") (a (the (these 0) color))
	       (newline-out)))))

6.4 with-format
===============

The with-format macro sets up an environment for calling :functions of
formats (using ``write-env'' -- see below) and :output-functions of
views (using ``write-the'' and ``write-the-object'').

The syntax is:

 (With-format (<format-name> <stream-or-pathname>) 
    <body>)


<format-name> is the name of a format which has been defined with
``define-format''
```

---

## usage.txt (chunk 29/34)
Source: usage.txt
Type: usage_guide

```
ax is:

 (With-format (<format-name> <stream-or-pathname>) 
    <body>)


<format-name> is the name of a format which has been defined with
``define-format''

<stream-or-pathname> is a variable or expression which evaluates to a
stream which can accept output or to a string or pathname which can be
opened to accept output.

<body> can contain any normal Lisp expressions as well as the format
and view reference macros ``write-env,'' ``write-the-object,'' and
``write-the'' (see below).

Within <body>, the parameter ``stream'' will be dynamically bound to
the stream specified by <stream-or-pathname>, or to a file stream
opened to stream-or-pathname, if it is a string or pathname. Because
it is dynamically bound, this means any other functions or methods
called within <body> will also see the correct value of ``stream.''

6.4.1 Write-Env
===============

Write-env must be called either within the (dynamic) body of a
``with-format'' or within an :output-function of a view, and is used
```

---

## usage.txt (chunk 30/34)
Source: usage.txt
Type: usage_guide

```
correct value of ``stream.''

6.4.1 Write-Env
===============

Write-env must be called either within the (dynamic) body of a
``with-format'' or within an :output-function of a view, and is used
to invoke :functions defined on the specified format

Examples:

(define-lens (base-format try)()
  :output-functions
  ((summary
    ()
    (write-env (a "The value is: ") (a (the value))
	       (newline-out)
	       (a "The color of ``this'' is: ") (a (the this color))
	       (newline-out)
	       (a "The color of ``that'' is: ") (a (the (these 0) color))
	       (newline-out)))))

(with-format (base-writer "/tmp/try.txt")
  (write-env (a "This is a test")))


6.4.2 Write-The-Object
======================

Syntax:

(write-the-object <object> <reference-chain>)

``write-the-object'' works in similar fashion to ``the-object'' in the
sense that it handles reference chains, but the last element in the
reference chain must name a :output-function defined in a relevant
view.
```

---

## usage.txt (chunk 31/34)
Source: usage.txt
Type: usage_guide

```
ilar fashion to ``the-object'' in the
sense that it handles reference chains, but the last element in the
reference chain must name a :output-function defined in a relevant
view. ``Write-the-object'' must be called inside the (dynamic) body of
a ``with-format'' so that the effective format and stream will be
known. The :output-function indicated by the last element of the
reference chain will be invoked, which presumably will write some
output to the specified stream.

Currently the ``evaluate'' macro is not implemented in GDL to resolve
the :write-method name at runtime, so this name must be given as a
literal symbol in the compiled source.


Example:
========

(with-writer (base-format *standard-output*)
  (write-the-object (make-object 'try) (summary)))

2.4.3 Write-The
===============

Syntax:

(write-the <reference-chain>)

``Write-the'' is similar to ``Write-the-object,'' but it assumes
``self'' as the object, so it is not necessary to pass the object
```

---

## usage.txt (chunk 32/34)
Source: usage.txt
Type: usage_guide

```
Syntax:

(write-the <reference-chain>)

``Write-the'' is similar to ``Write-the-object,'' but it assumes
``self'' as the object, so it is not necessary to pass the object
explicitly to ``write-the'' as is necessary with ``write-the-object.''

Example:
========

(with-writer (base-format *standard-output*)
  (let ((self (make-object 'try)))
    (write-the (summary))))


For further examples and a listing of built-in formats currently
shipping with GDL/GWL, please see output-formats.txt.



7 Object Amendments
===================

The macro define-object-amendment can be used to extend and/or
redefine both user-defined objects and built-in GDL objects. The
syntax for define-object-amendment is identical to that for
define-object. Any additional elements will be added to the
definition, and any elements with the same names as existing elements
will overwrite the existing elements currently loaded into the system.



8 Extensions and Implementations
================================
```

---

## usage.txt (chunk 33/34)
Source: usage.txt
Type: usage_guide

```
any elements with the same names as existing elements
will overwrite the existing elements currently loaded into the system.



8 Extensions and Implementations
================================

Genworks also provides a large set of built-in primitives and
interfaces for our GDL product, written in the GDL language. 

Although Genworks currently produces the only available full-featured
implementation of the GDL language specification, this core language
specification also represents something of a de-facto standard for KBE
languages based in ANSI Common Lisp. If new implementations emerge, we
encourage them to adopt this standard as well, and communicate with
Genworks regarding refinements and further extensions, so that the
Industry can move toward a true vendor-neutral Standard KBE language
specification.
```

---

## usage.txt (chunk 34/34)
Source: usage.txt
Type: usage_guide

```
uage
specification.
```

---

## system-index.txt (chunk 1/1)
Source: gornschool-training/system-index.txt
Type: tutorial

```
training.asd

```

---

## README.txt (chunk 1/1)
Source: gornschool-training/tailwind/README.txt
Type: tutorial

```
Development setup:

1. Make sure the "nodejs" package is installed on your system such
  that `node --version` in your command-line will return a version
  (OS-specific)

2. Change directories into this directory in your command-line and
   issue the command:

  `npm install`

3. Confirm the correct command-line for builds in `package.json`.

4. Start the running (& watching) build process with

   `npm run build`

5. Now your deployable css will be updated every time you trouch a
   file in your project. Keep an eye on the terminal where the build
   is running for any syntax errors (probably coming from your base
   css file in this directory, named with `-i` in the `package.json`
   but probably, <your-app>-base.css.





```

---

## yadd.html (chunk 1/1)
Source: yadd-reference/yadd.html
Type: reference

```
GDL Reference Documentation Reference Documentation for Genworks ® General-purpose Declarative Language, Related Packages, and User packages Documented Packages: CL-LITE (Compile-and-Load Lite Utility) GENDL (Base Core Kernel Engine) Nicknames: Gdl, Genworks, Base GENDL-MCP GEOM-BASE (Wireframe Geometry) GEYSR (Web-based Development Environment (geysr)) GWL (Generative Web Language (GWL)) YADD (Yet Another Definition Documenter (yadd)) Master Index Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
GendL Application - MASTER-INDEX Index for Relevant GDL Symbols Documentation Home *allow-nil-list-of-numbers?* ( Parameter or Constant, gendl) *bias-to-double-float?* ( Parameter or Constant, gendl) *break-leaders?* ( Parameter or Constant, geom-base) *break-on-set-self?* ( Parameter or Constant, gwl) *bypass-security-check?* ( Parameter or Constant, gwl) *color-table* ( Parameter or Constant, gendl) *color-table-decimal* ( Parameter or Constant, gendl) *colors-default* ( Parameter or Constant, gendl) *compile-circular-reference-detection?* ( Parameter or Constant, gendl) *compile-dependency-tracking?* ( Parameter or Constant, gendl) *compile-documentation-database?* ( Parameter or Constant, gendl) *compile-for-dgdl?* ( Parameter or Constant, gendl) *compile-source-code-database?* ( Parameter or Constant, gendl) *curve-chords* ( Parameter or Constant, gendl) *developing?* ( Parameter or Constant, gwl) *ensure-lists-when-bashing?* ( Parameter or Constant, gendl) *ent* ( Parameter or
```

---

## index.html (chunk 2/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
eter or Constant, gendl) *curve-chords* ( Parameter or Constant, gendl) *developing?* ( Parameter or Constant, gwl) *ensure-lists-when-bashing?* ( Parameter or Constant, gendl) *ent* ( Parameter or Constant, gwl) *failed-request-url* ( Parameter or Constant, gwl) *gs-graphics-alpha-bits* ( Parameter or Constant, geom-base) *gs-text-alpha-bits* ( Parameter or Constant, geom-base) *hash-transforms?* ( Parameter or Constant, geom-base) *instance-finalizers* ( Parameter or Constant, gwl) *instance-hash-table* ( Parameter or Constant, gwl) *jump-to-toplevel-on-set-self?* ( Parameter or Constant, gwl) *load-documentation-database?* ( Parameter or Constant, gendl) *load-source-code-database?* ( Parameter or Constant, gendl) *max-id-value* ( Parameter or Constant, gwl) *on-syntax-error* ( Parameter or Constant, gendl) *out-of-bounds-sequence-reference-action* ( Parameter or Constant, gendl) *publishers* ( Parameter or Constant, gwl) *query* ( Parameter or Constant, gwl)
```

---

## index.html (chunk 3/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
r or Constant, gendl) *out-of-bounds-sequence-reference-action* ( Parameter or Constant, gendl) *publishers* ( Parameter or Constant, gwl) *query* ( Parameter or Constant, gwl) *reap-expired-sessions?* ( Parameter or Constant, gwl) *recovery-url-default* ( Parameter or Constant, gwl) *remember-previous-slot-values?* ( Parameter or Constant, gendl) *req* ( Parameter or Constant, gwl) *root-checking-enabled?* ( Parameter or Constant, gendl) *run-with-circular-reference-detection?* ( Parameter or Constant, gendl) *run-with-dependency-tracking?* ( Parameter or Constant, gendl) *sort-children?* ( Parameter or Constant, gendl) *suppress-$$-messages?* ( Parameter or Constant, geysr) *suppress-%%-messages?* ( Parameter or Constant, geysr) *undeclared-parameters-enabled?* ( Parameter or Constant, gendl) *with-format-direction ( Parameter or Constant, gendl) *with-format-external-format* ( Parameter or Constant, gendl) *with-format-if-does-not-exist* ( Parameter or Constant, gendl)
```

---

## index.html (chunk 4/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
endl) *with-format-direction ( Parameter or Constant, gendl) *with-format-external-format* ( Parameter or Constant, gendl) *with-format-if-does-not-exist* ( Parameter or Constant, gendl) *with-format-if-exists* ( Parameter or Constant, gendl) *zero-epsilon* ( Parameter or Constant, gendl) *zero-vector-checking?* ( Parameter or Constant, geom-base) +phi+ ( Parameter or Constant, gendl) +postnet-bits+ ( Parameter or Constant, geom-base) 2pi ( Parameter or Constant, gendl) 3D-BOX ( GDL object message on GEOM-BASE::RENDERER-MIXIN, keyword) 3D-BOX ( GDL object message on GWL:WEB-DRAWING, keyword) 3D-BOX-CENTER ( GDL object message on GEOM-BASE::RENDERER-MIXIN, keyword) 3D-BOX-CENTER ( GDL object message on GWL:WEB-DRAWING, keyword) 3d-distance ( Function, geom-base) 3d-point-p ( Function, geom-base) 3d-point? ( Function, geom-base) 3d-vector-p ( Function, geom-base) 3d-vector-to-array ( Function, geom-base) 3d-vector? ( Function, geom-base) ACCEPT ( GDL object message on
```

---

## index.html (chunk 5/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
base) 3d-point? ( Function, geom-base) 3d-vector-p ( Function, geom-base) 3d-vector-to-array ( Function, geom-base) 3d-vector? ( Function, geom-base) ACCEPT ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) ACCEPT ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) ACCEPT ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) ACCEPT ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) ACCEPT ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) ACCESSKEY ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) ACCESSKEY ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) ACCESSKEY ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) ACCESSKEY ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) ACCESSKEY ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) acosd ( Function, geom-base) add-matrices ( Function, geom-base) add-vectors ( Function, geom-base) ADDITIONAL-HEADER-CONTENT ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword)
```

---

## index.html (chunk 6/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
ord) acosd ( Function, geom-base) add-matrices ( Function, geom-base) add-vectors ( Function, geom-base) ADDITIONAL-HEADER-CONTENT ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) ADDITIONAL-HEADER-CONTENT ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) ADDITIONAL-HEADER-CONTENT ( GDL object message on YADD::BASE-YADD-SHEET, keyword) ADDITIONAL-HEADER-JS ( GDL object message on YADD::BASE-YADD-SHEET, keyword) ADDITIONAL-HEADER-JS-CONTENT ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) ADDITIONAL-HEADER-JS-CONTENT ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) ADDITIONAL-HEADER-JS-CONTENT ( GDL object message on YADD:ASSEMBLY, keyword) ADDITIONAL-HEADER-JS-CONTENT ( GDL object message on YADD::BASE-YADD-SHEET, keyword) ADDITIONAL-HEADER-JS-CONTENT ( GDL object message on YADD::MASTER-INDEX, keyword) ADDITIONAL-HEADER-JS-CONTENT ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) ADDITIONAL-HEADER-JS-CONTENT ( GDL object message on
```

---

## index.html (chunk 7/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
GDL object message on YADD::MASTER-INDEX, keyword) ADDITIONAL-HEADER-JS-CONTENT ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) ADDITIONAL-HEADER-JS-CONTENT ( GDL object message on YADD::PACKAGE-FORM, keyword) AFTER-PRESENT! ( GDL object message on GWL:APPLICATION-MIXIN, keyword) AFTER-PRESENT! ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) AFTER-PRESENT! ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) AFTER-PRESENT! ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) AFTER-PRESENT! ( GDL object message on GWL:BASE-HTML-SHEET, keyword) AFTER-PRESENT! ( GDL object message on GWL::COLOR-MAP, keyword) AFTER-PRESENT! ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) AFTER-PRESENT! ( GDL object message on GWL::LAYOUT-MIXIN, keyword) AFTER-PRESENT! ( GDL object message on GWL:NODE-MIXIN, keyword) AFTER-PRESENT! ( GDL object message on YADD:ASSEMBLY, keyword) AFTER-PRESENT! ( GDL object message on YADD::BASE-YADD-SHEET, keyword)
```

---

## index.html (chunk 8/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
NT! ( GDL object message on GWL:NODE-MIXIN, keyword) AFTER-PRESENT! ( GDL object message on YADD:ASSEMBLY, keyword) AFTER-PRESENT! ( GDL object message on YADD::BASE-YADD-SHEET, keyword) AFTER-PRESENT! ( GDL object message on YADD::MASTER-INDEX, keyword) AFTER-PRESENT! ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) AFTER-PRESENT! ( GDL object message on YADD::PACKAGE-FORM, keyword) AFTER-SET! ( GDL object message on GWL:APPLICATION-MIXIN, keyword) AFTER-SET! ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) AFTER-SET! ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) AFTER-SET! ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) AFTER-SET! ( GDL object message on GWL:BASE-HTML-SHEET, keyword) AFTER-SET! ( GDL object message on GWL::COLOR-MAP, keyword) AFTER-SET! ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) AFTER-SET! ( GDL object message on GWL::LAYOUT-MIXIN, keyword) AFTER-SET! ( GDL object message on GWL:NODE-MIXIN, keyword)
```

---

## index.html (chunk 9/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
rd) AFTER-SET! ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) AFTER-SET! ( GDL object message on GWL::LAYOUT-MIXIN, keyword) AFTER-SET! ( GDL object message on GWL:NODE-MIXIN, keyword) AFTER-SET! ( GDL object message on YADD:ASSEMBLY, keyword) AFTER-SET! ( GDL object message on YADD::BASE-YADD-SHEET, keyword) AFTER-SET! ( GDL object message on YADD::MASTER-INDEX, keyword) AFTER-SET! ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) AFTER-SET! ( GDL object message on YADD::PACKAGE-FORM, keyword) AGGREGATE ( GDL object message on VANILLA-MIXIN*, keyword) AJAX-SUBMIT-ON-CHANGE? ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) AJAX-SUBMIT-ON-CHANGE? ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) AJAX-SUBMIT-ON-CHANGE? ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) AJAX-SUBMIT-ON-CHANGE? ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) AJAX-SUBMIT-ON-CHANGE? ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) AJAX-SUBMIT-ON-ENTER?
```

---

## index.html (chunk 10/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
CONTROL, keyword) AJAX-SUBMIT-ON-CHANGE? ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) AJAX-SUBMIT-ON-CHANGE? ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) AJAX-SUBMIT-ON-ENTER? ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) AJAX-SUBMIT-ON-ENTER? ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) AJAX-SUBMIT-ON-ENTER? ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) AJAX-SUBMIT-ON-ENTER? ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) AJAX-SUBMIT-ON-ENTER? ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) ALIGN ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) ALIGN ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) ALIGN ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) ALIGN ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) ALIGN ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) alignment ( Function, geom-base) alist2plist ( Function, gendl) ALL-MIXINS ( GDL object message on
```

---

## index.html (chunk 11/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
O-FORM-CONTROL, keyword) ALIGN ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) alignment ( Function, geom-base) alist2plist ( Function, gendl) ALL-MIXINS ( GDL object message on VANILLA-MIXIN*, keyword) ALLOW-INVALID-TYPE? ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) ALLOW-INVALID-TYPE? ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) ALLOW-INVALID-TYPE? ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) ALLOW-INVALID-TYPE? ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) ALLOW-INVALID-TYPE? ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) ALLOW-INVALID? ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) ALLOW-INVALID? ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) ALLOW-INVALID? ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) ALLOW-INVALID? ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) ALLOW-INVALID? ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) ALLOW-NIL? ( GDL object message on
```

---

## index.html (chunk 12/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
OL, keyword) ALLOW-INVALID? ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) ALLOW-INVALID? ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) ALLOW-NIL? ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) ALLOW-NIL? ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) ALLOW-NIL? ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) ALLOW-NIL? ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) ALLOW-NIL? ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) ALT ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) ALT ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) ALT ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) ALT ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) ALT ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) always ( Function, gendl) angle-between-vectors ( Function, geom-base) angle-between-vectors-d ( Function, geom-base) ANGULAR-DIMENSION ( Object, geom-base) ANNOTATION-OBJECTS ( GDL object message
```

---

## index.html (chunk 13/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
always ( Function, gendl) angle-between-vectors ( Function, geom-base) angle-between-vectors-d ( Function, geom-base) ANGULAR-DIMENSION ( Object, geom-base) ANNOTATION-OBJECTS ( GDL object message on BASE-VIEW, keyword) append-elements ( Macro, gendl) APPEND-ERROR-STRING? ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) APPEND-ERROR-STRING? ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) APPEND-ERROR-STRING? ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) APPEND-ERROR-STRING? ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) APPEND-ERROR-STRING? ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) APPLICATION-FASLS ( GDL object message on GDL-APP, keyword) APPLICATION-MIXIN ( Object, gwl) APPLICATION-NAME ( GDL object message on GDL-APP, keyword) apply-make-point ( Function, geom-base) ARC ( Object, geom-base) ARC ( GDL object message on TORUS, keyword) ARC-OBJECT ( GDL object message on ANGULAR-DIMENSION, keyword) ARCOID-MIXIN ( Object,
```

---

## index.html (chunk 14/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
-make-point ( Function, geom-base) ARC ( Object, geom-base) ARC ( GDL object message on TORUS, keyword) ARC-OBJECT ( GDL object message on ANGULAR-DIMENSION, keyword) ARCOID-MIXIN ( Object, geom-base) AREA ( GDL object message on CIRCLE, keyword) array-to-3d-vector ( Function, geom-base) array-to-list ( Function, geom-base) ARROWHEAD-LENGTH ( GDL object message on ANGULAR-DIMENSION, keyword) ARROWHEAD-LENGTH ( GDL object message on HORIZONTAL-DIMENSION, keyword) ARROWHEAD-LENGTH ( GDL object message on LABEL, keyword) ARROWHEAD-LENGTH ( GDL object message on LEADER-LINE, keyword) ARROWHEAD-LENGTH ( GDL object message on LINEAR-DIMENSION, keyword) ARROWHEAD-LENGTH ( GDL object message on PARALLEL-DIMENSION, keyword) ARROWHEAD-LENGTH ( GDL object message on VERTICAL-DIMENSION, keyword) ARROWHEAD-STYLE ( GDL object message on ANGULAR-DIMENSION, keyword) ARROWHEAD-STYLE ( GDL object message on HORIZONTAL-DIMENSION, keyword) ARROWHEAD-STYLE ( GDL object message on LABEL, keyword)
```

---

## index.html (chunk 15/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
HEAD-STYLE ( GDL object message on ANGULAR-DIMENSION, keyword) ARROWHEAD-STYLE ( GDL object message on HORIZONTAL-DIMENSION, keyword) ARROWHEAD-STYLE ( GDL object message on LABEL, keyword) ARROWHEAD-STYLE ( GDL object message on LEADER-LINE, keyword) ARROWHEAD-STYLE ( GDL object message on LINEAR-DIMENSION, keyword) ARROWHEAD-STYLE ( GDL object message on PARALLEL-DIMENSION, keyword) ARROWHEAD-STYLE ( GDL object message on VERTICAL-DIMENSION, keyword) ARROWHEAD-STYLE-2 ( GDL object message on ANGULAR-DIMENSION, keyword) ARROWHEAD-STYLE-2 ( GDL object message on HORIZONTAL-DIMENSION, keyword) ARROWHEAD-STYLE-2 ( GDL object message on LABEL, keyword) ARROWHEAD-STYLE-2 ( GDL object message on LEADER-LINE, keyword) ARROWHEAD-STYLE-2 ( GDL object message on LINEAR-DIMENSION, keyword) ARROWHEAD-STYLE-2 ( GDL object message on PARALLEL-DIMENSION, keyword) ARROWHEAD-STYLE-2 ( GDL object message on VERTICAL-DIMENSION, keyword) ARROWHEAD-WIDTH ( GDL object message on ANGULAR-DIMENSION,
```

---

## index.html (chunk 16/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
-STYLE-2 ( GDL object message on PARALLEL-DIMENSION, keyword) ARROWHEAD-STYLE-2 ( GDL object message on VERTICAL-DIMENSION, keyword) ARROWHEAD-WIDTH ( GDL object message on ANGULAR-DIMENSION, keyword) ARROWHEAD-WIDTH ( GDL object message on HORIZONTAL-DIMENSION, keyword) ARROWHEAD-WIDTH ( GDL object message on LABEL, keyword) ARROWHEAD-WIDTH ( GDL object message on LEADER-LINE, keyword) ARROWHEAD-WIDTH ( GDL object message on LINEAR-DIMENSION, keyword) ARROWHEAD-WIDTH ( GDL object message on PARALLEL-DIMENSION, keyword) ARROWHEAD-WIDTH ( GDL object message on VERTICAL-DIMENSION, keyword) asind ( Function, geom-base) ASSEMBLY ( Object, yadd) atand ( Function, geom-base) AVAILABLE-IMAGE-FORMATS ( GDL object message on GWL:APPLICATION-MIXIN, keyword) AVAILABLE-IMAGE-FORMATS ( GDL object message on GWL::LAYOUT-MIXIN, keyword) AVAILABLE-IMAGE-FORMATS ( GDL object message on GWL:NODE-MIXIN, keyword) AXIS-LENGTH ( GDL object message on SPHERICAL-CAP, keyword) AXIS-VECTOR ( GDL object message
```

---

## index.html (chunk 17/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
ge on GWL::LAYOUT-MIXIN, keyword) AVAILABLE-IMAGE-FORMATS ( GDL object message on GWL:NODE-MIXIN, keyword) AXIS-LENGTH ( GDL object message on SPHERICAL-CAP, keyword) AXIS-VECTOR ( GDL object message on BASE-OBJECT, keyword) BACKGROUND-COLOR ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) BACKGROUND-COLOR ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) BASE-AJAX-GRAPHICS-SHEET ( Object, gwl) BASE-AJAX-SHEET ( Object, gwl) BASE-COORDINATE-SYSTEM ( Object, geom-base) BASE-DRAWING ( Object, geom-base) BASE-FORM-CONTROL ( Object, gwl) BASE-HTML-GRAPHICS-SHEET ( Object, gwl) BASE-HTML-SHEET ( Object, gwl) BASE-OBJECT ( Object, geom-base) BASE-PLANE-NORMAL ( GDL object message on ANGULAR-DIMENSION, keyword) BASE-PLANE-NORMAL ( GDL object message on HORIZONTAL-DIMENSION, keyword) BASE-PLANE-NORMAL ( GDL object message on LINEAR-DIMENSION, keyword) BASE-PLANE-NORMAL ( GDL object message on PARALLEL-DIMENSION, keyword) BASE-PLANE-NORMAL ( GDL object message on
```

---

## index.html (chunk 18/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
ION, keyword) BASE-PLANE-NORMAL ( GDL object message on LINEAR-DIMENSION, keyword) BASE-PLANE-NORMAL ( GDL object message on PARALLEL-DIMENSION, keyword) BASE-PLANE-NORMAL ( GDL object message on VERTICAL-DIMENSION, keyword) BASE-RADIUS ( GDL object message on SPHERICAL-CAP, keyword) BASE-RULE-OBJECT ( Object, gendl) BASE-VIEW ( Object, geom-base) BASE-YADD-SHEET ( Object, yadd) base64-decode-list ( Function, gwl) base64-decode-safe ( Function, gwl) base64-encode-list ( Function, gwl) base64-encode-safe ( Function, gwl) BASHEE ( GDL object message on GEYSR::MENU, keyword) BASHEE ( GDL object message on GEYSR:TREE, keyword) BASHEE ( GDL object message on GWL:APPLICATION-MIXIN, keyword) BASHEE ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) BASHEE ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) BASHEE ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) BASHEE ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) BASHEE ( GDL object message on
```

---

## index.html (chunk 19/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
ASE-AJAX-SHEET, keyword) BASHEE ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) BASHEE ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) BASHEE ( GDL object message on GWL:BASE-HTML-SHEET, keyword) BASHEE ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) BASHEE ( GDL object message on GWL::COLOR-MAP, keyword) BASHEE ( GDL object message on GWL:GRID-FORM-CONTROL, keyword) BASHEE ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) BASHEE ( GDL object message on GWL::LAYOUT-MIXIN, keyword) BASHEE ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) BASHEE ( GDL object message on GWL:NODE-MIXIN, keyword) BASHEE ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) BASHEE ( GDL object message on GWL:SHEET-SECTION, keyword) BASHEE ( GDL object message on GWL::SKELETON-FORM-CONTROL, keyword) BASHEE ( GDL object message on GWL:SKELETON-UI-ELEMENT, keyword) BASHEE ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) BASHEE ( GDL object message on
```

---

## index.html (chunk 20/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
L::SKELETON-FORM-CONTROL, keyword) BASHEE ( GDL object message on GWL:SKELETON-UI-ELEMENT, keyword) BASHEE ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) BASHEE ( GDL object message on YADD:ASSEMBLY, keyword) BASHEE ( GDL object message on YADD::BASE-YADD-SHEET, keyword) BASHEE ( GDL object message on YADD::MASTER-INDEX, keyword) BASHEE ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) BASHEE ( GDL object message on YADD::PACKAGE-FORM, keyword) BEFORE-PRESENT! ( GDL object message on GWL:APPLICATION-MIXIN, keyword) BEFORE-PRESENT! ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) BEFORE-PRESENT! ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) BEFORE-PRESENT! ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) BEFORE-PRESENT! ( GDL object message on GWL:BASE-HTML-SHEET, keyword) BEFORE-PRESENT! ( GDL object message on GWL::COLOR-MAP, keyword) BEFORE-PRESENT! ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) BEFORE-PRESENT! ( GDL
```

---

## index.html (chunk 21/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
ssage on GWL:BASE-HTML-SHEET, keyword) BEFORE-PRESENT! ( GDL object message on GWL::COLOR-MAP, keyword) BEFORE-PRESENT! ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) BEFORE-PRESENT! ( GDL object message on GWL::LAYOUT-MIXIN, keyword) BEFORE-PRESENT! ( GDL object message on GWL:NODE-MIXIN, keyword) BEFORE-PRESENT! ( GDL object message on YADD:ASSEMBLY, keyword) BEFORE-PRESENT! ( GDL object message on YADD::BASE-YADD-SHEET, keyword) BEFORE-PRESENT! ( GDL object message on YADD::MASTER-INDEX, keyword) BEFORE-PRESENT! ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) BEFORE-PRESENT! ( GDL object message on YADD::PACKAGE-FORM, keyword) BEFORE-RESPONSE! ( GDL object message on GWL:APPLICATION-MIXIN, keyword) BEFORE-RESPONSE! ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) BEFORE-RESPONSE! ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) BEFORE-RESPONSE! ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) BEFORE-RESPONSE! ( GDL object message
```

---

## index.html (chunk 22/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
, keyword) BEFORE-RESPONSE! ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) BEFORE-RESPONSE! ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) BEFORE-RESPONSE! ( GDL object message on GWL:BASE-HTML-SHEET, keyword) BEFORE-RESPONSE! ( GDL object message on GWL::COLOR-MAP, keyword) BEFORE-RESPONSE! ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) BEFORE-RESPONSE! ( GDL object message on GWL::LAYOUT-MIXIN, keyword) BEFORE-RESPONSE! ( GDL object message on GWL:NODE-MIXIN, keyword) BEFORE-RESPONSE! ( GDL object message on YADD:ASSEMBLY, keyword) BEFORE-RESPONSE! ( GDL object message on YADD::BASE-YADD-SHEET, keyword) BEFORE-RESPONSE! ( GDL object message on YADD::MASTER-INDEX, keyword) BEFORE-RESPONSE! ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) BEFORE-RESPONSE! ( GDL object message on YADD::PACKAGE-FORM, keyword) BEFORE-SET! ( GDL object message on GWL:APPLICATION-MIXIN, keyword) BEFORE-SET! ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET,
```

---

## index.html (chunk 23/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
SPONSE! ( GDL object message on YADD::PACKAGE-FORM, keyword) BEFORE-SET! ( GDL object message on GWL:APPLICATION-MIXIN, keyword) BEFORE-SET! ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) BEFORE-SET! ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) BEFORE-SET! ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) BEFORE-SET! ( GDL object message on GWL:BASE-HTML-SHEET, keyword) BEFORE-SET! ( GDL object message on GWL::COLOR-MAP, keyword) BEFORE-SET! ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) BEFORE-SET! ( GDL object message on GWL::LAYOUT-MIXIN, keyword) BEFORE-SET! ( GDL object message on GWL:NODE-MIXIN, keyword) BEFORE-SET! ( GDL object message on YADD:ASSEMBLY, keyword) BEFORE-SET! ( GDL object message on YADD::BASE-YADD-SHEET, keyword) BEFORE-SET! ( GDL object message on YADD::MASTER-INDEX, keyword) BEFORE-SET! ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) BEFORE-SET! ( GDL object message on YADD::PACKAGE-FORM, keyword)
```

---

## index.html (chunk 24/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
T! ( GDL object message on YADD::MASTER-INDEX, keyword) BEFORE-SET! ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) BEFORE-SET! ( GDL object message on YADD::PACKAGE-FORM, keyword) BEZIER-CURVE ( Object, geom-base) BIN-SUBDIR-NAMES ( GDL object message on CL-LITE:CODEBASE-DIRECTORY-NODE, keyword) BODY-BGCOLOR ( GDL object message on GWL:APPLICATION-MIXIN, keyword) BODY-BGCOLOR ( GDL object message on GWL::LAYOUT-MIXIN, keyword) BODY-BGCOLOR ( GDL object message on GWL:NODE-MIXIN, keyword) BODY-CLASS ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) BODY-CLASS ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) BODY-CLASS ( GDL object message on YADD:ASSEMBLY, keyword) BODY-CLASS ( GDL object message on YADD::BASE-YADD-SHEET, keyword) BODY-CLASS ( GDL object message on YADD::MASTER-INDEX, keyword) BODY-CLASS ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) BODY-CLASS ( GDL object message on YADD::PACKAGE-FORM, keyword) BODY-ONLOAD ( GDL object
```

---

## index.html (chunk 25/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
age on YADD::MASTER-INDEX, keyword) BODY-CLASS ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) BODY-CLASS ( GDL object message on YADD::PACKAGE-FORM, keyword) BODY-ONLOAD ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) BODY-ONLOAD ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) BODY-ONLOAD ( GDL object message on YADD:ASSEMBLY, keyword) BODY-ONLOAD ( GDL object message on YADD::BASE-YADD-SHEET, keyword) BODY-ONLOAD ( GDL object message on YADD::MASTER-INDEX, keyword) BODY-ONLOAD ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) BODY-ONLOAD ( GDL object message on YADD::PACKAGE-FORM, keyword) BODY-ONPAGESHOW ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) BODY-ONPAGESHOW ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) BODY-ONPAGESHOW ( GDL object message on YADD:ASSEMBLY, keyword) BODY-ONPAGESHOW ( GDL object message on YADD::BASE-YADD-SHEET, keyword) BODY-ONPAGESHOW ( GDL object message on YADD::MASTER-INDEX, keyword)
```

---

## index.html (chunk 26/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
PAGESHOW ( GDL object message on YADD:ASSEMBLY, keyword) BODY-ONPAGESHOW ( GDL object message on YADD::BASE-YADD-SHEET, keyword) BODY-ONPAGESHOW ( GDL object message on YADD::MASTER-INDEX, keyword) BODY-ONPAGESHOW ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) BODY-ONPAGESHOW ( GDL object message on YADD::PACKAGE-FORM, keyword) BODY-ONRESIZE ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) BODY-ONRESIZE ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) BODY-ONRESIZE ( GDL object message on YADD:ASSEMBLY, keyword) BODY-ONRESIZE ( GDL object message on YADD::BASE-YADD-SHEET, keyword) BODY-ONRESIZE ( GDL object message on YADD::MASTER-INDEX, keyword) BODY-ONRESIZE ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) BODY-ONRESIZE ( GDL object message on YADD::PACKAGE-FORM, keyword) BORDER-BOX? ( GDL object message on BASE-VIEW, keyword) BOTTOM-CAP? ( GDL object message on C-CYLINDER, keyword) BOTTOM-CAP? ( GDL object message on CONE, keyword)
```

---

## index.html (chunk 27/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
YADD::PACKAGE-FORM, keyword) BORDER-BOX? ( GDL object message on BASE-VIEW, keyword) BOTTOM-CAP? ( GDL object message on C-CYLINDER, keyword) BOTTOM-CAP? ( GDL object message on CONE, keyword) BOTTOM-CAP? ( GDL object message on CYLINDER, keyword) BOUNDING-BBOX ( GDL object message on BASE-OBJECT, keyword) BOUNDING-BOX ( GDL object message on ANGULAR-DIMENSION, keyword) BOUNDING-BOX ( GDL object message on ARC, keyword) BOUNDING-BOX ( GDL object message on BASE-COORDINATE-SYSTEM, keyword) BOUNDING-BOX ( GDL object message on BASE-DRAWING, keyword) BOUNDING-BOX ( GDL object message on BASE-OBJECT, keyword) BOUNDING-BOX ( GDL object message on BASE-VIEW, keyword) BOUNDING-BOX ( GDL object message on BEZIER-CURVE, keyword) BOUNDING-BOX ( GDL object message on BOX, keyword) BOUNDING-BOX ( GDL object message on C-CYLINDER, keyword) BOUNDING-BOX ( GDL object message on CENTER-LINE, keyword) BOUNDING-BOX ( GDL object message on CIRCLE, keyword) BOUNDING-BOX ( GDL object message on CONE,
```

---

## index.html (chunk 28/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
object message on C-CYLINDER, keyword) BOUNDING-BOX ( GDL object message on CENTER-LINE, keyword) BOUNDING-BOX ( GDL object message on CIRCLE, keyword) BOUNDING-BOX ( GDL object message on CONE, keyword) BOUNDING-BOX ( GDL object message on CONSTRAINED-ARC, keyword) BOUNDING-BOX ( GDL object message on CONSTRAINED-FILLET, keyword) BOUNDING-BOX ( GDL object message on CYLINDER, keyword) BOUNDING-BOX ( GDL object message on ELLIPSE, keyword) BOUNDING-BOX ( GDL object message on GENERAL-NOTE, keyword) BOUNDING-BOX ( GDL object message on GLOBAL-POLYGON-PROJECTION, keyword) BOUNDING-BOX ( GDL object message on GEOM-BASE::GLOBAL-POLYLINE-MIXIN, keyword) BOUNDING-BOX ( GDL object message on HORIZONTAL-DIMENSION, keyword) BOUNDING-BOX ( GDL object message on LABEL, keyword) BOUNDING-BOX ( GDL object message on LEADER-LINE, keyword) BOUNDING-BOX ( GDL object message on LINE, keyword) BOUNDING-BOX ( GDL object message on LINEAR-DIMENSION, keyword) BOUNDING-BOX ( GDL object message on
```

---

## index.html (chunk 29/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
ject message on LEADER-LINE, keyword) BOUNDING-BOX ( GDL object message on LINE, keyword) BOUNDING-BOX ( GDL object message on LINEAR-DIMENSION, keyword) BOUNDING-BOX ( GDL object message on PARALLEL-DIMENSION, keyword) BOUNDING-BOX ( GDL object message on PIE-CHART, keyword) BOUNDING-BOX ( GDL object message on POINT, keyword) BOUNDING-BOX ( GDL object message on POINTS-DISPLAY, keyword) BOUNDING-BOX ( GDL object message on ROUTE-PIPE, keyword) BOUNDING-BOX ( GDL object message on SAMPLE-DRAWING, keyword) BOUNDING-BOX ( GDL object message on SPHERE, keyword) BOUNDING-BOX ( GDL object message on SPHERICAL-CAP, keyword) BOUNDING-BOX ( GDL object message on TEXT-LINE, keyword) BOUNDING-BOX ( GDL object message on TORUS, keyword) BOUNDING-BOX ( GDL object message on TYPESET-BLOCK, keyword) BOUNDING-BOX ( GDL object message on VERTICAL-DIMENSION, keyword) BOUNDING-BOX ( GDL object message on GWL:APPLICATION-MIXIN, keyword) BOUNDING-BOX ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET,
```

---

## index.html (chunk 30/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
UNDING-BOX ( GDL object message on VERTICAL-DIMENSION, keyword) BOUNDING-BOX ( GDL object message on GWL:APPLICATION-MIXIN, keyword) BOUNDING-BOX ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) BOUNDING-BOX ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) BOUNDING-BOX ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) BOUNDING-BOX ( GDL object message on GWL::LAYOUT-MIXIN, keyword) BOUNDING-BOX ( GDL object message on GWL:NODE-MIXIN, keyword) BOUNDING-BOX ( GDL object message on GWL:WEB-DRAWING, keyword) BOUNDING-SPHERE ( GDL object message on GEOM-BASE::RENDERER-MIXIN, keyword) BOUNDING-SPHERE ( GDL object message on GWL:WEB-DRAWING, keyword) BOX ( Object, geom-base) BREAK-POINTS ( GDL object message on LEADER-LINE, keyword) C-CYLINDER ( Object, geom-base) CAP-THICKNESS ( GDL object message on SPHERICAL-CAP, keyword) CENTER ( GDL object message on ANGULAR-DIMENSION, keyword) CENTER ( GDL object message on ARC, keyword) CENTER ( GDL object message on
```

---

## index.html (chunk 31/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
CAP-THICKNESS ( GDL object message on SPHERICAL-CAP, keyword) CENTER ( GDL object message on ANGULAR-DIMENSION, keyword) CENTER ( GDL object message on ARC, keyword) CENTER ( GDL object message on BASE-COORDINATE-SYSTEM, keyword) CENTER ( GDL object message on BASE-DRAWING, keyword) CENTER ( GDL object message on BASE-OBJECT, keyword) CENTER ( GDL object message on BASE-VIEW, keyword) CENTER ( GDL object message on BEZIER-CURVE, keyword) CENTER ( GDL object message on BOX, keyword) CENTER ( GDL object message on C-CYLINDER, keyword) CENTER ( GDL object message on CENTER-LINE, keyword) CENTER ( GDL object message on CIRCLE, keyword) CENTER ( GDL object message on CONE, keyword) CENTER ( GDL object message on CONSTRAINED-ARC, keyword) CENTER ( GDL object message on CYLINDER, keyword) CENTER ( GDL object message on ELLIPSE, keyword) CENTER ( GDL object message on GENERAL-NOTE, keyword) CENTER ( GDL object message on GLOBAL-FILLETED-POLYGON-PROJECTION, keyword) CENTER ( GDL object message
```

---

## index.html (chunk 32/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
( GDL object message on ELLIPSE, keyword) CENTER ( GDL object message on GENERAL-NOTE, keyword) CENTER ( GDL object message on GLOBAL-FILLETED-POLYGON-PROJECTION, keyword) CENTER ( GDL object message on GLOBAL-FILLETED-POLYLINE, keyword) CENTER ( GDL object message on GEOM-BASE::GLOBAL-FILLETED-POLYLINE-MIXIN, keyword) CENTER ( GDL object message on GLOBAL-POLYGON-PROJECTION, keyword) CENTER ( GDL object message on GLOBAL-POLYLINE, keyword) CENTER ( GDL object message on GEOM-BASE::GLOBAL-POLYLINE-MIXIN, keyword) CENTER ( GDL object message on HORIZONTAL-DIMENSION, keyword) CENTER ( GDL object message on LABEL, keyword) CENTER ( GDL object message on LEADER-LINE, keyword) CENTER ( GDL object message on LINE, keyword) CENTER ( GDL object message on LINEAR-DIMENSION, keyword) CENTER ( GDL object message on PARALLEL-DIMENSION, keyword) CENTER ( GDL object message on PIE-CHART, keyword) CENTER ( GDL object message on POINT, keyword) CENTER ( GDL object message on POINTS-DISPLAY, keyword)
```

---

## index.html (chunk 33/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
ject message on PARALLEL-DIMENSION, keyword) CENTER ( GDL object message on PIE-CHART, keyword) CENTER ( GDL object message on POINT, keyword) CENTER ( GDL object message on POINTS-DISPLAY, keyword) CENTER ( GDL object message on ROUTE-PIPE, keyword) CENTER ( GDL object message on SAMPLE-DRAWING, keyword) CENTER ( GDL object message on SPHERE, keyword) CENTER ( GDL object message on SPHERICAL-CAP, keyword) CENTER ( GDL object message on TEXT-LINE, keyword) CENTER ( GDL object message on TORUS, keyword) CENTER ( GDL object message on TYPESET-BLOCK, keyword) CENTER ( GDL object message on VERTICAL-DIMENSION, keyword) CENTER ( GDL object message on GWL:APPLICATION-MIXIN, keyword) CENTER ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) CENTER ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) CENTER ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) CENTER ( GDL object message on GWL::LAYOUT-MIXIN, keyword) CENTER ( GDL object message on GWL:NODE-MIXIN,
```

---

## index.html (chunk 34/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
-GRAPHICS-SHEET, keyword) CENTER ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) CENTER ( GDL object message on GWL::LAYOUT-MIXIN, keyword) CENTER ( GDL object message on GWL:NODE-MIXIN, keyword) CENTER ( GDL object message on GWL:WEB-DRAWING, keyword) CENTER-LINE ( GDL object message on C-CYLINDER, keyword) CENTER-LINE ( Object, geom-base) CENTER-POINT ( GDL object message on ANGULAR-DIMENSION, keyword) CHARACTER-SIZE ( GDL object message on ANGULAR-DIMENSION, keyword) CHARACTER-SIZE ( GDL object message on GENERAL-NOTE, keyword) CHARACTER-SIZE ( GDL object message on HORIZONTAL-DIMENSION, keyword) CHARACTER-SIZE ( GDL object message on LABEL, keyword) CHARACTER-SIZE ( GDL object message on LINEAR-DIMENSION, keyword) CHARACTER-SIZE ( GDL object message on PARALLEL-DIMENSION, keyword) CHARACTER-SIZE ( GDL object message on VERTICAL-DIMENSION, keyword) check-computed-slots ( Function, gendl) check-documentation ( Function, gendl) check-floating-string ( Function, gendl)
```

---

## index.html (chunk 35/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
) CHARACTER-SIZE ( GDL object message on VERTICAL-DIMENSION, keyword) check-computed-slots ( Function, gendl) check-documentation ( Function, gendl) check-floating-string ( Function, gendl) check-form ( Function, gendl) check-functions ( Function, gendl) check-input-slots ( Function, gendl) check-objects ( Function, gendl) check-query-slots ( Function, gendl) CHECK-SANITY ( GDL object message on GWL:BASE-HTML-SHEET, keyword) CHECK-SANITY? ( GDL object message on GWL:APPLICATION-MIXIN, keyword) CHECK-SANITY? ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) CHECK-SANITY? ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) CHECK-SANITY? ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) CHECK-SANITY? ( GDL object message on GWL:BASE-HTML-SHEET, keyword) CHECK-SANITY? ( GDL object message on GWL::COLOR-MAP, keyword) CHECK-SANITY? ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) CHECK-SANITY? ( GDL object message on GWL::LAYOUT-MIXIN, keyword) CHECK-SANITY?
```

---

## index.html (chunk 36/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
Y? ( GDL object message on GWL::COLOR-MAP, keyword) CHECK-SANITY? ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) CHECK-SANITY? ( GDL object message on GWL::LAYOUT-MIXIN, keyword) CHECK-SANITY? ( GDL object message on GWL:NODE-MIXIN, keyword) CHECK-SANITY? ( GDL object message on YADD:ASSEMBLY, keyword) CHECK-SANITY? ( GDL object message on YADD::BASE-YADD-SHEET, keyword) CHECK-SANITY? ( GDL object message on YADD::MASTER-INDEX, keyword) CHECK-SANITY? ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) CHECK-SANITY? ( GDL object message on YADD::PACKAGE-FORM, keyword) check-trickle-down-slots ( Function, gendl) CHECKBOX-FORM-CONTROL ( Object, gwl) CHILDREN ( GDL object message on VANILLA-MIXIN*, keyword) CHOICE-LIST ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) CHOICE-LIST ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) CHOICE-PLIST ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) CHOICE-PLIST ( GDL object message on GWL:RADIO-FORM-CONTROL,
```

---

## index.html (chunk 37/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
ICE-LIST ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) CHOICE-PLIST ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) CHOICE-PLIST ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) CHOICE-STYLES ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) CHOICE-STYLES ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) CIRCLE ( Object, geom-base) CIRCLE-INTERSECTION-2D ( GDL object message on BEZIER-CURVE, keyword) CIRCLE? ( GDL object message on CENTER-LINE, keyword) CIRCUMFERENCE ( GDL object message on CIRCLE, keyword) cl-lite ( Function, gendl) cl-patch ( Function, cl-lite) CLASS ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) CLASS ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) CLASS ( GDL object message on GWL:GRID-FORM-CONTROL, keyword) CLASS ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) CLASS ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) CLASS ( GDL object message on GWL::SKELETON-FORM-CONTROL, keyword) CLASS (
```

---

## index.html (chunk 38/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
CLASS ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) CLASS ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) CLASS ( GDL object message on GWL::SKELETON-FORM-CONTROL, keyword) CLASS ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) clear-all-instances ( Function, gwl) CLEAR-EXPIRED-SESSION ( GDL object message on GWL:SESSION-CONTROL-MIXIN, keyword) clear-instance ( Function, gwl) CLEAR-NOW? ( GDL object message on GWL:SESSION-CONTROL-MIXIN, keyword) clear-old-timers ( Function, gwl) CLOSED? ( GDL object message on C-CYLINDER, keyword) CLOSED? ( GDL object message on CONE, keyword) CLOSED? ( GDL object message on CYLINDER, keyword) CLOSED? ( GDL object message on GLOBAL-FILLETED-POLYLINE, keyword) CLOSED? ( GDL object message on GEOM-BASE::GLOBAL-FILLETED-POLYLINE-MIXIN, keyword) CLOSED? ( GDL object message on GLOBAL-POLYLINE, keyword) CLOSED? ( GDL object message on GEOM-BASE::GLOBAL-POLYLINE-MIXIN, keyword) CLOSED? ( GDL object message on ROUTE-PIPE, keyword)
```

---

## index.html (chunk 39/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
N, keyword) CLOSED? ( GDL object message on GLOBAL-POLYLINE, keyword) CLOSED? ( GDL object message on GEOM-BASE::GLOBAL-POLYLINE-MIXIN, keyword) CLOSED? ( GDL object message on ROUTE-PIPE, keyword) CLOSEST-VERTEX ( GDL object message on BOX, keyword) CODEBASE-DIRECTORY-NODE ( Object, cl-lite) coincident-point? ( Function, geom-base) COLOR-DECIMAL ( GDL object message on BASE-OBJECT, keyword) COLOR-MAP ( Object, gwl) COLS ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) CONE ( Object, geom-base) CONSTRAINED-ARC ( Object, geom-base) CONSTRAINED-FILLET ( Object, geom-base) CONSTRAINED-LINE ( Object, geom-base) CONTROL-POINTS ( GDL object message on BEZIER-CURVE, keyword) CORNER ( GDL object message on BASE-VIEW, keyword) crawl ( Function, gwl) CREATE-FASL? ( GDL object message on CL-LITE:CODEBASE-DIRECTORY-NODE, keyword) create-obliqueness ( Function, geom-base) cross-vectors ( Function, geom-base) CROSSHAIR-LENGTH ( GDL object message on POINT, keyword) CUSTOM-SNAP-RESTORE! ( GDL
```

---

## index.html (chunk 40/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
TE:CODEBASE-DIRECTORY-NODE, keyword) create-obliqueness ( Function, geom-base) cross-vectors ( Function, geom-base) CROSSHAIR-LENGTH ( GDL object message on POINT, keyword) CUSTOM-SNAP-RESTORE! ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) cyclic-nth ( Function, gendl) CYLINDER ( Object, geom-base) DATA ( GDL object message on PIE-CHART, keyword) DEFAULT ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) DEFAULT ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) DEFAULT ( GDL object message on GWL:GRID-FORM-CONTROL, keyword) DEFAULT ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) DEFAULT ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) DEFAULT ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) DEFAULT-HEADER-CONTENT ( GDL object message on YADD::BASE-YADD-SHEET, keyword) DEFAULT-RADIUS ( GDL object message on GLOBAL-FILLETED-POLYGON-PROJECTION, keyword) DEFAULT-RADIUS ( GDL object message on GLOBAL-FILLETED-POLYLINE, keyword) DEFAULT-RADIUS (
```

---

## index.html (chunk 41/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
ASE-YADD-SHEET, keyword) DEFAULT-RADIUS ( GDL object message on GLOBAL-FILLETED-POLYGON-PROJECTION, keyword) DEFAULT-RADIUS ( GDL object message on GLOBAL-FILLETED-POLYLINE, keyword) DEFAULT-RADIUS ( GDL object message on GEOM-BASE::GLOBAL-FILLETED-POLYLINE-MIXIN, keyword) DEFAULT-RADIUS ( GDL object message on ROUTE-PIPE, keyword) DEFAULT-TREE-DEPTH ( GDL object message on GWL:NODE-MIXIN, keyword) defaulting ( Macro, gendl) define-format ( Macro, gendl) define-lens ( Macro, gendl) define-object ( Macro, gendl) define-object-amendment ( Macro, gendl) degree ( Function, geom-base) degrees-to-radians ( Function, geom-base) DELETE! ( GDL object message on GENDL::VARIABLE-SEQUENCE, keyword) DESCRIPTION-POSITION ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) DESTINATION-DIRECTORY ( GDL object message on GDL-APP, keyword) DEVELOPMENT-LINKS ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) DIGITATION-MODE ( GDL object message on GWL:APPLICATION-MIXIN, keyword) DIGITATION-MODE (
```

---

## index.html (chunk 42/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
L object message on GDL-APP, keyword) DEVELOPMENT-LINKS ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) DIGITATION-MODE ( GDL object message on GWL:APPLICATION-MIXIN, keyword) DIGITATION-MODE ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) DIGITATION-MODE ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) DIGITATION-MODE ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) DIGITATION-MODE ( GDL object message on GWL::LAYOUT-MIXIN, keyword) DIGITATION-MODE ( GDL object message on GWL:NODE-MIXIN, keyword) DIM-TEXT ( GDL object message on ANGULAR-DIMENSION, keyword) DIM-TEXT ( GDL object message on HORIZONTAL-DIMENSION, keyword) DIM-TEXT ( GDL object message on LINEAR-DIMENSION, keyword) DIM-TEXT ( GDL object message on PARALLEL-DIMENSION, keyword) DIM-TEXT ( GDL object message on VERTICAL-DIMENSION, keyword) DIM-TEXT-BIAS ( GDL object message on ANGULAR-DIMENSION, keyword) DIM-TEXT-BIAS ( GDL object message on HORIZONTAL-DIMENSION, keyword)
```

---

## index.html (chunk 43/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
GDL object message on VERTICAL-DIMENSION, keyword) DIM-TEXT-BIAS ( GDL object message on ANGULAR-DIMENSION, keyword) DIM-TEXT-BIAS ( GDL object message on HORIZONTAL-DIMENSION, keyword) DIM-TEXT-BIAS ( GDL object message on LINEAR-DIMENSION, keyword) DIM-TEXT-BIAS ( GDL object message on PARALLEL-DIMENSION, keyword) DIM-TEXT-BIAS ( GDL object message on VERTICAL-DIMENSION, keyword) DIM-TEXT-START ( GDL object message on ANGULAR-DIMENSION, keyword) DIM-TEXT-START ( GDL object message on HORIZONTAL-DIMENSION, keyword) DIM-TEXT-START ( GDL object message on LINEAR-DIMENSION, keyword) DIM-TEXT-START ( GDL object message on PARALLEL-DIMENSION, keyword) DIM-TEXT-START ( GDL object message on VERTICAL-DIMENSION, keyword) DIM-TEXT-START-OFFSET ( GDL object message on ANGULAR-DIMENSION, keyword) DIM-TEXT-START-OFFSET ( GDL object message on HORIZONTAL-DIMENSION, keyword) DIM-TEXT-START-OFFSET ( GDL object message on LINEAR-DIMENSION, keyword) DIM-TEXT-START-OFFSET ( GDL object message on
```

---

## index.html (chunk 44/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
IM-TEXT-START-OFFSET ( GDL object message on HORIZONTAL-DIMENSION, keyword) DIM-TEXT-START-OFFSET ( GDL object message on LINEAR-DIMENSION, keyword) DIM-TEXT-START-OFFSET ( GDL object message on PARALLEL-DIMENSION, keyword) DIM-TEXT-START-OFFSET ( GDL object message on VERTICAL-DIMENSION, keyword) DIM-VALUE ( GDL object message on ANGULAR-DIMENSION, keyword) DIM-VALUE ( GDL object message on HORIZONTAL-DIMENSION, keyword) DIM-VALUE ( GDL object message on LINEAR-DIMENSION, keyword) DIM-VALUE ( GDL object message on PARALLEL-DIMENSION, keyword) DIM-VALUE ( GDL object message on VERTICAL-DIMENSION, keyword) DIRECT-MIXINS ( GDL object message on VANILLA-MIXIN*, keyword) DIRECTION-VECTOR ( GDL object message on CYLINDER, keyword) DIRECTION-VECTOR ( GDL object message on LINE, keyword) DISABLED-KEYS ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) DISABLED-KEYS ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) DISABLED? ( GDL object message on GWL:BASE-FORM-CONTROL, keyword)
```

---

## index.html (chunk 45/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
-KEYS ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) DISABLED-KEYS ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) DISABLED? ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) DISABLED? ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) DISABLED? ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) DISABLED? ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) DISABLED? ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) DISPLAY-CONTROLS ( GDL object message on ANGULAR-DIMENSION, keyword) DISPLAY-CONTROLS ( GDL object message on ARC, keyword) DISPLAY-CONTROLS ( GDL object message on BASE-COORDINATE-SYSTEM, keyword) DISPLAY-CONTROLS ( GDL object message on BASE-DRAWING, keyword) DISPLAY-CONTROLS ( GDL object message on BASE-OBJECT, keyword) DISPLAY-CONTROLS ( GDL object message on BASE-VIEW, keyword) DISPLAY-CONTROLS ( GDL object message on BEZIER-CURVE, keyword) DISPLAY-CONTROLS ( GDL object message on BOX, keyword) DISPLAY-CONTROLS ( GDL
```

---

## index.html (chunk 46/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
-CONTROLS ( GDL object message on BASE-VIEW, keyword) DISPLAY-CONTROLS ( GDL object message on BEZIER-CURVE, keyword) DISPLAY-CONTROLS ( GDL object message on BOX, keyword) DISPLAY-CONTROLS ( GDL object message on C-CYLINDER, keyword) DISPLAY-CONTROLS ( GDL object message on CENTER-LINE, keyword) DISPLAY-CONTROLS ( GDL object message on CIRCLE, keyword) DISPLAY-CONTROLS ( GDL object message on CONE, keyword) DISPLAY-CONTROLS ( GDL object message on CONSTRAINED-ARC, keyword) DISPLAY-CONTROLS ( GDL object message on CONSTRAINED-FILLET, keyword) DISPLAY-CONTROLS ( GDL object message on CONSTRAINED-LINE, keyword) DISPLAY-CONTROLS ( GDL object message on CYLINDER, keyword) DISPLAY-CONTROLS ( GDL object message on ELLIPSE, keyword) DISPLAY-CONTROLS ( GDL object message on GENERAL-NOTE, keyword) DISPLAY-CONTROLS ( GDL object message on GLOBAL-FILLETED-POLYGON-PROJECTION, keyword) DISPLAY-CONTROLS ( GDL object message on GLOBAL-FILLETED-POLYLINE, keyword) DISPLAY-CONTROLS ( GDL object message
```

---

## index.html (chunk 47/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
DISPLAY-CONTROLS ( GDL object message on GLOBAL-FILLETED-POLYGON-PROJECTION, keyword) DISPLAY-CONTROLS ( GDL object message on GLOBAL-FILLETED-POLYLINE, keyword) DISPLAY-CONTROLS ( GDL object message on GEOM-BASE::GLOBAL-FILLETED-POLYLINE-MIXIN, keyword) DISPLAY-CONTROLS ( GDL object message on GLOBAL-POLYGON-PROJECTION, keyword) DISPLAY-CONTROLS ( GDL object message on GLOBAL-POLYLINE, keyword) DISPLAY-CONTROLS ( GDL object message on GEOM-BASE::GLOBAL-POLYLINE-MIXIN, keyword) DISPLAY-CONTROLS ( GDL object message on HORIZONTAL-DIMENSION, keyword) DISPLAY-CONTROLS ( GDL object message on LABEL, keyword) DISPLAY-CONTROLS ( GDL object message on LEADER-LINE, keyword) DISPLAY-CONTROLS ( GDL object message on LINE, keyword) DISPLAY-CONTROLS ( GDL object message on LINEAR-DIMENSION, keyword) DISPLAY-CONTROLS ( GDL object message on PARALLEL-DIMENSION, keyword) DISPLAY-CONTROLS ( GDL object message on PIE-CHART, keyword) DISPLAY-CONTROLS ( GDL object message on POINT, keyword)
```

---

## index.html (chunk 48/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
ISPLAY-CONTROLS ( GDL object message on PARALLEL-DIMENSION, keyword) DISPLAY-CONTROLS ( GDL object message on PIE-CHART, keyword) DISPLAY-CONTROLS ( GDL object message on POINT, keyword) DISPLAY-CONTROLS ( GDL object message on POINTS-DISPLAY, keyword) DISPLAY-CONTROLS ( GDL object message on ROUTE-PIPE, keyword) DISPLAY-CONTROLS ( GDL object message on SAMPLE-DRAWING, keyword) DISPLAY-CONTROLS ( GDL object message on SPHERE, keyword) DISPLAY-CONTROLS ( GDL object message on SPHERICAL-CAP, keyword) DISPLAY-CONTROLS ( GDL object message on TEXT-LINE, keyword) DISPLAY-CONTROLS ( GDL object message on TORUS, keyword) DISPLAY-CONTROLS ( GDL object message on TYPESET-BLOCK, keyword) DISPLAY-CONTROLS ( GDL object message on VERTICAL-DIMENSION, keyword) DISPLAY-CONTROLS ( GDL object message on GWL:APPLICATION-MIXIN, keyword) DISPLAY-CONTROLS ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) DISPLAY-CONTROLS ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword)
```

---

## index.html (chunk 49/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
L:APPLICATION-MIXIN, keyword) DISPLAY-CONTROLS ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) DISPLAY-CONTROLS ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) DISPLAY-CONTROLS ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) DISPLAY-CONTROLS ( GDL object message on GWL::LAYOUT-MIXIN, keyword) DISPLAY-CONTROLS ( GDL object message on GWL:NODE-MIXIN, keyword) DISPLAY-CONTROLS ( GDL object message on GWL:WEB-DRAWING, keyword) DISPLAY-LIST-OBJECT-ROOTS ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) DISPLAY-LIST-OBJECTS ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) DISPLAY-RULES? ( GDL object message on GWL:APPLICATION-MIXIN, keyword) DISPLAY-RULES? ( GDL object message on GWL::LAYOUT-MIXIN, keyword) DISPLAY-RULES? ( GDL object message on GWL:NODE-MIXIN, keyword) DISPLAY-TREE? ( GDL object message on GWL:APPLICATION-MIXIN, keyword) DISPLAY-TREE? ( GDL object message on GWL::LAYOUT-MIXIN, keyword) DISPLAY-TREE? ( GDL
```

---

## index.html (chunk 50/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
t message on GWL:NODE-MIXIN, keyword) DISPLAY-TREE? ( GDL object message on GWL:APPLICATION-MIXIN, keyword) DISPLAY-TREE? ( GDL object message on GWL::LAYOUT-MIXIN, keyword) DISPLAY-TREE? ( GDL object message on GWL:NODE-MIXIN, keyword) distance-to-line ( Function, geom-base) div ( Function, gendl) DOCTYPE-STRING ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) DOCTYPE-STRING ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) DOCTYPE-STRING ( GDL object message on YADD:ASSEMBLY, keyword) DOCTYPE-STRING ( GDL object message on YADD::BASE-YADD-SHEET, keyword) DOCTYPE-STRING ( GDL object message on YADD::MASTER-INDEX, keyword) DOCTYPE-STRING ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) DOCTYPE-STRING ( GDL object message on YADD::PACKAGE-FORM, keyword) DOCUMENTATION ( GDL object message on VANILLA-MIXIN*, keyword) DOM-ID ( GDL object message on GEYSR::MENU, keyword) DOM-ID ( GDL object message on GEYSR:TREE, keyword) DOM-ID ( GDL object message on
```

---

## index.html (chunk 51/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
TATION ( GDL object message on VANILLA-MIXIN*, keyword) DOM-ID ( GDL object message on GEYSR::MENU, keyword) DOM-ID ( GDL object message on GEYSR:TREE, keyword) DOM-ID ( GDL object message on GWL:APPLICATION-MIXIN, keyword) DOM-ID ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) DOM-ID ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) DOM-ID ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) DOM-ID ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) DOM-ID ( GDL object message on GWL:BASE-HTML-SHEET, keyword) DOM-ID ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) DOM-ID ( GDL object message on GWL::COLOR-MAP, keyword) DOM-ID ( GDL object message on GWL:GRID-FORM-CONTROL, keyword) DOM-ID ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) DOM-ID ( GDL object message on GWL::LAYOUT-MIXIN, keyword) DOM-ID ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) DOM-ID ( GDL object message on GWL:NODE-MIXIN, keyword) DOM-ID ( GDL object
```

---

## index.html (chunk 52/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
ID ( GDL object message on GWL::LAYOUT-MIXIN, keyword) DOM-ID ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) DOM-ID ( GDL object message on GWL:NODE-MIXIN, keyword) DOM-ID ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) DOM-ID ( GDL object message on GWL:SHEET-SECTION, keyword) DOM-ID ( GDL object message on GWL::SKELETON-FORM-CONTROL, keyword) DOM-ID ( GDL object message on GWL:SKELETON-UI-ELEMENT, keyword) DOM-ID ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) DOM-ID ( GDL object message on YADD:ASSEMBLY, keyword) DOM-ID ( GDL object message on YADD::BASE-YADD-SHEET, keyword) DOM-ID ( GDL object message on YADD::MASTER-INDEX, keyword) DOM-ID ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) DOM-ID ( GDL object message on YADD::PACKAGE-FORM, keyword) DOM-SECTION ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) DOMAIN ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) DOMAIN ( GDL object message on GWL:CHECKBOX-FORM-CONTROL,
```

---

## index.html (chunk 53/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
OM-SECTION ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) DOMAIN ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) DOMAIN ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) DOMAIN ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) DOMAIN ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) DOMAIN ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) dot-vectors ( Function, geom-base) DRAW-CENTERLINE-ARC? ( GDL object message on TORUS, keyword) DROPPED-HEIGHT-WIDTH ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) DROPPED-OBJECT ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) DROPPED-X-Y ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) DXF-FONT ( GDL object message on ANGULAR-DIMENSION, keyword) DXF-FONT ( GDL object message on GENERAL-NOTE, keyword) DXF-FONT ( GDL object message on HORIZONTAL-DIMENSION, keyword) DXF-FONT ( GDL object message on LABEL, keyword) DXF-FONT ( GDL object message on
```

---

## index.html (chunk 54/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
object message on GENERAL-NOTE, keyword) DXF-FONT ( GDL object message on HORIZONTAL-DIMENSION, keyword) DXF-FONT ( GDL object message on LABEL, keyword) DXF-FONT ( GDL object message on LINEAR-DIMENSION, keyword) DXF-FONT ( GDL object message on PARALLEL-DIMENSION, keyword) DXF-FONT ( GDL object message on VERTICAL-DIMENSION, keyword) DXF-OFFSET ( GDL object message on ANGULAR-DIMENSION, keyword) DXF-OFFSET ( GDL object message on GENERAL-NOTE, keyword) DXF-OFFSET ( GDL object message on HORIZONTAL-DIMENSION, keyword) DXF-OFFSET ( GDL object message on LABEL, keyword) DXF-OFFSET ( GDL object message on LINEAR-DIMENSION, keyword) DXF-OFFSET ( GDL object message on PARALLEL-DIMENSION, keyword) DXF-OFFSET ( GDL object message on VERTICAL-DIMENSION, keyword) DXF-SIZE-RATIO ( GDL object message on ANGULAR-DIMENSION, keyword) DXF-SIZE-RATIO ( GDL object message on GENERAL-NOTE, keyword) DXF-SIZE-RATIO ( GDL object message on HORIZONTAL-DIMENSION, keyword) DXF-SIZE-RATIO ( GDL object
```

---

## index.html (chunk 55/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
ge on ANGULAR-DIMENSION, keyword) DXF-SIZE-RATIO ( GDL object message on GENERAL-NOTE, keyword) DXF-SIZE-RATIO ( GDL object message on HORIZONTAL-DIMENSION, keyword) DXF-SIZE-RATIO ( GDL object message on LABEL, keyword) DXF-SIZE-RATIO ( GDL object message on LINEAR-DIMENSION, keyword) DXF-SIZE-RATIO ( GDL object message on PARALLEL-DIMENSION, keyword) DXF-SIZE-RATIO ( GDL object message on VERTICAL-DIMENSION, keyword) DXF-TEXT-X-SCALE ( GDL object message on ANGULAR-DIMENSION, keyword) DXF-TEXT-X-SCALE ( GDL object message on GENERAL-NOTE, keyword) DXF-TEXT-X-SCALE ( GDL object message on HORIZONTAL-DIMENSION, keyword) DXF-TEXT-X-SCALE ( GDL object message on LABEL, keyword) DXF-TEXT-X-SCALE ( GDL object message on LINEAR-DIMENSION, keyword) DXF-TEXT-X-SCALE ( GDL object message on PARALLEL-DIMENSION, keyword) DXF-TEXT-X-SCALE ( GDL object message on VERTICAL-DIMENSION, keyword) EDGE-CENTER ( GDL object message on BASE-OBJECT, keyword) ELLIPSE ( Object, geom-base) END ( GDL object
```

---

## index.html (chunk 56/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
EL-DIMENSION, keyword) DXF-TEXT-X-SCALE ( GDL object message on VERTICAL-DIMENSION, keyword) EDGE-CENTER ( GDL object message on BASE-OBJECT, keyword) ELLIPSE ( Object, geom-base) END ( GDL object message on ARC, keyword) END ( GDL object message on C-CYLINDER, keyword) END ( GDL object message on CONSTRAINED-LINE, keyword) END ( GDL object message on CYLINDER, keyword) END ( GDL object message on LINE, keyword) END-ANGLE ( GDL object message on ARC, keyword) END-ANGLE ( GDL object message on GEOM-BASE::ARCOID-MIXIN, keyword) END-ANGLE ( GDL object message on C-CYLINDER, keyword) END-ANGLE ( GDL object message on CIRCLE, keyword) END-ANGLE ( GDL object message on CONE, keyword) END-ANGLE ( GDL object message on CONSTRAINED-ARC, keyword) END-ANGLE ( GDL object message on CONSTRAINED-FILLET, keyword) END-ANGLE ( GDL object message on CYLINDER, keyword) END-ANGLE ( GDL object message on ELLIPSE, keyword) END-ANGLE ( GDL object message on POINT, keyword) END-ANGLE ( GDL object message on
```

---

## index.html (chunk 57/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
keyword) END-ANGLE ( GDL object message on CYLINDER, keyword) END-ANGLE ( GDL object message on ELLIPSE, keyword) END-ANGLE ( GDL object message on POINT, keyword) END-ANGLE ( GDL object message on SPHERE, keyword) END-ANGLE ( GDL object message on SPHERICAL-CAP, keyword) END-ANGLE ( GDL object message on TORUS, keyword) END-CAPS? ( GDL object message on TORUS, keyword) END-HORIZONTAL-ARC ( GDL object message on POINT, keyword) END-HORIZONTAL-ARC ( GDL object message on SPHERE, keyword) END-POINT ( GDL object message on ANGULAR-DIMENSION, keyword) END-POINT ( GDL object message on HORIZONTAL-DIMENSION, keyword) END-POINT ( GDL object message on LINEAR-DIMENSION, keyword) END-POINT ( GDL object message on PARALLEL-DIMENSION, keyword) END-POINT ( GDL object message on VERTICAL-DIMENSION, keyword) END-VERTICAL-ARC ( GDL object message on POINT, keyword) END-VERTICAL-ARC ( GDL object message on SPHERE, keyword) ensure-list ( Function, gendl) equi-space-points ( Function, geom-base)
```

---

## index.html (chunk 58/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
yword) END-VERTICAL-ARC ( GDL object message on POINT, keyword) END-VERTICAL-ARC ( GDL object message on SPHERE, keyword) ensure-list ( Function, gendl) equi-space-points ( Function, geom-base) EQUI-SPACED-POINTS ( GDL object message on ARC, keyword) ERROR ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) EXPIRES-AT ( GDL object message on GWL:SESSION-CONTROL-MIXIN, keyword) EXTERNAL-ONLY? ( GDL object message on YADD:ASSEMBLY, keyword) EXTERNAL-ONLY? ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) FACE-CENTER ( GDL object message on BASE-OBJECT, keyword) FACE-NORMAL-VECTOR ( GDL object message on BASE-OBJECT, keyword) FACE-VERTICES ( GDL object message on BASE-OBJECT, keyword) FAILED-FORM-CONTROLS ( GDL object message on GWL:SKELETON-UI-ELEMENT, keyword) FAILED-VALUE ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) FASL-OUTPUT-NAME ( GDL object message on CL-LITE:CODEBASE-DIRECTORY-NODE, keyword) FASL-OUTPUT-PATH ( GDL object message on
```

---

## index.html (chunk 59/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
GDL object message on GWL:BASE-FORM-CONTROL, keyword) FASL-OUTPUT-NAME ( GDL object message on CL-LITE:CODEBASE-DIRECTORY-NODE, keyword) FASL-OUTPUT-PATH ( GDL object message on CL-LITE:CODEBASE-DIRECTORY-NODE, keyword) FASL-OUTPUT-TYPE ( GDL object message on CL-LITE:CODEBASE-DIRECTORY-NODE, keyword) FIELD-NAME ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) FIELD-NAME ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) FIELD-NAME ( GDL object message on GWL:GRID-FORM-CONTROL, keyword) FIELD-NAME ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) FIELD-NAME ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) FIELD-NAME ( GDL object message on GWL::SKELETON-FORM-CONTROL, keyword) FIELD-NAME ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) FIELD-OF-VIEW-DEFAULT ( GDL object message on GEOM-BASE::RENDERER-MIXIN, keyword) FIELD-OF-VIEW-DEFAULT ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) FIELD-OF-VIEW-DEFAULT ( GDL object message on
```

---

## index.html (chunk 60/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
ULT ( GDL object message on GEOM-BASE::RENDERER-MIXIN, keyword) FIELD-OF-VIEW-DEFAULT ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) FIELD-OF-VIEW-DEFAULT ( GDL object message on GWL:WEB-DRAWING, keyword) FILLETS ( GDL object message on GEOM-BASE::GLOBAL-FILLETED-POLYLINE-MIXIN, keyword) find-dependants ( Function, gendl) find-dependencies ( Function, gendl) find-messages-used-by ( Function, gendl) find-messages-which-use ( Function, gendl) FIRST ( GDL object message on GENDL::MATRIX-SEQUENCE, keyword) FIRST ( GDL object message on QUANTIFICATION, keyword) FIRST ( GDL object message on GENDL::STANDARD-SEQUENCE, keyword) FIRST ( GDL object message on GENDL::VARIABLE-SEQUENCE, keyword) FIRST? ( GDL object message on VANILLA-MIXIN*, keyword) flatten ( Function, gendl) FLIP-LEADERS? ( GDL object message on ANGULAR-DIMENSION, keyword) FLIP-LEADERS? ( GDL object message on HORIZONTAL-DIMENSION, keyword) FLIP-LEADERS? ( GDL object message on LINEAR-DIMENSION, keyword)
```

---

## index.html (chunk 61/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
DERS? ( GDL object message on ANGULAR-DIMENSION, keyword) FLIP-LEADERS? ( GDL object message on HORIZONTAL-DIMENSION, keyword) FLIP-LEADERS? ( GDL object message on LINEAR-DIMENSION, keyword) FLIP-LEADERS? ( GDL object message on PARALLEL-DIMENSION, keyword) FLIP-LEADERS? ( GDL object message on VERTICAL-DIMENSION, keyword) FOLLOW-ROOT-PATH ( GDL object message on VANILLA-MIXIN*, keyword) FONT ( GDL object message on ANGULAR-DIMENSION, keyword) FONT ( GDL object message on GENERAL-NOTE, keyword) FONT ( GDL object message on HORIZONTAL-DIMENSION, keyword) FONT ( GDL object message on LABEL, keyword) FONT ( GDL object message on LINEAR-DIMENSION, keyword) FONT ( GDL object message on PARALLEL-DIMENSION, keyword) FONT ( GDL object message on VERTICAL-DIMENSION, keyword) FORCE-VALIDATION-FOR ( GDL object message on GEYSR::MENU, keyword) FORCE-VALIDATION-FOR ( GDL object message on GEYSR:TREE, keyword) FORCE-VALIDATION-FOR ( GDL object message on GWL:APPLICATION-MIXIN, keyword)
```

---

## index.html (chunk 62/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
GDL object message on GEYSR::MENU, keyword) FORCE-VALIDATION-FOR ( GDL object message on GEYSR:TREE, keyword) FORCE-VALIDATION-FOR ( GDL object message on GWL:APPLICATION-MIXIN, keyword) FORCE-VALIDATION-FOR ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) FORCE-VALIDATION-FOR ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) FORCE-VALIDATION-FOR ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) FORCE-VALIDATION-FOR ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) FORCE-VALIDATION-FOR ( GDL object message on GWL:BASE-HTML-SHEET, keyword) FORCE-VALIDATION-FOR ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) FORCE-VALIDATION-FOR ( GDL object message on GWL::COLOR-MAP, keyword) FORCE-VALIDATION-FOR ( GDL object message on GWL:GRID-FORM-CONTROL, keyword) FORCE-VALIDATION-FOR ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) FORCE-VALIDATION-FOR ( GDL object message on GWL::LAYOUT-MIXIN, keyword) FORCE-VALIDATION-FOR ( GDL object
```

---

## index.html (chunk 63/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
, keyword) FORCE-VALIDATION-FOR ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) FORCE-VALIDATION-FOR ( GDL object message on GWL::LAYOUT-MIXIN, keyword) FORCE-VALIDATION-FOR ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) FORCE-VALIDATION-FOR ( GDL object message on GWL:NODE-MIXIN, keyword) FORCE-VALIDATION-FOR ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) FORCE-VALIDATION-FOR ( GDL object message on GWL:SHEET-SECTION, keyword) FORCE-VALIDATION-FOR ( GDL object message on GWL::SKELETON-FORM-CONTROL, keyword) FORCE-VALIDATION-FOR ( GDL object message on GWL:SKELETON-UI-ELEMENT, keyword) FORCE-VALIDATION-FOR ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) FORCE-VALIDATION-FOR ( GDL object message on YADD:ASSEMBLY, keyword) FORCE-VALIDATION-FOR ( GDL object message on YADD::BASE-YADD-SHEET, keyword) FORCE-VALIDATION-FOR ( GDL object message on YADD::MASTER-INDEX, keyword) FORCE-VALIDATION-FOR ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword)
```

---

## index.html (chunk 64/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
ssage on YADD::BASE-YADD-SHEET, keyword) FORCE-VALIDATION-FOR ( GDL object message on YADD::MASTER-INDEX, keyword) FORCE-VALIDATION-FOR ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) FORCE-VALIDATION-FOR ( GDL object message on YADD::PACKAGE-FORM, keyword) FOREGROUND-COLOR ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) FORM-CONTROL ( GDL object message on GWL::SKELETON-FORM-CONTROL, keyword) FORM-CONTROL-ATTRIBUTES ( GDL object message on GWL:GRID-FORM-CONTROL, keyword) FORM-CONTROL-INPUTS ( GDL object message on GWL:GRID-FORM-CONTROL, keyword) FORM-CONTROL-STRING ( GDL object message on GWL::SKELETON-FORM-CONTROL, keyword) FORM-CONTROL-TYPES ( GDL object message on GWL:GRID-FORM-CONTROL, keyword) FORM-CONTROLS ( GDL object message on GWL:GRID-FORM-CONTROL, keyword) FORM-CONTROLS ( GDL object message on GWL::SKELETON-FORM-CONTROL, keyword) FORM-CONTROLS ( GDL object message on GWL:SKELETON-UI-ELEMENT, keyword) format-slot ( Macro, gendl) FRONT-MARGIN (
```

---

## index.html (chunk 65/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
yword) FORM-CONTROLS ( GDL object message on GWL::SKELETON-FORM-CONTROL, keyword) FORM-CONTROLS ( GDL object message on GWL:SKELETON-UI-ELEMENT, keyword) format-slot ( Macro, gendl) FRONT-MARGIN ( GDL object message on BASE-VIEW, keyword) fround-to-nearest ( Function, gendl) FULL-LEADER-LINE-LENGTH ( GDL object message on ANGULAR-DIMENSION, keyword) FULL-LEADER-LINE-LENGTH ( GDL object message on HORIZONTAL-DIMENSION, keyword) FULL-LEADER-LINE-LENGTH ( GDL object message on LINEAR-DIMENSION, keyword) FULL-LEADER-LINE-LENGTH ( GDL object message on PARALLEL-DIMENSION, keyword) FULL-LEADER-LINE-LENGTH ( GDL object message on VERTICAL-DIMENSION, keyword) FUNCTION-DOCS ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) GAP-LENGTH ( GDL object message on CENTER-LINE, keyword) GDL-AJAX-CALL ( GDL object message on GWL:SKELETON-UI-ELEMENT, keyword) GDL-APP ( Object, gendl) GDLINIT-CONTENT ( GDL object message on GDL-APP, keyword) GENERAL-NOTE ( Object, geom-base)
```

---

## index.html (chunk 66/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
-CALL ( GDL object message on GWL:SKELETON-UI-ELEMENT, keyword) GDL-APP ( Object, gendl) GDLINIT-CONTENT ( GDL object message on GDL-APP, keyword) GENERAL-NOTE ( Object, geom-base) generate-mcp-openapi-spec ( Function, gendl-mcp) generate-mcp-tool-description ( Function, gendl-mcp) GEOMETRY-VIEW-MIXIN ( Object, gwl) get-u ( Function, geom-base) get-v ( Function, geom-base) get-w ( Function, geom-base) get-x ( Function, geom-base) get-y ( Function, geom-base) get-z ( Function, geom-base) GLOBAL-FILLETED-POLYGON-PROJECTION ( Object, geom-base) GLOBAL-FILLETED-POLYLINE ( Object, geom-base) GLOBAL-FILLETED-POLYLINE-MIXIN ( Object, geom-base) GLOBAL-POLYGON-PROJECTION ( Object, geom-base) GLOBAL-POLYLINE ( Object, geom-base) GLOBAL-POLYLINE-MIXIN ( Object, geom-base) GLOBAL-TO-LOCAL ( GDL object message on BASE-OBJECT, keyword) GRAPHICS ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) GRAPHICS-HEIGHT ( GDL object message on GWL:APPLICATION-MIXIN, keyword) GRAPHICS-HEIGHT (
```

---

## index.html (chunk 67/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
t message on BASE-OBJECT, keyword) GRAPHICS ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) GRAPHICS-HEIGHT ( GDL object message on GWL:APPLICATION-MIXIN, keyword) GRAPHICS-HEIGHT ( GDL object message on GWL::LAYOUT-MIXIN, keyword) GRAPHICS-HEIGHT ( GDL object message on GWL:NODE-MIXIN, keyword) GRAPHICS-WIDTH ( GDL object message on GWL:APPLICATION-MIXIN, keyword) GRAPHICS-WIDTH ( GDL object message on GWL::LAYOUT-MIXIN, keyword) GRAPHICS-WIDTH ( GDL object message on GWL:NODE-MIXIN, keyword) GRID-FORM-CONTROL ( Object, gwl) gwl-make-object ( Function, gwl) GWL-RULE-OBJECT ( Object, gwl) half ( Function, gendl) HEAD-CLASS ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) HEAD-CLASS ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) HEAD-CLASS ( GDL object message on YADD:ASSEMBLY, keyword) HEAD-CLASS ( GDL object message on YADD::BASE-YADD-SHEET, keyword) HEAD-CLASS ( GDL object message on YADD::MASTER-INDEX, keyword) HEAD-CLASS ( GDL object message on
```

---

## index.html (chunk 68/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
ssage on YADD:ASSEMBLY, keyword) HEAD-CLASS ( GDL object message on YADD::BASE-YADD-SHEET, keyword) HEAD-CLASS ( GDL object message on YADD::MASTER-INDEX, keyword) HEAD-CLASS ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) HEAD-CLASS ( GDL object message on YADD::PACKAGE-FORM, keyword) HEADER-PLIST ( GDL object message on GWL:BASE-HTML-SHEET, keyword) HEIGHT ( GDL object message on ANGULAR-DIMENSION, keyword) HEIGHT ( GDL object message on ARC, keyword) HEIGHT ( GDL object message on BASE-COORDINATE-SYSTEM, keyword) HEIGHT ( GDL object message on BASE-DRAWING, keyword) HEIGHT ( GDL object message on BASE-OBJECT, keyword) HEIGHT ( GDL object message on BASE-VIEW, keyword) HEIGHT ( GDL object message on BEZIER-CURVE, keyword) HEIGHT ( GDL object message on BOX, keyword) HEIGHT ( GDL object message on C-CYLINDER, keyword) HEIGHT ( GDL object message on CENTER-LINE, keyword) HEIGHT ( GDL object message on CONE, keyword) HEIGHT ( GDL object message on CONSTRAINED-LINE,
```

---

## index.html (chunk 69/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
GDL object message on C-CYLINDER, keyword) HEIGHT ( GDL object message on CENTER-LINE, keyword) HEIGHT ( GDL object message on CONE, keyword) HEIGHT ( GDL object message on CONSTRAINED-LINE, keyword) HEIGHT ( GDL object message on CYLINDER, keyword) HEIGHT ( GDL object message on ELLIPSE, keyword) HEIGHT ( GDL object message on GENERAL-NOTE, keyword) HEIGHT ( GDL object message on GLOBAL-FILLETED-POLYGON-PROJECTION, keyword) HEIGHT ( GDL object message on GLOBAL-FILLETED-POLYLINE, keyword) HEIGHT ( GDL object message on GEOM-BASE::GLOBAL-FILLETED-POLYLINE-MIXIN, keyword) HEIGHT ( GDL object message on GLOBAL-POLYGON-PROJECTION, keyword) HEIGHT ( GDL object message on GLOBAL-POLYLINE, keyword) HEIGHT ( GDL object message on GEOM-BASE::GLOBAL-POLYLINE-MIXIN, keyword) HEIGHT ( GDL object message on HORIZONTAL-DIMENSION, keyword) HEIGHT ( GDL object message on LABEL, keyword) HEIGHT ( GDL object message on LEADER-LINE, keyword) HEIGHT ( GDL object message on LINE, keyword) HEIGHT ( GDL
```

---

## index.html (chunk 70/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
sage on HORIZONTAL-DIMENSION, keyword) HEIGHT ( GDL object message on LABEL, keyword) HEIGHT ( GDL object message on LEADER-LINE, keyword) HEIGHT ( GDL object message on LINE, keyword) HEIGHT ( GDL object message on LINEAR-DIMENSION, keyword) HEIGHT ( GDL object message on PARALLEL-DIMENSION, keyword) HEIGHT ( GDL object message on PIE-CHART, keyword) HEIGHT ( GDL object message on POINTS-DISPLAY, keyword) HEIGHT ( GDL object message on ROUTE-PIPE, keyword) HEIGHT ( GDL object message on SAMPLE-DRAWING, keyword) HEIGHT ( GDL object message on SPHERE, keyword) HEIGHT ( GDL object message on SPHERICAL-CAP, keyword) HEIGHT ( GDL object message on TEXT-LINE, keyword) HEIGHT ( GDL object message on TORUS, keyword) HEIGHT ( GDL object message on TYPESET-BLOCK, keyword) HEIGHT ( GDL object message on VERTICAL-DIMENSION, keyword) HEIGHT ( GDL object message on GWL:APPLICATION-MIXIN, keyword) HEIGHT ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) HEIGHT ( GDL object message on
```

---

## index.html (chunk 71/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
e on VERTICAL-DIMENSION, keyword) HEIGHT ( GDL object message on GWL:APPLICATION-MIXIN, keyword) HEIGHT ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) HEIGHT ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) HEIGHT ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) HEIGHT ( GDL object message on GWL::LAYOUT-MIXIN, keyword) HEIGHT ( GDL object message on GWL:NODE-MIXIN, keyword) HEIGHT ( GDL object message on GWL:WEB-DRAWING, keyword) HIDDEN-CHILDREN ( GDL object message on VANILLA-MIXIN*, keyword) HIDDEN? ( GDL object message on CL-LITE:CODEBASE-DIRECTORY-NODE, keyword) HIDDEN? ( GDL object message on BASE-RULE-OBJECT, keyword) HIDDEN? ( GDL object message on GDL-APP, keyword) HIDDEN? ( GDL object message on GENDL::MATRIX-SEQUENCE, keyword) HIDDEN? ( GDL object message on NULL-OBJECT, keyword) HIDDEN? ( GDL object message on QUANTIFICATION, keyword) HIDDEN? ( GDL object message on GENDL::RADIAL-SEQUENCE, keyword) HIDDEN? ( GDL object message on
```

---

## index.html (chunk 72/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
ect message on NULL-OBJECT, keyword) HIDDEN? ( GDL object message on QUANTIFICATION, keyword) HIDDEN? ( GDL object message on GENDL::RADIAL-SEQUENCE, keyword) HIDDEN? ( GDL object message on GENDL::STANDARD-SEQUENCE, keyword) HIDDEN? ( GDL object message on VANILLA-MIXIN*, keyword) HIDDEN? ( GDL object message on GENDL::VARIABLE-SEQUENCE, keyword) HIDDEN? ( GDL object message on ANGULAR-DIMENSION, keyword) HIDDEN? ( GDL object message on ARC, keyword) HIDDEN? ( GDL object message on GEOM-BASE::ARCOID-MIXIN, keyword) HIDDEN? ( GDL object message on BASE-COORDINATE-SYSTEM, keyword) HIDDEN? ( GDL object message on BASE-DRAWING, keyword) HIDDEN? ( GDL object message on BASE-OBJECT, keyword) HIDDEN? ( GDL object message on BASE-VIEW, keyword) HIDDEN? ( GDL object message on BEZIER-CURVE, keyword) HIDDEN? ( GDL object message on BOX, keyword) HIDDEN? ( GDL object message on C-CYLINDER, keyword) HIDDEN? ( GDL object message on CENTER-LINE, keyword) HIDDEN? ( GDL object message on CIRCLE,
```

---

## index.html (chunk 73/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
d) HIDDEN? ( GDL object message on BOX, keyword) HIDDEN? ( GDL object message on C-CYLINDER, keyword) HIDDEN? ( GDL object message on CENTER-LINE, keyword) HIDDEN? ( GDL object message on CIRCLE, keyword) HIDDEN? ( GDL object message on CONE, keyword) HIDDEN? ( GDL object message on CONSTRAINED-ARC, keyword) HIDDEN? ( GDL object message on CONSTRAINED-FILLET, keyword) HIDDEN? ( GDL object message on CONSTRAINED-LINE, keyword) HIDDEN? ( GDL object message on CYLINDER, keyword) HIDDEN? ( GDL object message on ELLIPSE, keyword) HIDDEN? ( GDL object message on GENERAL-NOTE, keyword) HIDDEN? ( GDL object message on GLOBAL-FILLETED-POLYGON-PROJECTION, keyword) HIDDEN? ( GDL object message on GLOBAL-FILLETED-POLYLINE, keyword) HIDDEN? ( GDL object message on GEOM-BASE::GLOBAL-FILLETED-POLYLINE-MIXIN, keyword) HIDDEN? ( GDL object message on GLOBAL-POLYGON-PROJECTION, keyword) HIDDEN? ( GDL object message on GLOBAL-POLYLINE, keyword) HIDDEN? ( GDL object message on
```

---

## index.html (chunk 74/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
XIN, keyword) HIDDEN? ( GDL object message on GLOBAL-POLYGON-PROJECTION, keyword) HIDDEN? ( GDL object message on GLOBAL-POLYLINE, keyword) HIDDEN? ( GDL object message on GEOM-BASE::GLOBAL-POLYLINE-MIXIN, keyword) HIDDEN? ( GDL object message on HORIZONTAL-DIMENSION, keyword) HIDDEN? ( GDL object message on LABEL, keyword) HIDDEN? ( GDL object message on LEADER-LINE, keyword) HIDDEN? ( GDL object message on LINE, keyword) HIDDEN? ( GDL object message on LINEAR-DIMENSION, keyword) HIDDEN? ( GDL object message on PARALLEL-DIMENSION, keyword) HIDDEN? ( GDL object message on PIE-CHART, keyword) HIDDEN? ( GDL object message on POINT, keyword) HIDDEN? ( GDL object message on POINTS-DISPLAY, keyword) HIDDEN? ( GDL object message on GEOM-BASE::RENDERER-MIXIN, keyword) HIDDEN? ( GDL object message on ROUTE-PIPE, keyword) HIDDEN? ( GDL object message on SAMPLE-DRAWING, keyword) HIDDEN? ( GDL object message on SPHERE, keyword) HIDDEN? ( GDL object message on SPHERICAL-CAP, keyword) HIDDEN? (
```

---

## index.html (chunk 75/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
on ROUTE-PIPE, keyword) HIDDEN? ( GDL object message on SAMPLE-DRAWING, keyword) HIDDEN? ( GDL object message on SPHERE, keyword) HIDDEN? ( GDL object message on SPHERICAL-CAP, keyword) HIDDEN? ( GDL object message on TEXT-LINE, keyword) HIDDEN? ( GDL object message on TORUS, keyword) HIDDEN? ( GDL object message on TYPESET-BLOCK, keyword) HIDDEN? ( GDL object message on VERTICAL-DIMENSION, keyword) HIDDEN? ( GDL object message on GEYSR::MENU, keyword) HIDDEN? ( GDL object message on GEYSR:TREE, keyword) HIDDEN? ( GDL object message on GWL:APPLICATION-MIXIN, keyword) HIDDEN? ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) HIDDEN? ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) HIDDEN? ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) HIDDEN? ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) HIDDEN? ( GDL object message on GWL:BASE-HTML-SHEET, keyword) HIDDEN? ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) HIDDEN? ( GDL object
```

---

## index.html (chunk 76/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) HIDDEN? ( GDL object message on GWL:BASE-HTML-SHEET, keyword) HIDDEN? ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) HIDDEN? ( GDL object message on GWL::COLOR-MAP, keyword) HIDDEN? ( GDL object message on GWL::GEOMETRY-VIEW-MIXIN, keyword) HIDDEN? ( GDL object message on GWL:GRID-FORM-CONTROL, keyword) HIDDEN? ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) HIDDEN? ( GDL object message on GWL::LAYOUT-MIXIN, keyword) HIDDEN? ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) HIDDEN? ( GDL object message on GWL:NODE-MIXIN, keyword) HIDDEN? ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) HIDDEN? ( GDL object message on GWL:SESSION-CONTROL-MIXIN, keyword) HIDDEN? ( GDL object message on GWL:SHEET-SECTION, keyword) HIDDEN? ( GDL object message on GWL::SKELETON-FORM-CONTROL, keyword) HIDDEN? ( GDL object message on GWL:SKELETON-UI-ELEMENT, keyword) HIDDEN? ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword)
```

---

## index.html (chunk 77/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
DEN? ( GDL object message on GWL::SKELETON-FORM-CONTROL, keyword) HIDDEN? ( GDL object message on GWL:SKELETON-UI-ELEMENT, keyword) HIDDEN? ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) HIDDEN? ( GDL object message on GWL:WEB-DRAWING, keyword) HIDDEN? ( GDL object message on YADD:ASSEMBLY, keyword) HIDDEN? ( GDL object message on YADD::BASE-YADD-SHEET, keyword) HIDDEN? ( GDL object message on YADD::MASTER-INDEX, keyword) HIDDEN? ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) HIDDEN? ( GDL object message on YADD::PACKAGE-FORM, keyword) HOLLOW? ( GDL object message on CYLINDER, keyword) HORIZONTAL-DIMENSION ( Object, geom-base) HTML-CLASS ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) HTML-CLASS ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) HTML-CLASS ( GDL object message on YADD:ASSEMBLY, keyword) HTML-CLASS ( GDL object message on YADD::BASE-YADD-SHEET, keyword) HTML-CLASS ( GDL object message on YADD::MASTER-INDEX, keyword) HTML-CLASS (
```

---

## index.html (chunk 78/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
L-CLASS ( GDL object message on YADD:ASSEMBLY, keyword) HTML-CLASS ( GDL object message on YADD::BASE-YADD-SHEET, keyword) HTML-CLASS ( GDL object message on YADD::MASTER-INDEX, keyword) HTML-CLASS ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) HTML-CLASS ( GDL object message on YADD::PACKAGE-FORM, keyword) HTML-SECTIONS ( GDL object message on GEYSR::MENU, keyword) HTML-SECTIONS ( GDL object message on GEYSR:TREE, keyword) HTML-SECTIONS ( GDL object message on GWL:APPLICATION-MIXIN, keyword) HTML-SECTIONS ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) HTML-SECTIONS ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) HTML-SECTIONS ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) HTML-SECTIONS ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) HTML-SECTIONS ( GDL object message on GWL:BASE-HTML-SHEET, keyword) HTML-SECTIONS ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) HTML-SECTIONS ( GDL object message on GWL::COLOR-MAP,
```

---

## index.html (chunk 79/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
rd) HTML-SECTIONS ( GDL object message on GWL:BASE-HTML-SHEET, keyword) HTML-SECTIONS ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) HTML-SECTIONS ( GDL object message on GWL::COLOR-MAP, keyword) HTML-SECTIONS ( GDL object message on GWL:GRID-FORM-CONTROL, keyword) HTML-SECTIONS ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) HTML-SECTIONS ( GDL object message on GWL::LAYOUT-MIXIN, keyword) HTML-SECTIONS ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) HTML-SECTIONS ( GDL object message on GWL:NODE-MIXIN, keyword) HTML-SECTIONS ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) HTML-SECTIONS ( GDL object message on GWL:SHEET-SECTION, keyword) HTML-SECTIONS ( GDL object message on GWL::SKELETON-FORM-CONTROL, keyword) HTML-SECTIONS ( GDL object message on GWL:SKELETON-UI-ELEMENT, keyword) HTML-SECTIONS ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) HTML-SECTIONS ( GDL object message on YADD:ASSEMBLY, keyword) HTML-SECTIONS ( GDL object message on
```

---

## index.html (chunk 80/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
SKELETON-UI-ELEMENT, keyword) HTML-SECTIONS ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) HTML-SECTIONS ( GDL object message on YADD:ASSEMBLY, keyword) HTML-SECTIONS ( GDL object message on YADD::BASE-YADD-SHEET, keyword) HTML-SECTIONS ( GDL object message on YADD::MASTER-INDEX, keyword) HTML-SECTIONS ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) HTML-SECTIONS ( GDL object message on YADD::PACKAGE-FORM, keyword) HTML-SECTIONS-VALID ( GDL object message on GEYSR::MENU, keyword) HTML-SECTIONS-VALID ( GDL object message on GEYSR:TREE, keyword) HTML-SECTIONS-VALID ( GDL object message on GWL:APPLICATION-MIXIN, keyword) HTML-SECTIONS-VALID ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) HTML-SECTIONS-VALID ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) HTML-SECTIONS-VALID ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) HTML-SECTIONS-VALID ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) HTML-SECTIONS-VALID ( GDL object
```

---

## index.html (chunk 81/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
ord) HTML-SECTIONS-VALID ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) HTML-SECTIONS-VALID ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) HTML-SECTIONS-VALID ( GDL object message on GWL:BASE-HTML-SHEET, keyword) HTML-SECTIONS-VALID ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) HTML-SECTIONS-VALID ( GDL object message on GWL::COLOR-MAP, keyword) HTML-SECTIONS-VALID ( GDL object message on GWL:GRID-FORM-CONTROL, keyword) HTML-SECTIONS-VALID ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) HTML-SECTIONS-VALID ( GDL object message on GWL::LAYOUT-MIXIN, keyword) HTML-SECTIONS-VALID ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) HTML-SECTIONS-VALID ( GDL object message on GWL:NODE-MIXIN, keyword) HTML-SECTIONS-VALID ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) HTML-SECTIONS-VALID ( GDL object message on GWL:SHEET-SECTION, keyword) HTML-SECTIONS-VALID ( GDL object message on GWL::SKELETON-FORM-CONTROL, keyword)
```

---

## index.html (chunk 82/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
L:RADIO-FORM-CONTROL, keyword) HTML-SECTIONS-VALID ( GDL object message on GWL:SHEET-SECTION, keyword) HTML-SECTIONS-VALID ( GDL object message on GWL::SKELETON-FORM-CONTROL, keyword) HTML-SECTIONS-VALID ( GDL object message on GWL:SKELETON-UI-ELEMENT, keyword) HTML-SECTIONS-VALID ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) HTML-SECTIONS-VALID ( GDL object message on YADD:ASSEMBLY, keyword) HTML-SECTIONS-VALID ( GDL object message on YADD::BASE-YADD-SHEET, keyword) HTML-SECTIONS-VALID ( GDL object message on YADD::MASTER-INDEX, keyword) HTML-SECTIONS-VALID ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) HTML-SECTIONS-VALID ( GDL object message on YADD::PACKAGE-FORM, keyword) HTML-STRING ( GDL object message on GWL::SKELETON-FORM-CONTROL, keyword) ID ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) ID ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) ID ( GDL object message on GWL:GRID-FORM-CONTROL, keyword) ID ( GDL object message on
```

---

## index.html (chunk 83/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
ssage on GWL:BASE-FORM-CONTROL, keyword) ID ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) ID ( GDL object message on GWL:GRID-FORM-CONTROL, keyword) ID ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) ID ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) ID ( GDL object message on GWL::SKELETON-FORM-CONTROL, keyword) ID ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) ignore-errors-with-backtrace ( Macro, gendl) IMAGE-FILE ( GDL object message on ANGULAR-DIMENSION, keyword) IMAGE-FILE ( GDL object message on ARC, keyword) IMAGE-FILE ( GDL object message on BASE-COORDINATE-SYSTEM, keyword) IMAGE-FILE ( GDL object message on BASE-DRAWING, keyword) IMAGE-FILE ( GDL object message on BASE-OBJECT, keyword) IMAGE-FILE ( GDL object message on BASE-VIEW, keyword) IMAGE-FILE ( GDL object message on BEZIER-CURVE, keyword) IMAGE-FILE ( GDL object message on BOX, keyword) IMAGE-FILE ( GDL object message on C-CYLINDER, keyword) IMAGE-FILE ( GDL object message on
```

---

## index.html (chunk 84/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
IMAGE-FILE ( GDL object message on BEZIER-CURVE, keyword) IMAGE-FILE ( GDL object message on BOX, keyword) IMAGE-FILE ( GDL object message on C-CYLINDER, keyword) IMAGE-FILE ( GDL object message on CENTER-LINE, keyword) IMAGE-FILE ( GDL object message on CIRCLE, keyword) IMAGE-FILE ( GDL object message on CONE, keyword) IMAGE-FILE ( GDL object message on CONSTRAINED-ARC, keyword) IMAGE-FILE ( GDL object message on CONSTRAINED-FILLET, keyword) IMAGE-FILE ( GDL object message on CONSTRAINED-LINE, keyword) IMAGE-FILE ( GDL object message on CYLINDER, keyword) IMAGE-FILE ( GDL object message on ELLIPSE, keyword) IMAGE-FILE ( GDL object message on GENERAL-NOTE, keyword) IMAGE-FILE ( GDL object message on GLOBAL-FILLETED-POLYGON-PROJECTION, keyword) IMAGE-FILE ( GDL object message on GLOBAL-FILLETED-POLYLINE, keyword) IMAGE-FILE ( GDL object message on GEOM-BASE::GLOBAL-FILLETED-POLYLINE-MIXIN, keyword) IMAGE-FILE ( GDL object message on GLOBAL-POLYGON-PROJECTION, keyword) IMAGE-FILE ( GDL
```

---

## index.html (chunk 85/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
ETED-POLYLINE, keyword) IMAGE-FILE ( GDL object message on GEOM-BASE::GLOBAL-FILLETED-POLYLINE-MIXIN, keyword) IMAGE-FILE ( GDL object message on GLOBAL-POLYGON-PROJECTION, keyword) IMAGE-FILE ( GDL object message on GLOBAL-POLYLINE, keyword) IMAGE-FILE ( GDL object message on GEOM-BASE::GLOBAL-POLYLINE-MIXIN, keyword) IMAGE-FILE ( GDL object message on HORIZONTAL-DIMENSION, keyword) IMAGE-FILE ( GDL object message on LABEL, keyword) IMAGE-FILE ( GDL object message on LEADER-LINE, keyword) IMAGE-FILE ( GDL object message on LINE, keyword) IMAGE-FILE ( GDL object message on LINEAR-DIMENSION, keyword) IMAGE-FILE ( GDL object message on PARALLEL-DIMENSION, keyword) IMAGE-FILE ( GDL object message on PIE-CHART, keyword) IMAGE-FILE ( GDL object message on POINT, keyword) IMAGE-FILE ( GDL object message on POINTS-DISPLAY, keyword) IMAGE-FILE ( GDL object message on ROUTE-PIPE, keyword) IMAGE-FILE ( GDL object message on SAMPLE-DRAWING, keyword) IMAGE-FILE ( GDL object message on SPHERE,
```

---

## index.html (chunk 86/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
message on POINTS-DISPLAY, keyword) IMAGE-FILE ( GDL object message on ROUTE-PIPE, keyword) IMAGE-FILE ( GDL object message on SAMPLE-DRAWING, keyword) IMAGE-FILE ( GDL object message on SPHERE, keyword) IMAGE-FILE ( GDL object message on SPHERICAL-CAP, keyword) IMAGE-FILE ( GDL object message on TEXT-LINE, keyword) IMAGE-FILE ( GDL object message on TORUS, keyword) IMAGE-FILE ( GDL object message on TYPESET-BLOCK, keyword) IMAGE-FILE ( GDL object message on VERTICAL-DIMENSION, keyword) IMAGE-FILE ( GDL object message on GWL:APPLICATION-MIXIN, keyword) IMAGE-FILE ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) IMAGE-FILE ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) IMAGE-FILE ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) IMAGE-FILE ( GDL object message on GWL::LAYOUT-MIXIN, keyword) IMAGE-FILE ( GDL object message on GWL:NODE-MIXIN, keyword) IMAGE-FILE ( GDL object message on GWL:WEB-DRAWING, keyword) IMAGE-FORMAT ( GDL object message on
```

---

## index.html (chunk 87/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
sage on GWL::LAYOUT-MIXIN, keyword) IMAGE-FILE ( GDL object message on GWL:NODE-MIXIN, keyword) IMAGE-FILE ( GDL object message on GWL:WEB-DRAWING, keyword) IMAGE-FORMAT ( GDL object message on GWL:APPLICATION-MIXIN, keyword) IMAGE-FORMAT ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) IMAGE-FORMAT ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) IMAGE-FORMAT ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) IMAGE-FORMAT ( GDL object message on GWL::LAYOUT-MIXIN, keyword) IMAGE-FORMAT ( GDL object message on GWL:NODE-MIXIN, keyword) IMAGE-FORMAT-DEFAULT ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) IMAGE-FORMAT-PLIST ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) IMAGE-FORMAT-SELECTOR ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) IMMUNE-OBJECTS ( GDL object message on BASE-VIEW, keyword) IMMUNE-OBJECTS ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) IMMUNE-OBJECTS ( GDL object message on
```

---

## index.html (chunk 88/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
APHICS-SHEET, keyword) IMMUNE-OBJECTS ( GDL object message on BASE-VIEW, keyword) IMMUNE-OBJECTS ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) IMMUNE-OBJECTS ( GDL object message on GWL:WEB-DRAWING, keyword) IN-FACE? ( GDL object message on BASE-OBJECT, keyword) INCLUDE-DELETE-BUTTONS? ( GDL object message on GWL:GRID-FORM-CONTROL, keyword) INCLUDE-LEGEND? ( GDL object message on PIE-CHART, keyword) INCLUDE-VIEW-CONTROLS? ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) INCLUDED-CHILDREN ( GDL object message on GEYSR::MENU, keyword) INDEX ( GDL object message on QUANTIFICATION, keyword) INDEX ( GDL object message on VANILLA-MIXIN*, keyword) index-filter ( Function, gendl) initialize-standard-endpoints ( Function, gendl-mcp) INNER-BASE-RADIUS ( GDL object message on SPHERICAL-CAP, keyword) INNER-HTML ( GDL object message on GEYSR::MENU, keyword) INNER-HTML ( GDL object message on GEYSR:TREE, keyword) INNER-HTML ( GDL object message on
```

---

## index.html (chunk 89/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
age on SPHERICAL-CAP, keyword) INNER-HTML ( GDL object message on GEYSR::MENU, keyword) INNER-HTML ( GDL object message on GEYSR:TREE, keyword) INNER-HTML ( GDL object message on GWL:APPLICATION-MIXIN, keyword) INNER-HTML ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) INNER-HTML ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) INNER-HTML ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) INNER-HTML ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) INNER-HTML ( GDL object message on GWL:BASE-HTML-SHEET, keyword) INNER-HTML ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) INNER-HTML ( GDL object message on GWL::COLOR-MAP, keyword) INNER-HTML ( GDL object message on GWL:GRID-FORM-CONTROL, keyword) INNER-HTML ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) INNER-HTML ( GDL object message on GWL::LAYOUT-MIXIN, keyword) INNER-HTML ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) INNER-HTML ( GDL object message on GWL:NODE-MIXIN,
```

---

## index.html (chunk 90/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
E-OBJECT, keyword) INNER-HTML ( GDL object message on GWL::LAYOUT-MIXIN, keyword) INNER-HTML ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) INNER-HTML ( GDL object message on GWL:NODE-MIXIN, keyword) INNER-HTML ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) INNER-HTML ( GDL object message on GWL:SHEET-SECTION, keyword) INNER-HTML ( GDL object message on GWL::SKELETON-FORM-CONTROL, keyword) INNER-HTML ( GDL object message on GWL:SKELETON-UI-ELEMENT, keyword) INNER-HTML ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) INNER-HTML ( GDL object message on YADD:ASSEMBLY, keyword) INNER-HTML ( GDL object message on YADD::BASE-YADD-SHEET, keyword) INNER-HTML ( GDL object message on YADD::MASTER-INDEX, keyword) INNER-HTML ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) INNER-HTML ( GDL object message on YADD::PACKAGE-FORM, keyword) INNER-MINOR-RADIUS ( GDL object message on TORUS, keyword) INNER-PIPE-RADIUS ( GDL object message on ROUTE-PIPE, keyword)
```

---

## index.html (chunk 91/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
keyword) INNER-HTML ( GDL object message on YADD::PACKAGE-FORM, keyword) INNER-MINOR-RADIUS ( GDL object message on TORUS, keyword) INNER-PIPE-RADIUS ( GDL object message on ROUTE-PIPE, keyword) INNER-RADIUS ( GDL object message on C-CYLINDER, keyword) INNER-RADIUS ( GDL object message on CONE, keyword) INNER-RADIUS ( GDL object message on CYLINDER, keyword) INNER-RADIUS ( GDL object message on POINT, keyword) INNER-RADIUS ( GDL object message on SPHERE, keyword) INNER-RADIUS-1 ( GDL object message on CONE, keyword) INNER-RADIUS-2 ( GDL object message on CONE, keyword) INPUTS-BGCOLOR ( GDL object message on GWL:APPLICATION-MIXIN, keyword) INPUTS-BGCOLOR ( GDL object message on GWL::LAYOUT-MIXIN, keyword) INPUTS-BGCOLOR ( GDL object message on GWL:NODE-MIXIN, keyword) INPUTS-TITLE ( GDL object message on GWL:APPLICATION-MIXIN, keyword) INPUTS-TITLE ( GDL object message on GWL::LAYOUT-MIXIN, keyword) INPUTS-TITLE ( GDL object message on GWL:NODE-MIXIN, keyword) INSERT! ( GDL object
```

---

## index.html (chunk 92/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
ject message on GWL:APPLICATION-MIXIN, keyword) INPUTS-TITLE ( GDL object message on GWL::LAYOUT-MIXIN, keyword) INPUTS-TITLE ( GDL object message on GWL:NODE-MIXIN, keyword) INSERT! ( GDL object message on GENDL::VARIABLE-SEQUENCE, keyword) inter-circle-sphere ( Function, geom-base) inter-line-plane ( Function, geom-base) inter-line-sphere ( Function, geom-base) ISMAP? ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) ISMAP? ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) ISMAP? ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) ISMAP? ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) ISMAP? ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) iso-8601-date ( Function, gendl) JS-TO-EVAL ( GDL object message on GEYSR::MENU, keyword) JS-TO-EVAL ( GDL object message on GEYSR:TREE, keyword) JS-TO-EVAL ( GDL object message on GWL:APPLICATION-MIXIN, keyword) JS-TO-EVAL ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) JS-TO-EVAL ( GDL object
```

---

## index.html (chunk 93/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
message on GEYSR:TREE, keyword) JS-TO-EVAL ( GDL object message on GWL:APPLICATION-MIXIN, keyword) JS-TO-EVAL ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) JS-TO-EVAL ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) JS-TO-EVAL ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) JS-TO-EVAL ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) JS-TO-EVAL ( GDL object message on GWL:BASE-HTML-SHEET, keyword) JS-TO-EVAL ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) JS-TO-EVAL ( GDL object message on GWL::COLOR-MAP, keyword) JS-TO-EVAL ( GDL object message on GWL:GRID-FORM-CONTROL, keyword) JS-TO-EVAL ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) JS-TO-EVAL ( GDL object message on GWL::LAYOUT-MIXIN, keyword) JS-TO-EVAL ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) JS-TO-EVAL ( GDL object message on GWL:NODE-MIXIN, keyword) JS-TO-EVAL ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) JS-TO-EVAL ( GDL object message on
```

---

## index.html (chunk 94/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
e on GWL:MENU-FORM-CONTROL, keyword) JS-TO-EVAL ( GDL object message on GWL:NODE-MIXIN, keyword) JS-TO-EVAL ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) JS-TO-EVAL ( GDL object message on GWL:SHEET-SECTION, keyword) JS-TO-EVAL ( GDL object message on GWL::SKELETON-FORM-CONTROL, keyword) JS-TO-EVAL ( GDL object message on GWL:SKELETON-UI-ELEMENT, keyword) JS-TO-EVAL ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) JS-TO-EVAL ( GDL object message on YADD:ASSEMBLY, keyword) JS-TO-EVAL ( GDL object message on YADD::BASE-YADD-SHEET, keyword) JS-TO-EVAL ( GDL object message on YADD::MASTER-INDEX, keyword) JS-TO-EVAL ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) JS-TO-EVAL ( GDL object message on YADD::PACKAGE-FORM, keyword) JUSTIFICATION ( GDL object message on ANGULAR-DIMENSION, keyword) JUSTIFICATION ( GDL object message on GENERAL-NOTE, keyword) JUSTIFICATION ( GDL object message on HORIZONTAL-DIMENSION, keyword) JUSTIFICATION ( GDL object message on
```

---

## index.html (chunk 95/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
ANGULAR-DIMENSION, keyword) JUSTIFICATION ( GDL object message on GENERAL-NOTE, keyword) JUSTIFICATION ( GDL object message on HORIZONTAL-DIMENSION, keyword) JUSTIFICATION ( GDL object message on LINEAR-DIMENSION, keyword) JUSTIFICATION ( GDL object message on PARALLEL-DIMENSION, keyword) JUSTIFICATION ( GDL object message on VERTICAL-DIMENSION, keyword) LABEL ( Object, geom-base) LABEL-POSITION ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) LABEL-POSITION ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) LABEL-POSITION ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) LABEL-POSITION ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) LABEL-POSITION ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) LABELS&COLORS ( GDL object message on PIE-CHART, keyword) LANG ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) LANG ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) LANG ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) LANG ( GDL
```

---

## index.html (chunk 96/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
) LANG ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) LANG ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) LANG ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) LANG ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) LANG ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) LAST ( GDL object message on GENDL::MATRIX-SEQUENCE, keyword) LAST ( GDL object message on QUANTIFICATION, keyword) LAST ( GDL object message on GENDL::STANDARD-SEQUENCE, keyword) LAST ( GDL object message on GENDL::VARIABLE-SEQUENCE, keyword) LAST? ( GDL object message on VANILLA-MIXIN*, keyword) lastcar ( Function, gendl) LAYOUT-MIXIN ( Object, gwl) LEADER-1? ( GDL object message on ANGULAR-DIMENSION, keyword) LEADER-1? ( GDL object message on HORIZONTAL-DIMENSION, keyword) LEADER-1? ( GDL object message on LINEAR-DIMENSION, keyword) LEADER-1? ( GDL object message on PARALLEL-DIMENSION, keyword) LEADER-1? ( GDL object message on VERTICAL-DIMENSION, keyword) LEADER-2? ( GDL
```

---

## index.html (chunk 97/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
1? ( GDL object message on LINEAR-DIMENSION, keyword) LEADER-1? ( GDL object message on PARALLEL-DIMENSION, keyword) LEADER-1? ( GDL object message on VERTICAL-DIMENSION, keyword) LEADER-2? ( GDL object message on ANGULAR-DIMENSION, keyword) LEADER-2? ( GDL object message on HORIZONTAL-DIMENSION, keyword) LEADER-2? ( GDL object message on LINEAR-DIMENSION, keyword) LEADER-2? ( GDL object message on PARALLEL-DIMENSION, keyword) LEADER-2? ( GDL object message on VERTICAL-DIMENSION, keyword) LEADER-DIRECTION-1-VECTOR ( GDL object message on ANGULAR-DIMENSION, keyword) LEADER-DIRECTION-1-VECTOR ( GDL object message on HORIZONTAL-DIMENSION, keyword) LEADER-DIRECTION-1-VECTOR ( GDL object message on LINEAR-DIMENSION, keyword) LEADER-DIRECTION-1-VECTOR ( GDL object message on PARALLEL-DIMENSION, keyword) LEADER-DIRECTION-1-VECTOR ( GDL object message on VERTICAL-DIMENSION, keyword) LEADER-DIRECTION-2-VECTOR ( GDL object message on ANGULAR-DIMENSION, keyword) LEADER-DIRECTION-2-VECTOR ( GDL
```

---

## index.html (chunk 98/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
keyword) LEADER-DIRECTION-1-VECTOR ( GDL object message on VERTICAL-DIMENSION, keyword) LEADER-DIRECTION-2-VECTOR ( GDL object message on ANGULAR-DIMENSION, keyword) LEADER-DIRECTION-2-VECTOR ( GDL object message on HORIZONTAL-DIMENSION, keyword) LEADER-DIRECTION-2-VECTOR ( GDL object message on LINEAR-DIMENSION, keyword) LEADER-DIRECTION-2-VECTOR ( GDL object message on PARALLEL-DIMENSION, keyword) LEADER-DIRECTION-2-VECTOR ( GDL object message on VERTICAL-DIMENSION, keyword) LEADER-LINE ( Object, geom-base) LEADER-LINE-LENGTH ( GDL object message on ANGULAR-DIMENSION, keyword) LEADER-LINE-LENGTH ( GDL object message on HORIZONTAL-DIMENSION, keyword) LEADER-LINE-LENGTH ( GDL object message on LINEAR-DIMENSION, keyword) LEADER-LINE-LENGTH ( GDL object message on PARALLEL-DIMENSION, keyword) LEADER-LINE-LENGTH ( GDL object message on VERTICAL-DIMENSION, keyword) LEADER-LINE-LENGTH-2 ( GDL object message on ANGULAR-DIMENSION, keyword) LEADER-LINE-LENGTH-2 ( GDL object message on
```

---

## index.html (chunk 99/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
) LEADER-LINE-LENGTH ( GDL object message on VERTICAL-DIMENSION, keyword) LEADER-LINE-LENGTH-2 ( GDL object message on ANGULAR-DIMENSION, keyword) LEADER-LINE-LENGTH-2 ( GDL object message on HORIZONTAL-DIMENSION, keyword) LEADER-LINE-LENGTH-2 ( GDL object message on LINEAR-DIMENSION, keyword) LEADER-LINE-LENGTH-2 ( GDL object message on PARALLEL-DIMENSION, keyword) LEADER-LINE-LENGTH-2 ( GDL object message on VERTICAL-DIMENSION, keyword) LEADER-PATH ( GDL object message on LABEL, keyword) LEADER-RADIUS ( GDL object message on ANGULAR-DIMENSION, keyword) LEADER-TEXT-GAP ( GDL object message on ANGULAR-DIMENSION, keyword) LEADER-TEXT-GAP ( GDL object message on HORIZONTAL-DIMENSION, keyword) LEADER-TEXT-GAP ( GDL object message on LINEAR-DIMENSION, keyword) LEADER-TEXT-GAP ( GDL object message on PARALLEL-DIMENSION, keyword) LEADER-TEXT-GAP ( GDL object message on VERTICAL-DIMENSION, keyword) LEADING ( GDL object message on GENERAL-NOTE, keyword) LEAF? ( GDL object message on
```

---

## index.html (chunk 100/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
age on PARALLEL-DIMENSION, keyword) LEADER-TEXT-GAP ( GDL object message on VERTICAL-DIMENSION, keyword) LEADING ( GDL object message on GENERAL-NOTE, keyword) LEAF? ( GDL object message on VANILLA-MIXIN*, keyword) least ( Function, gendl) LEAVES ( GDL object message on VANILLA-MIXIN*, keyword) LEFT-MARGIN ( GDL object message on BASE-VIEW, keyword) LENGTH ( GDL object message on ANGULAR-DIMENSION, keyword) LENGTH ( GDL object message on ARC, keyword) LENGTH ( GDL object message on BASE-COORDINATE-SYSTEM, keyword) LENGTH ( GDL object message on BASE-DRAWING, keyword) LENGTH ( GDL object message on BASE-OBJECT, keyword) LENGTH ( GDL object message on BASE-VIEW, keyword) LENGTH ( GDL object message on BEZIER-CURVE, keyword) LENGTH ( GDL object message on BOX, keyword) LENGTH ( GDL object message on C-CYLINDER, keyword) LENGTH ( GDL object message on CENTER-LINE, keyword) LENGTH ( GDL object message on CONE, keyword) LENGTH ( GDL object message on CYLINDER, keyword) LENGTH ( GDL object
```

---

## index.html (chunk 101/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
sage on C-CYLINDER, keyword) LENGTH ( GDL object message on CENTER-LINE, keyword) LENGTH ( GDL object message on CONE, keyword) LENGTH ( GDL object message on CYLINDER, keyword) LENGTH ( GDL object message on ELLIPSE, keyword) LENGTH ( GDL object message on GENERAL-NOTE, keyword) LENGTH ( GDL object message on GLOBAL-FILLETED-POLYGON-PROJECTION, keyword) LENGTH ( GDL object message on GLOBAL-FILLETED-POLYLINE, keyword) LENGTH ( GDL object message on GEOM-BASE::GLOBAL-FILLETED-POLYLINE-MIXIN, keyword) LENGTH ( GDL object message on GLOBAL-POLYGON-PROJECTION, keyword) LENGTH ( GDL object message on GLOBAL-POLYLINE, keyword) LENGTH ( GDL object message on GEOM-BASE::GLOBAL-POLYLINE-MIXIN, keyword) LENGTH ( GDL object message on HORIZONTAL-DIMENSION, keyword) LENGTH ( GDL object message on LABEL, keyword) LENGTH ( GDL object message on LEADER-LINE, keyword) LENGTH ( GDL object message on LINE, keyword) LENGTH ( GDL object message on LINEAR-DIMENSION, keyword) LENGTH ( GDL object message
```

---

## index.html (chunk 102/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
EL, keyword) LENGTH ( GDL object message on LEADER-LINE, keyword) LENGTH ( GDL object message on LINE, keyword) LENGTH ( GDL object message on LINEAR-DIMENSION, keyword) LENGTH ( GDL object message on PARALLEL-DIMENSION, keyword) LENGTH ( GDL object message on PIE-CHART, keyword) LENGTH ( GDL object message on POINTS-DISPLAY, keyword) LENGTH ( GDL object message on ROUTE-PIPE, keyword) LENGTH ( GDL object message on SAMPLE-DRAWING, keyword) LENGTH ( GDL object message on SPHERE, keyword) LENGTH ( GDL object message on SPHERICAL-CAP, keyword) LENGTH ( GDL object message on TEXT-LINE, keyword) LENGTH ( GDL object message on TORUS, keyword) LENGTH ( GDL object message on TYPESET-BLOCK, keyword) LENGTH ( GDL object message on VERTICAL-DIMENSION, keyword) LENGTH ( GDL object message on GWL:APPLICATION-MIXIN, keyword) LENGTH ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) LENGTH ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) LENGTH ( GDL object message on
```

---

## index.html (chunk 103/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
ICATION-MIXIN, keyword) LENGTH ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) LENGTH ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) LENGTH ( GDL object message on GWL::GEOMETRY-VIEW-MIXIN, keyword) LENGTH ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) LENGTH ( GDL object message on GWL::LAYOUT-MIXIN, keyword) LENGTH ( GDL object message on GWL:NODE-MIXIN, keyword) LENGTH ( GDL object message on GWL:WEB-DRAWING, keyword) LENGTH-DEFAULT ( GDL object message on TYPESET-BLOCK, keyword) length-vector ( Function, geom-base) LINE ( Object, geom-base) LINE-COLOR ( GDL object message on PIE-CHART, keyword) LINE-INTERSECTION-2D ( GDL object message on BEZIER-CURVE, keyword) LINE-INTERSECTION-POINTS ( GDL object message on BASE-OBJECT, keyword) LINEAR-DIMENSION ( Object, geom-base) LINES ( GDL object message on GEOM-BASE::GLOBAL-POLYLINE-MIXIN, keyword) LINES ( GDL object message on TYPESET-BLOCK, keyword) LISP-HEAP-SIZE ( GDL object message on GDL-APP,
```

---

## index.html (chunk 104/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
( Object, geom-base) LINES ( GDL object message on GEOM-BASE::GLOBAL-POLYLINE-MIXIN, keyword) LINES ( GDL object message on TYPESET-BLOCK, keyword) LISP-HEAP-SIZE ( GDL object message on GDL-APP, keyword) list-elements ( Macro, gendl) list-of-n-numbers ( Function, gendl) list-of-numbers ( Function, gendl) LOAD-ALWAYS? ( GDL object message on CL-LITE:CODEBASE-DIRECTORY-NODE, keyword) load-glime ( Function, gendl) load-quicklisp ( Function, gendl) LOCAL-BBOX ( GDL object message on BASE-OBJECT, keyword) LOCAL-BOX ( GDL object message on ARC, keyword) LOCAL-BOX ( GDL object message on BASE-COORDINATE-SYSTEM, keyword) LOCAL-BOX ( GDL object message on BASE-DRAWING, keyword) LOCAL-BOX ( GDL object message on BASE-OBJECT, keyword) LOCAL-BOX ( GDL object message on BASE-VIEW, keyword) LOCAL-BOX ( GDL object message on BEZIER-CURVE, keyword) LOCAL-BOX ( GDL object message on BOX, keyword) LOCAL-BOX ( GDL object message on C-CYLINDER, keyword) LOCAL-BOX ( GDL object message on CIRCLE, keyword)
```

---

## index.html (chunk 105/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
GDL object message on BEZIER-CURVE, keyword) LOCAL-BOX ( GDL object message on BOX, keyword) LOCAL-BOX ( GDL object message on C-CYLINDER, keyword) LOCAL-BOX ( GDL object message on CIRCLE, keyword) LOCAL-BOX ( GDL object message on CONE, keyword) LOCAL-BOX ( GDL object message on CONSTRAINED-ARC, keyword) LOCAL-BOX ( GDL object message on CONSTRAINED-FILLET, keyword) LOCAL-BOX ( GDL object message on CONSTRAINED-LINE, keyword) LOCAL-BOX ( GDL object message on CYLINDER, keyword) LOCAL-BOX ( GDL object message on ELLIPSE, keyword) LOCAL-BOX ( GDL object message on GLOBAL-FILLETED-POLYGON-PROJECTION, keyword) LOCAL-BOX ( GDL object message on GLOBAL-FILLETED-POLYLINE, keyword) LOCAL-BOX ( GDL object message on GEOM-BASE::GLOBAL-FILLETED-POLYLINE-MIXIN, keyword) LOCAL-BOX ( GDL object message on GLOBAL-POLYGON-PROJECTION, keyword) LOCAL-BOX ( GDL object message on GLOBAL-POLYLINE, keyword) LOCAL-BOX ( GDL object message on GEOM-BASE::GLOBAL-POLYLINE-MIXIN, keyword) LOCAL-BOX ( GDL
```

---

## index.html (chunk 106/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
ge on GLOBAL-POLYGON-PROJECTION, keyword) LOCAL-BOX ( GDL object message on GLOBAL-POLYLINE, keyword) LOCAL-BOX ( GDL object message on GEOM-BASE::GLOBAL-POLYLINE-MIXIN, keyword) LOCAL-BOX ( GDL object message on LEADER-LINE, keyword) LOCAL-BOX ( GDL object message on LINE, keyword) LOCAL-BOX ( GDL object message on PIE-CHART, keyword) LOCAL-BOX ( GDL object message on POINT, keyword) LOCAL-BOX ( GDL object message on SAMPLE-DRAWING, keyword) LOCAL-BOX ( GDL object message on SPHERE, keyword) LOCAL-BOX ( GDL object message on SPHERICAL-CAP, keyword) LOCAL-BOX ( GDL object message on TEXT-LINE, keyword) LOCAL-BOX ( GDL object message on TORUS, keyword) LOCAL-BOX ( GDL object message on TYPESET-BLOCK, keyword) LOCAL-BOX ( GDL object message on GWL:APPLICATION-MIXIN, keyword) LOCAL-BOX ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) LOCAL-BOX ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) LOCAL-BOX ( GDL object message on GWL:GWL-RULE-OBJECT, keyword)
```

---

## index.html (chunk 107/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) LOCAL-BOX ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) LOCAL-BOX ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) LOCAL-BOX ( GDL object message on GWL::LAYOUT-MIXIN, keyword) LOCAL-BOX ( GDL object message on GWL:NODE-MIXIN, keyword) LOCAL-BOX ( GDL object message on GWL:WEB-DRAWING, keyword) LOCAL-CENTER ( GDL object message on BASE-OBJECT, keyword) LOCAL-CENTER* ( GDL object message on BASE-OBJECT, keyword) LOCAL-ORIENTATION ( GDL object message on BASE-OBJECT, keyword) LOCAL-TO-GLOBAL ( GDL object message on BASE-OBJECT, keyword) LONG-SEGMENT-LENGTH ( GDL object message on CENTER-LINE, keyword) MAIN-DIV% ( GDL object message on GWL:SKELETON-UI-ELEMENT, keyword) MAIN-SHEET-BODY ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) MAIN-SHEET-BODY ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) MAIN-SHEET-BODY ( GDL object message on YADD:ASSEMBLY, keyword) MAIN-SHEET-BODY ( GDL object
```

---

## index.html (chunk 108/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
SE-AJAX-GRAPHICS-SHEET, keyword) MAIN-SHEET-BODY ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) MAIN-SHEET-BODY ( GDL object message on YADD:ASSEMBLY, keyword) MAIN-SHEET-BODY ( GDL object message on YADD::BASE-YADD-SHEET, keyword) MAIN-SHEET-BODY ( GDL object message on YADD::MASTER-INDEX, keyword) MAIN-SHEET-BODY ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) MAIN-SHEET-BODY ( GDL object message on YADD::PACKAGE-FORM, keyword) MAIN-VIEW ( GDL object message on GWL:WEB-DRAWING, keyword) MAJOR-AXIS-LENGTH ( GDL object message on ELLIPSE, keyword) MAJOR-RADIUS ( GDL object message on TORUS, keyword) MAKE! ( GDL object message on GDL-APP, keyword) make-gdl-app ( Function, gendl) make-keyword ( Function, gendl) make-object ( Function, gendl) make-point ( Macro, geom-base) make-transform ( Function, geom-base) make-vector ( Macro, geom-base) mapsend ( Function, gendl) maptree ( Function, gendl) MASTER-INDEX ( GDL object message on YADD:ASSEMBLY, keyword) MASTER-INDEX
```

---

## index.html (chunk 109/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
make-transform ( Function, geom-base) make-vector ( Macro, geom-base) mapsend ( Function, gendl) maptree ( Function, gendl) MASTER-INDEX ( GDL object message on YADD:ASSEMBLY, keyword) MASTER-INDEX ( Object, yadd) matrix*vector ( Function, geom-base) MATRIX-SEQUENCE ( Object, gendl) matrix-to-quaternion ( Function, geom-base) max-of-elements ( Macro, gendl) MAXIMUM-TEXT-WIDTH ( GDL object message on GENERAL-NOTE, keyword) MAXLENGTH ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) MAXLENGTH ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) MAXLENGTH ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) MAXLENGTH ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) MAXLENGTH ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) MENU ( Object, geysr) MENU-FORM-CONTROL ( Object, gwl) merge-display-controls ( Macro, geom-base) MESSAGE-DOCUMENTATION ( GDL object message on VANILLA-MIXIN*, keyword) MESSAGE-LIST ( GDL object message on VANILLA-MIXIN*, keyword)
```

---

## index.html (chunk 110/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
L ( Object, gwl) merge-display-controls ( Macro, geom-base) MESSAGE-DOCUMENTATION ( GDL object message on VANILLA-MIXIN*, keyword) MESSAGE-LIST ( GDL object message on VANILLA-MIXIN*, keyword) midpoint ( Function, geom-base) min-of-elements ( Macro, gendl) MINOR-AXIS-LENGTH ( GDL object message on ELLIPSE, keyword) MINOR-RADIUS ( GDL object message on TORUS, keyword) MIXINS ( GDL object message on VANILLA-MIXIN*, keyword) MODEL-POINT ( GDL object message on BASE-VIEW, keyword) most ( Function, gendl) MULTIPART-FORM? ( GDL object message on GWL:APPLICATION-MIXIN, keyword) MULTIPART-FORM? ( GDL object message on GWL::LAYOUT-MIXIN, keyword) MULTIPART-FORM? ( GDL object message on GWL:NODE-MIXIN, keyword) MULTIPLE? ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) MULTIPLE? ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) multiply-matrices ( Function, geom-base) NAME-FOR-DISPLAY ( GDL object message on VANILLA-MIXIN*, keyword) near-to? ( Function, gendl) near-zero? (
```

---

## index.html (chunk 111/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
message on GWL:RADIO-FORM-CONTROL, keyword) multiply-matrices ( Function, geom-base) NAME-FOR-DISPLAY ( GDL object message on VANILLA-MIXIN*, keyword) near-to? ( Function, gendl) near-zero? ( Function, gendl) never ( Function, gendl) NEXT ( GDL object message on VANILLA-MIXIN*, keyword) NODE-MIXIN ( Object, gwl) NODE-UI-DISPLAY-LIST-OBJECTS ( GDL object message on GWL:NODE-MIXIN, keyword) NULL-OBJECT ( Object, gendl) NULLIFY-EMPTY-STRING? ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) NULLIFY-EMPTY-STRING? ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) NULLIFY-EMPTY-STRING? ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) NULLIFY-EMPTY-STRING? ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) NULLIFY-EMPTY-STRING? ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) number-format ( Function, gendl) NUMBER-OF-HORIZONTAL-SECTIONS ( GDL object message on POINT, keyword) NUMBER-OF-HORIZONTAL-SECTIONS ( GDL object message on SPHERE, keyword)
```

---

## index.html (chunk 112/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
NTROL, keyword) number-format ( Function, gendl) NUMBER-OF-HORIZONTAL-SECTIONS ( GDL object message on POINT, keyword) NUMBER-OF-HORIZONTAL-SECTIONS ( GDL object message on SPHERE, keyword) NUMBER-OF-HORIZONTAL-SECTIONS ( GDL object message on SPHERICAL-CAP, keyword) NUMBER-OF-LONGITUDINAL-SECTIONS ( GDL object message on TORUS, keyword) NUMBER-OF-SECTIONS ( GDL object message on C-CYLINDER, keyword) NUMBER-OF-SECTIONS ( GDL object message on CONE, keyword) NUMBER-OF-SECTIONS ( GDL object message on CYLINDER, keyword) NUMBER-OF-TRANSVERSE-SECTIONS ( GDL object message on TORUS, keyword) NUMBER-OF-VERTICAL-SECTIONS ( GDL object message on POINT, keyword) NUMBER-OF-VERTICAL-SECTIONS ( GDL object message on SPHERE, keyword) NUMBER-OF-VERTICAL-SECTIONS ( GDL object message on SPHERICAL-CAP, keyword) number-round ( Function, gendl) NUMBER? ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) OBJECT-DOCS ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) OBJECT-ROOTS ( GDL
```

---

## index.html (chunk 113/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
yword) number-round ( Function, gendl) NUMBER? ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) OBJECT-DOCS ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) OBJECT-ROOTS ( GDL object message on BASE-VIEW, keyword) OBJECT-ROOTS ( GDL object message on GEOM-BASE::RENDERER-MIXIN, keyword) OBJECT-ROOTS ( GDL object message on GWL:WEB-DRAWING, keyword) OBJECTS ( GDL object message on BASE-VIEW, keyword) OBJECTS ( GDL object message on GEOM-BASE::RENDERER-MIXIN, keyword) OBJECTS ( GDL object message on GWL:WEB-DRAWING, keyword) OBLIQUENESS ( GDL object message on ANGULAR-DIMENSION, keyword) OBLIQUENESS ( GDL object message on ARC, keyword) OBLIQUENESS ( GDL object message on BASE-COORDINATE-SYSTEM, keyword) OBLIQUENESS ( GDL object message on BASE-DRAWING, keyword) OBLIQUENESS ( GDL object message on BASE-OBJECT, keyword) OBLIQUENESS ( GDL object message on BASE-VIEW, keyword) OBLIQUENESS ( GDL object message on BEZIER-CURVE, keyword) OBLIQUENESS ( GDL object message on
```

---

## index.html (chunk 114/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
( GDL object message on BASE-OBJECT, keyword) OBLIQUENESS ( GDL object message on BASE-VIEW, keyword) OBLIQUENESS ( GDL object message on BEZIER-CURVE, keyword) OBLIQUENESS ( GDL object message on BOX, keyword) OBLIQUENESS ( GDL object message on C-CYLINDER, keyword) OBLIQUENESS ( GDL object message on CENTER-LINE, keyword) OBLIQUENESS ( GDL object message on CIRCLE, keyword) OBLIQUENESS ( GDL object message on CONE, keyword) OBLIQUENESS ( GDL object message on CONSTRAINED-ARC, keyword) OBLIQUENESS ( GDL object message on CONSTRAINED-FILLET, keyword) OBLIQUENESS ( GDL object message on CONSTRAINED-LINE, keyword) OBLIQUENESS ( GDL object message on CYLINDER, keyword) OBLIQUENESS ( GDL object message on ELLIPSE, keyword) OBLIQUENESS ( GDL object message on GENERAL-NOTE, keyword) OBLIQUENESS ( GDL object message on GLOBAL-FILLETED-POLYGON-PROJECTION, keyword) OBLIQUENESS ( GDL object message on GLOBAL-FILLETED-POLYLINE, keyword) OBLIQUENESS ( GDL object message on
```

---

## index.html (chunk 115/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
( GDL object message on GLOBAL-FILLETED-POLYGON-PROJECTION, keyword) OBLIQUENESS ( GDL object message on GLOBAL-FILLETED-POLYLINE, keyword) OBLIQUENESS ( GDL object message on GEOM-BASE::GLOBAL-FILLETED-POLYLINE-MIXIN, keyword) OBLIQUENESS ( GDL object message on GLOBAL-POLYGON-PROJECTION, keyword) OBLIQUENESS ( GDL object message on GLOBAL-POLYLINE, keyword) OBLIQUENESS ( GDL object message on GEOM-BASE::GLOBAL-POLYLINE-MIXIN, keyword) OBLIQUENESS ( GDL object message on HORIZONTAL-DIMENSION, keyword) OBLIQUENESS ( GDL object message on LABEL, keyword) OBLIQUENESS ( GDL object message on LEADER-LINE, keyword) OBLIQUENESS ( GDL object message on LINE, keyword) OBLIQUENESS ( GDL object message on LINEAR-DIMENSION, keyword) OBLIQUENESS ( GDL object message on PARALLEL-DIMENSION, keyword) OBLIQUENESS ( GDL object message on PIE-CHART, keyword) OBLIQUENESS ( GDL object message on POINT, keyword) OBLIQUENESS ( GDL object message on POINTS-DISPLAY, keyword) OBLIQUENESS ( GDL object message
```

---

## index.html (chunk 116/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
LIQUENESS ( GDL object message on PIE-CHART, keyword) OBLIQUENESS ( GDL object message on POINT, keyword) OBLIQUENESS ( GDL object message on POINTS-DISPLAY, keyword) OBLIQUENESS ( GDL object message on ROUTE-PIPE, keyword) OBLIQUENESS ( GDL object message on SAMPLE-DRAWING, keyword) OBLIQUENESS ( GDL object message on SPHERE, keyword) OBLIQUENESS ( GDL object message on SPHERICAL-CAP, keyword) OBLIQUENESS ( GDL object message on TEXT-LINE, keyword) OBLIQUENESS ( GDL object message on TORUS, keyword) OBLIQUENESS ( GDL object message on TYPESET-BLOCK, keyword) OBLIQUENESS ( GDL object message on VERTICAL-DIMENSION, keyword) OBLIQUENESS ( GDL object message on GWL:APPLICATION-MIXIN, keyword) OBLIQUENESS ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) OBLIQUENESS ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) OBLIQUENESS ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) OBLIQUENESS ( GDL object message on GWL::LAYOUT-MIXIN, keyword) OBLIQUENESS ( GDL
```

---

## index.html (chunk 117/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
essage on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) OBLIQUENESS ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) OBLIQUENESS ( GDL object message on GWL::LAYOUT-MIXIN, keyword) OBLIQUENESS ( GDL object message on GWL:NODE-MIXIN, keyword) OBLIQUENESS ( GDL object message on GWL:WEB-DRAWING, keyword) OFFSET ( GDL object message on GLOBAL-FILLETED-POLYGON-PROJECTION, keyword) OFFSET ( GDL object message on GLOBAL-POLYGON-PROJECTION, keyword) ONBLUR ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) ONBLUR ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) ONBLUR ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) ONBLUR ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) ONBLUR ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) ONCHANGE ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) ONCHANGE ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) ONCHANGE ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) ONCHANGE ( GDL object message on
```

---

## index.html (chunk 118/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
L:BASE-FORM-CONTROL, keyword) ONCHANGE ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) ONCHANGE ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) ONCHANGE ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) ONCHANGE ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) ONCLICK ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) ONCLICK ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) ONCLICK ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) ONCLICK ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) ONCLICK ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) ONCLICK-FUNCTION ( GDL object message on ANGULAR-DIMENSION, keyword) ONCLICK-FUNCTION ( GDL object message on ARC, keyword) ONCLICK-FUNCTION ( GDL object message on BASE-COORDINATE-SYSTEM, keyword) ONCLICK-FUNCTION ( GDL object message on BASE-DRAWING, keyword) ONCLICK-FUNCTION ( GDL object message on BASE-OBJECT, keyword) ONCLICK-FUNCTION ( GDL object message on BASE-VIEW,
```

---

## index.html (chunk 119/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
-SYSTEM, keyword) ONCLICK-FUNCTION ( GDL object message on BASE-DRAWING, keyword) ONCLICK-FUNCTION ( GDL object message on BASE-OBJECT, keyword) ONCLICK-FUNCTION ( GDL object message on BASE-VIEW, keyword) ONCLICK-FUNCTION ( GDL object message on BEZIER-CURVE, keyword) ONCLICK-FUNCTION ( GDL object message on BOX, keyword) ONCLICK-FUNCTION ( GDL object message on C-CYLINDER, keyword) ONCLICK-FUNCTION ( GDL object message on CENTER-LINE, keyword) ONCLICK-FUNCTION ( GDL object message on CIRCLE, keyword) ONCLICK-FUNCTION ( GDL object message on CONE, keyword) ONCLICK-FUNCTION ( GDL object message on CONSTRAINED-ARC, keyword) ONCLICK-FUNCTION ( GDL object message on CONSTRAINED-FILLET, keyword) ONCLICK-FUNCTION ( GDL object message on CONSTRAINED-LINE, keyword) ONCLICK-FUNCTION ( GDL object message on CYLINDER, keyword) ONCLICK-FUNCTION ( GDL object message on ELLIPSE, keyword) ONCLICK-FUNCTION ( GDL object message on GENERAL-NOTE, keyword) ONCLICK-FUNCTION ( GDL object message on
```

---

## index.html (chunk 120/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
essage on CYLINDER, keyword) ONCLICK-FUNCTION ( GDL object message on ELLIPSE, keyword) ONCLICK-FUNCTION ( GDL object message on GENERAL-NOTE, keyword) ONCLICK-FUNCTION ( GDL object message on GLOBAL-FILLETED-POLYGON-PROJECTION, keyword) ONCLICK-FUNCTION ( GDL object message on GLOBAL-FILLETED-POLYLINE, keyword) ONCLICK-FUNCTION ( GDL object message on GEOM-BASE::GLOBAL-FILLETED-POLYLINE-MIXIN, keyword) ONCLICK-FUNCTION ( GDL object message on GLOBAL-POLYGON-PROJECTION, keyword) ONCLICK-FUNCTION ( GDL object message on GLOBAL-POLYLINE, keyword) ONCLICK-FUNCTION ( GDL object message on GEOM-BASE::GLOBAL-POLYLINE-MIXIN, keyword) ONCLICK-FUNCTION ( GDL object message on HORIZONTAL-DIMENSION, keyword) ONCLICK-FUNCTION ( GDL object message on LABEL, keyword) ONCLICK-FUNCTION ( GDL object message on LEADER-LINE, keyword) ONCLICK-FUNCTION ( GDL object message on LINE, keyword) ONCLICK-FUNCTION ( GDL object message on LINEAR-DIMENSION, keyword) ONCLICK-FUNCTION ( GDL object message on
```

---

## index.html (chunk 121/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
e on LEADER-LINE, keyword) ONCLICK-FUNCTION ( GDL object message on LINE, keyword) ONCLICK-FUNCTION ( GDL object message on LINEAR-DIMENSION, keyword) ONCLICK-FUNCTION ( GDL object message on PARALLEL-DIMENSION, keyword) ONCLICK-FUNCTION ( GDL object message on PIE-CHART, keyword) ONCLICK-FUNCTION ( GDL object message on POINT, keyword) ONCLICK-FUNCTION ( GDL object message on POINTS-DISPLAY, keyword) ONCLICK-FUNCTION ( GDL object message on ROUTE-PIPE, keyword) ONCLICK-FUNCTION ( GDL object message on SAMPLE-DRAWING, keyword) ONCLICK-FUNCTION ( GDL object message on SPHERE, keyword) ONCLICK-FUNCTION ( GDL object message on SPHERICAL-CAP, keyword) ONCLICK-FUNCTION ( GDL object message on TEXT-LINE, keyword) ONCLICK-FUNCTION ( GDL object message on TORUS, keyword) ONCLICK-FUNCTION ( GDL object message on TYPESET-BLOCK, keyword) ONCLICK-FUNCTION ( GDL object message on VERTICAL-DIMENSION, keyword) ONCLICK-FUNCTION ( GDL object message on GEYSR:TREE, keyword) ONCLICK-FUNCTION ( GDL
```

---

## index.html (chunk 122/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
ect message on TYPESET-BLOCK, keyword) ONCLICK-FUNCTION ( GDL object message on VERTICAL-DIMENSION, keyword) ONCLICK-FUNCTION ( GDL object message on GEYSR:TREE, keyword) ONCLICK-FUNCTION ( GDL object message on GWL:APPLICATION-MIXIN, keyword) ONCLICK-FUNCTION ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) ONCLICK-FUNCTION ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) ONCLICK-FUNCTION ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) ONCLICK-FUNCTION ( GDL object message on GWL::LAYOUT-MIXIN, keyword) ONCLICK-FUNCTION ( GDL object message on GWL:NODE-MIXIN, keyword) ONCLICK-FUNCTION ( GDL object message on GWL:WEB-DRAWING, keyword) ONDBLCLICK ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) ONDBLCLICK ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) ONDBLCLICK ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) ONDBLCLICK ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) ONDBLCLICK ( GDL object message on
```

---

## index.html (chunk 123/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
TROL, keyword) ONDBLCLICK ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) ONDBLCLICK ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) ONDBLCLICK ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) ONENTER ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) ONENTER ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) ONENTER ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) ONENTER ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) ONENTER ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) ONFOCUS ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) ONFOCUS ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) ONFOCUS ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) ONFOCUS ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) ONFOCUS ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) ONKEYDOWN ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) ONKEYDOWN ( GDL object message on GWL:CHECKBOX-FORM-CONTROL,
```

---

## index.html (chunk 124/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
d) ONFOCUS ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) ONKEYDOWN ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) ONKEYDOWN ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) ONKEYDOWN ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) ONKEYDOWN ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) ONKEYDOWN ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) ONKEYPRESS ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) ONKEYPRESS ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) ONKEYPRESS ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) ONKEYPRESS ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) ONKEYPRESS ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) ONKEYUP ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) ONKEYUP ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) ONKEYUP ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) ONKEYUP ( GDL object message on GWL:RADIO-FORM-CONTROL,
```

---

## index.html (chunk 125/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
word) ONKEYUP ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) ONKEYUP ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) ONKEYUP ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) ONKEYUP ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) ONMOUSEDOWN ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) ONMOUSEDOWN ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) ONMOUSEDOWN ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) ONMOUSEDOWN ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) ONMOUSEDOWN ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) ONMOUSEMOVE ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) ONMOUSEMOVE ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) ONMOUSEMOVE ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) ONMOUSEMOVE ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) ONMOUSEMOVE ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) ONMOUSEOUT ( GDL object message on
```

---

## index.html (chunk 126/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
M-CONTROL, keyword) ONMOUSEMOVE ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) ONMOUSEMOVE ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) ONMOUSEOUT ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) ONMOUSEOUT ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) ONMOUSEOUT ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) ONMOUSEOUT ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) ONMOUSEOUT ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) ONMOUSEOVER ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) ONMOUSEOVER ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) ONMOUSEOVER ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) ONMOUSEOVER ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) ONMOUSEOVER ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) ONMOUSEUP ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) ONMOUSEUP ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) ONMOUSEUP ( GDL
```

---

## index.html (chunk 127/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
essage on GWL:TEXT-FORM-CONTROL, keyword) ONMOUSEUP ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) ONMOUSEUP ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) ONMOUSEUP ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) ONMOUSEUP ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) ONMOUSEUP ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) ONSELECT ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) ONSELECT ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) ONSELECT ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) ONSELECT ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) ONSELECT ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) ORDERED-FORM-CONTROLS ( GDL object message on GEYSR::MENU, keyword) ORDERED-FORM-CONTROLS ( GDL object message on GEYSR:TREE, keyword) ORDERED-FORM-CONTROLS ( GDL object message on GWL:APPLICATION-MIXIN, keyword) ORDERED-FORM-CONTROLS ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET,
```

---

## index.html (chunk 128/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
DL object message on GEYSR:TREE, keyword) ORDERED-FORM-CONTROLS ( GDL object message on GWL:APPLICATION-MIXIN, keyword) ORDERED-FORM-CONTROLS ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) ORDERED-FORM-CONTROLS ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) ORDERED-FORM-CONTROLS ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) ORDERED-FORM-CONTROLS ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) ORDERED-FORM-CONTROLS ( GDL object message on GWL:BASE-HTML-SHEET, keyword) ORDERED-FORM-CONTROLS ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) ORDERED-FORM-CONTROLS ( GDL object message on GWL::COLOR-MAP, keyword) ORDERED-FORM-CONTROLS ( GDL object message on GWL:GRID-FORM-CONTROL, keyword) ORDERED-FORM-CONTROLS ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) ORDERED-FORM-CONTROLS ( GDL object message on GWL::LAYOUT-MIXIN, keyword) ORDERED-FORM-CONTROLS ( GDL object message on GWL:MENU-FORM-CONTROL, keyword)
```

---

## index.html (chunk 129/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
:GWL-RULE-OBJECT, keyword) ORDERED-FORM-CONTROLS ( GDL object message on GWL::LAYOUT-MIXIN, keyword) ORDERED-FORM-CONTROLS ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) ORDERED-FORM-CONTROLS ( GDL object message on GWL:NODE-MIXIN, keyword) ORDERED-FORM-CONTROLS ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) ORDERED-FORM-CONTROLS ( GDL object message on GWL:SHEET-SECTION, keyword) ORDERED-FORM-CONTROLS ( GDL object message on GWL::SKELETON-FORM-CONTROL, keyword) ORDERED-FORM-CONTROLS ( GDL object message on GWL:SKELETON-UI-ELEMENT, keyword) ORDERED-FORM-CONTROLS ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) ORDERED-FORM-CONTROLS ( GDL object message on YADD:ASSEMBLY, keyword) ORDERED-FORM-CONTROLS ( GDL object message on YADD::BASE-YADD-SHEET, keyword) ORDERED-FORM-CONTROLS ( GDL object message on YADD::MASTER-INDEX, keyword) ORDERED-FORM-CONTROLS ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) ORDERED-FORM-CONTROLS ( GDL object message on
```

---

## index.html (chunk 130/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
FORM-CONTROLS ( GDL object message on YADD::MASTER-INDEX, keyword) ORDERED-FORM-CONTROLS ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) ORDERED-FORM-CONTROLS ( GDL object message on YADD::PACKAGE-FORM, keyword) ORG-TYPE ( GDL object message on GWL:SESSION-CONTROL-MIXIN, keyword) ORIENTATION ( GDL object message on ANGULAR-DIMENSION, keyword) ORIENTATION ( GDL object message on ARC, keyword) ORIENTATION ( GDL object message on BASE-COORDINATE-SYSTEM, keyword) ORIENTATION ( GDL object message on BASE-DRAWING, keyword) ORIENTATION ( GDL object message on BASE-OBJECT, keyword) ORIENTATION ( GDL object message on BASE-VIEW, keyword) ORIENTATION ( GDL object message on BEZIER-CURVE, keyword) ORIENTATION ( GDL object message on BOX, keyword) ORIENTATION ( GDL object message on C-CYLINDER, keyword) ORIENTATION ( GDL object message on CENTER-LINE, keyword) ORIENTATION ( GDL object message on CIRCLE, keyword) ORIENTATION ( GDL object message on CONE, keyword) ORIENTATION ( GDL
```

---

## index.html (chunk 131/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
INDER, keyword) ORIENTATION ( GDL object message on CENTER-LINE, keyword) ORIENTATION ( GDL object message on CIRCLE, keyword) ORIENTATION ( GDL object message on CONE, keyword) ORIENTATION ( GDL object message on CONSTRAINED-ARC, keyword) ORIENTATION ( GDL object message on CONSTRAINED-LINE, keyword) ORIENTATION ( GDL object message on CYLINDER, keyword) ORIENTATION ( GDL object message on ELLIPSE, keyword) ORIENTATION ( GDL object message on GENERAL-NOTE, keyword) ORIENTATION ( GDL object message on GLOBAL-FILLETED-POLYGON-PROJECTION, keyword) ORIENTATION ( GDL object message on GLOBAL-FILLETED-POLYLINE, keyword) ORIENTATION ( GDL object message on GEOM-BASE::GLOBAL-FILLETED-POLYLINE-MIXIN, keyword) ORIENTATION ( GDL object message on GLOBAL-POLYGON-PROJECTION, keyword) ORIENTATION ( GDL object message on GLOBAL-POLYLINE, keyword) ORIENTATION ( GDL object message on GEOM-BASE::GLOBAL-POLYLINE-MIXIN, keyword) ORIENTATION ( GDL object message on HORIZONTAL-DIMENSION, keyword)
```

---

## index.html (chunk 132/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
object message on GLOBAL-POLYLINE, keyword) ORIENTATION ( GDL object message on GEOM-BASE::GLOBAL-POLYLINE-MIXIN, keyword) ORIENTATION ( GDL object message on HORIZONTAL-DIMENSION, keyword) ORIENTATION ( GDL object message on LABEL, keyword) ORIENTATION ( GDL object message on LEADER-LINE, keyword) ORIENTATION ( GDL object message on LINE, keyword) ORIENTATION ( GDL object message on LINEAR-DIMENSION, keyword) ORIENTATION ( GDL object message on PARALLEL-DIMENSION, keyword) ORIENTATION ( GDL object message on PIE-CHART, keyword) ORIENTATION ( GDL object message on POINT, keyword) ORIENTATION ( GDL object message on POINTS-DISPLAY, keyword) ORIENTATION ( GDL object message on ROUTE-PIPE, keyword) ORIENTATION ( GDL object message on SAMPLE-DRAWING, keyword) ORIENTATION ( GDL object message on SPHERE, keyword) ORIENTATION ( GDL object message on SPHERICAL-CAP, keyword) ORIENTATION ( GDL object message on TEXT-LINE, keyword) ORIENTATION ( GDL object message on TORUS, keyword) ORIENTATION
```

---

## index.html (chunk 133/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
on SPHERE, keyword) ORIENTATION ( GDL object message on SPHERICAL-CAP, keyword) ORIENTATION ( GDL object message on TEXT-LINE, keyword) ORIENTATION ( GDL object message on TORUS, keyword) ORIENTATION ( GDL object message on TYPESET-BLOCK, keyword) ORIENTATION ( GDL object message on VERTICAL-DIMENSION, keyword) ORIENTATION ( GDL object message on GWL:APPLICATION-MIXIN, keyword) ORIENTATION ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) ORIENTATION ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) ORIENTATION ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) ORIENTATION ( GDL object message on GWL::LAYOUT-MIXIN, keyword) ORIENTATION ( GDL object message on GWL:NODE-MIXIN, keyword) ORIENTATION ( GDL object message on GWL:WEB-DRAWING, keyword) orthogonal-component ( Function, geom-base) OTHER-RULES ( GDL object message on GWL:APPLICATION-MIXIN, keyword) OTHER-RULES ( GDL object message on GWL::LAYOUT-MIXIN, keyword) OTHER-RULES ( GDL object message on
```

---

## index.html (chunk 134/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
nent ( Function, geom-base) OTHER-RULES ( GDL object message on GWL:APPLICATION-MIXIN, keyword) OTHER-RULES ( GDL object message on GWL::LAYOUT-MIXIN, keyword) OTHER-RULES ( GDL object message on GWL:NODE-MIXIN, keyword) OTHER-RULES-BGCOLOR ( GDL object message on GWL:APPLICATION-MIXIN, keyword) OTHER-RULES-BGCOLOR ( GDL object message on GWL::LAYOUT-MIXIN, keyword) OTHER-RULES-BGCOLOR ( GDL object message on GWL:NODE-MIXIN, keyword) OTHER-RULES-TITLE ( GDL object message on GWL:APPLICATION-MIXIN, keyword) OTHER-RULES-TITLE ( GDL object message on GWL::LAYOUT-MIXIN, keyword) OTHER-RULES-TITLE ( GDL object message on GWL:NODE-MIXIN, keyword) OUTER-PIPE-RADIUS ( GDL object message on ROUTE-PIPE, keyword) OUTLINE-SHAPE-TYPE ( GDL object message on ANGULAR-DIMENSION, keyword) OUTLINE-SHAPE-TYPE ( GDL object message on GENERAL-NOTE, keyword) OUTLINE-SHAPE-TYPE ( GDL object message on HORIZONTAL-DIMENSION, keyword) OUTLINE-SHAPE-TYPE ( GDL object message on LABEL, keyword)
```

---

## index.html (chunk 135/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
E ( GDL object message on GENERAL-NOTE, keyword) OUTLINE-SHAPE-TYPE ( GDL object message on HORIZONTAL-DIMENSION, keyword) OUTLINE-SHAPE-TYPE ( GDL object message on LABEL, keyword) OUTLINE-SHAPE-TYPE ( GDL object message on LINEAR-DIMENSION, keyword) OUTLINE-SHAPE-TYPE ( GDL object message on PARALLEL-DIMENSION, keyword) OUTLINE-SHAPE-TYPE ( GDL object message on VERTICAL-DIMENSION, keyword) OUTSIDE-LEADERS-LENGTH-FACTOR ( GDL object message on ANGULAR-DIMENSION, keyword) OUTSIDE-LEADERS-LENGTH-FACTOR ( GDL object message on HORIZONTAL-DIMENSION, keyword) OUTSIDE-LEADERS-LENGTH-FACTOR ( GDL object message on LINEAR-DIMENSION, keyword) OUTSIDE-LEADERS-LENGTH-FACTOR ( GDL object message on PARALLEL-DIMENSION, keyword) OUTSIDE-LEADERS-LENGTH-FACTOR ( GDL object message on VERTICAL-DIMENSION, keyword) OUTSIDE-LEADERS? ( GDL object message on ANGULAR-DIMENSION, keyword) OUTSIDE-LEADERS? ( GDL object message on HORIZONTAL-DIMENSION, keyword) OUTSIDE-LEADERS? ( GDL object message on
```

---

## index.html (chunk 136/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
keyword) OUTSIDE-LEADERS? ( GDL object message on ANGULAR-DIMENSION, keyword) OUTSIDE-LEADERS? ( GDL object message on HORIZONTAL-DIMENSION, keyword) OUTSIDE-LEADERS? ( GDL object message on LINEAR-DIMENSION, keyword) OUTSIDE-LEADERS? ( GDL object message on PARALLEL-DIMENSION, keyword) OUTSIDE-LEADERS? ( GDL object message on VERTICAL-DIMENSION, keyword) OVERWRITE-VALIDATION-FUNCTION ( GDL object message on GDL-APP, keyword) OVERWRITE? ( GDL object message on GDL-APP, keyword) PACKAGE ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) PACKAGE-DOKUMENTATION ( Object, yadd) PACKAGE-DOKUMENTATIONS ( GDL object message on YADD:ASSEMBLY, keyword) PACKAGE-FORM ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) PACKAGE-FORM ( Object, yadd) PACKAGES-TO-IGNORE ( GDL object message on YADD:ASSEMBLY, keyword) PAGE-LENGTH ( GDL object message on BASE-DRAWING, keyword) PAGE-LENGTH ( GDL object message on SAMPLE-DRAWING, keyword) PAGE-LENGTH ( GDL object message on
```

---

## index.html (chunk 137/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
sage on YADD:ASSEMBLY, keyword) PAGE-LENGTH ( GDL object message on BASE-DRAWING, keyword) PAGE-LENGTH ( GDL object message on SAMPLE-DRAWING, keyword) PAGE-LENGTH ( GDL object message on GWL:WEB-DRAWING, keyword) PAGE-TITLE ( GDL object message on GWL:APPLICATION-MIXIN, keyword) PAGE-TITLE ( GDL object message on GWL::LAYOUT-MIXIN, keyword) PAGE-TITLE ( GDL object message on GWL:NODE-MIXIN, keyword) PAGE-WIDTH ( GDL object message on BASE-DRAWING, keyword) PAGE-WIDTH ( GDL object message on SAMPLE-DRAWING, keyword) PAGE-WIDTH ( GDL object message on GWL:WEB-DRAWING, keyword) PARALLEL-DIMENSION ( Object, geom-base) parallel-vectors? ( Function, geom-base) PARENT ( GDL object message on VANILLA-MIXIN*, keyword) PASSWORD? ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) PATH-POINTS ( GDL object message on LEADER-LINE, keyword) pi/2 ( Parameter or Constant, gendl) PIE-CHART ( Object, geom-base) PLACEHOLDER ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) PLACEHOLDER ( GDL
```

---

## index.html (chunk 138/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
GDL object message on LEADER-LINE, keyword) pi/2 ( Parameter or Constant, gendl) PIE-CHART ( Object, geom-base) PLACEHOLDER ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) PLACEHOLDER ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) PLACEHOLDER ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) PLACEHOLDER ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) PLACEHOLDER ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) plist-keys ( Function, gendl) plist-values ( Function, gendl) POINT ( GDL object message on BEZIER-CURVE, keyword) POINT ( Object, geom-base) POINT-ON-ARC ( GDL object message on ARC, keyword) point-on-plane? ( Function, geom-base) point-on-vector? ( Function, geom-base) POINTS ( GDL object message on POINTS-DISPLAY, keyword) POINTS-DISPLAY ( Object, geom-base) POSSIBLE-NIL? ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) POSSIBLE-NIL? ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) POSSIBLE-NIL? ( GDL object message
```

---

## index.html (chunk 139/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
( Object, geom-base) POSSIBLE-NIL? ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) POSSIBLE-NIL? ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) POSSIBLE-NIL? ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) POSSIBLE-NILS ( GDL object message on GEYSR::MENU, keyword) POSSIBLE-NILS ( GDL object message on GEYSR:TREE, keyword) POSSIBLE-NILS ( GDL object message on GWL:APPLICATION-MIXIN, keyword) POSSIBLE-NILS ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) POSSIBLE-NILS ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) POSSIBLE-NILS ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) POSSIBLE-NILS ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) POSSIBLE-NILS ( GDL object message on GWL:BASE-HTML-SHEET, keyword) POSSIBLE-NILS ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) POSSIBLE-NILS ( GDL object message on GWL::COLOR-MAP, keyword) POSSIBLE-NILS ( GDL object message on GWL:GRID-FORM-CONTROL, keyword)
```

---

## index.html (chunk 140/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) POSSIBLE-NILS ( GDL object message on GWL::COLOR-MAP, keyword) POSSIBLE-NILS ( GDL object message on GWL:GRID-FORM-CONTROL, keyword) POSSIBLE-NILS ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) POSSIBLE-NILS ( GDL object message on GWL::LAYOUT-MIXIN, keyword) POSSIBLE-NILS ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) POSSIBLE-NILS ( GDL object message on GWL:NODE-MIXIN, keyword) POSSIBLE-NILS ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) POSSIBLE-NILS ( GDL object message on GWL:SHEET-SECTION, keyword) POSSIBLE-NILS ( GDL object message on GWL::SKELETON-FORM-CONTROL, keyword) POSSIBLE-NILS ( GDL object message on GWL:SKELETON-UI-ELEMENT, keyword) POSSIBLE-NILS ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) POSSIBLE-NILS ( GDL object message on YADD:ASSEMBLY, keyword) POSSIBLE-NILS ( GDL object message on YADD::BASE-YADD-SHEET, keyword) POSSIBLE-NILS ( GDL object message on
```

---

## index.html (chunk 141/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
OL, keyword) POSSIBLE-NILS ( GDL object message on YADD:ASSEMBLY, keyword) POSSIBLE-NILS ( GDL object message on YADD::BASE-YADD-SHEET, keyword) POSSIBLE-NILS ( GDL object message on YADD::MASTER-INDEX, keyword) POSSIBLE-NILS ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) POSSIBLE-NILS ( GDL object message on YADD::PACKAGE-FORM, keyword) POST-LOAD-FORM ( GDL object message on GDL-APP, keyword) POST-MAKE-FUNCTION ( GDL object message on GDL-APP, keyword) PRE-LOAD-FORM ( GDL object message on GDL-APP, keyword) PRE-MAKE-FUNCTION ( GDL object message on GDL-APP, keyword) PRESET-ALL? ( GDL object message on GEYSR::MENU, keyword) PRESET-ALL? ( GDL object message on GEYSR:TREE, keyword) PRESET-ALL? ( GDL object message on GWL:APPLICATION-MIXIN, keyword) PRESET-ALL? ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) PRESET-ALL? ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) PRESET-ALL? ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) PRESET-ALL? ( GDL
```

---

## index.html (chunk 142/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
age on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) PRESET-ALL? ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) PRESET-ALL? ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) PRESET-ALL? ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) PRESET-ALL? ( GDL object message on GWL:BASE-HTML-SHEET, keyword) PRESET-ALL? ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) PRESET-ALL? ( GDL object message on GWL::COLOR-MAP, keyword) PRESET-ALL? ( GDL object message on GWL:GRID-FORM-CONTROL, keyword) PRESET-ALL? ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) PRESET-ALL? ( GDL object message on GWL::LAYOUT-MIXIN, keyword) PRESET-ALL? ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) PRESET-ALL? ( GDL object message on GWL:NODE-MIXIN, keyword) PRESET-ALL? ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) PRESET-ALL? ( GDL object message on GWL:SHEET-SECTION, keyword) PRESET-ALL? ( GDL object message on GWL::SKELETON-FORM-CONTROL, keyword) PRESET-ALL?
```

---

## index.html (chunk 143/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
bject message on GWL:RADIO-FORM-CONTROL, keyword) PRESET-ALL? ( GDL object message on GWL:SHEET-SECTION, keyword) PRESET-ALL? ( GDL object message on GWL::SKELETON-FORM-CONTROL, keyword) PRESET-ALL? ( GDL object message on GWL:SKELETON-UI-ELEMENT, keyword) PRESET-ALL? ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) PRESET-ALL? ( GDL object message on YADD:ASSEMBLY, keyword) PRESET-ALL? ( GDL object message on YADD::BASE-YADD-SHEET, keyword) PRESET-ALL? ( GDL object message on YADD::MASTER-INDEX, keyword) PRESET-ALL? ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) PRESET-ALL? ( GDL object message on YADD::PACKAGE-FORM, keyword) PRESET? ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) PRESET? ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) PRESET? ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) PRESET? ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) PRESET? ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) PREVIOUS ( GDL
```

---

## index.html (chunk 144/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
L object message on GWL:MENU-FORM-CONTROL, keyword) PRESET? ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) PRESET? ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) PREVIOUS ( GDL object message on VANILLA-MIXIN*, keyword) PRIMARY? ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) PRIMARY? ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) PRIMARY? ( GDL object message on GWL:GRID-FORM-CONTROL, keyword) PRIMARY? ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) PRIMARY? ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) PRIMARY? ( GDL object message on GWL::SKELETON-FORM-CONTROL, keyword) PRIMARY? ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) print-characters ( Function, geom-base) print-messages ( Macro, gendl) print-variables ( Macro, gendl) PROCESS-COOKIES! ( GDL object message on GWL:APPLICATION-MIXIN, keyword) PROCESS-COOKIES! ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) PROCESS-COOKIES! ( GDL object message
```

---

## index.html (chunk 145/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
gendl) PROCESS-COOKIES! ( GDL object message on GWL:APPLICATION-MIXIN, keyword) PROCESS-COOKIES! ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) PROCESS-COOKIES! ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) PROCESS-COOKIES! ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) PROCESS-COOKIES! ( GDL object message on GWL:BASE-HTML-SHEET, keyword) PROCESS-COOKIES! ( GDL object message on GWL::COLOR-MAP, keyword) PROCESS-COOKIES! ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) PROCESS-COOKIES! ( GDL object message on GWL::LAYOUT-MIXIN, keyword) PROCESS-COOKIES! ( GDL object message on GWL:NODE-MIXIN, keyword) PROCESS-COOKIES! ( GDL object message on YADD:ASSEMBLY, keyword) PROCESS-COOKIES! ( GDL object message on YADD::BASE-YADD-SHEET, keyword) PROCESS-COOKIES! ( GDL object message on YADD::MASTER-INDEX, keyword) PROCESS-COOKIES! ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) PROCESS-COOKIES! ( GDL object message on
```

---

## index.html (chunk 146/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
COOKIES! ( GDL object message on YADD::MASTER-INDEX, keyword) PROCESS-COOKIES! ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) PROCESS-COOKIES! ( GDL object message on YADD::PACKAGE-FORM, keyword) proj-point-on-line ( Function, geom-base) projected-vector ( Function, geom-base) PROJECTION-DEPTH ( GDL object message on GLOBAL-FILLETED-POLYGON-PROJECTION, keyword) PROJECTION-DEPTH ( GDL object message on GLOBAL-POLYGON-PROJECTION, keyword) PROJECTION-VECTOR ( GDL object message on BASE-VIEW, keyword) PROJECTION-VECTOR ( GDL object message on GLOBAL-FILLETED-POLYGON-PROJECTION, keyword) PROJECTION-VECTOR ( GDL object message on GLOBAL-POLYGON-PROJECTION, keyword) PROJECTION-VECTOR ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) PROJECTION-VECTOR ( GDL object message on GWL:WEB-DRAWING, keyword) PROMPT ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) PROMPT ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) PROMPT ( GDL object message on
```

---

## index.html (chunk 147/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
e on GWL:WEB-DRAWING, keyword) PROMPT ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) PROMPT ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) PROMPT ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) PROMPT ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) PROMPT ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) publish-gwl-app ( Function, gwl) publish-shared ( Function, gwl) publish-string-content ( Function, gwl) pythagorize ( Function, geom-base) QUANTIFICATION ( Object, gendl) quaternion-to-matrix ( Function, geom-base) quaternion-to-rotation ( Function, geom-base) QUERY-PLIST ( GDL object message on GWL:BASE-HTML-SHEET, keyword) RADIAL-SEQUENCE ( Object, gendl) radians-to-degrees ( Function, geom-base) radians-to-grads ( Function, geom-base) RADIO-FORM-CONTROL ( Object, gwl) RADIUS ( GDL object message on ARC, keyword) RADIUS ( GDL object message on GEOM-BASE::ARCOID-MIXIN, keyword) RADIUS ( GDL object message on C-CYLINDER, keyword) RADIUS (
```

---

## index.html (chunk 148/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
FORM-CONTROL ( Object, gwl) RADIUS ( GDL object message on ARC, keyword) RADIUS ( GDL object message on GEOM-BASE::ARCOID-MIXIN, keyword) RADIUS ( GDL object message on C-CYLINDER, keyword) RADIUS ( GDL object message on CIRCLE, keyword) RADIUS ( GDL object message on CONE, keyword) RADIUS ( GDL object message on CONSTRAINED-ARC, keyword) RADIUS ( GDL object message on CYLINDER, keyword) RADIUS ( GDL object message on ELLIPSE, keyword) RADIUS ( GDL object message on PIE-CHART, keyword) RADIUS ( GDL object message on POINT, keyword) RADIUS ( GDL object message on SPHERE, keyword) RADIUS ( GDL object message on SPHERICAL-CAP, keyword) RADIUS ( GDL object message on TORUS, keyword) RADIUS-1 ( GDL object message on CONE, keyword) RADIUS-2 ( GDL object message on CONE, keyword) RADIUS-LIST ( GDL object message on GLOBAL-FILLETED-POLYGON-PROJECTION, keyword) RADIUS-LIST ( GDL object message on GLOBAL-FILLETED-POLYLINE, keyword) RADIUS-LIST ( GDL object message on
```

---

## index.html (chunk 149/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
L object message on GLOBAL-FILLETED-POLYGON-PROJECTION, keyword) RADIUS-LIST ( GDL object message on GLOBAL-FILLETED-POLYLINE, keyword) RADIUS-LIST ( GDL object message on GEOM-BASE::GLOBAL-FILLETED-POLYLINE-MIXIN, keyword) RADIUS-LIST ( GDL object message on ROUTE-PIPE, keyword) RAPHAEL-CANVAS-ID ( GDL object message on GWL:WEB-DRAWING, keyword) RASTER-GRAPHICS ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) read-safe-string ( Function, gendl) READ-SAVED-SLOTS ( GDL object message on GWL::LAYOUT-MIXIN, keyword) read-snapshot ( Function, gendl) READONLY? ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) READONLY? ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) READONLY? ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) READONLY? ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) READONLY? ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) RECOVERY-EXPIRES-AT ( GDL object message on GWL:SESSION-CONTROL-MIXIN, keyword) RECOVERY-URL ( GDL
```

---

## index.html (chunk 150/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
n GWL:RADIO-FORM-CONTROL, keyword) READONLY? ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) RECOVERY-EXPIRES-AT ( GDL object message on GWL:SESSION-CONTROL-MIXIN, keyword) RECOVERY-URL ( GDL object message on GWL:SESSION-CONTROL-MIXIN, keyword) relativize-pathname ( Function, gwl) remove-plist-entry ( Function, gendl) RENDERER-MIXIN ( Object, geom-base) replace-substring ( Function, gendl) REPORT-POINT ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) RESET! ( GDL object message on GENDL::VARIABLE-SEQUENCE, keyword) RESPONDENT ( GDL object message on GEYSR::MENU, keyword) RESPONDENT ( GDL object message on GEYSR:TREE, keyword) RESPONDENT ( GDL object message on GWL:APPLICATION-MIXIN, keyword) RESPONDENT ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) RESPONDENT ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) RESPONDENT ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) RESPONDENT ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET,
```

---

## index.html (chunk 151/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
SPONDENT ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) RESPONDENT ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) RESPONDENT ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) RESPONDENT ( GDL object message on GWL:BASE-HTML-SHEET, keyword) RESPONDENT ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) RESPONDENT ( GDL object message on GWL::COLOR-MAP, keyword) RESPONDENT ( GDL object message on GWL:GRID-FORM-CONTROL, keyword) RESPONDENT ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) RESPONDENT ( GDL object message on GWL::LAYOUT-MIXIN, keyword) RESPONDENT ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) RESPONDENT ( GDL object message on GWL:NODE-MIXIN, keyword) RESPONDENT ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) RESPONDENT ( GDL object message on GWL:SHEET-SECTION, keyword) RESPONDENT ( GDL object message on GWL::SKELETON-FORM-CONTROL, keyword) RESPONDENT ( GDL object message on GWL:SKELETON-UI-ELEMENT, keyword)
```

---

## index.html (chunk 152/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
( GDL object message on GWL:SHEET-SECTION, keyword) RESPONDENT ( GDL object message on GWL::SKELETON-FORM-CONTROL, keyword) RESPONDENT ( GDL object message on GWL:SKELETON-UI-ELEMENT, keyword) RESPONDENT ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) RESPONDENT ( GDL object message on YADD:ASSEMBLY, keyword) RESPONDENT ( GDL object message on YADD::BASE-YADD-SHEET, keyword) RESPONDENT ( GDL object message on YADD::MASTER-INDEX, keyword) RESPONDENT ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) RESPONDENT ( GDL object message on YADD::PACKAGE-FORM, keyword) RESTART-APP-FUNCTION ( GDL object message on GDL-APP, keyword) RESTART-INIT-FUNCTION ( GDL object message on GDL-APP, keyword) RESTORE-ALL-DEFAULTS! ( GDL object message on VANILLA-MIXIN*, keyword) RESTORE-DEFAULTS! ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) RESTORE-FORM-CONTROLS! ( GDL object message on GWL:BASE-HTML-SHEET, keyword) RESTORE-ROOT! ( GDL object message on VANILLA-MIXIN*, keyword)
```

---

## index.html (chunk 153/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
S! ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) RESTORE-FORM-CONTROLS! ( GDL object message on GWL:BASE-HTML-SHEET, keyword) RESTORE-ROOT! ( GDL object message on VANILLA-MIXIN*, keyword) RESTORE-SLOT-DEFAULT! ( GDL object message on VANILLA-MIXIN*, keyword) RESTORE-SLOT-DEFAULTS! ( GDL object message on VANILLA-MIXIN*, keyword) RESTORE-TREE! ( GDL object message on VANILLA-MIXIN*, keyword) RETURN-OBJECT ( GDL object message on GWL:APPLICATION-MIXIN, keyword) RETURN-OBJECT ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) RETURN-OBJECT ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) RETURN-OBJECT ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) RETURN-OBJECT ( GDL object message on GWL:BASE-HTML-SHEET, keyword) RETURN-OBJECT ( GDL object message on GWL::COLOR-MAP, keyword) RETURN-OBJECT ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) RETURN-OBJECT ( GDL object message on GWL::LAYOUT-MIXIN, keyword) RETURN-OBJECT ( GDL object message on
```

---

## index.html (chunk 154/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
on GWL::COLOR-MAP, keyword) RETURN-OBJECT ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) RETURN-OBJECT ( GDL object message on GWL::LAYOUT-MIXIN, keyword) RETURN-OBJECT ( GDL object message on GWL:NODE-MIXIN, keyword) RETURN-OBJECT ( GDL object message on YADD:ASSEMBLY, keyword) RETURN-OBJECT ( GDL object message on YADD::BASE-YADD-SHEET, keyword) RETURN-OBJECT ( GDL object message on YADD::MASTER-INDEX, keyword) RETURN-OBJECT ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) RETURN-OBJECT ( GDL object message on YADD::PACKAGE-FORM, keyword) reverse-vector ( Function, geom-base) roll ( Macro, geom-base) ROOT ( GDL object message on CL-LITE:CODEBASE-DIRECTORY-NODE, keyword) ROOT ( GDL object message on BASE-RULE-OBJECT, keyword) ROOT ( GDL object message on GDL-APP, keyword) ROOT ( GDL object message on GENDL::MATRIX-SEQUENCE, keyword) ROOT ( GDL object message on NULL-OBJECT, keyword) ROOT ( GDL object message on QUANTIFICATION, keyword) ROOT ( GDL object message on
```

---

## index.html (chunk 155/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
d) ROOT ( GDL object message on GENDL::MATRIX-SEQUENCE, keyword) ROOT ( GDL object message on NULL-OBJECT, keyword) ROOT ( GDL object message on QUANTIFICATION, keyword) ROOT ( GDL object message on GENDL::RADIAL-SEQUENCE, keyword) ROOT ( GDL object message on GENDL::STANDARD-SEQUENCE, keyword) ROOT ( GDL object message on VANILLA-MIXIN*, keyword) ROOT ( GDL object message on GENDL::VARIABLE-SEQUENCE, keyword) ROOT ( GDL object message on ANGULAR-DIMENSION, keyword) ROOT ( GDL object message on ARC, keyword) ROOT ( GDL object message on GEOM-BASE::ARCOID-MIXIN, keyword) ROOT ( GDL object message on BASE-COORDINATE-SYSTEM, keyword) ROOT ( GDL object message on BASE-DRAWING, keyword) ROOT ( GDL object message on BASE-OBJECT, keyword) ROOT ( GDL object message on BASE-VIEW, keyword) ROOT ( GDL object message on BEZIER-CURVE, keyword) ROOT ( GDL object message on BOX, keyword) ROOT ( GDL object message on C-CYLINDER, keyword) ROOT ( GDL object message on CENTER-LINE, keyword) ROOT ( GDL
```

---

## index.html (chunk 156/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
L object message on BEZIER-CURVE, keyword) ROOT ( GDL object message on BOX, keyword) ROOT ( GDL object message on C-CYLINDER, keyword) ROOT ( GDL object message on CENTER-LINE, keyword) ROOT ( GDL object message on CIRCLE, keyword) ROOT ( GDL object message on CONE, keyword) ROOT ( GDL object message on CONSTRAINED-ARC, keyword) ROOT ( GDL object message on CONSTRAINED-FILLET, keyword) ROOT ( GDL object message on CONSTRAINED-LINE, keyword) ROOT ( GDL object message on CYLINDER, keyword) ROOT ( GDL object message on ELLIPSE, keyword) ROOT ( GDL object message on GENERAL-NOTE, keyword) ROOT ( GDL object message on GLOBAL-FILLETED-POLYGON-PROJECTION, keyword) ROOT ( GDL object message on GLOBAL-FILLETED-POLYLINE, keyword) ROOT ( GDL object message on GEOM-BASE::GLOBAL-FILLETED-POLYLINE-MIXIN, keyword) ROOT ( GDL object message on GLOBAL-POLYGON-PROJECTION, keyword) ROOT ( GDL object message on GLOBAL-POLYLINE, keyword) ROOT ( GDL object message on GEOM-BASE::GLOBAL-POLYLINE-MIXIN,
```

---

## index.html (chunk 157/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
N, keyword) ROOT ( GDL object message on GLOBAL-POLYGON-PROJECTION, keyword) ROOT ( GDL object message on GLOBAL-POLYLINE, keyword) ROOT ( GDL object message on GEOM-BASE::GLOBAL-POLYLINE-MIXIN, keyword) ROOT ( GDL object message on HORIZONTAL-DIMENSION, keyword) ROOT ( GDL object message on LABEL, keyword) ROOT ( GDL object message on LEADER-LINE, keyword) ROOT ( GDL object message on LINE, keyword) ROOT ( GDL object message on LINEAR-DIMENSION, keyword) ROOT ( GDL object message on PARALLEL-DIMENSION, keyword) ROOT ( GDL object message on PIE-CHART, keyword) ROOT ( GDL object message on POINT, keyword) ROOT ( GDL object message on POINTS-DISPLAY, keyword) ROOT ( GDL object message on GEOM-BASE::RENDERER-MIXIN, keyword) ROOT ( GDL object message on ROUTE-PIPE, keyword) ROOT ( GDL object message on SAMPLE-DRAWING, keyword) ROOT ( GDL object message on SPHERE, keyword) ROOT ( GDL object message on SPHERICAL-CAP, keyword) ROOT ( GDL object message on TEXT-LINE, keyword) ROOT ( GDL
```

---

## index.html (chunk 158/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
essage on SAMPLE-DRAWING, keyword) ROOT ( GDL object message on SPHERE, keyword) ROOT ( GDL object message on SPHERICAL-CAP, keyword) ROOT ( GDL object message on TEXT-LINE, keyword) ROOT ( GDL object message on TORUS, keyword) ROOT ( GDL object message on TYPESET-BLOCK, keyword) ROOT ( GDL object message on VERTICAL-DIMENSION, keyword) ROOT ( GDL object message on GEYSR::MENU, keyword) ROOT ( GDL object message on GEYSR:TREE, keyword) ROOT ( GDL object message on GWL:APPLICATION-MIXIN, keyword) ROOT ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) ROOT ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) ROOT ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) ROOT ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) ROOT ( GDL object message on GWL:BASE-HTML-SHEET, keyword) ROOT ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) ROOT ( GDL object message on GWL::COLOR-MAP, keyword) ROOT ( GDL object message on GWL::GEOMETRY-VIEW-MIXIN, keyword)
```

---

## index.html (chunk 159/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
SHEET, keyword) ROOT ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) ROOT ( GDL object message on GWL::COLOR-MAP, keyword) ROOT ( GDL object message on GWL::GEOMETRY-VIEW-MIXIN, keyword) ROOT ( GDL object message on GWL:GRID-FORM-CONTROL, keyword) ROOT ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) ROOT ( GDL object message on GWL::LAYOUT-MIXIN, keyword) ROOT ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) ROOT ( GDL object message on GWL:NODE-MIXIN, keyword) ROOT ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) ROOT ( GDL object message on GWL:SESSION-CONTROL-MIXIN, keyword) ROOT ( GDL object message on GWL:SHEET-SECTION, keyword) ROOT ( GDL object message on GWL::SKELETON-FORM-CONTROL, keyword) ROOT ( GDL object message on GWL:SKELETON-UI-ELEMENT, keyword) ROOT ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) ROOT ( GDL object message on GWL:WEB-DRAWING, keyword) ROOT ( GDL object message on YADD:ASSEMBLY, keyword) ROOT ( GDL object message
```

---

## index.html (chunk 160/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
rd) ROOT ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) ROOT ( GDL object message on GWL:WEB-DRAWING, keyword) ROOT ( GDL object message on YADD:ASSEMBLY, keyword) ROOT ( GDL object message on YADD::BASE-YADD-SHEET, keyword) ROOT ( GDL object message on YADD::MASTER-INDEX, keyword) ROOT ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) ROOT ( GDL object message on YADD::PACKAGE-FORM, keyword) ROOT-PATH ( GDL object message on VANILLA-MIXIN*, keyword) ROOT-PATH-LOCAL ( GDL object message on VANILLA-MIXIN*, keyword) ROOT? ( GDL object message on VANILLA-MIXIN*, keyword) rotate-point ( Function, geom-base) rotate-point-d ( Function, geom-base) rotate-vector ( Function, geom-base) rotate-vector-d ( Function, geom-base) rotation ( Function, geom-base) round-to-nearest ( Function, gendl) ROUTE-PIPE ( Object, geom-base) ROW-LABELS ( GDL object message on GWL:GRID-FORM-CONTROL, keyword) ROWS ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) RULE-DESCRIPTION ( GDL
```

---

## index.html (chunk 161/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
tion, gendl) ROUTE-PIPE ( Object, geom-base) ROW-LABELS ( GDL object message on GWL:GRID-FORM-CONTROL, keyword) ROWS ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) RULE-DESCRIPTION ( GDL object message on BASE-RULE-OBJECT, keyword) RULE-DESCRIPTION ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) RULE-DESCRIPTION-HELP ( GDL object message on BASE-RULE-OBJECT, keyword) RULE-DESCRIPTION-HELP ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) RULE-RESULT ( GDL object message on BASE-RULE-OBJECT, keyword) RULE-RESULT ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) RULE-RESULT-HELP ( GDL object message on BASE-RULE-OBJECT, keyword) RULE-RESULT-HELP ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) RULE-TITLE ( GDL object message on BASE-RULE-OBJECT, keyword) RULE-TITLE ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) SAFE-CHILDREN ( GDL object message on CL-LITE:CODEBASE-DIRECTORY-NODE, keyword) SAFE-CHILDREN ( GDL object message on BASE-RULE-OBJECT,
```

---

## index.html (chunk 162/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
E ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) SAFE-CHILDREN ( GDL object message on CL-LITE:CODEBASE-DIRECTORY-NODE, keyword) SAFE-CHILDREN ( GDL object message on BASE-RULE-OBJECT, keyword) SAFE-CHILDREN ( GDL object message on GDL-APP, keyword) SAFE-CHILDREN ( GDL object message on GENDL::MATRIX-SEQUENCE, keyword) SAFE-CHILDREN ( GDL object message on NULL-OBJECT, keyword) SAFE-CHILDREN ( GDL object message on QUANTIFICATION, keyword) SAFE-CHILDREN ( GDL object message on GENDL::RADIAL-SEQUENCE, keyword) SAFE-CHILDREN ( GDL object message on GENDL::STANDARD-SEQUENCE, keyword) SAFE-CHILDREN ( GDL object message on VANILLA-MIXIN*, keyword) SAFE-CHILDREN ( GDL object message on GENDL::VARIABLE-SEQUENCE, keyword) SAFE-CHILDREN ( GDL object message on ANGULAR-DIMENSION, keyword) SAFE-CHILDREN ( GDL object message on ARC, keyword) SAFE-CHILDREN ( GDL object message on GEOM-BASE::ARCOID-MIXIN, keyword) SAFE-CHILDREN ( GDL object message on BASE-COORDINATE-SYSTEM, keyword)
```

---

## index.html (chunk 163/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
E-CHILDREN ( GDL object message on ARC, keyword) SAFE-CHILDREN ( GDL object message on GEOM-BASE::ARCOID-MIXIN, keyword) SAFE-CHILDREN ( GDL object message on BASE-COORDINATE-SYSTEM, keyword) SAFE-CHILDREN ( GDL object message on BASE-DRAWING, keyword) SAFE-CHILDREN ( GDL object message on BASE-OBJECT, keyword) SAFE-CHILDREN ( GDL object message on BASE-VIEW, keyword) SAFE-CHILDREN ( GDL object message on BEZIER-CURVE, keyword) SAFE-CHILDREN ( GDL object message on BOX, keyword) SAFE-CHILDREN ( GDL object message on C-CYLINDER, keyword) SAFE-CHILDREN ( GDL object message on CENTER-LINE, keyword) SAFE-CHILDREN ( GDL object message on CIRCLE, keyword) SAFE-CHILDREN ( GDL object message on CONE, keyword) SAFE-CHILDREN ( GDL object message on CONSTRAINED-ARC, keyword) SAFE-CHILDREN ( GDL object message on CONSTRAINED-FILLET, keyword) SAFE-CHILDREN ( GDL object message on CONSTRAINED-LINE, keyword) SAFE-CHILDREN ( GDL object message on CYLINDER, keyword) SAFE-CHILDREN ( GDL object message
```

---

## index.html (chunk 164/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
t message on CONSTRAINED-FILLET, keyword) SAFE-CHILDREN ( GDL object message on CONSTRAINED-LINE, keyword) SAFE-CHILDREN ( GDL object message on CYLINDER, keyword) SAFE-CHILDREN ( GDL object message on ELLIPSE, keyword) SAFE-CHILDREN ( GDL object message on GENERAL-NOTE, keyword) SAFE-CHILDREN ( GDL object message on GLOBAL-FILLETED-POLYGON-PROJECTION, keyword) SAFE-CHILDREN ( GDL object message on GLOBAL-FILLETED-POLYLINE, keyword) SAFE-CHILDREN ( GDL object message on GEOM-BASE::GLOBAL-FILLETED-POLYLINE-MIXIN, keyword) SAFE-CHILDREN ( GDL object message on GLOBAL-POLYGON-PROJECTION, keyword) SAFE-CHILDREN ( GDL object message on GLOBAL-POLYLINE, keyword) SAFE-CHILDREN ( GDL object message on GEOM-BASE::GLOBAL-POLYLINE-MIXIN, keyword) SAFE-CHILDREN ( GDL object message on HORIZONTAL-DIMENSION, keyword) SAFE-CHILDREN ( GDL object message on LABEL, keyword) SAFE-CHILDREN ( GDL object message on LEADER-LINE, keyword) SAFE-CHILDREN ( GDL object message on LINE, keyword) SAFE-CHILDREN (
```

---

## index.html (chunk 165/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
SION, keyword) SAFE-CHILDREN ( GDL object message on LABEL, keyword) SAFE-CHILDREN ( GDL object message on LEADER-LINE, keyword) SAFE-CHILDREN ( GDL object message on LINE, keyword) SAFE-CHILDREN ( GDL object message on LINEAR-DIMENSION, keyword) SAFE-CHILDREN ( GDL object message on PARALLEL-DIMENSION, keyword) SAFE-CHILDREN ( GDL object message on PIE-CHART, keyword) SAFE-CHILDREN ( GDL object message on POINT, keyword) SAFE-CHILDREN ( GDL object message on POINTS-DISPLAY, keyword) SAFE-CHILDREN ( GDL object message on GEOM-BASE::RENDERER-MIXIN, keyword) SAFE-CHILDREN ( GDL object message on ROUTE-PIPE, keyword) SAFE-CHILDREN ( GDL object message on SAMPLE-DRAWING, keyword) SAFE-CHILDREN ( GDL object message on SPHERE, keyword) SAFE-CHILDREN ( GDL object message on SPHERICAL-CAP, keyword) SAFE-CHILDREN ( GDL object message on TEXT-LINE, keyword) SAFE-CHILDREN ( GDL object message on TORUS, keyword) SAFE-CHILDREN ( GDL object message on TYPESET-BLOCK, keyword) SAFE-CHILDREN ( GDL
```

---

## index.html (chunk 166/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
) SAFE-CHILDREN ( GDL object message on TEXT-LINE, keyword) SAFE-CHILDREN ( GDL object message on TORUS, keyword) SAFE-CHILDREN ( GDL object message on TYPESET-BLOCK, keyword) SAFE-CHILDREN ( GDL object message on VERTICAL-DIMENSION, keyword) SAFE-CHILDREN ( GDL object message on GEYSR::MENU, keyword) SAFE-CHILDREN ( GDL object message on GEYSR:TREE, keyword) SAFE-CHILDREN ( GDL object message on GWL:APPLICATION-MIXIN, keyword) SAFE-CHILDREN ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) SAFE-CHILDREN ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) SAFE-CHILDREN ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) SAFE-CHILDREN ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) SAFE-CHILDREN ( GDL object message on GWL:BASE-HTML-SHEET, keyword) SAFE-CHILDREN ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) SAFE-CHILDREN ( GDL object message on GWL::COLOR-MAP, keyword) SAFE-CHILDREN ( GDL object message on GWL::GEOMETRY-VIEW-MIXIN,
```

---

## index.html (chunk 167/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
LDREN ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) SAFE-CHILDREN ( GDL object message on GWL::COLOR-MAP, keyword) SAFE-CHILDREN ( GDL object message on GWL::GEOMETRY-VIEW-MIXIN, keyword) SAFE-CHILDREN ( GDL object message on GWL:GRID-FORM-CONTROL, keyword) SAFE-CHILDREN ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) SAFE-CHILDREN ( GDL object message on GWL::LAYOUT-MIXIN, keyword) SAFE-CHILDREN ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) SAFE-CHILDREN ( GDL object message on GWL:NODE-MIXIN, keyword) SAFE-CHILDREN ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) SAFE-CHILDREN ( GDL object message on GWL:SESSION-CONTROL-MIXIN, keyword) SAFE-CHILDREN ( GDL object message on GWL:SHEET-SECTION, keyword) SAFE-CHILDREN ( GDL object message on GWL::SKELETON-FORM-CONTROL, keyword) SAFE-CHILDREN ( GDL object message on GWL:SKELETON-UI-ELEMENT, keyword) SAFE-CHILDREN ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) SAFE-CHILDREN ( GDL object
```

---

## index.html (chunk 168/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
N-FORM-CONTROL, keyword) SAFE-CHILDREN ( GDL object message on GWL:SKELETON-UI-ELEMENT, keyword) SAFE-CHILDREN ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) SAFE-CHILDREN ( GDL object message on GWL:WEB-DRAWING, keyword) SAFE-CHILDREN ( GDL object message on YADD:ASSEMBLY, keyword) SAFE-CHILDREN ( GDL object message on YADD::BASE-YADD-SHEET, keyword) SAFE-CHILDREN ( GDL object message on YADD::MASTER-INDEX, keyword) SAFE-CHILDREN ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) SAFE-CHILDREN ( GDL object message on YADD::PACKAGE-FORM, keyword) safe-float ( Function, gendl) SAFE-HIDDEN-CHILDREN ( GDL object message on VANILLA-MIXIN*, keyword) safe-sort ( Function, gendl) same-direction-vectors? ( Function, geom-base) SAMPLE-DRAWING ( Object, geom-base) SANITY-ERROR ( GDL object message on GWL:BASE-HTML-SHEET, keyword) SAVED-SLOTS ( GDL object message on GWL::LAYOUT-MIXIN, keyword) scalar*matrix ( Function, geom-base) scalar*vector ( Function, geom-base) SCALED? (
```

---

## index.html (chunk 169/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
DL object message on GWL:BASE-HTML-SHEET, keyword) SAVED-SLOTS ( GDL object message on GWL::LAYOUT-MIXIN, keyword) scalar*matrix ( Function, geom-base) scalar*vector ( Function, geom-base) SCALED? ( GDL object message on POINT, keyword) SELECT-CHOICES ( GDL object message on GWL:BASE-HTML-SHEET, keyword) SESSION-CLEAN-UP ( GDL object message on GWL:SESSION-CONTROL-MIXIN, keyword) session-control-auto-refresh ( Function, gwl) SESSION-CONTROL-MIXIN ( Object, gwl) SESSION-DURATION ( GDL object message on GWL:SESSION-CONTROL-MIXIN, keyword) session-report ( Function, gwl) SET-EXPIRES-AT ( GDL object message on GWL:SESSION-CONTROL-MIXIN, keyword) set-format-slot ( Macro, gendl) SET-SLOT! ( GDL object message on VANILLA-MIXIN*, keyword) SET-SLOTS! ( GDL object message on VANILLA-MIXIN*, keyword) SHEET-SECTION ( Object, gwl) SHORT-SEGMENT-LENGTH ( GDL object message on CENTER-LINE, keyword) SHOW-SUPPORTED-FLAG ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) SHOW-TITLE? ( GDL
```

---

## index.html (chunk 170/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
SHEET-SECTION ( Object, gwl) SHORT-SEGMENT-LENGTH ( GDL object message on CENTER-LINE, keyword) SHOW-SUPPORTED-FLAG ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) SHOW-TITLE? ( GDL object message on GWL:APPLICATION-MIXIN, keyword) SHOW-TITLE? ( GDL object message on GWL::LAYOUT-MIXIN, keyword) SHOW-TITLE? ( GDL object message on GWL:NODE-MIXIN, keyword) SIZE ( GDL object message on CENTER-LINE, keyword) SIZE ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) SIZE ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) SIZE ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) SIZE ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) SIZE ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) SKELETON-FORM-CONTROL ( Object, gwl) SKELETON-UI-ELEMENT ( Object, gwl) SLOT-DOCUMENTATION ( GDL object message on VANILLA-MIXIN*, keyword) SLOT-SOURCE ( GDL object message on VANILLA-MIXIN*, keyword) SLOT-STATUS ( GDL object message on VANILLA-MIXIN*, keyword)
```

---

## index.html (chunk 171/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
wl) SLOT-DOCUMENTATION ( GDL object message on VANILLA-MIXIN*, keyword) SLOT-SOURCE ( GDL object message on VANILLA-MIXIN*, keyword) SLOT-STATUS ( GDL object message on VANILLA-MIXIN*, keyword) SNAP-TO ( GDL object message on BASE-VIEW, keyword) sort-points-along-vector ( Function, geom-base) SOURCE-FILES-TO-IGNORE ( GDL object message on CL-LITE:CODEBASE-DIRECTORY-NODE, keyword) SPECIAL-SUBDIR-NAMES ( GDL object message on CL-LITE:CODEBASE-DIRECTORY-NODE, keyword) SPHERE ( Object, geom-base) SPHERE-CENTER ( GDL object message on SPHERICAL-CAP, keyword) SPHERE-RADIUS ( GDL object message on SPHERICAL-CAP, keyword) SPHERICAL-CAP ( Object, geom-base) split ( Function, gendl) SRC ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) SRC ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) SRC ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) SRC ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) SRC ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword)
```

---

## index.html (chunk 172/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
word) SRC ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) SRC ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) SRC ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) STANDARD-SAVED-SLOTS ( GDL object message on GWL::LAYOUT-MIXIN, keyword) STANDARD-SEQUENCE ( Object, gendl) STANDARD-VIEWS ( GDL object message on GWL:APPLICATION-MIXIN, keyword) STANDARD-VIEWS ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) STANDARD-VIEWS ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) STANDARD-VIEWS ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) STANDARD-VIEWS ( GDL object message on GWL::LAYOUT-MIXIN, keyword) STANDARD-VIEWS ( GDL object message on GWL:NODE-MIXIN, keyword) START ( GDL object message on ARC, keyword) START ( GDL object message on C-CYLINDER, keyword) START ( GDL object message on CONSTRAINED-LINE, keyword) START ( GDL object message on CYLINDER, keyword) START ( GDL object message on GENERAL-NOTE, keyword) START ( GDL object
```

---

## index.html (chunk 173/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
LINDER, keyword) START ( GDL object message on CONSTRAINED-LINE, keyword) START ( GDL object message on CYLINDER, keyword) START ( GDL object message on GENERAL-NOTE, keyword) START ( GDL object message on LINE, keyword) START ( GDL object message on TEXT-LINE, keyword) START ( GDL object message on TYPESET-BLOCK, keyword) START-ANGLE ( GDL object message on ARC, keyword) START-ANGLE ( GDL object message on GEOM-BASE::ARCOID-MIXIN, keyword) START-ANGLE ( GDL object message on C-CYLINDER, keyword) START-ANGLE ( GDL object message on CIRCLE, keyword) START-ANGLE ( GDL object message on CONE, keyword) START-ANGLE ( GDL object message on CONSTRAINED-ARC, keyword) START-ANGLE ( GDL object message on CONSTRAINED-FILLET, keyword) START-ANGLE ( GDL object message on CYLINDER, keyword) START-ANGLE ( GDL object message on ELLIPSE, keyword) START-ANGLE ( GDL object message on POINT, keyword) START-ANGLE ( GDL object message on SPHERE, keyword) START-ANGLE ( GDL object message on SPHERICAL-CAP,
```

---

## index.html (chunk 174/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
( GDL object message on ELLIPSE, keyword) START-ANGLE ( GDL object message on POINT, keyword) START-ANGLE ( GDL object message on SPHERE, keyword) START-ANGLE ( GDL object message on SPHERICAL-CAP, keyword) START-ANGLE ( GDL object message on TORUS, keyword) START-HORIZONTAL-ARC ( GDL object message on POINT, keyword) START-HORIZONTAL-ARC ( GDL object message on SPHERE, keyword) START-LINE-INDEX ( GDL object message on TYPESET-BLOCK, keyword) START-POINT ( GDL object message on ANGULAR-DIMENSION, keyword) START-POINT ( GDL object message on HORIZONTAL-DIMENSION, keyword) START-POINT ( GDL object message on LINEAR-DIMENSION, keyword) START-POINT ( GDL object message on PARALLEL-DIMENSION, keyword) START-POINT ( GDL object message on VERTICAL-DIMENSION, keyword) START-VERTICAL-ARC ( GDL object message on POINT, keyword) START-VERTICAL-ARC ( GDL object message on SPHERE, keyword) status-message ( Function, gendl) STRAIGHTS ( GDL object message on
```

---

## index.html (chunk 175/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
ct message on POINT, keyword) START-VERTICAL-ARC ( GDL object message on SPHERE, keyword) status-message ( Function, gendl) STRAIGHTS ( GDL object message on GEOM-BASE::GLOBAL-FILLETED-POLYLINE-MIXIN, keyword) string-append ( Function, gendl) STRINGS ( GDL object message on GENERAL-NOTE, keyword) STRINGS ( GDL object message on LABEL, keyword) STRINGS-FOR-DISPLAY ( GDL object message on CL-LITE:CODEBASE-DIRECTORY-NODE, keyword) STRINGS-FOR-DISPLAY ( GDL object message on BASE-RULE-OBJECT, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GDL-APP, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GENDL::MATRIX-SEQUENCE, keyword) STRINGS-FOR-DISPLAY ( GDL object message on NULL-OBJECT, keyword) STRINGS-FOR-DISPLAY ( GDL object message on QUANTIFICATION, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GENDL::RADIAL-SEQUENCE, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GENDL::STANDARD-SEQUENCE, keyword) STRINGS-FOR-DISPLAY ( GDL object message on VANILLA-MIXIN*,
```

---

## index.html (chunk 176/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
DL object message on GENDL::RADIAL-SEQUENCE, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GENDL::STANDARD-SEQUENCE, keyword) STRINGS-FOR-DISPLAY ( GDL object message on VANILLA-MIXIN*, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GENDL::VARIABLE-SEQUENCE, keyword) STRINGS-FOR-DISPLAY ( GDL object message on ANGULAR-DIMENSION, keyword) STRINGS-FOR-DISPLAY ( GDL object message on ARC, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GEOM-BASE::ARCOID-MIXIN, keyword) STRINGS-FOR-DISPLAY ( GDL object message on BASE-COORDINATE-SYSTEM, keyword) STRINGS-FOR-DISPLAY ( GDL object message on BASE-DRAWING, keyword) STRINGS-FOR-DISPLAY ( GDL object message on BASE-OBJECT, keyword) STRINGS-FOR-DISPLAY ( GDL object message on BASE-VIEW, keyword) STRINGS-FOR-DISPLAY ( GDL object message on BEZIER-CURVE, keyword) STRINGS-FOR-DISPLAY ( GDL object message on BOX, keyword) STRINGS-FOR-DISPLAY ( GDL object message on C-CYLINDER, keyword) STRINGS-FOR-DISPLAY ( GDL object message on
```

---

## index.html (chunk 177/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
age on BEZIER-CURVE, keyword) STRINGS-FOR-DISPLAY ( GDL object message on BOX, keyword) STRINGS-FOR-DISPLAY ( GDL object message on C-CYLINDER, keyword) STRINGS-FOR-DISPLAY ( GDL object message on CENTER-LINE, keyword) STRINGS-FOR-DISPLAY ( GDL object message on CIRCLE, keyword) STRINGS-FOR-DISPLAY ( GDL object message on CONE, keyword) STRINGS-FOR-DISPLAY ( GDL object message on CONSTRAINED-ARC, keyword) STRINGS-FOR-DISPLAY ( GDL object message on CONSTRAINED-FILLET, keyword) STRINGS-FOR-DISPLAY ( GDL object message on CONSTRAINED-LINE, keyword) STRINGS-FOR-DISPLAY ( GDL object message on CYLINDER, keyword) STRINGS-FOR-DISPLAY ( GDL object message on ELLIPSE, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GENERAL-NOTE, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GLOBAL-FILLETED-POLYGON-PROJECTION, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GLOBAL-FILLETED-POLYLINE, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GEOM-BASE::GLOBAL-FILLETED-POLYLINE-MIXIN,
```

---

## index.html (chunk 178/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
ETED-POLYGON-PROJECTION, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GLOBAL-FILLETED-POLYLINE, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GEOM-BASE::GLOBAL-FILLETED-POLYLINE-MIXIN, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GLOBAL-POLYGON-PROJECTION, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GLOBAL-POLYLINE, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GEOM-BASE::GLOBAL-POLYLINE-MIXIN, keyword) STRINGS-FOR-DISPLAY ( GDL object message on HORIZONTAL-DIMENSION, keyword) STRINGS-FOR-DISPLAY ( GDL object message on LABEL, keyword) STRINGS-FOR-DISPLAY ( GDL object message on LEADER-LINE, keyword) STRINGS-FOR-DISPLAY ( GDL object message on LINE, keyword) STRINGS-FOR-DISPLAY ( GDL object message on LINEAR-DIMENSION, keyword) STRINGS-FOR-DISPLAY ( GDL object message on PARALLEL-DIMENSION, keyword) STRINGS-FOR-DISPLAY ( GDL object message on PIE-CHART, keyword) STRINGS-FOR-DISPLAY ( GDL object message on POINT, keyword) STRINGS-FOR-DISPLAY ( GDL
```

---

## index.html (chunk 179/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
DL object message on PARALLEL-DIMENSION, keyword) STRINGS-FOR-DISPLAY ( GDL object message on PIE-CHART, keyword) STRINGS-FOR-DISPLAY ( GDL object message on POINT, keyword) STRINGS-FOR-DISPLAY ( GDL object message on POINTS-DISPLAY, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GEOM-BASE::RENDERER-MIXIN, keyword) STRINGS-FOR-DISPLAY ( GDL object message on ROUTE-PIPE, keyword) STRINGS-FOR-DISPLAY ( GDL object message on SAMPLE-DRAWING, keyword) STRINGS-FOR-DISPLAY ( GDL object message on SPHERE, keyword) STRINGS-FOR-DISPLAY ( GDL object message on SPHERICAL-CAP, keyword) STRINGS-FOR-DISPLAY ( GDL object message on TEXT-LINE, keyword) STRINGS-FOR-DISPLAY ( GDL object message on TORUS, keyword) STRINGS-FOR-DISPLAY ( GDL object message on TYPESET-BLOCK, keyword) STRINGS-FOR-DISPLAY ( GDL object message on VERTICAL-DIMENSION, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GEYSR::MENU, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GEYSR:TREE, keyword)
```

---

## index.html (chunk 180/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
GDL object message on VERTICAL-DIMENSION, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GEYSR::MENU, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GEYSR:TREE, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GWL:APPLICATION-MIXIN, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GWL:BASE-HTML-SHEET, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GWL::COLOR-MAP, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GWL::GEOMETRY-VIEW-MIXIN, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GWL:GRID-FORM-CONTROL, keyword) STRINGS-FOR-DISPLAY ( GDL object message on
```

---

## index.html (chunk 181/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
INGS-FOR-DISPLAY ( GDL object message on GWL::GEOMETRY-VIEW-MIXIN, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GWL:GRID-FORM-CONTROL, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GWL::LAYOUT-MIXIN, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GWL:NODE-MIXIN, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GWL:SESSION-CONTROL-MIXIN, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GWL:SHEET-SECTION, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GWL::SKELETON-FORM-CONTROL, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GWL:SKELETON-UI-ELEMENT, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GWL:WEB-DRAWING, keyword) STRINGS-FOR-DISPLAY (
```

---

## index.html (chunk 182/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
:SKELETON-UI-ELEMENT, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) STRINGS-FOR-DISPLAY ( GDL object message on GWL:WEB-DRAWING, keyword) STRINGS-FOR-DISPLAY ( GDL object message on YADD:ASSEMBLY, keyword) STRINGS-FOR-DISPLAY ( GDL object message on YADD::BASE-YADD-SHEET, keyword) STRINGS-FOR-DISPLAY ( GDL object message on YADD::MASTER-INDEX, keyword) STRINGS-FOR-DISPLAY ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) STRINGS-FOR-DISPLAY ( GDL object message on YADD::PACKAGE-FORM, keyword) STYLE ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) STYLE ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) STYLE ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) STYLE ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) STYLE ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) subtract-vectors ( Function, geom-base) sum-elements ( Macro, gendl) SUPPRESS-DISPLAY? ( GDL object message on BASE-RULE-OBJECT,
```

---

## index.html (chunk 183/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
STYLE ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) subtract-vectors ( Function, geom-base) sum-elements ( Macro, gendl) SUPPRESS-DISPLAY? ( GDL object message on BASE-RULE-OBJECT, keyword) SUPPRESS-DISPLAY? ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) SVG-CLASS ( GDL object message on BASE-DRAWING, keyword) SVG-CLASS ( GDL object message on SAMPLE-DRAWING, keyword) SVG-CLASS ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) SVG-CLASS ( GDL object message on GWL:WEB-DRAWING, keyword) SYMBOLS-FOR-INDEX ( GDL object message on YADD::MASTER-INDEX, keyword) TABINDEX ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) TABINDEX ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) TABINDEX ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) TABINDEX ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) TABINDEX ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) TABLE-CLASS ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) TANGENT
```

---

## index.html (chunk 184/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) TABINDEX ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) TABLE-CLASS ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) TANGENT ( GDL object message on ARC, keyword) TANGENT-POINT ( GDL object message on CONSTRAINED-LINE, keyword) TARGET ( GDL object message on GWL:APPLICATION-MIXIN, keyword) TARGET ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) TARGET ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) TARGET ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) TARGET ( GDL object message on GWL:BASE-HTML-SHEET, keyword) TARGET ( GDL object message on GWL::COLOR-MAP, keyword) TARGET ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) TARGET ( GDL object message on GWL::LAYOUT-MIXIN, keyword) TARGET ( GDL object message on GWL:NODE-MIXIN, keyword) TARGET ( GDL object message on YADD:ASSEMBLY, keyword) TARGET ( GDL object message on YADD::BASE-YADD-SHEET, keyword) TARGET ( GDL object
```

---

## index.html (chunk 185/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
rd) TARGET ( GDL object message on GWL:NODE-MIXIN, keyword) TARGET ( GDL object message on YADD:ASSEMBLY, keyword) TARGET ( GDL object message on YADD::BASE-YADD-SHEET, keyword) TARGET ( GDL object message on YADD::MASTER-INDEX, keyword) TARGET ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) TARGET ( GDL object message on YADD::PACKAGE-FORM, keyword) TEST ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) TEST ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) TEXT ( GDL object message on LABEL, keyword) TEXT-ABOVE-LEADER? ( GDL object message on ANGULAR-DIMENSION, keyword) TEXT-ABOVE-LEADER? ( GDL object message on HORIZONTAL-DIMENSION, keyword) TEXT-ABOVE-LEADER? ( GDL object message on LINEAR-DIMENSION, keyword) TEXT-ABOVE-LEADER? ( GDL object message on PARALLEL-DIMENSION, keyword) TEXT-ABOVE-LEADER? ( GDL object message on VERTICAL-DIMENSION, keyword) TEXT-ALONG-AXIS? ( GDL object message on ANGULAR-DIMENSION, keyword) TEXT-ALONG-AXIS? ( GDL object message
```

---

## index.html (chunk 186/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
-DIMENSION, keyword) TEXT-ABOVE-LEADER? ( GDL object message on VERTICAL-DIMENSION, keyword) TEXT-ALONG-AXIS? ( GDL object message on ANGULAR-DIMENSION, keyword) TEXT-ALONG-AXIS? ( GDL object message on HORIZONTAL-DIMENSION, keyword) TEXT-ALONG-AXIS? ( GDL object message on LINEAR-DIMENSION, keyword) TEXT-ALONG-AXIS? ( GDL object message on PARALLEL-DIMENSION, keyword) TEXT-ALONG-AXIS? ( GDL object message on VERTICAL-DIMENSION, keyword) TEXT-ALONG-LEADER-PADDING-FACTOR ( GDL object message on ANGULAR-DIMENSION, keyword) TEXT-FORM-CONTROL ( Object, gwl) TEXT-GAP ( GDL object message on LABEL, keyword) TEXT-LINE ( Object, geom-base) TEXT-SIDE ( GDL object message on LABEL, keyword) TEXT-X-SCALE ( GDL object message on ANGULAR-DIMENSION, keyword) TEXT-X-SCALE ( GDL object message on GENERAL-NOTE, keyword) TEXT-X-SCALE ( GDL object message on HORIZONTAL-DIMENSION, keyword) TEXT-X-SCALE ( GDL object message on LINEAR-DIMENSION, keyword) TEXT-X-SCALE ( GDL object message on
```

---

## index.html (chunk 187/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
NOTE, keyword) TEXT-X-SCALE ( GDL object message on HORIZONTAL-DIMENSION, keyword) TEXT-X-SCALE ( GDL object message on LINEAR-DIMENSION, keyword) TEXT-X-SCALE ( GDL object message on PARALLEL-DIMENSION, keyword) TEXT-X-SCALE ( GDL object message on VERTICAL-DIMENSION, keyword) the ( Macro, gendl) the-child ( Macro, gendl) the-element ( Macro, gendl) the-object ( Macro, gendl) TITLE ( GDL object message on PIE-CHART, keyword) TITLE ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) TITLE ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) TITLE ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) TITLE ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) TITLE ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) TITLE ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) TITLE ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) TITLE ( GDL object message on YADD:ASSEMBLY, keyword) TITLE ( GDL object message on YADD::BASE-YADD-SHEET, keyword) TITLE (
```

---

## index.html (chunk 188/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
L, keyword) TITLE ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) TITLE ( GDL object message on YADD:ASSEMBLY, keyword) TITLE ( GDL object message on YADD::BASE-YADD-SHEET, keyword) TITLE ( GDL object message on YADD::MASTER-INDEX, keyword) TITLE ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) TITLE ( GDL object message on YADD::PACKAGE-FORM, keyword) TITLE-COLOR ( GDL object message on PIE-CHART, keyword) TITLE-FONT ( GDL object message on PIE-CHART, keyword) TITLE-FONT-SIZE ( GDL object message on PIE-CHART, keyword) TOGGLE-SLOT! ( GDL object message on VANILLA-MIXIN*, keyword) TOP-CAP? ( GDL object message on C-CYLINDER, keyword) TOP-CAP? ( GDL object message on CONE, keyword) TOP-CAP? ( GDL object message on CYLINDER, keyword) TORUS ( Object, geom-base) transform-and-translate-point ( Function, geom-base) transform-numeric-point ( Function, geom-base) TRANSITORY-SLOTS ( GDL object message on GWL:APPLICATION-MIXIN, keyword) TRANSITORY-SLOTS ( GDL object message
```

---

## index.html (chunk 189/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
nd-translate-point ( Function, geom-base) transform-numeric-point ( Function, geom-base) TRANSITORY-SLOTS ( GDL object message on GWL:APPLICATION-MIXIN, keyword) TRANSITORY-SLOTS ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) TRANSITORY-SLOTS ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) TRANSITORY-SLOTS ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) TRANSITORY-SLOTS ( GDL object message on GWL:BASE-HTML-SHEET, keyword) TRANSITORY-SLOTS ( GDL object message on GWL::COLOR-MAP, keyword) TRANSITORY-SLOTS ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) TRANSITORY-SLOTS ( GDL object message on GWL::LAYOUT-MIXIN, keyword) TRANSITORY-SLOTS ( GDL object message on GWL:NODE-MIXIN, keyword) TRANSITORY-SLOTS ( GDL object message on YADD:ASSEMBLY, keyword) TRANSITORY-SLOTS ( GDL object message on YADD::BASE-YADD-SHEET, keyword) TRANSITORY-SLOTS ( GDL object message on YADD::MASTER-INDEX, keyword) TRANSITORY-SLOTS ( GDL object message on
```

---

## index.html (chunk 190/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
TRANSITORY-SLOTS ( GDL object message on YADD::BASE-YADD-SHEET, keyword) TRANSITORY-SLOTS ( GDL object message on YADD::MASTER-INDEX, keyword) TRANSITORY-SLOTS ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) TRANSITORY-SLOTS ( GDL object message on YADD::PACKAGE-FORM, keyword) translate ( Macro, geom-base) translate-along-vector ( Function, geom-base) transpose-matrix ( Function, geom-base) TREE ( Object, geysr) TREE-BGCOLOR ( GDL object message on GWL:APPLICATION-MIXIN, keyword) TREE-BGCOLOR ( GDL object message on GWL::LAYOUT-MIXIN, keyword) TREE-BGCOLOR ( GDL object message on GWL:NODE-MIXIN, keyword) TREE-TITLE ( GDL object message on GWL:APPLICATION-MIXIN, keyword) TREE-TITLE ( GDL object message on GWL::LAYOUT-MIXIN, keyword) TREE-TITLE ( GDL object message on GWL:NODE-MIXIN, keyword) twice ( Function, gendl) TYPE ( GDL object message on VANILLA-MIXIN*, keyword) TYPE-MAPPING ( GDL object message on CL-LITE:CODEBASE-DIRECTORY-NODE, keyword) TYPESET-BLOCK ( Object,
```

---

## index.html (chunk 191/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
MIXIN, keyword) twice ( Function, gendl) TYPE ( GDL object message on VANILLA-MIXIN*, keyword) TYPE-MAPPING ( GDL object message on CL-LITE:CODEBASE-DIRECTORY-NODE, keyword) TYPESET-BLOCK ( Object, geom-base) UI-DISPLAY-LIST-LEAVES ( GDL object message on GWL:APPLICATION-MIXIN, keyword) UI-DISPLAY-LIST-LEAVES ( GDL object message on GWL::LAYOUT-MIXIN, keyword) UI-DISPLAY-LIST-LEAVES ( GDL object message on GWL:NODE-MIXIN, keyword) UI-DISPLAY-LIST-OBJECTS ( GDL object message on GWL:APPLICATION-MIXIN, keyword) UI-DISPLAY-LIST-OBJECTS ( GDL object message on GWL::LAYOUT-MIXIN, keyword) UI-DISPLAY-LIST-OBJECTS ( GDL object message on GWL:NODE-MIXIN, keyword) UI-SPECIFIC-LAYOUT-JS ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) UI-SPECIFIC-LAYOUT-JS ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) UI-SPECIFIC-LAYOUT-JS ( GDL object message on YADD:ASSEMBLY, keyword) UI-SPECIFIC-LAYOUT-JS ( GDL object message on YADD::BASE-YADD-SHEET, keyword) UI-SPECIFIC-LAYOUT-JS (
```

---

## index.html (chunk 192/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
ASE-AJAX-SHEET, keyword) UI-SPECIFIC-LAYOUT-JS ( GDL object message on YADD:ASSEMBLY, keyword) UI-SPECIFIC-LAYOUT-JS ( GDL object message on YADD::BASE-YADD-SHEET, keyword) UI-SPECIFIC-LAYOUT-JS ( GDL object message on YADD::MASTER-INDEX, keyword) UI-SPECIFIC-LAYOUT-JS ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) UI-SPECIFIC-LAYOUT-JS ( GDL object message on YADD::PACKAGE-FORM, keyword) undefine-object ( Function, gendl) UNDERLINE? ( GDL object message on ANGULAR-DIMENSION, keyword) UNDERLINE? ( GDL object message on GENERAL-NOTE, keyword) UNDERLINE? ( GDL object message on HORIZONTAL-DIMENSION, keyword) UNDERLINE? ( GDL object message on LINEAR-DIMENSION, keyword) UNDERLINE? ( GDL object message on PARALLEL-DIMENSION, keyword) UNDERLINE? ( GDL object message on VERTICAL-DIMENSION, keyword) unitize-vector ( Function, geom-base) universal-time-from-iso-8601 ( Function, gendl) UPDATE! ( GDL object message on VANILLA-MIXIN*, keyword) URL ( GDL object message on
```

---

## index.html (chunk 193/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
MENSION, keyword) unitize-vector ( Function, geom-base) universal-time-from-iso-8601 ( Function, gendl) UPDATE! ( GDL object message on VANILLA-MIXIN*, keyword) URL ( GDL object message on GWL:BASE-HTML-SHEET, keyword) USE-BSPLINES? ( GDL object message on GWL:APPLICATION-MIXIN, keyword) USE-BSPLINES? ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) USE-BSPLINES? ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) USE-BSPLINES? ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) USE-BSPLINES? ( GDL object message on GWL::LAYOUT-MIXIN, keyword) USE-BSPLINES? ( GDL object message on GWL:NODE-MIXIN, keyword) USE-JQUERY? ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) USE-JQUERY? ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) USE-JQUERY? ( GDL object message on YADD:ASSEMBLY, keyword) USE-JQUERY? ( GDL object message on YADD::BASE-YADD-SHEET, keyword) USE-JQUERY? ( GDL object message on YADD::MASTER-INDEX, keyword) USE-JQUERY? ( GDL object
```

---

## index.html (chunk 194/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
bject message on YADD:ASSEMBLY, keyword) USE-JQUERY? ( GDL object message on YADD::BASE-YADD-SHEET, keyword) USE-JQUERY? ( GDL object message on YADD::MASTER-INDEX, keyword) USE-JQUERY? ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) USE-JQUERY? ( GDL object message on YADD::PACKAGE-FORM, keyword) USE-RAPHAEL-GRAF? ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) USE-RAPHAEL? ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) USE-RECOVERY-OBJECT? ( GDL object message on GWL:SESSION-CONTROL-MIXIN, keyword) USE-STANDARD-SAVED-SLOTS? ( GDL object message on GWL:APPLICATION-MIXIN, keyword) USE-STANDARD-SAVED-SLOTS? ( GDL object message on GWL::LAYOUT-MIXIN, keyword) USE-STANDARD-SAVED-SLOTS? ( GDL object message on GWL:NODE-MIXIN, keyword) USEMAP ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) USEMAP ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) USEMAP ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) USEMAP ( GDL object
```

---

## index.html (chunk 195/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
ject message on GWL:BASE-FORM-CONTROL, keyword) USEMAP ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) USEMAP ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) USEMAP ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) USEMAP ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) VALIDATION-FUNCTION ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) VALIDATION-FUNCTION ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) VALIDATION-FUNCTION ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) VALIDATION-FUNCTION ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) VALIDATION-FUNCTION ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) VALUE ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) VANILLA-MIXIN* ( Object, gendl) VARIABLE-DOCS ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) VARIABLE-SEQUENCE ( Object, gendl) VECTOR-GRAPHICS ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) VERTEX ( GDL object
```

---

## index.html (chunk 196/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) VARIABLE-SEQUENCE ( Object, gendl) VECTOR-GRAPHICS ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) VERTEX ( GDL object message on BASE-OBJECT, keyword) VERTEX-LIST ( GDL object message on GLOBAL-FILLETED-POLYGON-PROJECTION, keyword) VERTEX-LIST ( GDL object message on GLOBAL-FILLETED-POLYLINE, keyword) VERTEX-LIST ( GDL object message on GEOM-BASE::GLOBAL-FILLETED-POLYLINE-MIXIN, keyword) VERTEX-LIST ( GDL object message on GLOBAL-POLYGON-PROJECTION, keyword) VERTEX-LIST ( GDL object message on GLOBAL-POLYLINE, keyword) VERTEX-LIST ( GDL object message on GEOM-BASE::GLOBAL-POLYLINE-MIXIN, keyword) VERTEX-LIST ( GDL object message on ROUTE-PIPE, keyword) VERTICAL-DIMENSION ( Object, geom-base) VIEW ( GDL object message on GWL:APPLICATION-MIXIN, keyword) VIEW ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) VIEW ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) VIEW ( GDL object message
```

---

## index.html (chunk 197/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
e on GWL:APPLICATION-MIXIN, keyword) VIEW ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) VIEW ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) VIEW ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) VIEW ( GDL object message on GWL::LAYOUT-MIXIN, keyword) VIEW ( GDL object message on GWL:NODE-MIXIN, keyword) VIEW-CENTER ( GDL object message on BASE-VIEW, keyword) VIEW-CONTROLS ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) VIEW-DIRECTION-DEFAULT ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) VIEW-OBJECT ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) VIEW-OBJECT ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) VIEW-OBJECT ( GDL object message on GWL::GEOMETRY-VIEW-MIXIN, keyword) VIEW-OBJECT ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) VIEW-OBJECT ( GDL object message on GWL::LAYOUT-MIXIN, keyword) VIEW-POINT ( GDL object message on BASE-VIEW, keyword) VIEW-REFERENCE-OBJECT ( GDL
```

---

## index.html (chunk 198/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
object message on GWL:GWL-RULE-OBJECT, keyword) VIEW-OBJECT ( GDL object message on GWL::LAYOUT-MIXIN, keyword) VIEW-POINT ( GDL object message on BASE-VIEW, keyword) VIEW-REFERENCE-OBJECT ( GDL object message on ANGULAR-DIMENSION, keyword) VIEW-REFERENCE-OBJECT ( GDL object message on HORIZONTAL-DIMENSION, keyword) VIEW-REFERENCE-OBJECT ( GDL object message on LABEL, keyword) VIEW-REFERENCE-OBJECT ( GDL object message on LINEAR-DIMENSION, keyword) VIEW-REFERENCE-OBJECT ( GDL object message on PARALLEL-DIMENSION, keyword) VIEW-REFERENCE-OBJECT ( GDL object message on VERTICAL-DIMENSION, keyword) VIEW-SCALE ( GDL object message on BASE-VIEW, keyword) VIEW-VECTORS ( GDL object message on GEOM-BASE::RENDERER-MIXIN, keyword) VIEW-VECTORS ( GDL object message on GWL:WEB-DRAWING, keyword) VIEWPOINTS ( GDL object message on GEOM-BASE::RENDERER-MIXIN, keyword) VIEWPOINTS ( GDL object message on GWL:WEB-DRAWING, keyword) VIEWPORT-BORDER-DEFAULT ( GDL object message on
```

---

## index.html (chunk 199/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
OINTS ( GDL object message on GEOM-BASE::RENDERER-MIXIN, keyword) VIEWPOINTS ( GDL object message on GWL:WEB-DRAWING, keyword) VIEWPORT-BORDER-DEFAULT ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) VIOLATED-RULES ( GDL object message on GWL:APPLICATION-MIXIN, keyword) VIOLATED-RULES ( GDL object message on GWL::LAYOUT-MIXIN, keyword) VIOLATED-RULES ( GDL object message on GWL:NODE-MIXIN, keyword) VIOLATED-RULES-BGCOLOR ( GDL object message on GWL:APPLICATION-MIXIN, keyword) VIOLATED-RULES-BGCOLOR ( GDL object message on GWL::LAYOUT-MIXIN, keyword) VIOLATED-RULES-BGCOLOR ( GDL object message on GWL:NODE-MIXIN, keyword) VIOLATED-RULES-TITLE ( GDL object message on GWL:APPLICATION-MIXIN, keyword) VIOLATED-RULES-TITLE ( GDL object message on GWL::LAYOUT-MIXIN, keyword) VIOLATED-RULES-TITLE ( GDL object message on GWL:NODE-MIXIN, keyword) VIOLATED? ( GDL object message on BASE-RULE-OBJECT, keyword) VIOLATED? ( GDL object message on GWL:GWL-RULE-OBJECT, keyword)
```

---

## index.html (chunk 200/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
-RULES-TITLE ( GDL object message on GWL:NODE-MIXIN, keyword) VIOLATED? ( GDL object message on BASE-RULE-OBJECT, keyword) VIOLATED? ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) VISIBLE-CHILDREN ( GDL object message on CL-LITE:CODEBASE-DIRECTORY-NODE, keyword) VISIBLE-CHILDREN ( GDL object message on BASE-RULE-OBJECT, keyword) VISIBLE-CHILDREN ( GDL object message on GDL-APP, keyword) VISIBLE-CHILDREN ( GDL object message on GENDL::MATRIX-SEQUENCE, keyword) VISIBLE-CHILDREN ( GDL object message on NULL-OBJECT, keyword) VISIBLE-CHILDREN ( GDL object message on QUANTIFICATION, keyword) VISIBLE-CHILDREN ( GDL object message on GENDL::RADIAL-SEQUENCE, keyword) VISIBLE-CHILDREN ( GDL object message on GENDL::STANDARD-SEQUENCE, keyword) VISIBLE-CHILDREN ( GDL object message on VANILLA-MIXIN*, keyword) VISIBLE-CHILDREN ( GDL object message on GENDL::VARIABLE-SEQUENCE, keyword) VISIBLE-CHILDREN ( GDL object message on ANGULAR-DIMENSION, keyword) VISIBLE-CHILDREN ( GDL object message
```

---

## index.html (chunk 201/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
IXIN*, keyword) VISIBLE-CHILDREN ( GDL object message on GENDL::VARIABLE-SEQUENCE, keyword) VISIBLE-CHILDREN ( GDL object message on ANGULAR-DIMENSION, keyword) VISIBLE-CHILDREN ( GDL object message on ARC, keyword) VISIBLE-CHILDREN ( GDL object message on GEOM-BASE::ARCOID-MIXIN, keyword) VISIBLE-CHILDREN ( GDL object message on BASE-COORDINATE-SYSTEM, keyword) VISIBLE-CHILDREN ( GDL object message on BASE-DRAWING, keyword) VISIBLE-CHILDREN ( GDL object message on BASE-OBJECT, keyword) VISIBLE-CHILDREN ( GDL object message on BASE-VIEW, keyword) VISIBLE-CHILDREN ( GDL object message on BEZIER-CURVE, keyword) VISIBLE-CHILDREN ( GDL object message on BOX, keyword) VISIBLE-CHILDREN ( GDL object message on C-CYLINDER, keyword) VISIBLE-CHILDREN ( GDL object message on CENTER-LINE, keyword) VISIBLE-CHILDREN ( GDL object message on CIRCLE, keyword) VISIBLE-CHILDREN ( GDL object message on CONE, keyword) VISIBLE-CHILDREN ( GDL object message on CONSTRAINED-ARC, keyword) VISIBLE-CHILDREN (
```

---

## index.html (chunk 202/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
IBLE-CHILDREN ( GDL object message on CIRCLE, keyword) VISIBLE-CHILDREN ( GDL object message on CONE, keyword) VISIBLE-CHILDREN ( GDL object message on CONSTRAINED-ARC, keyword) VISIBLE-CHILDREN ( GDL object message on CONSTRAINED-FILLET, keyword) VISIBLE-CHILDREN ( GDL object message on CONSTRAINED-LINE, keyword) VISIBLE-CHILDREN ( GDL object message on CYLINDER, keyword) VISIBLE-CHILDREN ( GDL object message on ELLIPSE, keyword) VISIBLE-CHILDREN ( GDL object message on GENERAL-NOTE, keyword) VISIBLE-CHILDREN ( GDL object message on GLOBAL-FILLETED-POLYGON-PROJECTION, keyword) VISIBLE-CHILDREN ( GDL object message on GLOBAL-FILLETED-POLYLINE, keyword) VISIBLE-CHILDREN ( GDL object message on GEOM-BASE::GLOBAL-FILLETED-POLYLINE-MIXIN, keyword) VISIBLE-CHILDREN ( GDL object message on GLOBAL-POLYGON-PROJECTION, keyword) VISIBLE-CHILDREN ( GDL object message on GLOBAL-POLYLINE, keyword) VISIBLE-CHILDREN ( GDL object message on GEOM-BASE::GLOBAL-POLYLINE-MIXIN, keyword) VISIBLE-CHILDREN
```

---

## index.html (chunk 203/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
L-POLYGON-PROJECTION, keyword) VISIBLE-CHILDREN ( GDL object message on GLOBAL-POLYLINE, keyword) VISIBLE-CHILDREN ( GDL object message on GEOM-BASE::GLOBAL-POLYLINE-MIXIN, keyword) VISIBLE-CHILDREN ( GDL object message on HORIZONTAL-DIMENSION, keyword) VISIBLE-CHILDREN ( GDL object message on LABEL, keyword) VISIBLE-CHILDREN ( GDL object message on LEADER-LINE, keyword) VISIBLE-CHILDREN ( GDL object message on LINE, keyword) VISIBLE-CHILDREN ( GDL object message on LINEAR-DIMENSION, keyword) VISIBLE-CHILDREN ( GDL object message on PARALLEL-DIMENSION, keyword) VISIBLE-CHILDREN ( GDL object message on PIE-CHART, keyword) VISIBLE-CHILDREN ( GDL object message on POINT, keyword) VISIBLE-CHILDREN ( GDL object message on POINTS-DISPLAY, keyword) VISIBLE-CHILDREN ( GDL object message on GEOM-BASE::RENDERER-MIXIN, keyword) VISIBLE-CHILDREN ( GDL object message on ROUTE-PIPE, keyword) VISIBLE-CHILDREN ( GDL object message on SAMPLE-DRAWING, keyword) VISIBLE-CHILDREN ( GDL object message on
```

---

## index.html (chunk 204/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
SE::RENDERER-MIXIN, keyword) VISIBLE-CHILDREN ( GDL object message on ROUTE-PIPE, keyword) VISIBLE-CHILDREN ( GDL object message on SAMPLE-DRAWING, keyword) VISIBLE-CHILDREN ( GDL object message on SPHERE, keyword) VISIBLE-CHILDREN ( GDL object message on SPHERICAL-CAP, keyword) VISIBLE-CHILDREN ( GDL object message on TEXT-LINE, keyword) VISIBLE-CHILDREN ( GDL object message on TORUS, keyword) VISIBLE-CHILDREN ( GDL object message on TYPESET-BLOCK, keyword) VISIBLE-CHILDREN ( GDL object message on VERTICAL-DIMENSION, keyword) VISIBLE-CHILDREN ( GDL object message on GEYSR::MENU, keyword) VISIBLE-CHILDREN ( GDL object message on GEYSR:TREE, keyword) VISIBLE-CHILDREN ( GDL object message on GWL:APPLICATION-MIXIN, keyword) VISIBLE-CHILDREN ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) VISIBLE-CHILDREN ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) VISIBLE-CHILDREN ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) VISIBLE-CHILDREN ( GDL object message on
```

---

## index.html (chunk 205/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
, keyword) VISIBLE-CHILDREN ( GDL object message on GWL:BASE-AJAX-SHEET, keyword) VISIBLE-CHILDREN ( GDL object message on GWL:BASE-FORM-CONTROL, keyword) VISIBLE-CHILDREN ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) VISIBLE-CHILDREN ( GDL object message on GWL:BASE-HTML-SHEET, keyword) VISIBLE-CHILDREN ( GDL object message on GWL:CHECKBOX-FORM-CONTROL, keyword) VISIBLE-CHILDREN ( GDL object message on GWL::COLOR-MAP, keyword) VISIBLE-CHILDREN ( GDL object message on GWL::GEOMETRY-VIEW-MIXIN, keyword) VISIBLE-CHILDREN ( GDL object message on GWL:GRID-FORM-CONTROL, keyword) VISIBLE-CHILDREN ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) VISIBLE-CHILDREN ( GDL object message on GWL::LAYOUT-MIXIN, keyword) VISIBLE-CHILDREN ( GDL object message on GWL:MENU-FORM-CONTROL, keyword) VISIBLE-CHILDREN ( GDL object message on GWL:NODE-MIXIN, keyword) VISIBLE-CHILDREN ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) VISIBLE-CHILDREN ( GDL object message on
```

---

## index.html (chunk 206/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
, keyword) VISIBLE-CHILDREN ( GDL object message on GWL:NODE-MIXIN, keyword) VISIBLE-CHILDREN ( GDL object message on GWL:RADIO-FORM-CONTROL, keyword) VISIBLE-CHILDREN ( GDL object message on GWL:SESSION-CONTROL-MIXIN, keyword) VISIBLE-CHILDREN ( GDL object message on GWL:SHEET-SECTION, keyword) VISIBLE-CHILDREN ( GDL object message on GWL::SKELETON-FORM-CONTROL, keyword) VISIBLE-CHILDREN ( GDL object message on GWL:SKELETON-UI-ELEMENT, keyword) VISIBLE-CHILDREN ( GDL object message on GWL:TEXT-FORM-CONTROL, keyword) VISIBLE-CHILDREN ( GDL object message on GWL:WEB-DRAWING, keyword) VISIBLE-CHILDREN ( GDL object message on YADD:ASSEMBLY, keyword) VISIBLE-CHILDREN ( GDL object message on YADD::BASE-YADD-SHEET, keyword) VISIBLE-CHILDREN ( GDL object message on YADD::MASTER-INDEX, keyword) VISIBLE-CHILDREN ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) VISIBLE-CHILDREN ( GDL object message on YADD::PACKAGE-FORM, keyword) VOLUME ( GDL object message on BOX, keyword)
```

---

## index.html (chunk 207/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
SIBLE-CHILDREN ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) VISIBLE-CHILDREN ( GDL object message on YADD::PACKAGE-FORM, keyword) VOLUME ( GDL object message on BOX, keyword) WEB-DRAWING ( Object, gwl) WIDTH ( GDL object message on ANGULAR-DIMENSION, keyword) WIDTH ( GDL object message on ARC, keyword) WIDTH ( GDL object message on BASE-COORDINATE-SYSTEM, keyword) WIDTH ( GDL object message on BASE-DRAWING, keyword) WIDTH ( GDL object message on BASE-OBJECT, keyword) WIDTH ( GDL object message on BASE-VIEW, keyword) WIDTH ( GDL object message on BEZIER-CURVE, keyword) WIDTH ( GDL object message on BOX, keyword) WIDTH ( GDL object message on C-CYLINDER, keyword) WIDTH ( GDL object message on CENTER-LINE, keyword) WIDTH ( GDL object message on CONE, keyword) WIDTH ( GDL object message on CONSTRAINED-LINE, keyword) WIDTH ( GDL object message on CYLINDER, keyword) WIDTH ( GDL object message on ELLIPSE, keyword) WIDTH ( GDL object message on GENERAL-NOTE, keyword) WIDTH (
```

---

## index.html (chunk 208/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
t message on CONSTRAINED-LINE, keyword) WIDTH ( GDL object message on CYLINDER, keyword) WIDTH ( GDL object message on ELLIPSE, keyword) WIDTH ( GDL object message on GENERAL-NOTE, keyword) WIDTH ( GDL object message on GLOBAL-FILLETED-POLYGON-PROJECTION, keyword) WIDTH ( GDL object message on GLOBAL-FILLETED-POLYLINE, keyword) WIDTH ( GDL object message on GEOM-BASE::GLOBAL-FILLETED-POLYLINE-MIXIN, keyword) WIDTH ( GDL object message on GLOBAL-POLYGON-PROJECTION, keyword) WIDTH ( GDL object message on GLOBAL-POLYLINE, keyword) WIDTH ( GDL object message on GEOM-BASE::GLOBAL-POLYLINE-MIXIN, keyword) WIDTH ( GDL object message on HORIZONTAL-DIMENSION, keyword) WIDTH ( GDL object message on LABEL, keyword) WIDTH ( GDL object message on LEADER-LINE, keyword) WIDTH ( GDL object message on LINE, keyword) WIDTH ( GDL object message on LINEAR-DIMENSION, keyword) WIDTH ( GDL object message on PARALLEL-DIMENSION, keyword) WIDTH ( GDL object message on PIE-CHART, keyword) WIDTH ( GDL object
```

---

## index.html (chunk 209/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
, keyword) WIDTH ( GDL object message on LINEAR-DIMENSION, keyword) WIDTH ( GDL object message on PARALLEL-DIMENSION, keyword) WIDTH ( GDL object message on PIE-CHART, keyword) WIDTH ( GDL object message on POINTS-DISPLAY, keyword) WIDTH ( GDL object message on ROUTE-PIPE, keyword) WIDTH ( GDL object message on SAMPLE-DRAWING, keyword) WIDTH ( GDL object message on SPHERE, keyword) WIDTH ( GDL object message on SPHERICAL-CAP, keyword) WIDTH ( GDL object message on TEXT-LINE, keyword) WIDTH ( GDL object message on TORUS, keyword) WIDTH ( GDL object message on TYPESET-BLOCK, keyword) WIDTH ( GDL object message on VERTICAL-DIMENSION, keyword) WIDTH ( GDL object message on GWL:APPLICATION-MIXIN, keyword) WIDTH ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) WIDTH ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) WIDTH ( GDL object message on GWL::GEOMETRY-VIEW-MIXIN, keyword) WIDTH ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) WIDTH ( GDL object message
```

---

## index.html (chunk 210/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
ssage on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) WIDTH ( GDL object message on GWL::GEOMETRY-VIEW-MIXIN, keyword) WIDTH ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) WIDTH ( GDL object message on GWL::LAYOUT-MIXIN, keyword) WIDTH ( GDL object message on GWL:NODE-MIXIN, keyword) WIDTH ( GDL object message on GWL:WEB-DRAWING, keyword) with-cl-who ( Macro, gwl) with-cl-who-string ( Macro, gwl) with-error-handling ( Macro, gendl) with-format ( Macro, gendl) with-format-slots ( Macro, gendl) with-html-form ( Macro, gwl) WITNESS-1-TO-CENTER? ( GDL object message on ANGULAR-DIMENSION, keyword) WITNESS-2-TO-CENTER? ( GDL object message on ANGULAR-DIMENSION, keyword) WITNESS-DIRECTION-VECTOR ( GDL object message on ANGULAR-DIMENSION, keyword) WITNESS-DIRECTION-VECTOR ( GDL object message on HORIZONTAL-DIMENSION, keyword) WITNESS-DIRECTION-VECTOR ( GDL object message on LINEAR-DIMENSION, keyword) WITNESS-DIRECTION-VECTOR ( GDL object message on PARALLEL-DIMENSION, keyword)
```

---

## index.html (chunk 211/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
HORIZONTAL-DIMENSION, keyword) WITNESS-DIRECTION-VECTOR ( GDL object message on LINEAR-DIMENSION, keyword) WITNESS-DIRECTION-VECTOR ( GDL object message on PARALLEL-DIMENSION, keyword) WITNESS-DIRECTION-VECTOR ( GDL object message on VERTICAL-DIMENSION, keyword) WITNESS-LINE-2? ( GDL object message on ANGULAR-DIMENSION, keyword) WITNESS-LINE-2? ( GDL object message on HORIZONTAL-DIMENSION, keyword) WITNESS-LINE-2? ( GDL object message on LINEAR-DIMENSION, keyword) WITNESS-LINE-2? ( GDL object message on PARALLEL-DIMENSION, keyword) WITNESS-LINE-2? ( GDL object message on VERTICAL-DIMENSION, keyword) WITNESS-LINE-EXT ( GDL object message on ANGULAR-DIMENSION, keyword) WITNESS-LINE-EXT ( GDL object message on HORIZONTAL-DIMENSION, keyword) WITNESS-LINE-EXT ( GDL object message on LINEAR-DIMENSION, keyword) WITNESS-LINE-EXT ( GDL object message on PARALLEL-DIMENSION, keyword) WITNESS-LINE-EXT ( GDL object message on VERTICAL-DIMENSION, keyword) WITNESS-LINE-GAP ( GDL object message on
```

---

## index.html (chunk 212/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
NSION, keyword) WITNESS-LINE-EXT ( GDL object message on PARALLEL-DIMENSION, keyword) WITNESS-LINE-EXT ( GDL object message on VERTICAL-DIMENSION, keyword) WITNESS-LINE-GAP ( GDL object message on ANGULAR-DIMENSION, keyword) WITNESS-LINE-GAP ( GDL object message on HORIZONTAL-DIMENSION, keyword) WITNESS-LINE-GAP ( GDL object message on LINEAR-DIMENSION, keyword) WITNESS-LINE-GAP ( GDL object message on PARALLEL-DIMENSION, keyword) WITNESS-LINE-GAP ( GDL object message on VERTICAL-DIMENSION, keyword) WITNESS-LINE-LENGTH ( GDL object message on ANGULAR-DIMENSION, keyword) WITNESS-LINE-LENGTH ( GDL object message on HORIZONTAL-DIMENSION, keyword) WITNESS-LINE-LENGTH ( GDL object message on LINEAR-DIMENSION, keyword) WITNESS-LINE-LENGTH ( GDL object message on PARALLEL-DIMENSION, keyword) WITNESS-LINE-LENGTH ( GDL object message on VERTICAL-DIMENSION, keyword) WITNESS-LINE? ( GDL object message on ANGULAR-DIMENSION, keyword) WITNESS-LINE? ( GDL object message on HORIZONTAL-DIMENSION,
```

---

## index.html (chunk 213/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
ESS-LINE-LENGTH ( GDL object message on VERTICAL-DIMENSION, keyword) WITNESS-LINE? ( GDL object message on ANGULAR-DIMENSION, keyword) WITNESS-LINE? ( GDL object message on HORIZONTAL-DIMENSION, keyword) WITNESS-LINE? ( GDL object message on LINEAR-DIMENSION, keyword) WITNESS-LINE? ( GDL object message on PARALLEL-DIMENSION, keyword) WITNESS-LINE? ( GDL object message on VERTICAL-DIMENSION, keyword) WRITE-CHILD-LINKS ( GDL object message on GWL:BASE-HTML-SHEET, keyword) WRITE-DEVELOPMENT-LINKS ( GDL object message on GWL:BASE-HTML-SHEET, keyword) WRITE-EMBEDDED-VRML-WORLD ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) WRITE-EMBEDDED-X3D-WORLD ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) WRITE-EMBEDDED-X3DOM-WORLD ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) write-env ( Macro, gendl) WRITE-GEOMETRY ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) WRITE-HTML-SHEET ( GDL object message on GWL:BASE-HTML-SHEET, keyword)
```

---

## index.html (chunk 214/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
ICS-SHEET, keyword) write-env ( Macro, gendl) WRITE-GEOMETRY ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) WRITE-HTML-SHEET ( GDL object message on GWL:BASE-HTML-SHEET, keyword) WRITE-HTML-SHEET ( GDL object message on GWL::COLOR-MAP, keyword) WRITE-HTML-SHEET ( GDL object message on GWL::LAYOUT-MIXIN, keyword) WRITE-HTML-SHEET ( GDL object message on YADD:PACKAGE-DOKUMENTATION, keyword) WRITE-HTML-SHEET ( GDL object message on YADD::PACKAGE-FORM, keyword) write-plist ( Function, gendl) WRITE-SAVED-SLOTS ( GDL object message on GWL::LAYOUT-MIXIN, keyword) WRITE-SELF-LINK ( GDL object message on GWL:BASE-HTML-SHEET, keyword) WRITE-SNAPSHOT ( GDL object message on VANILLA-MIXIN*, keyword) WRITE-STANDARD-FOOTER ( GDL object message on GWL:BASE-HTML-SHEET, keyword) write-the ( Macro, gendl) write-the-object ( Macro, gendl) X3DOM-GRAPHICS ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) zero-vector? ( Function, geom-base) ZOOM-FACTOR ( GDL object message on
```

---

## index.html (chunk 215/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
Macro, gendl) write-the-object ( Macro, gendl) X3DOM-GRAPHICS ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) zero-vector? ( Function, geom-base) ZOOM-FACTOR ( GDL object message on GWL:APPLICATION-MIXIN, keyword) ZOOM-FACTOR ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) ZOOM-FACTOR ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) ZOOM-FACTOR ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) ZOOM-FACTOR ( GDL object message on GWL::LAYOUT-MIXIN, keyword) ZOOM-FACTOR ( GDL object message on GWL:NODE-MIXIN, keyword) ZOOM-MODE ( GDL object message on GWL:APPLICATION-MIXIN, keyword) ZOOM-MODE ( GDL object message on GWL:BASE-AJAX-GRAPHICS-SHEET, keyword) ZOOM-MODE ( GDL object message on GWL:BASE-HTML-GRAPHICS-SHEET, keyword) ZOOM-MODE ( GDL object message on GWL:GWL-RULE-OBJECT, keyword) ZOOM-MODE ( GDL object message on GWL::LAYOUT-MIXIN, keyword) ZOOM-MODE ( GDL object message on GWL:NODE-MIXIN, keyword) ^2 ( Function, gendl) <-Back
```

---

## index.html (chunk 216/216)
Source: yadd-reference/master-index/index.html
Type: reference

```
bject message on GWL:GWL-RULE-OBJECT, keyword) ZOOM-MODE ( GDL object message on GWL::LAYOUT-MIXIN, keyword) ZOOM-MODE ( GDL object message on GWL:NODE-MIXIN, keyword) ^2 ( Function, gendl) <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/master-index-grouped/719/index.html
Type: reference

```
GendL Application - MASTER-INDEX-GROUPED 719 Matches for ARC Matches for ARC ARC ( Object, geom-base)
```

---

## index.html (chunk 1/1)
Source: yadd-reference/master-index-grouped/125/index.html
Type: reference

```
GendL Application - MASTER-INDEX-GROUPED 125 Matches for GLOBAL-POLYGON-PROJECTION Matches for GLOBAL-POLYGON-PROJECTION GLOBAL-POLYGON-PROJECTION ( Object, geom-base)
```

---

## index.html (chunk 1/1)
Source: yadd-reference/master-index-grouped/717/index.html
Type: reference

```
GendL Application - MASTER-INDEX-GROUPED 717 Matches for GWL:BASE-AJAX-SHEET Matches for GWL:BASE-AJAX-SHEET BASE-AJAX-SHEET ( Object, gwl)
```

---

## index.html (chunk 1/1)
Source: yadd-reference/master-index-grouped/457/index.html
Type: reference

```
GendL Application - MASTER-INDEX-GROUPED 457 Matches for GWL::LAYOUT-MIXIN Matches for GWL::LAYOUT-MIXIN LAYOUT-MIXIN ( Object, gwl)
```

---

## index.html (chunk 1/1)
Source: yadd-reference/master-index-grouped/521/index.html
Type: reference

```
GendL Application - MASTER-INDEX-GROUPED 521 Matches for GWL:BASE-FORM-CONTROL Matches for GWL:BASE-FORM-CONTROL BASE-FORM-CONTROL ( Object, gwl)
```

---

## index.html (chunk 1/1)
Source: yadd-reference/master-index-grouped/208/index.html
Type: reference

```
GendL Application - MASTER-INDEX-GROUPED 208 Matches for GWL:BASE-HTML-SHEET Matches for GWL:BASE-HTML-SHEET BASE-HTML-SHEET ( Object, gwl)
```

---

## index.html (chunk 1/1)
Source: yadd-reference/master-index-grouped/349/index.html
Type: reference

```
GendL Application - MASTER-INDEX-GROUPED 349 Matches for QUANTIFICATION Matches for QUANTIFICATION QUANTIFICATION ( Object, gendl)
```

---

## index.html (chunk 1/1)
Source: yadd-reference/master-index-grouped/239/index.html
Type: reference

```
GendL Application - MASTER-INDEX-GROUPED 239 Matches for YADD:PACKAGE-DOKUMENTATION Matches for YADD:PACKAGE-DOKUMENTATION PACKAGE-DOKUMENTATION ( Object, yadd)
```

---

## index.html (chunk 1/1)
Source: yadd-reference/master-index-grouped/663/index.html
Type: reference

```
GendL Application - MASTER-INDEX-GROUPED 663 Matches for YADD::PACKAGE-FORM Matches for YADD::PACKAGE-FORM PACKAGE-FORM ( Object, yadd)
```

---

## index.html (chunk 1/1)
Source: yadd-reference/master-index-grouped/94/index.html
Type: reference

```
GendL Application - MASTER-INDEX-GROUPED 94 Matches for GWL::SKELETON-FORM-CONTROL Matches for GWL::SKELETON-FORM-CONTROL SKELETON-FORM-CONTROL ( Object, gwl)
```

---

## index.html (chunk 1/1)
Source: yadd-reference/master-index-grouped/185/index.html
Type: reference

```
GendL Application - MASTER-INDEX-GROUPED 185 Matches for BASE-RULE-OBJECT Matches for BASE-RULE-OBJECT BASE-RULE-OBJECT ( Object, gendl)
```

---

## index.html (chunk 1/1)
Source: yadd-reference/master-index-grouped/91/index.html
Type: reference

```
GendL Application - MASTER-INDEX-GROUPED 91 Matches for LINEAR-DIMENSION Matches for LINEAR-DIMENSION LINEAR-DIMENSION ( Object, geom-base)
```

---

## index.html (chunk 1/1)
Source: yadd-reference/master-index-grouped/192/index.html
Type: reference

```
GendL Application - MASTER-INDEX-GROUPED 192 Matches for GEOM-BASE::GLOBAL-FILLETED-POLYLINE-MIXIN Matches for GEOM-BASE::GLOBAL-FILLETED-POLYLINE-MIXIN GLOBAL-FILLETED-POLYLINE-MIXIN ( Object, geom-base)
```

---

## index.html (chunk 1/1)
Source: yadd-reference/master-index-grouped/751/index.html
Type: reference

```
GendL Application - MASTER-INDEX-GROUPED 751 Matches for BASE-OBJECT Matches for BASE-OBJECT BASE-OBJECT ( Object, geom-base)
```

---

## index.html (chunk 1/1)
Source: yadd-reference/master-index-grouped/469/index.html
Type: reference

```
GendL Application - MASTER-INDEX-GROUPED 469 Matches for GEOM-BASE::ARCOID-MIXIN Matches for GEOM-BASE::ARCOID-MIXIN ARCOID-MIXIN ( Object, geom-base)
```

---

## index.html (chunk 1/1)
Source: yadd-reference/master-index-grouped/715/index.html
Type: reference

```
GendL Application - MASTER-INDEX-GROUPED 715 Matches for GWL::GEOMETRY-VIEW-MIXIN Matches for GWL::GEOMETRY-VIEW-MIXIN GEOMETRY-VIEW-MIXIN ( Object, gwl)
```

---

## index.html (chunk 1/1)
Source: yadd-reference/master-index-grouped/506/index.html
Type: reference

```
GendL Application - MASTER-INDEX-GROUPED 506 Matches for GWL:SKELETON-UI-ELEMENT Matches for GWL:SKELETON-UI-ELEMENT SKELETON-UI-ELEMENT ( Object, gwl)
```

---

## index.html (chunk 1/1)
Source: yadd-reference/master-index-grouped/141/index.html
Type: reference

```
GendL Application - MASTER-INDEX-GROUPED 141 Matches for LINE Matches for LINE LINE ( Object, geom-base)
```

---

## index.html (chunk 1/1)
Source: yadd-reference/master-index-grouped/772/index.html
Type: reference

```
GendL Application - MASTER-INDEX-GROUPED 772 Matches for SPHERE Matches for SPHERE SPHERE ( Object, geom-base)
```

---

## index.html (chunk 1/1)
Source: yadd-reference/master-index-grouped/709/index.html
Type: reference

```
GendL Application - MASTER-INDEX-GROUPED 709 Matches for GEOM-BASE::RENDERER-MIXIN Matches for GEOM-BASE::RENDERER-MIXIN RENDERER-MIXIN ( Object, geom-base)
```

---

## index.html (chunk 1/1)
Source: yadd-reference/master-index-grouped/587/index.html
Type: reference

```
GendL Application - MASTER-INDEX-GROUPED 587 Matches for YADD::BASE-YADD-SHEET Matches for YADD::BASE-YADD-SHEET BASE-YADD-SHEET ( Object, yadd)
```

---

## index.html (chunk 1/1)
Source: yadd-reference/master-index-grouped/629/index.html
Type: reference

```
GendL Application - MASTER-INDEX-GROUPED 629 Matches for CONSTRAINED-ARC Matches for CONSTRAINED-ARC CONSTRAINED-ARC ( Object, geom-base)
```

---

## index.html (chunk 1/1)
Source: yadd-reference/master-index-grouped/312/index.html
Type: reference

```
GendL Application - MASTER-INDEX-GROUPED 312 Matches for GWL:MENU-FORM-CONTROL Matches for GWL:MENU-FORM-CONTROL MENU-FORM-CONTROL ( Object, gwl)
```

---

## index.html (chunk 1/1)
Source: yadd-reference/master-index-grouped/759/index.html
Type: reference

```
GendL Application - MASTER-INDEX-GROUPED 759 Matches for CYLINDER Matches for CYLINDER CYLINDER ( Object, geom-base)
```

---

## index.html (chunk 1/1)
Source: yadd-reference/master-index-grouped/426/index.html
Type: reference

```
GendL Application - MASTER-INDEX-GROUPED 426 Matches for GWL:BASE-HTML-GRAPHICS-SHEET Matches for GWL:BASE-HTML-GRAPHICS-SHEET BASE-HTML-GRAPHICS-SHEET ( Object, gwl)
```

---

## index.html (chunk 1/1)
Source: yadd-reference/master-index-grouped/298/index.html
Type: reference

```
GendL Application - MASTER-INDEX-GROUPED 298 Matches for GEOM-BASE::GLOBAL-POLYLINE-MIXIN Matches for GEOM-BASE::GLOBAL-POLYLINE-MIXIN GLOBAL-POLYLINE-MIXIN ( Object, geom-base)
```

---

## index.html (chunk 1/1)
Source: yadd-reference/master-index-grouped/351/index.html
Type: reference

```
GendL Application - MASTER-INDEX-GROUPED 351 Matches for GENDL::STANDARD-SEQUENCE Matches for GENDL::STANDARD-SEQUENCE STANDARD-SEQUENCE ( Object, gendl)
```

---

## index.html (chunk 1/1)
Source: yadd-reference/master-index-grouped/157/index.html
Type: reference

```
GendL Application - MASTER-INDEX-GROUPED 157 Matches for BASE-DRAWING Matches for BASE-DRAWING BASE-DRAWING ( Object, geom-base)
```

---

## index.html (chunk 1/1)
Source: yadd-reference/master-index-grouped/654/index.html
Type: reference

```
GendL Application - MASTER-INDEX-GROUPED 654 Matches for VANILLA-MIXIN* Matches for VANILLA-MIXIN* VANILLA-MIXIN* ( Object, gendl)
```

---

## index.html (chunk 1/1)
Source: yadd-reference/master-index-grouped/532/index.html
Type: reference

```
GendL Application - MASTER-INDEX-GROUPED 532 Matches for GWL:SHEET-SECTION Matches for GWL:SHEET-SECTION SHEET-SECTION ( Object, gwl)
```

---

## README.md (chunk 1/1)
Source: gornschool-training/README.md
Type: tutorial

```

# GendL Training 

This is a collaborative effort between Genworks and Mike Twelves, with
the intention of releasing the materials under a shared copyright


## Compiling/Loading


First, (re)generate the `training.asd` file if necessary (only needed if files were added/removed/renamed):

```
(cl-lite ".../path/to/training/" :create-asd-file? t)
```

Next, set up Quicklisp (this can be in your `gdlinit.cl`):

```
(load-quicklisp)
(pushnew ".../path/to/training/" ql:*local-project-directories*)
(ql:quickload :training)

```

To visit the toplevel you can go to

  `http://localhost:9000/gendl-self-start-backdoor`

or to any of the tutorial sets with e.g.

  `http://localhost:9000/t2-backdoor`


  






```

---

## README.md (chunk 1/1)
Source: gornschool-training/quiz/README.md
Type: tutorial

```

## External program dependency:

(from https://github.com/indic-transliteration/indic_transliteration_py):



0. [ Install python pip if you don't already have it ]
1. sudo pip install indic_transliteration -U
2. sudo pip install git+https://github.com/indic-transliteration/indic_transliteration_py/@master -U



```

---

