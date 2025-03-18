# Gendl Documentation - tutorial_3

## packages.lisp - header
Source: gornschool-training/t3/resources/source/packages.lisp
Type: tutorial

```
(in-package :gdl-user)


```

---

## packages.lisp - :my-app
Source: gornschool-training/t3/resources/source/packages.lisp
Type: tutorial

```
(define-package :my-app
    (:export #:my-slot))


```

---

## packages.lisp - :my-other-app
Source: gornschool-training/t3/resources/source/packages.lisp
Type: tutorial

```
(define-package :my-other-app
    (:use :my-app))


```

---

## packages.lisp - :yet-another-app
Source: gornschool-training/t3/resources/source/packages.lisp
Type: tutorial

```
(define-package :yet-another-app
  (:export #:my-slot-1
	   #:my-slot-2)
  (:use :my-app))

```

---

