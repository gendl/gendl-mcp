# Gendl Documentation - tutorial_1_package

## package.lisp - header
Source: gornschool-training/t1/source/package.lisp
Type: tutorial

```
(in-package :gdl-user)

(gwl:define-package :training-1
    (:use :training-common)
  (:shadow #:*home*)
  (:export #:assembly))

```

---

