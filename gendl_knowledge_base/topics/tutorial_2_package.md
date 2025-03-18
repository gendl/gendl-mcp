# Gendl Documentation - tutorial_2_package

## package.lisp - header
Source: gornschool-training/t2/source/package.lisp
Type: tutorial

```
(in-package :gdl-user)

(gwl:define-package :training-2
    (:use :training-common)
  (:shadow #:*home*)
  (:export #:assembly))


```

---

