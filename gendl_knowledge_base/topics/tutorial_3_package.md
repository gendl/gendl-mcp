# Gendl Documentation - tutorial_3_package

## package.lisp - header
Source: gornschool-training/t3/source/package.lisp
Type: tutorial

```
(in-package :gdl-user)

(gwl:define-package :training-3
    (:use :training-common)
  (:shadow #:*home*)
  (:export #:assembly))


```

---

