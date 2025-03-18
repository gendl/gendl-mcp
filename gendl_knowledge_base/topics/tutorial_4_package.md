# Gendl Documentation - tutorial_4_package

## package.lisp - header
Source: gornschool-training/t4/source/package.lisp
Type: tutorial

```
(in-package :gdl-user)

(gwl:define-package :training-4
    (:use :training-common)
  (:shadow #:*home*)
  (:export #:assembly))


```

---

