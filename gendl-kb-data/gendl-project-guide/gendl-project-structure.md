# Gendl Project Structure Best Practices

## Creating a New Project with gendl-skel

The easiest way to create a new Gendl project is using the `gendl-skel` utility. Once Gendl is loaded, you can create a new project with:

```lisp
;; Load the gendl-skel package if not already loaded
(ql:quickload :gendl-skel)

;; Create a new project named "my-project" in the /projects/xfer/ directory
(gendl-skel:gendl-skel :project-name "my-project" :base-path "/projects/xfer/")
```

This will create a complete project structure with all necessary files.

## Standard Directory Structure

A well-organized Gendl project typically follows this structure:

```
project-name/
  ├── project-name.asd     # ASDF system definition (auto-generated)
  ├── init.lisp            # Initialization file for loading the project
  ├── source/              # Source code directory
  │   ├── package.lisp     # Package definition
  │   ├── file-ordering.isc # Controls file loading order
  │   └── *.lisp           # Project source files
```

## Package Definition

Use the simplified `gdl:define-package` approach:

```lisp
;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: gdl-user; Base: 10 -*-

(in-package :gdl-user)

(gdl:define-package :your-package-name
  (:export #:assembly)) ;; export other symbols as needed
```

`gdl:define-package` automatically includes `:gdl`, `:gdl-user`, etc., so you don't need to specify them.

## System Loading

The recommended way to load a Gendl project is:

```lisp
;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: gdl-user; Base: 10 -*-

(in-package :gdl-user)

;; Load Quicklisp
(load-quicklisp)

;; Define project path
(defparameter *my-project-path* 
  (make-pathname :name nil :type nil
                :defaults (glisp:source-pathname)))

;; Generate/update the ASDF system definition if needed
(unless (probe-file (merge-pathnames "my-project.asd" *my-project-path*))
  (cl-lite *my-project-path* :create-asd-file? t))

;; Add to Quicklisp's local projects
(pushnew *my-project-path* ql:*local-project-directories* :test #'equalp)

;; Load the project
(ql:quickload :my-project)
```

## Object Definitions

Gendl objects are defined using `define-object`:

```lisp
(define-object custom-object (parent-class)
  :input-slots
  ([docstring] my-required-input ;; no default, no extra parens, required on instantiation. 
   ([docstring] input-name <default-value-or-expression>)
   ([docstring] another-input <default-value-or-expression>))
   
  :computed-slots
  (([docstring] computed-slot-1 value-or-expression)
   ([docstring] computed-slot-2 value-or-expression))
   
  :objects
  (([docstring] child-object :type 'some-object-type
                              :parameter-name (value-or-expression)))
```

## Object Referencing

Use `theo` (shorthand for `the-object`) for cleaner reference syntax:

```lisp
;; Instead of:
(the-object my-object slot-name)

;; Use:
(theo my-object slot-name)

;; For nested references:
(theo container child-object slot-name)
```

Note that `(the ...)` is shorthand for `(theo self ...)` and `self` is
implicitly bound to the current object when in the body of a
`define-object` form.


## Tips for Efficient Project Organization

1. **Use file-ordering.isc**: For controlling the order of file loading within a directory.
2. **Leverage cl-lite**: Let it generate the .asd files; never write .asd files manually.
3. **Modular Design**: Break complex objects into modular components in separate files.
4. **Package Organization**: Keep related functionality in the same package.
