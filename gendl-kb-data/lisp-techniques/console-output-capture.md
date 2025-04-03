# Capturing Console Output in Common Lisp

## Technique: Using `with-output-to-string`

### Overview
The `with-output-to-string` macro allows you to capture console output as a string instead of printing to the console.

### Basic Syntax
```lisp
(with-output-to-string (*standard-output*)
  ;; Code that would normally print to console
  (function-that-prints-to-stdout))
```

### Example
```lisp
;; Define a function that prints to stdout
(defun example-function ()
  (format t "Hello, world!"))

;; Capture the output as a string
(let ((output (with-output-to-string (*standard-output*)
                (example-function))))
  ;; output will be "Hello, world!\n"
  output)
```

### Key Points
- Redirects standard output to a string
- Useful for testing or capturing console output
- Includes newline characters by default
- Works with `format`, `print`, and other stdout-writing functions

### Use Cases
- Unit testing
- Capturing diagnostic output
- Debugging print statements
- Collecting function output without side effects
