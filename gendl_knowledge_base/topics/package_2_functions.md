# Gendl Documentation - package_2_functions

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/39/index.html
Type: reference

```
Function: MAKE-GDL-APP <-Back Function: Make-Gdl-App MAKE-GDL-APP void This function simply passes its arguments to an instance of gdl-app. Please see that object definition for the documentation of the inputs. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/56/index.html
Type: reference

```
Function: READ-SAFE-STRING <-Back Function: Read-Safe-String READ-SAFE-STRING lisp object Reads an item from string, protecting against lisp evaluation with the `#.' reader macro. Throws an error if evaluation would have occured. arguments: string string <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/59/index.html
Type: reference

```
Function: REPLACE-SUBSTRING <-Back Function: Replace-Substring REPLACE-SUBSTRING string Replaces all substring occurrences of old with new in string . Note: In a full GDL system, you will have glisp:replace-regexp, which is more powerful and probably more efficient than this. arguments: string String The source string old String The substring to be replaced new String The substring to replace it with see-also: excl:replace-regexp <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/48/index.html
Type: reference

```
Function: NEAR-ZERO? <-Back Function: Near-Zero? NEAR-ZERO? boolean Returns non-NIL iff number is greater than tolerance different from zero. arguments: number Number optional arguments: tolerance Number , Default Value: *ZERO-EPSILON* see-also: zerop (Common Lisp function) <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/65/index.html
Type: reference

```
Function: STATUS-MESSAGE <-Back Function: Status-Message STATUS-MESSAGE nil Prints string , followed by a newline, to *trace-output* , which is generally the system console. arguments: string String <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/22/index.html
Type: reference

```
Function: GENDL::FIND-DEPENDENCIES <-Back Function: Gendl::Find-Dependencies FIND-DEPENDENCIES list of pairs of instance/keyword Synonymous with find-messages-which-use. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/64/index.html
Type: reference

```
Function: SPLIT <-Back Function: Split SPLIT list of strings Returns a list containing the elements of string after having been split according to split-chars as delimiting characters. arguments: string String optional arguments: split-chars List of characters , Default Value: (LIST #\ #\Newline #\Return #\Tab) see-also: glisp:split-regexp <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/7/index.html
Type: reference

```
Function: GENDL::CHECK-FUNCTIONS <-Back Function: Gendl::Check-Functions CHECK-FUNCTIONS void Checks :functions or :methods grammar according to following BNF: = :functions | :methods ( *) = ( * + ) = :cached | :cached-eql | :cached-= | :cached-eq | :cached-equal | :cached-equalp <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/37/index.html
Type: reference

```
Function: LOAD-GLIME <-Back Function: Load-Glime LOAD-GLIME void If the Glime (Slime Gendl auto-completion extensions) file exists, load it. Path is currently hardcoded to (merge-pathnames "emacs/glime.lisp" glime:*gdl-home*) or ~/genworks/gendl/emacs/glime.lisp . <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/2)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/25/index.html
Type: reference

```
Function: FLATTEN <-Back Function: Flatten FLATTEN list Returns a new list consisting of only the leaf-level atoms from list . Since nil is technically a list, flatten also has the effect of removing nils from list , but may be inefficient if used only for this purpose. For removing nil values from a list, consider using remove nil ... instead. note: from Stack Overflow forum: http://stackoverflow.com/questions/25866292/flatten-a-list-using-common-lisp note: Creative Commons license (defun flatten (lst &aux (result '())) (labels ((rflatten (lst1) (dolist (el lst1 result) (if (listp el) (rflatten el) (push el result))))) (nreverse (rflatten lst)))) note: This will not work with dotted lists, only with actual lists.
```

---

## index.html (chunk 2/2)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/25/index.html
Type: reference

```
If you need dotted lists, use the old definition of flatten, from Paul Graham On Lisp: (defun flatten (tree) (if (atom tree) (ensure-list tree) (nconc (flatten (car tree)) (if (cdr tree) (flatten (cdr tree)))))) arguments: list List see-also: remove <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/20/index.html
Type: reference

```
Function: ENSURE-LIST <-Back Function: Ensure-List ENSURE-LIST list If argument is not list, returns it in a list. If argument is a list, returns it unchanged. arguments: possible-list Lisp object <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/47/index.html
Type: reference

```
Function: NEAR-TO? <-Back Function: Near-To? NEAR-TO? boolean Predicate to test if number is within tolerance of near-to . The default tolerance is the value of the parameter *zero-epsilon* . arguments: number Number near-to Number optional arguments: tolerance Number , Default Value: *ZERO-EPSILON* <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/75/index.html
Type: reference

```
Macro: WITH-ERROR-HANDLING <-Back Macro: With-Error-Handling WITH-ERROR-HANDLING [macro] Wraps the body of code with error-trapping and system timeout. A warning is given if an error condition occurs with body . keyword arguments: timeout Timeout in Seconds , Default Value: 2 timeout-body Body of code to evaluate if timeout occurs , Default Value: NIL Default is to print a warning and return nil rest arguments: body Body of code to be wrapped <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/34/index.html
Type: reference

```
Macro: LIST-ELEMENTS <-Back Macro: List-Elements LIST-ELEMENTS list of gdl objects [macro] Returns a listing of the elements of an aggregate, with an optional the-element expression and filter. If an expression is given, the list of results from the expressions is returned. If no expression is given, a list of the objects themselves is returned. arguments: aggregate GDL aggregate object (e.g. from a :sequence (:size .) :object specification) optional arguments: expression Expression using the-element , Default Value: NIL Similar to a the-object reference filter Function of one argument , Default Value: NIL Can be used to filter the result list <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/14/index.html
Type: reference

```
Macro: DEFAULTING <-Back Macro: Defaulting DEFAULTING lisp object Returns a default value if the reference-chain is not handled. arguments: form Reference-chain with the or the-object default Lisp expression Default value to return if reference-chain cannot be handled <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/76/index.html
Type: reference

```
Macro: WITH-FORMAT <-Back Macro: With-Format WITH-FORMAT void [macro] Used to establish an output format and a stream to which data is to be sent. This supports a full range of output options such as page dimensions, view transforms, view scales, etc. example: (gdl::with-format (pdf "/tmp/box.pdf" :view-transform (getf *standard-views* :trimetric)) (write-the-object (make-instance 'box :length 100 :width 100 :height 100) cad-output)) <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/42/index.html
Type: reference

```
Function: MAPSEND <-Back Function: Mapsend MAPSEND list Returns a new list which is the result of sending message to each GDL object in object-list . arguments: object-list List of GDL objects message Keyword symbol <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/66/index.html
Type: reference

```
Function: STRING-APPEND <-Back Function: String-Append STRING-APPEND string Returns a new string made up of concatenating the arguments. arguments: &rest (ARGS strings) <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/27/index.html
Type: reference

```
Function: FROUND-TO-NEAREST <-Back Function: Fround-To-Nearest FROUND-TO-NEAREST number Rounds number to the nearest interval , using type contagion rules for floating-point similar to the CL "fround" function. arguments: number Number interval Number <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/68/index.html
Type: reference

```
Macro: THE <-Back Macro: The THE lisp object Sends the reference-chain to self , which typically means it is used within the context of a define-object where self is automatically lexically bound. arguments: reference-chain (&rest) A spliced-in list of symbols naming messages, which can be slots or objects starting from self . For referring to elements of a quantified set, or for passing arguments to GDL functions which take arguments, use parentheses around the message name and enclose the quantified element index or function arguments after the message name example: This example sends the length message to the ``zeroth'' element of the quantified set of arms contained in the body which is contained in the robot which is contained in self: (the robot body (arms 0) length) <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/55/index.html
Type: reference

```
Macro: PRINT-VARIABLES <-Back Macro: Print-Variables PRINT-VARIABLES [macro] void Prints the specified variables and current values to standard output. arguments: vars unquoted symbols (&rest argument) <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/0/index.html
Type: reference

```
Function: ALIST2PLIST <-Back Function: Alist2plist ALIST2PLIST plist Converts an assoc-list to a plist. arguments: alist Assoc-List <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/16/index.html
Type: reference

```
Macro: DEFINE-LENS <-Back Macro: Define-Lens DEFINE-LENS void [macro] Defines output-functions for the combination of the given output-format and GDL object. arguments: format-and-object List of two symbols The first should name an output-format previously defined with define-format , and the second should name a GDL object previously defined with define-object mixin-list NIL This is not supported and should be left as NIL or an empty list for now keyword arguments: skin Name of a skin defined with define-skin , Default Value: T This allows a class hierarchy of look and feel for each view combination. Defaults to T, a generic skin <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/78/index.html
Type: reference

```
Macro: WRITE-ENV <-Back Macro: Write-Env WRITE-ENV void [macro] (usually used just for outputting) Within the context of a with-format , calls functions of the format object, optionally with arguments. Typically these functions will output data to the stream established by the with-format . arguments: function-calls (&rest) Functions on the named output-format to be called example: (with-format (base-format my-object) (write-env (:a "Hello World, my object's length is: ") (:a (the length)))) <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/36/index.html
Type: reference

```
Function: LIST-OF-NUMBERS <-Back Function: List-Of-Numbers LIST-OF-NUMBERS list of numbers Returns a list of incrementing numbers starting from num1 and ending with num2 , inclusive. arguments: num1 Number num2 Number optional arguments: increment Number , Default Value: 1 The distance between the returned listed numbers <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/50/index.html
Type: reference

```
Function: NUMBER-FORMAT <-Back Function: Number-Format NUMBER-FORMAT string Returns a string displaying number rounded to decimal-places decimal places. arguments: number Number decimal-places Integer <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/10/index.html
Type: reference

```
Function: GENDL::CHECK-QUERY-SLOTS <-Back Function: Gendl::Check-Query-Slots CHECK-QUERY-SLOTS unknown what is is <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/2)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/80/index.html
Type: reference

```
Macro: WRITE-THE <-Back Macro: Write-The WRITE-THE lisp object [macro] Typcially used only to send output, not for the return value. This macro is used within the body of a with-format . It sends the reference-chain to self , which typically means it is used within the context of a define-object where self is automatically lexically bound. The reference-chain must terminate with an output-function defined for the combination of the output-format specified in the enclosing with-format , and the object identified by self . arguments: reference-chain (&rest) A spliced-in list of symbols naming messages, which can be slots or objects starting from self , terminating with the name of an output-function. For referring to elements of a quantified set, or for passing arguments to GDL functions which take arguments, use parentheses around the message name and enclose the quantified element index or function arguments after the message name <-Back Copyright © 2025 Genworks ® International .
```

---

## index.html (chunk 2/2)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/80/index.html
Type: reference

```
ch take arguments, use parentheses around the message name and enclose the quantified element index or function arguments after the message name <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/52/index.html
Type: reference

```
Function: PLIST-KEYS <-Back Function: Plist-Keys PLIST-KEYS list of keyword symbols Returns the keys from a plist. arguments: plist Plist <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/8/index.html
Type: reference

```
Function: GENDL::CHECK-INPUT-SLOTS <-Back Function: Gendl::Check-Input-Slots CHECK-INPUT-SLOTS void input-slots: grammar: = :input-slots ( *) = | | ( * + *) = :settable | :defaulting Also check for special case in which only strings without a symbol following. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/71/index.html
Type: reference

```
Macro: THE-OBJECT <-Back Macro: The-Object THE-OBJECT lisp object Sends the reference-chain to object , which must be specified as a Lisp expression (e.g. a variable) which evaluates to a GDL object. arguments: reference-chain (&rest) A spliced-in list of symbols naming messages, which can be slots or objects starting from object . For referring to elements of a quantified set, or for passing arguments to GDL functions which take arguments, use parentheses around the message name and enclose the quantified element index or function arguments after the message name example: This example sends the length message to the ``zeroth'' element of the quantified set of arms contained in the body which is contained in the robot which is contained in object : (the-object object robot body (arms 0) length) <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/40/index.html
Type: reference

```
Function: MAKE-KEYWORD <-Back Function: Make-Keyword MAKE-KEYWORD keyword symbol Converts given strings to a keyword. If any of the given arguments is not a string, it will be converted to one with (format nil "~a" string). arguments: strings &rest Strings <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/70/index.html
Type: reference

```
Macro: THE-ELEMENT <-Back Macro: The-Element THE-ELEMENT lisp object [macro] Acts similarly to the-object for each element of an aggregate, within the context of a list-elements , append-elements , max-of-elements , min-of-elements , sum-elements , or a query operator (query operators are not yet documented). arguments: args (&rest) Standard reference chain applicable to the element <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/69/index.html
Type: reference

```
Macro: THE-CHILD <-Back Macro: The-Child THE-CHILD similar to ``the,'' but used to refer to the child part from within an :objects or :hidden-objects specification . This is often used for sending the index message to an element of a quantified set. arguments: reference-chain (&rest) A spliced-in list of symbols naming messages relative to the child object <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/23/index.html
Type: reference

```
Function: FIND-MESSAGES-USED-BY <-Back Function: Find-Messages-Used-By FIND-MESSAGES-USED-BY list of pairs of instance/keyword This returns the list of direct dependants of a given message in a given instance. Note that this is not recursive; if you want to generate a tree, then you have to call this recursively yourself. If you want an easy way to remember the meaning of dependant and dependency: You have a dependency on caffeine. Your children are your dependants. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/33/index.html
Type: reference

```
Function: LEAST <-Back Function: Least LEAST list Returns the member of list which returns the minimum numerical value when function is applied to it. As second value is returned which is the actual minimum value (the return-value of function as applied). This function comes from the Paul Graham book ANSI Common Lisp . arguments: function Function list List <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/60/index.html
Type: reference

```
Function: ROUND-TO-NEAREST <-Back Function: Round-To-Nearest ROUND-TO-NEAREST number Rounds number to the nearest interval . arguments: number Number interval Number <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/63/index.html
Type: reference

```
Macro: SET-FORMAT-SLOT <-Back Macro: Set-Format-Slot SET-FORMAT-SLOT void [macro] Sets the value of the given slot within the context of the current with-format output format object. arguments: slot-name Symbol value Lisp Value <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/41/index.html
Type: reference

```
Function: MAKE-OBJECT <-Back Function: Make-Object MAKE-OBJECT gdl object Instantiates an object with specified initial values for input-slots. arguments: object-name Symbol Should name a GDL object type arguments spliced-in plist A plist of keyword symbols and values for initial input-slots <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/4/index.html
Type: reference

```
Function: GENDL::CHECK-DOCUMENTATION <-Back Function: Gendl::Check-Documentation CHECK-DOCUMENTATION plist containing keys :description :author :examples :date :version <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/58/index.html
Type: reference

```
Function: REMOVE-PLIST-ENTRY <-Back Function: Remove-Plist-Entry REMOVE-PLIST-ENTRY plist Returns a new plist sans any key/value pairs where the plist key is eql to the given key. Optionally a different test than #'eql can be specified with the :test keyword argument. arguments: plist Plist The source plist key matching key, typically a keyword symbol The key to target for removal keyword arguments: test predicate equality function taking two arguments , Default Value: #'EQL The function to use for matching examples: (remove-plist-entry (list :a "a" :b :a) :a) <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/32/index.html
Type: reference

```
Function: LASTCAR <-Back Function: Lastcar LASTCAR lisp object Returns the last element of list . arguments: list List <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/2/index.html
Type: reference

```
Macro: APPEND-ELEMENTS <-Back Macro: Append-Elements APPEND-ELEMENTS list of objects [macro] Returns an appended list of expression from each element of an aggregate, with an optional filter. arguments: aggregate GDL aggregate object (e.g. from a :sequence (:size .) :object specification) optional arguments: expression Expression using the-element , Default Value: NIL Similar to a the-object reference, which should return a list <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/2)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/57/index.html
Type: reference

```
Function: READ-SNAPSHOT <-Back Function: Read-Snapshot READ-SNAPSHOT gdl instance Reads the snapshot data from stream, from the string, or from file indicated by filename. If no optional keyword object argument is given, a new GDL instance based on the data in the snapshot file is returned. If an object is given, the object should be compatible in type to that specified in the snapshot file, and this existing object will be modified to contain the set slot values and toplevel inputs as specified in the snapshot file. keyword arguments: filename String or pathname , Default Value: "/tmp/snap.gdl" File to be read. If either string or stream is specified, this will not be used string String of data , Default Value: NIL The actual snapshot contents, stored in a string.
```

---

## index.html (chunk 2/2)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/57/index.html
Type: reference

```
If stream is specified, this will not be used stream Stream open for input , Default Value: NIL A stream from which the snapshot data can be read keep-bashed-values? Boolean , Default Value: NIL Indicates whether to keep the currently bashed values in object before reading snap values into it object GDL object , Default Value: NIL Existing object to be modified with restored values <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/28/index.html
Type: reference

```
Function: HALF <-Back Function: Half HALF number Returns the result of dividing num by the integer 2 . The type of the returned number will depend on the type of num . arguments: num Number <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/26/index.html
Type: reference

```
Macro: FORMAT-SLOT <-Back Macro: Format-Slot FORMAT-SLOT lisp object [macro] Returns the value of the given slot within the context of the current with-format output format object. arguments: slot-name Symbol <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/15/index.html
Type: reference

```
Macro: DEFINE-FORMAT <-Back Macro: Define-Format DEFINE-FORMAT standard-class [macro] Defines a standard GDL output format for use with GDL views. arguments: name Symbol mixin-list List of symbols keyword arguments: documentation Plist containing keys and strings for author, description, etc , Default Value: NIL slots List of lists or symbols , Default Value: NIL If a list, the list should contain a symbol, a default value, and optionally a documentation string. If a symbol, this is the name of the slot and there will be no default value functions List of format-function definitions , Default Value: NIL Each definition is made up of a symbol, an argument-list, and a body <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/54/index.html
Type: reference

```
Macro: PRINT-MESSAGES <-Back Macro: Print-Messages PRINT-MESSAGES [macro] void Prints the specified GDL object messages (i.e. slots) and their current values to standard output. arguments: vars unquoted symbols (&rest argument) <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/5/index.html
Type: reference

```
Function: GENDL::CHECK-FLOATING-STRING <-Back Function: Gendl::Check-Floating-String CHECK-FLOATING-STRING check for special case in which documentation isn't followed by symbol spec <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/18/index.html
Type: reference

```
Macro: DEFINE-OBJECT-AMENDMENT <-Back Macro: Define-Object-Amendment DEFINE-OBJECT-AMENDMENT supplements or alters an existing gdl object definition Syntax is similar to that for define-object . Note that there is currently no way to undefine messages defined with this macro, other than redefining the original object or restarting the GDL session. Support for surgically removing messages will be added in a future GenDL release. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/61/index.html
Type: reference

```
Function: SAFE-FLOAT <-Back Function: Safe-Float SAFE-FLOAT double-float number Coerces number to a double-precision floating-point number if possible. If this is not possible, returns 0.0d0 (i.e. zero in the form of a double-precision floating-point number). arguments: number Number <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/73/index.html
Type: reference

```
Function: UNDEFINE-OBJECT <-Back Function: Undefine-Object UNDEFINE-OBJECT nil Clears all definitions associated with object-name from the currently running GDL session. arguments: object-name Non-keyword Symbol naming a GDL object type <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/21/index.html
Type: reference

```
Function: GENDL::FIND-DEPENDANTS <-Back Function: Gendl::Find-Dependants FIND-DEPENDANTS list of pairs of instance/keyword Synonymous with find-messages-used-by. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/72/index.html
Type: reference

```
Function: TWICE <-Back Function: Twice TWICE number Returns the result of multiplying num by the integer 2 . The type of the returned number will depend on the type of num . arguments: num Number <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/82/index.html
Type: reference

```
Function: ^2 <-Back Function: ^2 ^2 number Return number raised to the power two (2). arguments: number Number <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/29/index.html
Type: reference

```
Macro: IGNORE-ERRORS-WITH-BACKTRACE <-Back Macro: Ignore-Errors-With-Backtrace IGNORE-ERRORS-WITH-BACKTRACE like ignore-errors, but in case of failure, return backtrace string as third value . <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/30/index.html
Type: reference

```
Function: INDEX-FILTER <-Back Function: Index-Filter INDEX-FILTER list Returns all elements of list for whose index (starting at zero) the function fn returns non-NIL. arguments: fn Function object (e g. a lambda expression) list List <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/79/index.html
Type: reference

```
Function: WRITE-PLIST <-Back Function: Write-Plist WRITE-PLIST pretty-prints a plist to a file with standard i/o syntax . keyword arguments: plist List , Default Value: NIL The list to be printed to the file output-path Pathname of a file , Default Value: NIL The file to be created or superseded <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/11/index.html
Type: reference

```
Function: GENDL::CHECK-TRICKLE-DOWN-SLOTS <-Back Function: Gendl::Check-Trickle-Down-Slots CHECK-TRICKLE-DOWN-SLOTS functions: | :methods grammar: = :trickle-down-slots ( *) <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/38/index.html
Type: reference

```
Function: LOAD-QUICKLISP <-Back Function: Load-Quicklisp LOAD-QUICKLISP void This is intended for pre-built Gendl or GDL images. If the preconfigured quicklisp load file exists, load it. You can customize quicklisp location by setting global *quicklisp-home* or passing :path keyword argument to this function. keyword arguments: path Pathname or string , Default Value: *QUICKLISP-HOME* Quicklisp location <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/35/index.html
Type: reference

```
Function: LIST-OF-N-NUMBERS <-Back Function: List-Of-N-Numbers LIST-OF-N-NUMBERS returns a list of n numbers equally spaced between bounds num1 and num2, inclusive . <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/13/index.html
Type: reference

```
Function: CYCLIC-NTH <-Back Function: Cyclic-Nth CYCLIC-NTH lisp object Returns nth from the list, or wraps around if nth is greater than the length of the list. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/1/index.html
Type: reference

```
Function: ALWAYS <-Back Function: Always ALWAYS t Always returns the value T regardless of arg . arguments: arg Lisp object Ignored <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/31/index.html
Type: reference

```
Function: ISO-8601-DATE <-Back Function: Iso-8601-Date ISO-8601-DATE string Returns the ISO8601 formatted date and possibly time from a Common Lisp universal time integer, e.g. 2007-11-30 or 2007-11-30T13:45:10 <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/53/index.html
Type: reference

```
Function: PLIST-VALUES <-Back Function: Plist-Values PLIST-VALUES list of lisp objects Returns the values from a plist. arguments: plist Plist <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/49/index.html
Type: reference

```
Function: NEVER <-Back Function: Never NEVER nil Always returns the value NIL regardless of arg . arguments: arg Lisp object Ignored <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/46/index.html
Type: reference

```
Function: MOST <-Back Function: Most MOST list Returns the member of list which returns the maximum numerical value when function is applied to it. As second value is returned which is the actual maximum value (the return-value of function as applied). This function comes from the Paul Graham book ANSI Common Lisp . arguments: function Function list List <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/44/index.html
Type: reference

```
Macro: MAX-OF-ELEMENTS <-Back Macro: Max-Of-Elements MAX-OF-ELEMENTS number [macro] Returns the maximum of expression from each element of an aggregate, with an optional filter. arguments: aggregate GDL aggregate object (e.g. from a :sequence (:size .) :object specification) optional arguments: expression Expression using the-element , Default Value: NIL Similar to a the-object reference, which should return a number <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/74/index.html
Type: reference

```
Function: UNIVERSAL-TIME-FROM-ISO-8601 <-Back Function: Universal-Time-From-Iso-8601 UNIVERSAL-TIME-FROM-ISO-8601 integer representing common lisp universal time Returns the universal time from a date formatted as an iso-8601 date, optionally with time, e.g. 2012-07-08 or 2012-07-08T13:33 or 2012-07-08T13:33:00 <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/45/index.html
Type: reference

```
Macro: MIN-OF-ELEMENTS <-Back Macro: Min-Of-Elements MIN-OF-ELEMENTS number [macro] Returns the minimum of expression from each element of an aggregate, with an optional filter. arguments: aggregate GDL aggregate object (e.g. from a :sequence (:size .) :object specification) optional arguments: expression Expression using the-element , Default Value: NIL Similar to a the-object reference, which should return a number <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/17/index.html
Type: reference

```
Macro: DEFINE-OBJECT <-Back Macro: Define-Object DEFINE-OBJECT defines a standard gdl object Please see the document USAGE.TXT for an overview of define-object syntax. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/2)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/81/index.html
Type: reference

```
Macro: WRITE-THE-OBJECT <-Back Macro: Write-The-Object WRITE-THE-OBJECT lisp object [macro] Typcially used only to send output, not for the return value. This macro is used within the body of a with-format . It sends the reference-chain to object , which must be specified as a Lisp expression (e.g. a variable) which evaluates to a GDL object. The reference-chain must terminate with an output-function defined for the combination of the output-format specified in the enclosing with-format , and the object identified by object . arguments: reference-chain (&rest) A spliced-in list of symbols naming messages, which can be slots or objects starting from object , terminating with the name of an output-function.
```

---

## index.html (chunk 2/2)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/81/index.html
Type: reference

```
For referring to elements of a quantified set, or for passing arguments to GDL functions which take arguments, use parentheses around the message name and enclose the quantified element index or function arguments after the message name <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/62/index.html
Type: reference

```
Function: SAFE-SORT <-Back Function: Safe-Sort SAFE-SORT list Nondestructive analog of the Common Lisp sort function. Returns a freshly created list. arguments: list List The list to be sorted rest arguments: args Argument list Identical to the arguments for Common Lisp sort <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/43/index.html
Type: reference

```
Function: MAPTREE <-Back Function: Maptree MAPTREE list Returns the results of applying fn to each GDL object in the object tree rooted at node in a ``depth-first'' tree traversal. arguments: node GDL object fn Function Operates on a single argument which is a GDL object optional arguments: accept? Function , Default Value: #'ALWAYS Determines which nodes to accept in the final result prune? Function , Default Value: #'NEVER Determines which nodes to prune from the tree traversal get-children Keyword symbol :children or Function , Default Value: :CHILDREN Function applied to a given node to get its children. The default, keyword symbol :children, uses the node's normal children as returned by (the-object node children) <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/19/index.html
Type: reference

```
Function: DIV <-Back Function: Div DIV floating-point number Divides using rational division and converts the result (which may be a pure rational number) to a floating-point number. arguments: numerator Number denominator Number optional arguments: more-denominators (&rest) , Default Value: NIL More numbers to divide by <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/3/index.html
Type: reference

```
Function: GENDL::CHECK-COMPUTED-SLOTS <-Back Function: Gendl::Check-Computed-Slots CHECK-COMPUTED-SLOTS void computed-slots: grammar: = :computed-slots ( *) = | ( * + +) = :settable | :uncached Also check for special case in which only strings without a symbol following. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/67/index.html
Type: reference

```
Macro: SUM-ELEMENTS <-Back Macro: Sum-Elements SUM-ELEMENTS number [macro] Returns the sum of expression from each element of an aggregate, with an optional filter. arguments: aggregate GDL aggregate object (e.g. from a :sequence (:size .) :object specification) optional arguments: expression Expression using the-element , Default Value: NIL Similar to a the-object reference, which should return a number <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/6/index.html
Type: reference

```
Function: GENDL::CHECK-FORM <-Back Function: Gendl::Check-Form CHECK-FORM general function that, given a predicate, validates all tokens in a slot declaration form <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/3)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/12/index.html
Type: reference

```
Function: CL-LITE <-Back Function: Cl-Lite CL-LITE Traverses pathname in an alphabetical depth-first order, compiling and loading any lisp files found in source/ subdirectories. A lisp source file will only be compiled if it is newer than the corresponding compiled fasl binary file, or if the corresponding compiled fasl binary file does not exist. A bin/source/ will be created, as a sibling to each source/ subdirectory, to contain the compiled fasl files. If the :create-fasl? keyword argument is specified as non-nil, a concatenated fasl file, named after the last directory component of pathname, will be created in the (glisp:temporary-directory). [Note: this new documentation still needs proper formatting] If the :create-asd-file? keyword argument is specified as non-nil, a .asd file suitable for use with ASDF will be emitted into the directory indicated by the pathname argument.
```

---

## index.html (chunk 2/3)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/12/index.html
Type: reference

```
ble for use with ASDF will be emitted into the directory indicated by the pathname argument. Note that asdf: (Another System Definition Utility), possibly with help of Quicklisp, is (as of 2013-03-12) the recommended way for handling Common Lisp system modules. As of version 2.31.9, ASDF is also capable of generating fasl "bundle" files as with the :create-fasl? argument to cl-lite. For the :author, :version, and :license arguments in the generated .asd file, the files author.isc, version.isc, and license.isc, respectively, are consulted, if they exist. They are searched for first in the codebase toplevel directory (the pathname argument to this function), then in the (user-homedir-pathname). The version defaults to the current ISO-8601 date without dashes, e.g. "20130312". Please see the Genworks Documentation for an overview of Quicklisp and ASDF, and see the Quicklisp and ASDF project documentation for detailed information.
```

---

## index.html (chunk 3/3)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/12/index.html
Type: reference

```
Genworks Documentation for an overview of Quicklisp and ASDF, and see the Quicklisp and ASDF project documentation for detailed information. The source code for Quicklisp and ASDF should also be included with your Gendl distribution, and these are typically loaded by default into the development environment. For additional inputs to the cl-lite function, please see codebase-directory-node object for additional inputs (which can be given as keyword args to this function). <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/77/index.html
Type: reference

```
Macro: WITH-FORMAT-SLOTS <-Back Macro: With-Format-Slots WITH-FORMAT-SLOTS void [macro] Wrap this around a body of code which should have access to multiple slots from the context of the current with-format output format object. arguments: slots List of Symbols <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/9/index.html
Type: reference

```
Function: GENDL::CHECK-OBJECTS <-Back Function: Gendl::Check-Objects CHECK-OBJECTS void hidden-objects: and :objects grammar: = (hidden-):objects ( *) = ( * *) = <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/51/index.html
Type: reference

```
Function: NUMBER-ROUND <-Back Function: Number-Round NUMBER-ROUND number Returns number rounded to decimal-places decimal places. arguments: number Number decimal-places Integer <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/function-docs/dokumentation/24/index.html
Type: reference

```
Function: FIND-MESSAGES-WHICH-USE <-Back Function: Find-Messages-Which-Use FIND-MESSAGES-WHICH-USE list of pairs of instance/keyword This returns the list of direct dependencies of a given message in a given instance. Note that this is not recursive; if you want to generate a tree, then you have to call this recursively yourself. If you want an easy way to remember the meaning of dependant and dependency: You have a dependency on caffeine. Your children are your dependants. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

