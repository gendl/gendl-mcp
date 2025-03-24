# Gendl Documentation - package_2_variables

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/variable-docs/dokumentation/22/index.html
Type: reference

```
Documentation for GENDL::*WITH-FORMAT-DIRECTION <-Back Parameter: Gendl::*With-Format-Direction *WITH-FORMAT-DIRECTION keyword symbol Establishes the default for the :direction format-slot of the base-format. If you want to change this default behavior, you can override this parameter globally or bind it dynamically. Alternatively you can specify a different value for :direction in the call to with-format. Valid keywords are the same as for Common Lisp open or with-open-file. Default is :output. Normally this should not be changed in user code. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/variable-docs/dokumentation/7/index.html
Type: reference

```
Documentation for *COMPILE-DOCUMENTATION-DATABASE?* <-Back Parameter: *Compile-Documentation-Database?* *COMPILE-DOCUMENTATION-DATABASE?* boolean Determines whether documentation strings information will be compiled into compiled files. Defaults to T. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/variable-docs/dokumentation/25/index.html
Type: reference

```
Documentation for GENDL::*WITH-FORMAT-IF-EXISTS* <-Back Parameter: Gendl::*With-Format-If-Exists* *WITH-FORMAT-IF-EXISTS* keyword symbol Establishes the default for the :if-exists format-slot of the base-format. If you want to change this default behavior, you can override this parameter globally or bind it dynamically. Alternatively you can specify a different value for :if-exists in the call to with-format. Valid keywords are the same as for Common Lisp open or with-open-file. Default is :supersede. Example: (let ((*with-format-if-exists* :error)) (with-format (x3d "/tmp/box.x3d") (write-the-object (make-instance 'box :length 100 :width 100 :height 100) cad-output))) (with-format (x3d "/tmp/box.x3d" :if-exists :error) (write-the-object (make-instance 'box :length 100 :width 100 :height 100) cad-output) <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/variable-docs/dokumentation/20/index.html
Type: reference

```
Documentation for *SORT-CHILDREN?* <-Back Parameter: *Sort-Children?* *SORT-CHILDREN?* boolean Determine whether to sort child objects in lexigraphical order. Defaults to nil, which is the behavior previous to gdl1585. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/variable-docs/dokumentation/14/index.html
Type: reference

```
Documentation for GENDL::*ON-SYNTAX-ERROR* <-Back Parameter: Gendl::*On-Syntax-Error* *ON-SYNTAX-ERROR* keyword symbol Can be either :warn or :error. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/variable-docs/dokumentation/27/index.html
Type: reference

```
Documentation for +PHI+ <-Back Constant: +Phi+ +PHI+ number (constant) The Golden Ratio. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/variable-docs/dokumentation/0/index.html
Type: reference

```
Documentation for *ALLOW-NIL-LIST-OF-NUMBERS?* <-Back Parameter: *Allow-Nil-List-Of-Numbers?* *ALLOW-NIL-LIST-OF-NUMBERS?* boolean If set to t, allows list-of-numbers function to return nil instead of error when num1 is greater than num2 with positive increment or vice-versa. Default is nil. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/variable-docs/dokumentation/16/index.html
Type: reference

```
Documentation for *REMEMBER-PREVIOUS-SLOT-VALUES?* <-Back Parameter: *Remember-Previous-Slot-Values?* *REMEMBER-PREVIOUS-SLOT-VALUES?* boolean Determines whether the system keeps track of previous slot values (accessible with previous-value function) after bashings are done. Leave this set to nil to improve memory performance. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/variable-docs/dokumentation/10/index.html
Type: reference

```
Documentation for *CURVE-CHORDS* <-Back Parameter: *Curve-Chords* *CURVE-CHORDS* integer The number of chords to use per Bezier curve when rendering curves as a sequence of straight chords (e.g. in VRML). <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/variable-docs/dokumentation/8/index.html
Type: reference

```
Documentation for *COMPILE-FOR-DGDL?* <-Back Parameter: *Compile-For-Dgdl?* *COMPILE-FOR-DGDL?* boolean Determines whether global methods are defined during compilation to allow calling any message on a gdl remote-object. This functionality is not available in the base by itself, it requires the :gwl system as well. Defaults to nil. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/variable-docs/dokumentation/23/index.html
Type: reference

```
Documentation for *WITH-FORMAT-EXTERNAL-FORMAT* <-Back Parameter: *With-Format-External-Format* *WITH-FORMAT-EXTERNAL-FORMAT* external-format The default for the :external-format format-slot for the base format. Defaults to gdl::*external-text-format*. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/variable-docs/dokumentation/4/index.html
Type: reference

```
Documentation for *COLORS-DEFAULT* <-Back Parameter: *Colors-Default* *COLORS-DEFAULT* plist Should contain keys of at least :foreground and :background , whose values are a color indicator for the default foreground and background of graphics viewports. The default is :black for foreground, and :white for background. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/variable-docs/dokumentation/2/index.html
Type: reference

```
Documentation for *COLOR-TABLE* <-Back Parameter: *Color-Table* *COLOR-TABLE* hash table Built from the *color-plist* , this hash table is keyed on the same keys. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/variable-docs/dokumentation/28/index.html
Type: reference

```
Documentation for 2PI <-Back Constant: 2pi 2PI number (constant) Twice the internal Lisp value for pi. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/variable-docs/dokumentation/26/index.html
Type: reference

```
Documentation for *ZERO-EPSILON* <-Back Parameter: *Zero-Epsilon* *ZERO-EPSILON* number The value used to test for closeness to zero in some functions. Defaults to 0.001 <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/variable-docs/dokumentation/15/index.html
Type: reference

```
Documentation for *OUT-OF-BOUNDS-SEQUENCE-REFERENCE-ACTION* <-Back Parameter: *Out-Of-Bounds-Sequence-Reference-Action* *OUT-OF-BOUNDS-SEQUENCE-REFERENCE-ACTION* keyword symbol :warn, :error, or :silent Determines what happens when you try to access a member of a GDL sequence which is out of bounds. If :warn or :silent, an out-of-bounds reference will simply return nil. If :error, it will throw an error as was the old behavior. Defaults to :error. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/variable-docs/dokumentation/5/index.html
Type: reference

```
Documentation for *COMPILE-CIRCULAR-REFERENCE-DETECTION?* <-Back Parameter: *Compile-Circular-Reference-Detection?* *COMPILE-CIRCULAR-REFERENCE-DETECTION?* boolean This is a compile-time switch. Determines whether the system detects circular references in messages. Defaults to NIL. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/variable-docs/dokumentation/18/index.html
Type: reference

```
Documentation for *RUN-WITH-CIRCULAR-REFERENCE-DETECTION?* <-Back Parameter: *Run-With-Circular-Reference-Detection?* *RUN-WITH-CIRCULAR-REFERENCE-DETECTION?* boolean This is a runtime switch. Determines whether the system detects circular references in messages. Defaults to NIL. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/variable-docs/dokumentation/21/index.html
Type: reference

```
Documentation for *UNDECLARED-PARAMETERS-ENABLED?* <-Back Parameter: *Undeclared-Parameters-Enabled?* *UNDECLARED-PARAMETERS-ENABLED?* boolean This is a compile-time switch. Determines whether the system will handle inputs passed to child parts through :parameters plists, where the input is not declared in any other part as either an input-slot or computed-slot. If you leave this as NIL when compiling your application you may see an improvement in runtime performance of up to 10% as compared to applications compiled with it set to T. Defaults to NIL. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/variable-docs/dokumentation/29/index.html
Type: reference

```
Documentation for PI/2 <-Back Constant: Pi/2 PI/2 number (constant) Half the internal Lisp value for pi. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/variable-docs/dokumentation/11/index.html
Type: reference

```
Documentation for *ENSURE-LISTS-WHEN-BASHING?* <-Back Parameter: *Ensure-Lists-When-Bashing?* *ENSURE-LISTS-WHEN-BASHING?* boolean Determines whether lists are enforced to stay as lists with set-slot-if-needed method of vanilla-mixin. Default is nil. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/variable-docs/dokumentation/13/index.html
Type: reference

```
Documentation for *LOAD-SOURCE-CODE-DATABASE?* <-Back Parameter: *Load-Source-Code-Database?* *LOAD-SOURCE-CODE-DATABASE?* boolean Determines whether pre-compiled source code information will be loaded from compiled files. Defaults to T. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/variable-docs/dokumentation/1/index.html
Type: reference

```
Documentation for *BIAS-TO-DOUBLE-FLOAT?* <-Back Parameter: *Bias-To-Double-Float?* *BIAS-TO-DOUBLE-FLOAT?* boolean Indicates whether the following functions should always return double-floats: half twice . Defaults to nil. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/variable-docs/dokumentation/17/index.html
Type: reference

```
Documentation for GENDL::*ROOT-CHECKING-ENABLED?* <-Back Parameter: Gendl::*Root-Checking-Enabled?* *ROOT-CHECKING-ENABLED?* boolean Determines whether dependency-tracking carries over between objects which do not share a common root. Default is T which means dependency-tracking does not carry over (the checking prevents it). <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/variable-docs/dokumentation/19/index.html
Type: reference

```
Documentation for *RUN-WITH-DEPENDENCY-TRACKING?* <-Back Parameter: *Run-With-Dependency-Tracking?* *RUN-WITH-DEPENDENCY-TRACKING?* boolean This is a runtime switch. Determines whether the system keeps track of object and message dependencies at runtime, thereby enabling the modification of messages and subsequent proper demand-driven recomputation of other messages in the object hierarchy. This switch must be set at the beginning of a session; switching it in the middle of a session (especially from NIL to T) will have unpredictable effects and very likely will result in incorrect operation. Defaults to T. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/variable-docs/dokumentation/3/index.html
Type: reference

```
Documentation for *COLOR-TABLE-DECIMAL* <-Back Parameter: *Color-Table-Decimal* *COLOR-TABLE-DECIMAL* hash table Same as *color-table* except the results are returned as a list of three decimal integers (for Red, Green, Blue) in the range of 0-254. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/variable-docs/dokumentation/6/index.html
Type: reference

```
Documentation for *COMPILE-DEPENDENCY-TRACKING?* <-Back Parameter: *Compile-Dependency-Tracking?* *COMPILE-DEPENDENCY-TRACKING?* boolean This is a compile-time switch. Determines whether the system keeps track of object and message dependencies at runtime, thereby enabling the modification of messages and subsequent proper demand-driven recomputation of other messages in the object hierarchy. This switch must be set at the beginning of a session before comiling all code; switching it in the middle of a session (especially from NIL to T) will have unpredictable effects and very likely will result in incorrect operation. Defaults to T. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/variable-docs/dokumentation/12/index.html
Type: reference

```
Documentation for *LOAD-DOCUMENTATION-DATABASE?* <-Back Parameter: *Load-Documentation-Database?* *LOAD-DOCUMENTATION-DATABASE?* boolean Determines whether pre-compiled documentation strings information will be loaded from compiled files. Defaults to T. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/variable-docs/dokumentation/9/index.html
Type: reference

```
Documentation for *COMPILE-SOURCE-CODE-DATABASE?* <-Back Parameter: *Compile-Source-Code-Database?* *COMPILE-SOURCE-CODE-DATABASE?* boolean Determines whether source code information information will be compiled into compiled files. Defaults to T. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/2/variable-docs/dokumentation/24/index.html
Type: reference

```
Documentation for GENDL::*WITH-FORMAT-IF-DOES-NOT-EXIST* <-Back Parameter: Gendl::*With-Format-If-Does-Not-Exist* *WITH-FORMAT-IF-DOES-NOT-EXIST* keyword symbol Establishes the default for the :if-does-not-exist format-slot of the base-format. If you want to change this default behavior, you can override this parameter globally or bind it dynamically. Alternatively you can specify a different value for :if-does-not-exist in the call to with-format. Valid keywords are the same as for Common Lisp open or with-open-file. Default is :create. Example: (let ((*with-format-if-does-not-exist* :error)) (with-format (x3d "/tmp/box.x3d") (write-the-object (make-instance 'box :length 100 :width 100 :height 100) cad-output))) (with-format (x3d "/tmp/box.x3d" :if-does-not-exist :error) (write-the-object (make-instance 'box :length 100 :width 100 :height 100) cad-output) <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

