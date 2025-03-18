# Gendl Documentation - package_2_objects

## index.html (chunk 1/7)
Source: yadd-reference/package-dokumentations/2/object-docs/dokumentation/1/index.html
Type: reference

```
GendL Application - GDL-APP Package Documentation Object: GDL-APP (The :GENDL Package) Mixins: GDL-APP-SCRIPTS-MIXIN, VANILLA-MIXIN Author Dave Cooper, Genworks International Description This object serves as the driver for the build process for GDL runtime applications. There is also an undocumented function called make-gdl-app ; in order to perform a runtime build process, simply make an instance of this object with the appropriate input values, and invoke (the make!) on it, or call make-gdl-app with the same arguments as the input-slot you give to this object. Input Slots (optional) APPLICATION-FASLS list of pathnames This list should contain the pre-compiled fasls for your GDL application, in correct load order. These can be produced, for example, by calling genworks:cl-lite with the :create-fasl? keyword argument set to t .
```

---

## index.html (chunk 2/7)
Source: yadd-reference/package-dokumentations/2/object-docs/dokumentation/1/index.html
Type: reference

```
create-fasl? keyword argument set to t . If you are using the ASDF build management system, note that ASDF3 is now capable of producing a single fasl file for your application including its ASDF/Quicklisp dependencies, using (asdf:operate 'asdf:monolithic-compile-bundle-op :your-application-system-name) (asdf:output-file 'asdf:monolithic-compile-bundle-op :your-application-system-name) See the ASDF documentation for details. APPLICATION-NAME string The name which will be used for your application's executable and possibly image file. Defaults to "gdl-test-runtime". DESTINATION-DIRECTORY pathname Indicates the directory to be created or overwritten for producing the runtime distribution. Defaults to a directory called (the application-name) in the system temporary directory, returned by (glisp:temporary-folder) . GDLINIT-CONTENT string The contents of this string will be copied to a file gdlinit.cl and placed in the destination-directory. Default is empty string.
```

---

## index.html (chunk 3/7)
Source: yadd-reference/package-dokumentations/2/object-docs/dokumentation/1/index.html
Type: reference

```
isp:temporary-folder) . GDLINIT-CONTENT string The contents of this string will be copied to a file gdlinit.cl and placed in the destination-directory. Default is empty string. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. LISP-HEAP-SIZE number The size of the reserved space which will be requested from the OS when the produced application starts up. Defaults to 800000000 (eight hundred million) bytes. OVERWRITE-VALIDATION-FUNCTION function, t, or nil Validates the target of overwrite? before deleting. T is unconditional "yes" - use at your own risk. OVERWRITE? boolean Indicates whether a build will overwrite a previously existing destination directory. Defaults to nil. POST-LOAD-FORM lisp expression This form will be evaluated in the image being built, after the loading of application-fasls is complete. Defaults to nil. POST-MAKE-FUNCTION lisp function of zero arguments .
```

---

## index.html (chunk 4/7)
Source: yadd-reference/package-dokumentations/2/object-docs/dokumentation/1/index.html
Type: reference

```
M lisp expression This form will be evaluated in the image being built, after the loading of application-fasls is complete. Defaults to nil. POST-MAKE-FUNCTION lisp function of zero arguments . This function will be run in the initiating image after the build is complete. PRE-LOAD-FORM lisp expression This form will be evaluated in the image being built, before the loading of application-fasls begins, but after the GDL runtime modules are loaded. Defaults to nil. PRE-MAKE-FUNCTION lisp function of zero arguments . This function will be run in the initiating image before the build is begun. RESTART-APP-FUNCTION Lambda expression with empty argument list or symbol naming a function with no arguments. This will be run when the runtime application starts up. The alternative to using this to achieve initializations is to put expressions in a gdlinit.cl or .gdlinit.cl in the application directory or user home directory. Defaults to nil.
```

---

## index.html (chunk 5/7)
Source: yadd-reference/package-dokumentations/2/object-docs/dokumentation/1/index.html
Type: reference

```
hieve initializations is to put expressions in a gdlinit.cl or .gdlinit.cl in the application directory or user home directory. Defaults to nil. RESTART-INIT-FUNCTION Lambda expression with empty argument list or symbol naming a function with no arguments. This will be run when the runtime application starts up. The alternative to using this to achieve initializations is to put expressions in a gdlinit.cl or .gdlinit.cl in the application directory or user home directory. Defaults to nil. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places.
```

---

## index.html (chunk 6/7)
Source: yadd-reference/package-dokumentations/2/object-docs/dokumentation/1/index.html
Type: reference

```
a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. GDL Functions MAKE! void Does the application build and creates or replaces (the destination-directory) . Examples (in-package :gdl-user) (make-gdl-app :application-name "moon-shot" :destination-directory "/tmp/moon-shot/" :overwrite? t :application-fasls (list "/fasl-home/booster-rocket.fasl" "/fasl-home/lunar-module.fasl")) Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 7/7)
Source: yadd-reference/package-dokumentations/2/object-docs/dokumentation/1/index.html
Type: reference

```
ter-rocket.fasl" "/fasl-home/lunar-module.fasl")) Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/3)
Source: yadd-reference/package-dokumentations/2/object-docs/dokumentation/0/index.html
Type: reference

```
GendL Application - BASE-RULE-OBJECT Package Documentation Object: BASE-RULE-OBJECT (The :GENDL Package) Mixins: VANILLA-MIXIN Description Encapsulates a basic computation, usually to be displayed to the user. Typically this would be used as a mixin into a more sophisticated rule-object, but the type can be used to detect objects which should be processed as "rules." Input Slots (optional) HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). RULE-DESCRIPTION string Short description of the rule (generally one line). Defaults to NIL. RULE-DESCRIPTION-HELP string Verbose description of the purpose of the rule. RULE-RESULT string The basic return-value, or result, of evaluating the rule. RULE-RESULT-HELP string Verbose description of how the rule result is computed.
```

---

## index.html (chunk 2/3)
Source: yadd-reference/package-dokumentations/2/object-docs/dokumentation/0/index.html
Type: reference

```
f the purpose of the rule. RULE-RESULT string The basic return-value, or result, of evaluating the rule. RULE-RESULT-HELP string Verbose description of how the rule result is computed. RULE-TITLE string Title to be used with the rule object. Defaults to NIL. SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY string Determines the rule's default name in various internal GDL contexts. Defaults to the rule-title , or "Unnamed Rule" if rule-title is NIL. SUPPRESS-DISPLAY? boolean Determines whether the rule is displayed by default in reports etc. VIOLATED? boolean Indicates whether this rule violates a standard condition. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL.
```

---

## index.html (chunk 3/3)
Source: yadd-reference/package-dokumentations/2/object-docs/dokumentation/0/index.html
Type: reference

```
a standard condition. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/2)
Source: yadd-reference/package-dokumentations/2/object-docs/dokumentation/3/index.html
Type: reference

```
GendL Application - NULL-OBJECT Package Documentation Object: NULL-OBJECT (The :GENDL Package) Mixins: VANILLA-MIXIN Description A part with no geometric representation and no children. Use this in a conditional :type expression if you want to turn off a branch of the tree conditionally. Input Slots (optional) HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places.
```

---

## index.html (chunk 2/2)
Source: yadd-reference/package-dokumentations/2/object-docs/dokumentation/3/index.html
Type: reference

```
ion STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/15)
Source: yadd-reference/package-dokumentations/2/object-docs/dokumentation/7/index.html
Type: reference

```
GendL Application - VANILLA-MIXIN* Package Documentation Object: VANILLA-MIXIN* (The :GENDL Package) Mixins: STANDARD-OBJECT Description Vanilla-Mixin is automatically inherited by every object created in GDL. It provides basic messages which are common to all GDL objects defined with the define-object macro, unless :no-vanilla-mixin t is specified at the toplevel of the define-object form. Input Slots (optional) HIDDEN? boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. ROOT gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY string or list of strings Determines how the name of objects of this type will be printed in most places.
```

---

## index.html (chunk 2/15)
Source: yadd-reference/package-dokumentations/2/object-docs/dokumentation/7/index.html
Type: reference

```
ck as a plist with error information STRINGS-FOR-DISPLAY string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VISIBLE-CHILDREN list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Computed Slots AGGREGATE gdl instance In an element of a sequence, this is the container object which holds all elements. ALL-MIXINS list of symbols Lists all the superclasses of the type of this object. CHILDREN list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. DIRECT-MIXINS list of symbols Lists the direct superclasses of the type of this object. FIRST? boolean For elements of sequences, T iff there is no previous element.
```

---

## index.html (chunk 3/15)
Source: yadd-reference/package-dokumentations/2/object-docs/dokumentation/7/index.html
Type: reference

```
ists. DIRECT-MIXINS list of symbols Lists the direct superclasses of the type of this object. FIRST? boolean For elements of sequences, T iff there is no previous element. HIDDEN-CHILDREN list of gdl instances All objects from the :hidden-objects specification, including elements of sequences as flat lists. INDEX integer Sequential index number for elements of a sequence, NIL for singular objects. LAST? boolean For elements of sequences, T iff there is no next element. LEAF? boolean T if this object has no children, NIL otherwise. LEAVES list of gdl objects A Collection of the leaf nodes of the given object. NAME-FOR-DISPLAY keyword symbol The part's simple name, derived from its object specification in the parent or from the type name if this is the root instance. NEXT gdl instance For elements of sequences, returns the next part in the sequence. PARENT gdl instance The parent of this object, or NIL if this is the root object.
```

---

## index.html (chunk 4/15)
Source: yadd-reference/package-dokumentations/2/object-docs/dokumentation/7/index.html
Type: reference

```
ements of sequences, returns the next part in the sequence. PARENT gdl instance The parent of this object, or NIL if this is the root object. PREVIOUS gdl instance For elements of sequences, returns the previous part in the sequence. ROOT-PATH list of symbols or of pairs of symbol and integer Indicates the path through the instance hierarchy from the root to this object. Can be used in conjunction with the follow-root-path GDL function to return the actual instance. ROOT-PATH-LOCAL list of symbols or of pairs of symbol and integer Indicates the path through the instance hierarchy from the local root to this object. Can be used in conjunction with the follow-root-path GDL function to return the actual instance. ROOT? boolean T iff this part has NIL as its parent and therefore is the root node. SAFE-HIDDEN-CHILDREN list of gdl instances All objects from the :hidden-objects specification, including elements of sequences as flat lists.
```

---

## index.html (chunk 5/15)
Source: yadd-reference/package-dokumentations/2/object-docs/dokumentation/7/index.html
Type: reference

```
de. SAFE-HIDDEN-CHILDREN list of gdl instances All objects from the :hidden-objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information TYPE symbol The GDL Type of this object. GDL Functions DOCUMENTATION plist Returns the :documentation plist which has been specified the specific part type of this instance. FOLLOW-ROOT-PATH gdl instance Using this instance as the root, follow the reference chain represented by the given path. arguments: path List of Symbols or Pairs of Symbol and Integer MESSAGE-DOCUMENTATION string This is synonymous with slot-documentation MESSAGE-LIST list of keyword symbols Returns the messages (slots, objects, and functions) of this object, according to the filtering criteria as specified by the arguments. keyword arguments: category Keyword , Default Value: :ALL Either :all or the individual category of messages to be returned.
```

---

## index.html (chunk 6/15)
Source: yadd-reference/package-dokumentations/2/object-docs/dokumentation/7/index.html
Type: reference

```
cified by the arguments. keyword arguments: category Keyword , Default Value: :ALL Either :all or the individual category of messages to be returned. This can be one of: :computed-slots :settable-computed-slots :required-input-slots :optional-input-slots :defaulted-input-slots :query-slots :functions :objects :quantified-objects :hidden-objects :quantified-hidden-objects message-type Keyword Symbol, :local or :global , Default Value: :GLOBAL Indicates whether to return messages only from the local specific part type, or from all superclasses (mixins) as well return-category? Boolean , Default Value: NIL Indicates whether or not the category of each message should be returned before each message in the returned list base-part-type Symbol naming a GDL Part Type , Default Value: NIL Indicates a ``base'' part from which no messages should be returned, nor should messages be returned from superclasses (mixins) of this base part.
```

---

## index.html (chunk 7/15)
Source: yadd-reference/package-dokumentations/2/object-docs/dokumentation/7/index.html
Type: reference

```
a ``base'' part from which no messages should be returned, nor should messages be returned from superclasses (mixins) of this base part. If NIL (the default), messages are considered from all superclasses sort-order Keyword Symbol , Default Value: :UNSORTED One of: :unsorted , :by-category , or :by-name filter Function Object of two arguments or :normal , Default Value: :NORMAL If a function object, applies this function to each returned category and message keyword, and filters out all pairs for which the function returns NIL. If :normal (the default), then no filtering is done MIXINS list of symbols Returns the names of the immediate superclasses of this object. keyword arguments: local? Boolean , Default Value: T Indicates whether to give only direct mixins or all mixins from the entire inheritance hierarchy RESTORE-ALL-DEFAULTS! void Restores all settable-slots in this instance to their default values.
```

---

## index.html (chunk 8/15)
Source: yadd-reference/package-dokumentations/2/object-docs/dokumentation/7/index.html
Type: reference

```
e inheritance hierarchy RESTORE-ALL-DEFAULTS! void Restores all settable-slots in this instance to their default values. RESTORE-ROOT! Multiple Values: Total root-paths affected and total slots affected. Reverts any "remembered" bashed slots, starting from the root, to their default values. Note that any call to `set-slot!` or `set-slots!` or any use of web form-controls will result in "remembered values". Note that there is a `:remember?` keyword argument to `set-slot!` which defaults to `t` but if you specify it as `nil`, you can bash values without having them "remembered" and such slots would not be affected by this function. RESTORE-SLOT-DEFAULT! nil Restores the value of the given slot to its default, thus ``undoing'' any forcibly set value in the slot. Any dependent slots in the tree will respond accordingly when they are next demanded. Note that the slot must be specified as a keyword symbol (i.e.
```

---

## index.html (chunk 9/15)
Source: yadd-reference/package-dokumentations/2/object-docs/dokumentation/7/index.html
Type: reference

```
e will respond accordingly when they are next demanded. Note that the slot must be specified as a keyword symbol (i.e. prepended with a colon (``:'')), otherwise it will be evaluated as a variable according to normal Lisp functional evaluation rules. arguments: slot Keyword Symbol key: (force? "Boolean. Specify as t if you want to force non-settable slots to recompute (e.g. reading from databases or external files). Defaults to nil.") RESTORE-SLOT-DEFAULTS! nil Restores the value of the given slots to their defaults, thus ``undoing'' any forcibly set values in the slots. Any dependent slots in the tree will respond accordingly when they are next demanded. Note that the slots must be specified as keyword symbols (i.e. prepended with colons (``:'')), otherwise they will be evaluated as variables according to normal Lisp functional evaluation rules.
```

---

## index.html (chunk 10/15)
Source: yadd-reference/package-dokumentations/2/object-docs/dokumentation/7/index.html
Type: reference

```
bles according to normal Lisp functional evaluation rules. arguments: slots List of Keyword Symbols keyword arguments: force? Boolean , Default Value: *FORCE-RESTORE-SLOT-DEFAULT?* Indicates whether the slot values should be unbound, regardless of whether it had actually been bashed previously RESTORE-TREE! void Restores all settable-slots in this instance, and recursively in all descendant instances, to their default values. SET-SLOT! nil Forcibly sets the value of the given slot to the given value. The slot must be defined as :settable for this to work properly. Any dependent slots in the tree will respond accordingly when they are next demanded. Note that the slot must be specified as a keyword symbol (i.e. prepended with a colon (``:'')), otherwise it will be evaluated as a variable according to normal Lisp functional evaluation rules. Note also that this must not be called (either directly or indirectly) from within the body of a Gendl computed-slot.
```

---

## index.html (chunk 11/15)
Source: yadd-reference/package-dokumentations/2/object-docs/dokumentation/7/index.html
Type: reference

```
cording to normal Lisp functional evaluation rules. Note also that this must not be called (either directly or indirectly) from within the body of a Gendl computed-slot. The caching and dependency tracking mechanism in Gendl will not work properly if this is called from the body of a computed-slot, and furthermore a runtime error will be generated. arguments: slot Keyword Symbol value Lisp Object (e.g. Number, String, List, etc.) keyword arguments: remember? Boolean , Default Value: T Determines whether to save in current version-tree warn-on-non-toplevel? Boolean , Default Value: T Determines whether to warn if this is called from the body of a cached slot SET-SLOTS! nil Forcibly sets the value of the given slots to the given values. The slots must be defined as :settable for this to work properly. Any dependent slots in the tree will respond accordingly when they are next demanded. Note that the slots must be specified as a keyword symbols (i.e.
```

---

## index.html (chunk 12/15)
Source: yadd-reference/package-dokumentations/2/object-docs/dokumentation/7/index.html
Type: reference

```
properly. Any dependent slots in the tree will respond accordingly when they are next demanded. Note that the slots must be specified as a keyword symbols (i.e. prepended with a colon (``:'')), otherwise they will be evaluated as variables according to normal Lisp functional evaluation rules. arguments: slots-and-values Plist Contains alternating slots and values to which they are to be set warn-on-non-toplevel? Boolean Indicates whether a warning should be issued for calling from inside the body of a cached slot. Default is t SLOT-DOCUMENTATION plist of symbols and strings Returns the part types and slot documentation which has been specified for the given slot, from most specific to least specific in the CLOS inheritance order. Note that the slot must be specified as a keyword symbol (i.e. prepended with a colon (``:'')), otherwise it will be evaluated as a variable according to normal Lisp functional evaluation rules.
```

---

## index.html (chunk 13/15)
Source: yadd-reference/package-dokumentations/2/object-docs/dokumentation/7/index.html
Type: reference

```
.e. prepended with a colon (``:'')), otherwise it will be evaluated as a variable according to normal Lisp functional evaluation rules. arguments: slot Keyword Symbol Names the slot for which documentation is being requested SLOT-SOURCE body of gdl code, in list form . arguments: slot Keyword Symbol Names the slot for which documentation is being requested SLOT-STATUS keyword symbol Describes the current status of the requested slot: :unbound : it has not yet been demanded (this could mean either it has never been demanded, or something it depends on has been modified since the last time it was demanded and eager setting is not enabled). :evaluated : it has been demanded and it is currently bound to the default value based on the code. :set : (for :settable slots only, which includes all required :input-slots) it has been modified and is currently bound to the value to which it was explicitly set.
```

---

## index.html (chunk 14/15)
Source: yadd-reference/package-dokumentations/2/object-docs/dokumentation/7/index.html
Type: reference

```
equired :input-slots) it has been modified and is currently bound to the value to which it was explicitly set. :toplevel : (for root-level object only) its value was passed into the root-level object as a toplevel input at the time of object instantiation. TOGGLE-SLOT! void Sets giving slot to its boolean opposite. &args (slot "Keyword symbol. Name of the slot as a keyword symbol.") UPDATE! void Uncaches all cached data in slots and objects throughout the instance tree from this node, forcing all code to run again the next time values are demanded. This is useful for updating an existing model or part of an existing model after making changes and recompiling/reloading the code of the underlying definitions. Any set (modified) slot values will, however, be preserved by the update. WRITE-SNAPSHOT void Writes a file containing the toplevel inputs and modified settable-slots starting from the root of the current instance.
```

---

## index.html (chunk 15/15)
Source: yadd-reference/package-dokumentations/2/object-docs/dokumentation/7/index.html
Type: reference

```
PSHOT void Writes a file containing the toplevel inputs and modified settable-slots starting from the root of the current instance. Typically this file can be read back into the system using the read-snapshot function. keyword arguments: filename String or pathname , Default Value: "/tmp/snap.gdl" The target file to be written root-paths-to-ignore List of root-paths or nil , Default Value: NIL Any objects with matching root-path will be ignored for the snapshot write Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/3)
Source: yadd-reference/package-dokumentations/2/object-docs/dokumentation/2/index.html
Type: reference

```
GendL Application - MATRIX-SEQUENCE Package Documentation Object: MATRIX-SEQUENCE (The :GENDL Package) Mixins: STANDARD-SEQUENCE , VANILLA-MIXIN Description A matrix sequence quantification is generated as a result of specifying :sequence (:matrix direction-keyword number direction-keyword number)) in an :objects specification. The direction-keywords can be one of :lateral , :longitudinal , and :vertical . The items will be arranged spread out evenly in the directions specified. Centers can also be provided explicitly based on the indices. The indices to a matrix sequence consist of a list of numbers rather than a single number as with a normal sequence. Input Slots (optional) HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy).
```

---

## index.html (chunk 2/3)
Source: yadd-reference/package-dokumentations/2/object-docs/dokumentation/2/index.html
Type: reference

```
specified in :objects. Default is nil. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Computed Slots FIRST [ from QUANTIFICATION ] gdl object Returns the first element of the aggregate.
```

---

## index.html (chunk 3/3)
Source: yadd-reference/package-dokumentations/2/object-docs/dokumentation/2/index.html
Type: reference

```
pically this would be a subset of hidden-children. Defaults to NIL. Computed Slots FIRST [ from QUANTIFICATION ] gdl object Returns the first element of the aggregate. LAST [ from QUANTIFICATION ] gdl object Returns the last element of the aggregate. Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/2)
Source: yadd-reference/package-dokumentations/2/object-docs/dokumentation/5/index.html
Type: reference

```
GendL Application - RADIAL-SEQUENCE Package Documentation Object: RADIAL-SEQUENCE (The :GENDL Package) Mixins: STANDARD-SEQUENCE , VANILLA-MIXIN Description A radial sequence quantification is generated as a result of specifying :sequence (:radial [number-expression])) in an :objects specification. Input Slots (optional) HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places.
```

---

## index.html (chunk 2/2)
Source: yadd-reference/package-dokumentations/2/object-docs/dokumentation/5/index.html
Type: reference

```
or information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/3)
Source: yadd-reference/package-dokumentations/2/object-docs/dokumentation/8/index.html
Type: reference

```
GendL Application - VARIABLE-SEQUENCE Package Documentation Object: VARIABLE-SEQUENCE (The :GENDL Package) Mixins: QUANTIFICATION Description A variable-sequence quantification is generated as a result of specifying :sequence (:indices ...)) in an :objects specification. Unlike a normal sequence quantification (specified with :sequence (:size ...)) ), elements can be surgically inserted and deleted from a variable-sequence. Input Slots (optional) HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists.
```

---

## index.html (chunk 2/3)
Source: yadd-reference/package-dokumentations/2/object-docs/dokumentation/8/index.html
Type: reference

```
ects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Computed Slots FIRST [ from QUANTIFICATION ] gdl object Returns the first element of the aggregate. LAST [ from QUANTIFICATION ] gdl object Returns the last element of the aggregate. GDL Functions DELETE! void Deletes the element identified with the given index.
```

---

## index.html (chunk 3/3)
Source: yadd-reference/package-dokumentations/2/object-docs/dokumentation/8/index.html
Type: reference

```
the aggregate. GDL Functions DELETE! void Deletes the element identified with the given index. arguments: index Integer, Symbol, or other object matching with eql The identifier used when the element was initialized or inserted INSERT! void Inserts a new element identified with the given index. arguments: index Integer, Symbol, or other object matching with eql The identifier to be used to access this element RESET! void Resets the variable sequence to its default list of indices (i.e. clears out any inserted or deleted elements and re-evaluates the expression to compute the original list of indices) Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/3)
Source: yadd-reference/package-dokumentations/2/object-docs/dokumentation/6/index.html
Type: reference

```
GendL Application - STANDARD-SEQUENCE Package Documentation Object: STANDARD-SEQUENCE (The :GENDL Package) Mixins: QUANTIFICATION Description A standard sequence quantification is generated as a result of specifying :sequence (:size [number-expression])) in an :objects specification. Unlike a variable-sequence quantification (specified with :sequence (:indices ...)) ), elements cannot be surgically inserted or deleted from a standard sequence. If a value upon which the [number-expression] depends becomes modified, each member of the sequence will be reinstantiated as it is demanded. Input Slots (optional) HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy).
```

---

## index.html (chunk 2/3)
Source: yadd-reference/package-dokumentations/2/object-docs/dokumentation/6/index.html
Type: reference

```
instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Computed Slots FIRST [ from QUANTIFICATION ] gdl object Returns the first element of the aggregate. LAST [ from QUANTIFICATION ] gdl object Returns the last element of the aggregate.
```

---

## index.html (chunk 3/3)
Source: yadd-reference/package-dokumentations/2/object-docs/dokumentation/6/index.html
Type: reference

```
ted Slots FIRST [ from QUANTIFICATION ] gdl object Returns the first element of the aggregate. LAST [ from QUANTIFICATION ] gdl object Returns the last element of the aggregate. Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/3)
Source: yadd-reference/package-dokumentations/2/object-docs/dokumentation/4/index.html
Type: reference

```
GendL Application - QUANTIFICATION Package Documentation Object: QUANTIFICATION (The :GENDL Package) Mixins: VANILLA-MIXIN Description A quantification is an aggregate created as a result of specifying :sequence (:size ...)) or :sequence (:indices ...)) in an :objects specification. Usually, the elements of a quantified set are referenced by using extra parentheses around the message in the reference chain and using the index number. But the aggregate itself also supports certain messages, documented here. One message, number-of-elements , is not listed in the normal messages section because it is internal. It can be used, and returns an integer representing the cardinality of the aggregate. Input Slots (optional) HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy).
```

---

## index.html (chunk 2/3)
Source: yadd-reference/package-dokumentations/2/object-docs/dokumentation/4/index.html
Type: reference

```
effectively be a hidden-object even if specified in :objects. Default is nil. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Computed Slots FIRST gdl object Returns the first element of the aggregate.
```

---

## index.html (chunk 3/3)
Source: yadd-reference/package-dokumentations/2/object-docs/dokumentation/4/index.html
Type: reference

```
al objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Computed Slots FIRST gdl object Returns the first element of the aggregate. INDEX [ from VANILLA-MIXIN* ] integer Sequential index number for elements of a sequence, NIL for singular objects. LAST gdl object Returns the last element of the aggregate. Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

