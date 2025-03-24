# Gendl Documentation - package_6_objects

## index.html (chunk 1/7)
Source: yadd-reference/package-dokumentations/6/object-docs/dokumentation/0/index.html
Type: reference

```
GendL Application - MENU Package Documentation Object: MENU (The :GEYSR Package) Mixins: SHEET-SECTION Input Slots (optional) BASHEE [ from SKELETON-UI-ELEMENT ] gdl object Object to have its settable computed-slots and/or query-plist set from the fields on the form upon submission. Defaults to self. DOM-ID [ from SKELETON-UI-ELEMENT ] string This is the auto-computed dom-id which should be used for rendering this section. If you use the main-div HTML string for rendering this object as a page section, then you do not have to generate the :div tag yourself - the main-div will be a string of HTML which is wrapped in the correct :div tag already. FORCE-VALIDATION-FOR [ from SKELETON-UI-ELEMENT ] list of gdl objects of type form-control The validation-function will be forced on these objects when a form is submitted, even if the object's html form-control does not happen to be included in the values submitted with the form. Defaults to nil.
```

---

## index.html (chunk 2/7)
Source: yadd-reference/package-dokumentations/6/object-docs/dokumentation/0/index.html
Type: reference

```
when a form is submitted, even if the object's html form-control does not happen to be included in the values submitted with the form. Defaults to nil. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. HTML-SECTIONS [ from SKELETON-UI-ELEMENT ] List of HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded. HTML-SECTIONS-VALID [ from SKELETON-UI-ELEMENT ] List of valid HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded.
```

---

## index.html (chunk 3/7)
Source: yadd-reference/package-dokumentations/6/object-docs/dokumentation/0/index.html
Type: reference

```
is slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded. INCLUDED-CHILDREN keyword `:all` or list of keyword symbols . These dropdown menus will be included in the rendering, or `:all` to include all defined ones. JS-TO-EVAL [ from SKELETON-UI-ELEMENT ] string of valid javascript This Javascript will be send with the Ajax response, and evaluated after the innerHTML for this section has been replaced. ORDERED-FORM-CONTROLS [ from SKELETON-UI-ELEMENT ] list of gdl objects, which should be of type 'base-form-control . [Note -- this slot is not really necessary for protecting out-of-bounds sequence references anymore, the form-control processor protects against this by itself now]. These objects are validated and bashed first, in the order given.
```

---

## index.html (chunk 4/7)
Source: yadd-reference/package-dokumentations/6/object-docs/dokumentation/0/index.html
Type: reference

```
inst this by itself now]. These objects are validated and bashed first, in the order given. If the cardinality of one form-control depends on another as in the example below, then you should list those dependent objects first. Default is nil. examples: ... :computed-slots ((number-of-nozzles (the number-of-nozzles-form value)) (ordered-form-controls (append (list-elements (the inner-flange-form)) (list (the number-of-nozzles-form))))) :objects ((inner-flange-form :type 'menu-form-control :choice-plist (list :hey "hey" :now "now") :default :hey :sequence (:size (the number-of-nozzles))) (number-of-nozzles-form :type 'text-form-control :prompt "Number of Shell Nozzles Required: " :domain :number :default 0) POSSIBLE-NILS [ from SKELETON-UI-ELEMENT ] list of keyword symbols Messages corresponding to form fields which could be missing from form submission (e.g. checkbox fields). Defaults to the names of any children or hidden-children of type menu-form-control or checkbox-form-control.
```

---

## index.html (chunk 5/7)
Source: yadd-reference/package-dokumentations/6/object-docs/dokumentation/0/index.html
Type: reference

```
ding to form fields which could be missing from form submission (e.g. checkbox fields). Defaults to the names of any children or hidden-children of type menu-form-control or checkbox-form-control. PRESET-ALL? [ from SKELETON-UI-ELEMENT ] boolean This switch determines whether all form-controls should be preset before the final setting, in order to allow any interdependencies to be detected for validation or detecting changed values. If this is specified as a non-nil value, then any nil values of (the preset?) on individual form controls will be ignored. If this is specified as nil, then (the preset?) of individual form-controls (default of these is also nil) will be respected. Default is nil. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists.
```

---

## index.html (chunk 6/7)
Source: yadd-reference/package-dokumentations/6/object-docs/dokumentation/0/index.html
Type: reference

```
hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, defaulting) RESPONDENT [ from SKELETON-UI-ELEMENT ] gdl object Object to respond to the form submission. Defaults to self.
```

---

## index.html (chunk 7/7)
Source: yadd-reference/package-dokumentations/6/object-docs/dokumentation/0/index.html
Type: reference

```
NT ] gdl object Object to respond to the form submission. Defaults to self. Computed Slots INNER-HTML [ from SKELETON-UI-ELEMENT ] string This can be used with (str .) [in cl-who] or (:princ .) [in htmlGen] to output this section of the page, without the wrapping :div tag [so if you use this, your code would be responsible for wrapping the :div tag with :id (the dom-id).] Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/8)
Source: yadd-reference/package-dokumentations/6/object-docs/dokumentation/1/index.html
Type: reference

```
GendL Application - TREE Package Documentation Object: TREE (The :GEYSR Package) Mixins: SHEET-SECTION Description Implements an interactive graphical tree from a nested list using HTML list element and CSS. Input Slots (optional) BASHEE [ from SKELETON-UI-ELEMENT ] gdl object Object to have its settable computed-slots and/or query-plist set from the fields on the form upon submission. Defaults to self. DOM-ID [ from SKELETON-UI-ELEMENT ] string This is the auto-computed dom-id which should be used for rendering this section. If you use the main-div HTML string for rendering this object as a page section, then you do not have to generate the :div tag yourself - the main-div will be a string of HTML which is wrapped in the correct :div tag already.
```

---

## index.html (chunk 2/8)
Source: yadd-reference/package-dokumentations/6/object-docs/dokumentation/1/index.html
Type: reference

```
FORCE-VALIDATION-FOR [ from SKELETON-UI-ELEMENT ] list of gdl objects of type form-control The validation-function will be forced on these objects when a form is submitted, even if the object's html form-control does not happen to be included in the values submitted with the form. Defaults to nil. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. HTML-SECTIONS [ from SKELETON-UI-ELEMENT ] List of HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded. HTML-SECTIONS-VALID [ from SKELETON-UI-ELEMENT ] List of valid HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk.
```

---

## index.html (chunk 3/8)
Source: yadd-reference/package-dokumentations/6/object-docs/dokumentation/1/index.html
Type: reference

```
CTIONS-VALID [ from SKELETON-UI-ELEMENT ] List of valid HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded. JS-TO-EVAL [ from SKELETON-UI-ELEMENT ] string of valid javascript This Javascript will be send with the Ajax response, and evaluated after the innerHTML for this section has been replaced. ONCLICK-FUNCTION function of one argument This function takes a node in the tree as an argument, and should return a plist with keys :function and :arguments, which is a function in the bashee which will be called with the given arguments when the given node in the tree is clicked. ORDERED-FORM-CONTROLS [ from SKELETON-UI-ELEMENT ] list of gdl objects, which should be of type 'base-form-control .
```

---

## index.html (chunk 4/8)
Source: yadd-reference/package-dokumentations/6/object-docs/dokumentation/1/index.html
Type: reference

```
tree is clicked. ORDERED-FORM-CONTROLS [ from SKELETON-UI-ELEMENT ] list of gdl objects, which should be of type 'base-form-control . [Note -- this slot is not really necessary for protecting out-of-bounds sequence references anymore, the form-control processor protects against this by itself now]. These objects are validated and bashed first, in the order given. If the cardinality of one form-control depends on another as in the example below, then you should list those dependent objects first. Default is nil. examples: ...
```

---

## index.html (chunk 5/8)
Source: yadd-reference/package-dokumentations/6/object-docs/dokumentation/1/index.html
Type: reference

```
:computed-slots ((number-of-nozzles (the number-of-nozzles-form value)) (ordered-form-controls (append (list-elements (the inner-flange-form)) (list (the number-of-nozzles-form))))) :objects ((inner-flange-form :type 'menu-form-control :choice-plist (list :hey "hey" :now "now") :default :hey :sequence (:size (the number-of-nozzles))) (number-of-nozzles-form :type 'text-form-control :prompt "Number of Shell Nozzles Required: " :domain :number :default 0) POSSIBLE-NILS [ from SKELETON-UI-ELEMENT ] list of keyword symbols Messages corresponding to form fields which could be missing from form submission (e.g. checkbox fields). Defaults to the names of any children or hidden-children of type menu-form-control or checkbox-form-control. PRESET-ALL? [ from SKELETON-UI-ELEMENT ] boolean This switch determines whether all form-controls should be preset before the final setting, in order to allow any interdependencies to be detected for validation or detecting changed values.
```

---

## index.html (chunk 6/8)
Source: yadd-reference/package-dokumentations/6/object-docs/dokumentation/1/index.html
Type: reference

```
h determines whether all form-controls should be preset before the final setting, in order to allow any interdependencies to be detected for validation or detecting changed values. If this is specified as a non-nil value, then any nil values of (the preset?) on individual form controls will be ignored. If this is specified as nil, then (the preset?) of individual form-controls (default of these is also nil) will be respected. Default is nil. RESPONDENT [ from SKELETON-UI-ELEMENT ] gdl object Object to respond to the form submission. Defaults to self. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence.
```

---

## index.html (chunk 7/8)
Source: yadd-reference/package-dokumentations/6/object-docs/dokumentation/1/index.html
Type: reference

```
in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Computed Slots INNER-HTML [ from SKELETON-UI-ELEMENT ] string This can be used with (str .) [in cl-who] or (:princ .) [in htmlGen] to output this section of the page, without the wrapping :div tag [so if you use this, your code would be responsible for wrapping the :div tag with :id (the dom-id).] SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 8/8)
Source: yadd-reference/package-dokumentations/6/object-docs/dokumentation/1/index.html
Type: reference

```
lists. Any children which throw errors come back as a plist with error information Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

