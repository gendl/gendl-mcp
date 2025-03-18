# Gendl Documentation - package_14_objects

## index.html (chunk 1/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/1/index.html
Type: reference

```
GendL Application - BASE-YADD-SHEET Package Documentation Object: BASE-YADD-SHEET (The :YADD Package) Mixins: BASE-AJAX-SHEET Author Dave Cooper (Genworks) Description Base mixin for a yadd sheet Input Slots (optional) AFTER-PRESENT! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the respondent of a form, to do some processing after the respondent's write-html-sheet function runs to present the object. AFTER-SET! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the requestor of a form, to do some processing after the requestor's form values are set into the specified bashee. BASHEE [ from SKELETON-UI-ELEMENT ] gdl object Object to have its settable computed-slots and/or query-plist set from the fields on the form upon submission. Defaults to self.
```

---

## index.html (chunk 2/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/1/index.html
Type: reference

```
on the form upon submission. Defaults to self. BEFORE-PRESENT! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the respondent of a form, to do some processing before the respondent's write-html-sheet function runs to present the object. This can be useful especially for objects which are subclasses of higher-level mixins such as application-mixin and node-mixin , where you do not have direct access to the write-html-sheet function and typically only define the model-inputs function. It is not always reliable to do processing in the model-inputs function, since some slots which depend on your intended modifications may already have been evaluated by the time the model-inputs function runs.
```

---

## index.html (chunk 3/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/1/index.html
Type: reference

```
BEFORE-RESPONSE! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in a user specialization of base-html-sheet, to do some processing before the header-plist is evaluated and before the HTTP response is actually initiated. BEFORE-SET! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the requestor of a form, to do some processing before the requestor's form values are set into the specified bashee. BODY-CLASS [ from BASE-AJAX-SHEET ] string or nil Names the value of class attribute for the body tag. Default is nil. BODY-ONLOAD [ from BASE-AJAX-SHEET ] string of javascript or nil This Javascript will go into the :onload event of the body. Default is nil. BODY-ONPAGESHOW [ from BASE-AJAX-SHEET ] string of javascript or nil This Javascript will go into the :onpageshow event of the body. Default is nil.
```

---

## index.html (chunk 4/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/1/index.html
Type: reference

```
ascript or nil This Javascript will go into the :onpageshow event of the body. Default is nil. BODY-ONRESIZE [ from BASE-AJAX-SHEET ] string of javascript or nil This Javascript will go into the :onresize event of the body. Default is nil. CHECK-SANITY? [ from BASE-HTML-SHEET ] boolean Determines whether a a sanity check is done (with the check-sanity function) before presenting the response page if this page is a respondent. Default is NIL. DOCTYPE-STRING [ from BASE-AJAX-SHEET ] string or nil Contains the string for the doctype at the top of the document. Default is the standard doctype for HTML5 and later. DOM-ID [ from SKELETON-UI-ELEMENT ] string This is the auto-computed dom-id which should be used for rendering this section. If you use the main-div HTML string for rendering this object as a page section, then you do not have to generate the :div tag yourself - the main-div will be a string of HTML which is wrapped in the correct :div tag already.
```

---

## index.html (chunk 5/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/1/index.html
Type: reference

```
ect as a page section, then you do not have to generate the :div tag yourself - the main-div will be a string of HTML which is wrapped in the correct :div tag already. FORCE-VALIDATION-FOR [ from SKELETON-UI-ELEMENT ] list of gdl objects of type form-control The validation-function will be forced on these objects when a form is submitted, even if the object's html form-control does not happen to be included in the values submitted with the form. Defaults to nil. HEAD-CLASS [ from BASE-AJAX-SHEET ] string or nil Names the value of class attribute for the head tag. Default is nil. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. HTML-CLASS [ from BASE-AJAX-SHEET ] string or nil Names the value of class attribute for the html tag. Default is nil. HTML-SECTIONS [ from SKELETON-UI-ELEMENT ] List of HTML sections to be scanned and possibly replaced in response to GDL Ajax calls.
```

---

## index.html (chunk 6/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/1/index.html
Type: reference

```
value of class attribute for the html tag. Default is nil. HTML-SECTIONS [ from SKELETON-UI-ELEMENT ] List of HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded. HTML-SECTIONS-VALID [ from SKELETON-UI-ELEMENT ] List of valid HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded.
```

---

## index.html (chunk 7/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/1/index.html
Type: reference

```
INNER-HTML [ from SKELETON-UI-ELEMENT ] string This can be used with (str .) [in cl-who] or (:princ .) [in htmlGen] to output this section of the page, without the wrapping :div tag [so if you use this, your code would be responsible for wrapping the :div tag with :id (the dom-id).] JS-TO-EVAL [ from SKELETON-UI-ELEMENT ] string of valid javascript This Javascript will be send with the Ajax response, and evaluated after the innerHTML for this section has been replaced. MAIN-SHEET-BODY [ from BASE-AJAX-SHEET ] string of html The main body of the page. This can be specified as input or overridden in subclass, otherwise it defaults to the content produced by the :output-function of the same name in the applicable lens for html-format. ORDERED-FORM-CONTROLS [ from SKELETON-UI-ELEMENT ] list of gdl objects, which should be of type 'base-form-control .
```

---

## index.html (chunk 8/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/1/index.html
Type: reference

```
f gdl objects, which should be of type 'base-form-control . [Note -- this slot is not really necessary for protecting out-of-bounds sequence references anymore, the form-control processor protects against this by itself now]. These objects are validated and bashed first, in the order given. If the cardinality of one form-control depends on another as in the example below, then you should list those dependent objects first. Default is nil. examples: ... :computed-slots ((number-of-nozzles (the number-of-nozzles-form value)) (ordered-form-controls (append (list-elements (the inner-flange-form)) (list (the number-of-nozzles-form))))) :objects ((inner-flange-form :type 'menu-form-control :choice-plist (list :hey "hey" :now "now") :default :hey :sequence (:size (the number-of-nozzles))) (number-of-nozzles-form :type 'text-form-control :prompt "Number of Shell Nozzles Required: " :domain :number :default 0) POSSIBLE-NILS [ from SKELETON-UI-ELEMENT ] list of keyword symbols Messages
```

---

## index.html (chunk 9/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/1/index.html
Type: reference

```
-of-nozzles-form :type 'text-form-control :prompt "Number of Shell Nozzles Required: " :domain :number :default 0) POSSIBLE-NILS [ from SKELETON-UI-ELEMENT ] list of keyword symbols Messages corresponding to form fields which could be missing from form submission (e.g. checkbox fields). Defaults to the names of any children or hidden-children of type menu-form-control or checkbox-form-control. PRESET-ALL? [ from SKELETON-UI-ELEMENT ] boolean This switch determines whether all form-controls should be preset before the final setting, in order to allow any interdependencies to be detected for validation or detecting changed values. If this is specified as a non-nil value, then any nil values of (the preset?) on individual form controls will be ignored. If this is specified as nil, then (the preset?) of individual form-controls (default of these is also nil) will be respected. Default is nil.
```

---

## index.html (chunk 10/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/1/index.html
Type: reference

```
reset?) of individual form-controls (default of these is also nil) will be respected. Default is nil. PROCESS-COOKIES! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in a user specialization of base-html-sheet, to do some processing before the header-plist is evaluated and before the HTTP response is actually initiated, but after the cookies-received have been set. RESPONDENT [ from SKELETON-UI-ELEMENT ] gdl object Object to respond to the form submission. Defaults to self. RETURN-OBJECT [ from BASE-HTML-SHEET ] gdl object Default object to which control will return with the write-back-link method ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists.
```

---

## index.html (chunk 11/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/1/index.html
Type: reference

```
of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. TARGET [ from BASE-HTML-SHEET ] string Name of a browser frame or window to display this page. Default of NIL indicates to use the same window. TITLE [ from BASE-AJAX-SHEET ] string The title of the web page. Defaults to "GendL Application -" .followed by the strings-for-display. TRANSITORY-SLOTS [ from BASE-HTML-SHEET ] list of keyword symbols Messages corresponding to form fields which should not be retained against Updates to the model (e.g.
```

---

## index.html (chunk 12/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/1/index.html
Type: reference

```
rom BASE-HTML-SHEET ] list of keyword symbols Messages corresponding to form fields which should not be retained against Updates to the model (e.g. calls to the update! function or hitting the Update button or link in the browser in development mode). Defaults to NIL (the empty list). VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, settable) ADDITIONAL-HEADER-JS-CONTENT [ from BASE-AJAX-SHEET ] valid javascript This javascript is added to the head of the page, just before the body. UI-SPECIFIC-LAYOUT-JS [ from BASE-AJAX-SHEET ] absolute uri in the browser . This is additional JavaScript that needs to be loaded in order to initiate the layout of a user interface. Defaults to nil. USE-JQUERY? [ from BASE-AJAX-SHEET ] boolean Include jquery javascript libraries in the page header? Default nil.
```

---

## index.html (chunk 13/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/1/index.html
Type: reference

```
t of a user interface. Defaults to nil. USE-JQUERY? [ from BASE-AJAX-SHEET ] boolean Include jquery javascript libraries in the page header? Default nil. Computed Slots ADDITIONAL-HEADER-CONTENT [ from BASE-AJAX-SHEET ] string of valid html Additional tag content to go into the page header, if you use the default main-sheet message and just fill in your own main-sheet-body, as is the intended use of the base-ajax-sheet primitive. ADDITIONAL-HEADER-JS string of valid html Contains standard jQuery files to include in the header for additional search funcionality. This computed-slot contains javascript files, found in the *gdl-install-dir* and used throughout the yadd pages for the generation of automatic search forms (like the master-index). The javascript loaded is jquery. DEFAULT-HEADER-CONTENT string of valid html Contains default header contents for yadd html files. This computed-slot is available in all children of this object.
```

---

## index.html (chunk 14/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/1/index.html
Type: reference

```
NTENT string of valid html Contains default header contents for yadd html files. This computed-slot is available in all children of this object. It contains links to default header content of a HTML generated yadd page. This contains a link to the favicon.ico and a link to a default CSS sheet. All these elements can be found in the *gdl-install-dir*/static/gwl/ directories. Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/0/index.html
Type: reference

```
GendL Application - ASSEMBLY Package Documentation Object: ASSEMBLY (The :YADD Package) Mixins: BASE-YADD-SHEET Author Dave Cooper (Genworks) Description ``Yet Another Definition Documenter.'' Generates documentation for all the relevant packages in the current Lisp session. Presents a standard :write-html-sheet method which can also be crawled with a call to (gwl:crawl "yadd:assembly") The packages to be documented, and whether the green/red supported messages flags show up, can be controlled with optional-inputs. Input Slots (optional) AFTER-PRESENT! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the respondent of a form, to do some processing after the respondent's write-html-sheet function runs to present the object. AFTER-SET! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the requestor of a form, to do some processing after the requestor's form values are set into the specified bashee.
```

---

## index.html (chunk 2/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/0/index.html
Type: reference

```
HTML-SHEET ] void This is an empty function by default, but can be overridden in the requestor of a form, to do some processing after the requestor's form values are set into the specified bashee. BASHEE [ from SKELETON-UI-ELEMENT ] gdl object Object to have its settable computed-slots and/or query-plist set from the fields on the form upon submission. Defaults to self. BEFORE-PRESENT! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the respondent of a form, to do some processing before the respondent's write-html-sheet function runs to present the object. This can be useful especially for objects which are subclasses of higher-level mixins such as application-mixin and node-mixin , where you do not have direct access to the write-html-sheet function and typically only define the model-inputs function.
```

---

## index.html (chunk 3/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/0/index.html
Type: reference

```
ction and typically only define the model-inputs function. It is not always reliable to do processing in the model-inputs function, since some slots which depend on your intended modifications may already have been evaluated by the time the model-inputs function runs. BEFORE-RESPONSE! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in a user specialization of base-html-sheet, to do some processing before the header-plist is evaluated and before the HTTP response is actually initiated. BEFORE-SET! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the requestor of a form, to do some processing before the requestor's form values are set into the specified bashee. BODY-CLASS [ from BASE-AJAX-SHEET ] string or nil Names the value of class attribute for the body tag. Default is nil. BODY-ONLOAD [ from BASE-AJAX-SHEET ] string of javascript or nil This Javascript will go into the :onload event of the body.
```

---

## index.html (chunk 4/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/0/index.html
Type: reference

```
nil Names the value of class attribute for the body tag. Default is nil. BODY-ONLOAD [ from BASE-AJAX-SHEET ] string of javascript or nil This Javascript will go into the :onload event of the body. Default is nil. BODY-ONPAGESHOW [ from BASE-AJAX-SHEET ] string of javascript or nil This Javascript will go into the :onpageshow event of the body. Default is nil. BODY-ONRESIZE [ from BASE-AJAX-SHEET ] string of javascript or nil This Javascript will go into the :onresize event of the body. Default is nil. CHECK-SANITY? [ from BASE-HTML-SHEET ] boolean Determines whether a a sanity check is done (with the check-sanity function) before presenting the response page if this page is a respondent. Default is NIL. DOCTYPE-STRING [ from BASE-AJAX-SHEET ] string or nil Contains the string for the doctype at the top of the document. Default is the standard doctype for HTML5 and later.
```

---

## index.html (chunk 5/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/0/index.html
Type: reference

```
ype at the top of the document. Default is the standard doctype for HTML5 and later. DOM-ID [ from SKELETON-UI-ELEMENT ] string This is the auto-computed dom-id which should be used for rendering this section. If you use the main-div HTML string for rendering this object as a page section, then you do not have to generate the :div tag yourself - the main-div will be a string of HTML which is wrapped in the correct :div tag already. EXTERNAL-ONLY? boolean This defaults to nil, if it is set to t, only exported symbols will be considered for documentation. FORCE-VALIDATION-FOR [ from SKELETON-UI-ELEMENT ] list of gdl objects of type form-control The validation-function will be forced on these objects when a form is submitted, even if the object's html form-control does not happen to be included in the values submitted with the form. Defaults to nil. HEAD-CLASS [ from BASE-AJAX-SHEET ] string or nil Names the value of class attribute for the head tag. Default is nil.
```

---

## index.html (chunk 6/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/0/index.html
Type: reference

```
ed in the values submitted with the form. Defaults to nil. HEAD-CLASS [ from BASE-AJAX-SHEET ] string or nil Names the value of class attribute for the head tag. Default is nil. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. HTML-CLASS [ from BASE-AJAX-SHEET ] string or nil Names the value of class attribute for the html tag. Default is nil. HTML-SECTIONS [ from SKELETON-UI-ELEMENT ] List of HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded. HTML-SECTIONS-VALID [ from SKELETON-UI-ELEMENT ] List of valid HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk.
```

---

## index.html (chunk 7/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/0/index.html
Type: reference

```
ONS-VALID [ from SKELETON-UI-ELEMENT ] List of valid HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded. INNER-HTML [ from SKELETON-UI-ELEMENT ] string This can be used with (str .) [in cl-who] or (:princ .) [in htmlGen] to output this section of the page, without the wrapping :div tag [so if you use this, your code would be responsible for wrapping the :div tag with :id (the dom-id).] JS-TO-EVAL [ from SKELETON-UI-ELEMENT ] string of valid javascript This Javascript will be send with the Ajax response, and evaluated after the innerHTML for this section has been replaced. ORDERED-FORM-CONTROLS [ from SKELETON-UI-ELEMENT ] list of gdl objects, which should be of type 'base-form-control .
```

---

## index.html (chunk 8/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/0/index.html
Type: reference

```
been replaced. ORDERED-FORM-CONTROLS [ from SKELETON-UI-ELEMENT ] list of gdl objects, which should be of type 'base-form-control . [Note -- this slot is not really necessary for protecting out-of-bounds sequence references anymore, the form-control processor protects against this by itself now]. These objects are validated and bashed first, in the order given. If the cardinality of one form-control depends on another as in the example below, then you should list those dependent objects first. Default is nil. examples: ...
```

---

## index.html (chunk 9/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/0/index.html
Type: reference

```
:computed-slots ((number-of-nozzles (the number-of-nozzles-form value)) (ordered-form-controls (append (list-elements (the inner-flange-form)) (list (the number-of-nozzles-form))))) :objects ((inner-flange-form :type 'menu-form-control :choice-plist (list :hey "hey" :now "now") :default :hey :sequence (:size (the number-of-nozzles))) (number-of-nozzles-form :type 'text-form-control :prompt "Number of Shell Nozzles Required: " :domain :number :default 0) PACKAGES-TO-IGNORE list of keyword symbols These packages will be ignored. This list defaults to standard internal and test packages POSSIBLE-NILS [ from SKELETON-UI-ELEMENT ] list of keyword symbols Messages corresponding to form fields which could be missing from form submission (e.g. checkbox fields). Defaults to the names of any children or hidden-children of type menu-form-control or checkbox-form-control.
```

---

## index.html (chunk 10/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/0/index.html
Type: reference

```
en or hidden-children of type menu-form-control or checkbox-form-control. PRESET-ALL? [ from SKELETON-UI-ELEMENT ] boolean This switch determines whether all form-controls should be preset before the final setting, in order to allow any interdependencies to be detected for validation or detecting changed values. If this is specified as a non-nil value, then any nil values of (the preset?) on individual form controls will be ignored. If this is specified as nil, then (the preset?) of individual form-controls (default of these is also nil) will be respected. Default is nil. PROCESS-COOKIES! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in a user specialization of base-html-sheet, to do some processing before the header-plist is evaluated and before the HTTP response is actually initiated, but after the cookies-received have been set. RESPONDENT [ from SKELETON-UI-ELEMENT ] gdl object Object to respond to the form submission. Defaults to self.
```

---

## index.html (chunk 11/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/0/index.html
Type: reference

```
the HTTP response is actually initiated, but after the cookies-received have been set. RESPONDENT [ from SKELETON-UI-ELEMENT ] gdl object Object to respond to the form submission. Defaults to self. RETURN-OBJECT [ from BASE-HTML-SHEET ] gdl object Default object to which control will return with the write-back-link method ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence.
```

---

## index.html (chunk 12/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/0/index.html
Type: reference

```
most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. TARGET [ from BASE-HTML-SHEET ] string Name of a browser frame or window to display this page. Default of NIL indicates to use the same window. TRANSITORY-SLOTS [ from BASE-HTML-SHEET ] list of keyword symbols Messages corresponding to form fields which should not be retained against Updates to the model (e.g. calls to the update! function or hitting the Update button or link in the browser in development mode). Defaults to NIL (the empty list). VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, settable) ADDITIONAL-HEADER-JS-CONTENT [ from BASE-AJAX-SHEET ] valid javascript This javascript is added to the head of the page, just before the body.
```

---

## index.html (chunk 13/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/0/index.html
Type: reference

```
L. Input Slots (optional, settable) ADDITIONAL-HEADER-JS-CONTENT [ from BASE-AJAX-SHEET ] valid javascript This javascript is added to the head of the page, just before the body. UI-SPECIFIC-LAYOUT-JS [ from BASE-AJAX-SHEET ] absolute uri in the browser . This is additional JavaScript that needs to be loaded in order to initiate the layout of a user interface. Defaults to nil. USE-JQUERY? [ from BASE-AJAX-SHEET ] boolean Include jquery javascript libraries in the page header? Default nil. Computed Slots TITLE [ from BASE-AJAX-SHEET ] string The title of the web page. Defaults to "GendL Application -" .followed by the strings-for-display. Objects MASTER-INDEX index Master index of all symbols (objects, functions, parameters, variables, constants) Objects (sequence) PACKAGE-DOKUMENTATIONS package-dokumentation Quantified, one for each :package-to-document GDL Functions MAIN-SHEET-BODY [ from BASE-AJAX-SHEET ] string of html The main body of the page.
```

---

## index.html (chunk 14/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/0/index.html
Type: reference

```
ckage-dokumentation Quantified, one for each :package-to-document GDL Functions MAIN-SHEET-BODY [ from BASE-AJAX-SHEET ] string of html The main body of the page. This can be specified as input or overridden in subclass, otherwise it defaults to the content produced by the :output-function of the same name in the applicable lens for html-format. Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/3/index.html
Type: reference

```
GendL Application - PACKAGE-DOKUMENTATION Package Documentation Object: PACKAGE-DOKUMENTATION (The :YADD Package) Mixins: BASE-YADD-SHEET Author Dave Cooper Description Prepares documentation for all relevant symbols in a given Lisp package. Input Slots (optional) AFTER-PRESENT! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the respondent of a form, to do some processing after the respondent's write-html-sheet function runs to present the object. AFTER-SET! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the requestor of a form, to do some processing after the requestor's form values are set into the specified bashee. BASHEE [ from SKELETON-UI-ELEMENT ] gdl object Object to have its settable computed-slots and/or query-plist set from the fields on the form upon submission. Defaults to self.
```

---

## index.html (chunk 2/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/3/index.html
Type: reference

```
d-slots and/or query-plist set from the fields on the form upon submission. Defaults to self. BEFORE-PRESENT! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the respondent of a form, to do some processing before the respondent's write-html-sheet function runs to present the object. This can be useful especially for objects which are subclasses of higher-level mixins such as application-mixin and node-mixin , where you do not have direct access to the write-html-sheet function and typically only define the model-inputs function. It is not always reliable to do processing in the model-inputs function, since some slots which depend on your intended modifications may already have been evaluated by the time the model-inputs function runs.
```

---

## index.html (chunk 3/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/3/index.html
Type: reference

```
BEFORE-RESPONSE! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in a user specialization of base-html-sheet, to do some processing before the header-plist is evaluated and before the HTTP response is actually initiated. BEFORE-SET! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the requestor of a form, to do some processing before the requestor's form values are set into the specified bashee. BODY-CLASS [ from BASE-AJAX-SHEET ] string or nil Names the value of class attribute for the body tag. Default is nil. BODY-ONLOAD [ from BASE-AJAX-SHEET ] string of javascript or nil This Javascript will go into the :onload event of the body. Default is nil. BODY-ONPAGESHOW [ from BASE-AJAX-SHEET ] string of javascript or nil This Javascript will go into the :onpageshow event of the body. Default is nil.
```

---

## index.html (chunk 4/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/3/index.html
Type: reference

```
ascript or nil This Javascript will go into the :onpageshow event of the body. Default is nil. BODY-ONRESIZE [ from BASE-AJAX-SHEET ] string of javascript or nil This Javascript will go into the :onresize event of the body. Default is nil. CHECK-SANITY? [ from BASE-HTML-SHEET ] boolean Determines whether a a sanity check is done (with the check-sanity function) before presenting the response page if this page is a respondent. Default is NIL. DOCTYPE-STRING [ from BASE-AJAX-SHEET ] string or nil Contains the string for the doctype at the top of the document. Default is the standard doctype for HTML5 and later. DOM-ID [ from SKELETON-UI-ELEMENT ] string This is the auto-computed dom-id which should be used for rendering this section. If you use the main-div HTML string for rendering this object as a page section, then you do not have to generate the :div tag yourself - the main-div will be a string of HTML which is wrapped in the correct :div tag already.
```

---

## index.html (chunk 5/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/3/index.html
Type: reference

```
ect as a page section, then you do not have to generate the :div tag yourself - the main-div will be a string of HTML which is wrapped in the correct :div tag already. EXTERNAL-ONLY? boolean Determines whether to consider all symbols in the package or just the exported ones. FORCE-VALIDATION-FOR [ from SKELETON-UI-ELEMENT ] list of gdl objects of type form-control The validation-function will be forced on these objects when a form is submitted, even if the object's html form-control does not happen to be included in the values submitted with the form. Defaults to nil. HEAD-CLASS [ from BASE-AJAX-SHEET ] string or nil Names the value of class attribute for the head tag. Default is nil. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. HTML-CLASS [ from BASE-AJAX-SHEET ] string or nil Names the value of class attribute for the html tag. Default is nil.
```

---

## index.html (chunk 6/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/3/index.html
Type: reference

```
even if specified in :objects. Default is nil. HTML-CLASS [ from BASE-AJAX-SHEET ] string or nil Names the value of class attribute for the html tag. Default is nil. HTML-SECTIONS [ from SKELETON-UI-ELEMENT ] List of HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded. HTML-SECTIONS-VALID [ from SKELETON-UI-ELEMENT ] List of valid HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded.
```

---

## index.html (chunk 7/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/3/index.html
Type: reference

```
lly every time the sheet section's main-div is demanded. INNER-HTML [ from SKELETON-UI-ELEMENT ] string This can be used with (str .) [in cl-who] or (:princ .) [in htmlGen] to output this section of the page, without the wrapping :div tag [so if you use this, your code would be responsible for wrapping the :div tag with :id (the dom-id).] JS-TO-EVAL [ from SKELETON-UI-ELEMENT ] string of valid javascript This Javascript will be send with the Ajax response, and evaluated after the innerHTML for this section has been replaced. MAIN-SHEET-BODY [ from BASE-AJAX-SHEET ] string of html The main body of the page. This can be specified as input or overridden in subclass, otherwise it defaults to the content produced by the :output-function of the same name in the applicable lens for html-format. ORDERED-FORM-CONTROLS [ from SKELETON-UI-ELEMENT ] list of gdl objects, which should be of type 'base-form-control .
```

---

## index.html (chunk 8/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/3/index.html
Type: reference

```
RDERED-FORM-CONTROLS [ from SKELETON-UI-ELEMENT ] list of gdl objects, which should be of type 'base-form-control . [Note -- this slot is not really necessary for protecting out-of-bounds sequence references anymore, the form-control processor protects against this by itself now]. These objects are validated and bashed first, in the order given. If the cardinality of one form-control depends on another as in the example below, then you should list those dependent objects first. Default is nil. examples: ...
```

---

## index.html (chunk 9/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/3/index.html
Type: reference

```
:computed-slots ((number-of-nozzles (the number-of-nozzles-form value)) (ordered-form-controls (append (list-elements (the inner-flange-form)) (list (the number-of-nozzles-form))))) :objects ((inner-flange-form :type 'menu-form-control :choice-plist (list :hey "hey" :now "now") :default :hey :sequence (:size (the number-of-nozzles))) (number-of-nozzles-form :type 'text-form-control :prompt "Number of Shell Nozzles Required: " :domain :number :default 0) PACKAGE string or keyword symbol Names the package, or a nickname of the package, to be documented. POSSIBLE-NILS [ from SKELETON-UI-ELEMENT ] list of keyword symbols Messages corresponding to form fields which could be missing from form submission (e.g. checkbox fields). Defaults to the names of any children or hidden-children of type menu-form-control or checkbox-form-control.
```

---

## index.html (chunk 10/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/3/index.html
Type: reference

```
u-form-control or checkbox-form-control. PRESET-ALL? [ from SKELETON-UI-ELEMENT ] boolean This switch determines whether all form-controls should be preset before the final setting, in order to allow any interdependencies to be detected for validation or detecting changed values. If this is specified as a non-nil value, then any nil values of (the preset?) on individual form controls will be ignored. If this is specified as nil, then (the preset?) of individual form-controls (default of these is also nil) will be respected. Default is nil. PROCESS-COOKIES! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in a user specialization of base-html-sheet, to do some processing before the header-plist is evaluated and before the HTTP response is actually initiated, but after the cookies-received have been set. RESPONDENT [ from SKELETON-UI-ELEMENT ] gdl object Object to respond to the form submission. Defaults to self.
```

---

## index.html (chunk 11/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/3/index.html
Type: reference

```
itiated, but after the cookies-received have been set. RESPONDENT [ from SKELETON-UI-ELEMENT ] gdl object Object to respond to the form submission. Defaults to self. RETURN-OBJECT [ from BASE-HTML-SHEET ] gdl object Default object to which control will return with the write-back-link method ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information SHOW-SUPPORTED-FLAG boolean Determines whether to show red/green flag on each message indicating whether it is a supported message. TARGET [ from BASE-HTML-SHEET ] string Name of a browser frame or window to display this page. Default of NIL indicates to use the same window.
```

---

## index.html (chunk 12/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/3/index.html
Type: reference

```
Name of a browser frame or window to display this page. Default of NIL indicates to use the same window. TRANSITORY-SLOTS [ from BASE-HTML-SHEET ] list of keyword symbols Messages corresponding to form fields which should not be retained against Updates to the model (e.g. calls to the update! function or hitting the Update button or link in the browser in development mode). Defaults to NIL (the empty list). VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, settable) ADDITIONAL-HEADER-JS-CONTENT [ from BASE-AJAX-SHEET ] valid javascript This javascript is added to the head of the page, just before the body. UI-SPECIFIC-LAYOUT-JS [ from BASE-AJAX-SHEET ] absolute uri in the browser . This is additional JavaScript that needs to be loaded in order to initiate the layout of a user interface. Defaults to nil.
```

---

## index.html (chunk 13/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/3/index.html
Type: reference

```
JAX-SHEET ] absolute uri in the browser . This is additional JavaScript that needs to be loaded in order to initiate the layout of a user interface. Defaults to nil. USE-JQUERY? [ from BASE-AJAX-SHEET ] boolean Include jquery javascript libraries in the page header? Default nil. Computed Slots STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. TITLE [ from BASE-AJAX-SHEET ] string The title of the web page. Defaults to "GendL Application -" .followed by the strings-for-display. Objects FUNCTION-DOCS function-doc Container for set of all Function documentation sheets. OBJECT-DOCS object-doc Container for set of all Object documentation sheets.
```

---

## index.html (chunk 14/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/3/index.html
Type: reference

```
all Function documentation sheets. OBJECT-DOCS object-doc Container for set of all Object documentation sheets. VARIABLE-DOCS variable-doc Container for set of all Parameter/Variable/Constant documentation sheets. Hidden Objects PACKAGE-FORM package-form Allows user to modify toplevel optional-inputs. GDL Functions DOM-SECTION list in gdl dom authoring format Suitable for filling in a section of output document. WRITE-HTML-SHEET void Prints to *html-stream* a bulleted list for each of the three categories of docs in the package. Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/13)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/2/index.html
Type: reference

```
GendL Application - MASTER-INDEX Package Documentation Object: MASTER-INDEX (The :YADD Package) Mixins: BASE-YADD-SHEET Author Dave Cooper (Genworks) Description Prints bullet list of symbols as links to their documentation pages. Input Slots (required) SYMBOLS-FOR-INDEX list of lists Each list contains the page object for the symbol's documentation and the symbol's print-name. The list should be sorted based on the symbols' print-names. Input Slots (optional) AFTER-PRESENT! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the respondent of a form, to do some processing after the respondent's write-html-sheet function runs to present the object. AFTER-SET! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the requestor of a form, to do some processing after the requestor's form values are set into the specified bashee.
```

---

## index.html (chunk 2/13)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/2/index.html
Type: reference

```
n the requestor of a form, to do some processing after the requestor's form values are set into the specified bashee. BASHEE [ from SKELETON-UI-ELEMENT ] gdl object Object to have its settable computed-slots and/or query-plist set from the fields on the form upon submission. Defaults to self. BEFORE-PRESENT! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the respondent of a form, to do some processing before the respondent's write-html-sheet function runs to present the object. This can be useful especially for objects which are subclasses of higher-level mixins such as application-mixin and node-mixin , where you do not have direct access to the write-html-sheet function and typically only define the model-inputs function. It is not always reliable to do processing in the model-inputs function, since some slots which depend on your intended modifications may already have been evaluated by the time the model-inputs function runs.
```

---

## index.html (chunk 3/13)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/2/index.html
Type: reference

```
iable to do processing in the model-inputs function, since some slots which depend on your intended modifications may already have been evaluated by the time the model-inputs function runs. BEFORE-RESPONSE! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in a user specialization of base-html-sheet, to do some processing before the header-plist is evaluated and before the HTTP response is actually initiated. BEFORE-SET! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the requestor of a form, to do some processing before the requestor's form values are set into the specified bashee. BODY-CLASS [ from BASE-AJAX-SHEET ] string or nil Names the value of class attribute for the body tag. Default is nil. BODY-ONLOAD [ from BASE-AJAX-SHEET ] string of javascript or nil This Javascript will go into the :onload event of the body. Default is nil.
```

---

## index.html (chunk 4/13)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/2/index.html
Type: reference

```
NLOAD [ from BASE-AJAX-SHEET ] string of javascript or nil This Javascript will go into the :onload event of the body. Default is nil. BODY-ONPAGESHOW [ from BASE-AJAX-SHEET ] string of javascript or nil This Javascript will go into the :onpageshow event of the body. Default is nil. BODY-ONRESIZE [ from BASE-AJAX-SHEET ] string of javascript or nil This Javascript will go into the :onresize event of the body. Default is nil. CHECK-SANITY? [ from BASE-HTML-SHEET ] boolean Determines whether a a sanity check is done (with the check-sanity function) before presenting the response page if this page is a respondent. Default is NIL. DOCTYPE-STRING [ from BASE-AJAX-SHEET ] string or nil Contains the string for the doctype at the top of the document. Default is the standard doctype for HTML5 and later. DOM-ID [ from SKELETON-UI-ELEMENT ] string This is the auto-computed dom-id which should be used for rendering this section.
```

---

## index.html (chunk 5/13)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/2/index.html
Type: reference

```
ater. DOM-ID [ from SKELETON-UI-ELEMENT ] string This is the auto-computed dom-id which should be used for rendering this section. If you use the main-div HTML string for rendering this object as a page section, then you do not have to generate the :div tag yourself - the main-div will be a string of HTML which is wrapped in the correct :div tag already. FORCE-VALIDATION-FOR [ from SKELETON-UI-ELEMENT ] list of gdl objects of type form-control The validation-function will be forced on these objects when a form is submitted, even if the object's html form-control does not happen to be included in the values submitted with the form. Defaults to nil. HEAD-CLASS [ from BASE-AJAX-SHEET ] string or nil Names the value of class attribute for the head tag. Default is nil. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil.
```

---

## index.html (chunk 6/13)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/2/index.html
Type: reference

```
XIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. HTML-CLASS [ from BASE-AJAX-SHEET ] string or nil Names the value of class attribute for the html tag. Default is nil. HTML-SECTIONS [ from SKELETON-UI-ELEMENT ] List of HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded. HTML-SECTIONS-VALID [ from SKELETON-UI-ELEMENT ] List of valid HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded.
```

---

## index.html (chunk 7/13)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/2/index.html
Type: reference

```
are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded. INNER-HTML [ from SKELETON-UI-ELEMENT ] string This can be used with (str .) [in cl-who] or (:princ .) [in htmlGen] to output this section of the page, without the wrapping :div tag [so if you use this, your code would be responsible for wrapping the :div tag with :id (the dom-id).] JS-TO-EVAL [ from SKELETON-UI-ELEMENT ] string of valid javascript This Javascript will be send with the Ajax response, and evaluated after the innerHTML for this section has been replaced. ORDERED-FORM-CONTROLS [ from SKELETON-UI-ELEMENT ] list of gdl objects, which should be of type 'base-form-control . [Note -- this slot is not really necessary for protecting out-of-bounds sequence references anymore, the form-control processor protects against this by itself now]. These objects are validated and bashed first, in the order given.
```

---

## index.html (chunk 8/13)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/2/index.html
Type: reference

```
sequence references anymore, the form-control processor protects against this by itself now]. These objects are validated and bashed first, in the order given. If the cardinality of one form-control depends on another as in the example below, then you should list those dependent objects first. Default is nil. examples: ... :computed-slots ((number-of-nozzles (the number-of-nozzles-form value)) (ordered-form-controls (append (list-elements (the inner-flange-form)) (list (the number-of-nozzles-form))))) :objects ((inner-flange-form :type 'menu-form-control :choice-plist (list :hey "hey" :now "now") :default :hey :sequence (:size (the number-of-nozzles))) (number-of-nozzles-form :type 'text-form-control :prompt "Number of Shell Nozzles Required: " :domain :number :default 0) POSSIBLE-NILS [ from SKELETON-UI-ELEMENT ] list of keyword symbols Messages corresponding to form fields which could be missing from form submission (e.g. checkbox fields).
```

---

## index.html (chunk 9/13)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/2/index.html
Type: reference

```
from SKELETON-UI-ELEMENT ] list of keyword symbols Messages corresponding to form fields which could be missing from form submission (e.g. checkbox fields). Defaults to the names of any children or hidden-children of type menu-form-control or checkbox-form-control. PRESET-ALL? [ from SKELETON-UI-ELEMENT ] boolean This switch determines whether all form-controls should be preset before the final setting, in order to allow any interdependencies to be detected for validation or detecting changed values. If this is specified as a non-nil value, then any nil values of (the preset?) on individual form controls will be ignored. If this is specified as nil, then (the preset?) of individual form-controls (default of these is also nil) will be respected. Default is nil.
```

---

## index.html (chunk 10/13)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/2/index.html
Type: reference

```
PROCESS-COOKIES! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in a user specialization of base-html-sheet, to do some processing before the header-plist is evaluated and before the HTTP response is actually initiated, but after the cookies-received have been set. RESPONDENT [ from SKELETON-UI-ELEMENT ] gdl object Object to respond to the form submission. Defaults to self. RETURN-OBJECT [ from BASE-HTML-SHEET ] gdl object Default object to which control will return with the write-back-link method ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists.
```

---

## index.html (chunk 11/13)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/2/index.html
Type: reference

```
t lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. TARGET [ from BASE-HTML-SHEET ] string Name of a browser frame or window to display this page. Default of NIL indicates to use the same window. TITLE [ from BASE-AJAX-SHEET ] string The title of the web page. Defaults to "GendL Application -" .followed by the strings-for-display. TRANSITORY-SLOTS [ from BASE-HTML-SHEET ] list of keyword symbols Messages corresponding to form fields which should not be retained against Updates to the model (e.g. calls to the update! function or hitting the Update button or link in the browser in development mode). Defaults to NIL (the empty list).
```

---

## index.html (chunk 12/13)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/2/index.html
Type: reference

```
be retained against Updates to the model (e.g. calls to the update! function or hitting the Update button or link in the browser in development mode). Defaults to NIL (the empty list). VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, settable) UI-SPECIFIC-LAYOUT-JS [ from BASE-AJAX-SHEET ] absolute uri in the browser . This is additional JavaScript that needs to be loaded in order to initiate the layout of a user interface. Defaults to nil. Computed Slots ADDITIONAL-HEADER-JS-CONTENT [ from BASE-AJAX-SHEET ] valid javascript This javascript is added to the head of the page, just before the body. MAIN-SHEET-BODY [ from BASE-AJAX-SHEET ] string of html The main body of the page.
```

---

## index.html (chunk 13/13)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/2/index.html
Type: reference

```
g of html The main body of the page. This can be specified as input or overridden in subclass, otherwise it defaults to the content produced by the :output-function of the same name in the applicable lens for html-format. USE-JQUERY? [ from BASE-AJAX-SHEET ] boolean Include jquery javascript libraries in the page header? Default nil. Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/4/index.html
Type: reference

```
GendL Application - PACKAGE-FORM Package Documentation Object: PACKAGE-FORM (The :YADD Package) Mixins: BASE-YADD-SHEET Author Dave Cooper (Genworks) Description Presents a form to the user to be able to modify the Package, supported-flag, and external flag. Input Slots (optional) AFTER-PRESENT! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the respondent of a form, to do some processing after the respondent's write-html-sheet function runs to present the object. AFTER-SET! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the requestor of a form, to do some processing after the requestor's form values are set into the specified bashee. BASHEE [ from SKELETON-UI-ELEMENT ] gdl object Object to have its settable computed-slots and/or query-plist set from the fields on the form upon submission. Defaults to self.
```

---

## index.html (chunk 2/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/4/index.html
Type: reference

```
settable computed-slots and/or query-plist set from the fields on the form upon submission. Defaults to self. BEFORE-PRESENT! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the respondent of a form, to do some processing before the respondent's write-html-sheet function runs to present the object. This can be useful especially for objects which are subclasses of higher-level mixins such as application-mixin and node-mixin , where you do not have direct access to the write-html-sheet function and typically only define the model-inputs function. It is not always reliable to do processing in the model-inputs function, since some slots which depend on your intended modifications may already have been evaluated by the time the model-inputs function runs.
```

---

## index.html (chunk 3/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/4/index.html
Type: reference

```
runs. BEFORE-RESPONSE! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in a user specialization of base-html-sheet, to do some processing before the header-plist is evaluated and before the HTTP response is actually initiated. BEFORE-SET! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the requestor of a form, to do some processing before the requestor's form values are set into the specified bashee. BODY-CLASS [ from BASE-AJAX-SHEET ] string or nil Names the value of class attribute for the body tag. Default is nil. BODY-ONLOAD [ from BASE-AJAX-SHEET ] string of javascript or nil This Javascript will go into the :onload event of the body. Default is nil. BODY-ONPAGESHOW [ from BASE-AJAX-SHEET ] string of javascript or nil This Javascript will go into the :onpageshow event of the body. Default is nil.
```

---

## index.html (chunk 4/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/4/index.html
Type: reference

```
of javascript or nil This Javascript will go into the :onpageshow event of the body. Default is nil. BODY-ONRESIZE [ from BASE-AJAX-SHEET ] string of javascript or nil This Javascript will go into the :onresize event of the body. Default is nil. CHECK-SANITY? [ from BASE-HTML-SHEET ] boolean Determines whether a a sanity check is done (with the check-sanity function) before presenting the response page if this page is a respondent. Default is NIL. DOCTYPE-STRING [ from BASE-AJAX-SHEET ] string or nil Contains the string for the doctype at the top of the document. Default is the standard doctype for HTML5 and later. DOM-ID [ from SKELETON-UI-ELEMENT ] string This is the auto-computed dom-id which should be used for rendering this section. If you use the main-div HTML string for rendering this object as a page section, then you do not have to generate the :div tag yourself - the main-div will be a string of HTML which is wrapped in the correct :div tag already.
```

---

## index.html (chunk 5/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/4/index.html
Type: reference

```
is object as a page section, then you do not have to generate the :div tag yourself - the main-div will be a string of HTML which is wrapped in the correct :div tag already. FORCE-VALIDATION-FOR [ from SKELETON-UI-ELEMENT ] list of gdl objects of type form-control The validation-function will be forced on these objects when a form is submitted, even if the object's html form-control does not happen to be included in the values submitted with the form. Defaults to nil. HEAD-CLASS [ from BASE-AJAX-SHEET ] string or nil Names the value of class attribute for the head tag. Default is nil. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. HTML-CLASS [ from BASE-AJAX-SHEET ] string or nil Names the value of class attribute for the html tag. Default is nil. HTML-SECTIONS [ from SKELETON-UI-ELEMENT ] List of HTML sections to be scanned and possibly replaced in response to GDL Ajax calls.
```

---

## index.html (chunk 6/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/4/index.html
Type: reference

```
s the value of class attribute for the html tag. Default is nil. HTML-SECTIONS [ from SKELETON-UI-ELEMENT ] List of HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded. HTML-SECTIONS-VALID [ from SKELETON-UI-ELEMENT ] List of valid HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded.
```

---

## index.html (chunk 7/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/4/index.html
Type: reference

```
INNER-HTML [ from SKELETON-UI-ELEMENT ] string This can be used with (str .) [in cl-who] or (:princ .) [in htmlGen] to output this section of the page, without the wrapping :div tag [so if you use this, your code would be responsible for wrapping the :div tag with :id (the dom-id).] JS-TO-EVAL [ from SKELETON-UI-ELEMENT ] string of valid javascript This Javascript will be send with the Ajax response, and evaluated after the innerHTML for this section has been replaced. MAIN-SHEET-BODY [ from BASE-AJAX-SHEET ] string of html The main body of the page. This can be specified as input or overridden in subclass, otherwise it defaults to the content produced by the :output-function of the same name in the applicable lens for html-format. ORDERED-FORM-CONTROLS [ from SKELETON-UI-ELEMENT ] list of gdl objects, which should be of type 'base-form-control .
```

---

## index.html (chunk 8/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/4/index.html
Type: reference

```
f gdl objects, which should be of type 'base-form-control . [Note -- this slot is not really necessary for protecting out-of-bounds sequence references anymore, the form-control processor protects against this by itself now]. These objects are validated and bashed first, in the order given. If the cardinality of one form-control depends on another as in the example below, then you should list those dependent objects first. Default is nil. examples: ... :computed-slots ((number-of-nozzles (the number-of-nozzles-form value)) (ordered-form-controls (append (list-elements (the inner-flange-form)) (list (the number-of-nozzles-form))))) :objects ((inner-flange-form :type 'menu-form-control :choice-plist (list :hey "hey" :now "now") :default :hey :sequence (:size (the number-of-nozzles))) (number-of-nozzles-form :type 'text-form-control :prompt "Number of Shell Nozzles Required: " :domain :number :default 0) POSSIBLE-NILS [ from SKELETON-UI-ELEMENT ] list of keyword symbols Messages
```

---

## index.html (chunk 9/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/4/index.html
Type: reference

```
-of-nozzles-form :type 'text-form-control :prompt "Number of Shell Nozzles Required: " :domain :number :default 0) POSSIBLE-NILS [ from SKELETON-UI-ELEMENT ] list of keyword symbols Messages corresponding to form fields which could be missing from form submission (e.g. checkbox fields). Defaults to the names of any children or hidden-children of type menu-form-control or checkbox-form-control. PRESET-ALL? [ from SKELETON-UI-ELEMENT ] boolean This switch determines whether all form-controls should be preset before the final setting, in order to allow any interdependencies to be detected for validation or detecting changed values. If this is specified as a non-nil value, then any nil values of (the preset?) on individual form controls will be ignored. If this is specified as nil, then (the preset?) of individual form-controls (default of these is also nil) will be respected. Default is nil.
```

---

## index.html (chunk 10/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/4/index.html
Type: reference

```
reset?) of individual form-controls (default of these is also nil) will be respected. Default is nil. PROCESS-COOKIES! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in a user specialization of base-html-sheet, to do some processing before the header-plist is evaluated and before the HTTP response is actually initiated, but after the cookies-received have been set. RESPONDENT [ from SKELETON-UI-ELEMENT ] gdl object Object to respond to the form submission. Defaults to self. RETURN-OBJECT [ from BASE-HTML-SHEET ] gdl object Default object to which control will return with the write-back-link method ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists.
```

---

## index.html (chunk 11/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/4/index.html
Type: reference

```
of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. TARGET [ from BASE-HTML-SHEET ] string Name of a browser frame or window to display this page. Default of NIL indicates to use the same window. TITLE [ from BASE-AJAX-SHEET ] string The title of the web page. Defaults to "GendL Application -" .followed by the strings-for-display. TRANSITORY-SLOTS [ from BASE-HTML-SHEET ] list of keyword symbols Messages corresponding to form fields which should not be retained against Updates to the model (e.g.
```

---

## index.html (chunk 12/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/4/index.html
Type: reference

```
rom BASE-HTML-SHEET ] list of keyword symbols Messages corresponding to form fields which should not be retained against Updates to the model (e.g. calls to the update! function or hitting the Update button or link in the browser in development mode). Defaults to NIL (the empty list). VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, settable) ADDITIONAL-HEADER-JS-CONTENT [ from BASE-AJAX-SHEET ] valid javascript This javascript is added to the head of the page, just before the body. UI-SPECIFIC-LAYOUT-JS [ from BASE-AJAX-SHEET ] absolute uri in the browser . This is additional JavaScript that needs to be loaded in order to initiate the layout of a user interface. Defaults to nil. USE-JQUERY? [ from BASE-AJAX-SHEET ] boolean Include jquery javascript libraries in the page header? Default nil.
```

---

## index.html (chunk 13/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/4/index.html
Type: reference

```
t of a user interface. Defaults to nil. USE-JQUERY? [ from BASE-AJAX-SHEET ] boolean Include jquery javascript libraries in the page header? Default nil. GDL Functions WRITE-HTML-SHEET [ from BASE-HTML-SHEET ] void This GDL function should be redefined to generate the HTML page corresponding to this object. It can be specified here, or as the main-sheet output-function in an html-format lens for this object's type. This write-html-sheet function, if defined, will override any main-sheet function defined in the lens. Typically a write-html-sheet function would look as follows: example: (write-html-sheet () (html (:html (:head (:title (:princ (the :page-title)))) (:body ;;; fill in your body here )))) Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 14/14)
Source: yadd-reference/package-dokumentations/14/object-docs/dokumentation/4/index.html
Type: reference

```
orks Build: 1598p001
```

---

