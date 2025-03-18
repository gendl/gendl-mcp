# Gendl Documentation - package_8_objects

## index.html (chunk 1/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/1/index.html
Type: reference

```
GendL Application - BASE-AJAX-GRAPHICS-SHEET Package Documentation Object: BASE-AJAX-GRAPHICS-SHEET (The :GWL Package) Mixins: BASE-AJAX-SHEET , BASE-HTML-GRAPHICS-SHEET Description This mixes together base-ajax-sheet with base-html-graphics-sheet, and adds html-format output-functions for several of the new formats such as ajax-enabled png/jpeg and Raphael vector graphics. Input Slots (optional) AFTER-PRESENT! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the respondent of a form, to do some processing after the respondent's write-html-sheet function runs to present the object. AFTER-SET! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the requestor of a form, to do some processing after the requestor's form values are set into the specified bashee. BACKGROUND-COLOR array of three numbers between 0 and 1 RGB Color in decimal format. Color to be used for the background of the viewport.
```

---

## index.html (chunk 2/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/1/index.html
Type: reference

```
tor's form values are set into the specified bashee. BACKGROUND-COLOR array of three numbers between 0 and 1 RGB Color in decimal format. Color to be used for the background of the viewport. Defaults to the :background from the global *colors-default* parameter. BASHEE [ from SKELETON-UI-ELEMENT ] gdl object Object to have its settable computed-slots and/or query-plist set from the fields on the form upon submission. Defaults to self. BEFORE-PRESENT! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the respondent of a form, to do some processing before the respondent's write-html-sheet function runs to present the object. This can be useful especially for objects which are subclasses of higher-level mixins such as application-mixin and node-mixin , where you do not have direct access to the write-html-sheet function and typically only define the model-inputs function.
```

---

## index.html (chunk 3/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/1/index.html
Type: reference

```
, where you do not have direct access to the write-html-sheet function and typically only define the model-inputs function. It is not always reliable to do processing in the model-inputs function, since some slots which depend on your intended modifications may already have been evaluated by the time the model-inputs function runs. BEFORE-RESPONSE! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in a user specialization of base-html-sheet, to do some processing before the header-plist is evaluated and before the HTTP response is actually initiated. BEFORE-SET! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the requestor of a form, to do some processing before the requestor's form values are set into the specified bashee. BODY-CLASS [ from BASE-AJAX-SHEET ] string or nil Names the value of class attribute for the body tag. Default is nil.
```

---

## index.html (chunk 4/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/1/index.html
Type: reference

```
e specified bashee. BODY-CLASS [ from BASE-AJAX-SHEET ] string or nil Names the value of class attribute for the body tag. Default is nil. BODY-ONLOAD [ from BASE-AJAX-SHEET ] string of javascript or nil This Javascript will go into the :onload event of the body. Default is nil. BODY-ONPAGESHOW [ from BASE-AJAX-SHEET ] string of javascript or nil This Javascript will go into the :onpageshow event of the body. Default is nil. BODY-ONRESIZE [ from BASE-AJAX-SHEET ] string of javascript or nil This Javascript will go into the :onresize event of the body. Default is nil. BOUNDING-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object. CENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located.
```

---

## index.html (chunk 5/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/1/index.html
Type: reference

```
from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located. CHECK-SANITY? [ from BASE-HTML-SHEET ] boolean Determines whether a a sanity check is done (with the check-sanity function) before presenting the response page if this page is a respondent. Default is NIL. DISPLAY-LIST-OBJECT-ROOTS list of gdl objects The leaves of each of these objects will be included in the geometry display. Defaults to nil. DISPLAY-LIST-OBJECTS list of gdl objects containing geometry These are the actual objects themselves, not nodes which have children or other descendants that you want to display. If you want to display the leaves of certain nodes, include the objects for those nodes in the display-list-object-roots, not here. Defaults to nil. DOCTYPE-STRING [ from BASE-AJAX-SHEET ] string or nil Contains the string for the doctype at the top of the document. Default is the standard doctype for HTML5 and later.
```

---

## index.html (chunk 6/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/1/index.html
Type: reference

```
o nil. DOCTYPE-STRING [ from BASE-AJAX-SHEET ] string or nil Contains the string for the doctype at the top of the document. Default is the standard doctype for HTML5 and later. DOM-ID [ from SKELETON-UI-ELEMENT ] string This is the auto-computed dom-id which should be used for rendering this section. If you use the main-div HTML string for rendering this object as a page section, then you do not have to generate the :div tag yourself - the main-div will be a string of HTML which is wrapped in the correct :div tag already. FIELD-OF-VIEW-DEFAULT number in angular degrees The maximum angle of the view frustrum for perspective views. Defaults to 45 which is natural human eye field of view. FORCE-VALIDATION-FOR [ from SKELETON-UI-ELEMENT ] list of gdl objects of type form-control The validation-function will be forced on these objects when a form is submitted, even if the object's html form-control does not happen to be included in the values submitted with the form. Defaults to nil.
```

---

## index.html (chunk 7/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/1/index.html
Type: reference

```
n-function will be forced on these objects when a form is submitted, even if the object's html form-control does not happen to be included in the values submitted with the form. Defaults to nil. HEAD-CLASS [ from BASE-AJAX-SHEET ] string or nil Names the value of class attribute for the head tag. Default is nil. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. HTML-CLASS [ from BASE-AJAX-SHEET ] string or nil Names the value of class attribute for the html tag. Default is nil. HTML-SECTIONS [ from SKELETON-UI-ELEMENT ] List of HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded.
```

---

## index.html (chunk 8/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/1/index.html
Type: reference

```
ently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded. HTML-SECTIONS-VALID [ from SKELETON-UI-ELEMENT ] List of valid HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil IMAGE-FORMAT keyword symbol Determines the default image format. Defaults to the currently selected value of the image-format-selector, which itself defaults to :raphael. IMAGE-FORMAT-DEFAULT keyword symbol, one of the keys from (the image-format-plist) . Default for the image-format-selector. Defaults to :png.
```

---

## index.html (chunk 9/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/1/index.html
Type: reference

```
:raphael. IMAGE-FORMAT-DEFAULT keyword symbol, one of the keys from (the image-format-plist) . Default for the image-format-selector. Defaults to :png. IMAGE-FORMAT-PLIST plist of keys and strings The default formats for graphics display. Defaults to: (list :png "PNG image" :jpeg "jpeg image" :raphael "SVG/VML") IMMUNE-OBJECTS list of gdl objects These objects are not used in computing the scale or centering for the display list. Defaults to nil. INCLUDE-VIEW-CONTROLS? boolean Indicates whether standard view-controls panel should be included with the graphics. INNER-HTML [ from SKELETON-UI-ELEMENT ] string This can be used with (str .) [in cl-who] or (:princ .) [in htmlGen] to output this section of the page, without the wrapping :div tag [so if you use this, your code would be responsible for wrapping the :div tag with :id (the dom-id).] LENGTH [ from GEOMETRY-VIEW-MIXIN ] number Length ("height" of screen window) of the graphics viewport. Default is 300.
```

---

## index.html (chunk 10/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/1/index.html
Type: reference

```
e for wrapping the :div tag with :id (the dom-id).] LENGTH [ from GEOMETRY-VIEW-MIXIN ] number Length ("height" of screen window) of the graphics viewport. Default is 300. LOCAL-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object. MAIN-SHEET-BODY [ from BASE-AJAX-SHEET ] string of html The main body of the page. This can be specified as input or overridden in subclass, otherwise it defaults to the content produced by the :output-function of the same name in the applicable lens for html-format. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM.
```

---

## index.html (chunk 11/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/1/index.html
Type: reference

```
of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. ORDERED-FORM-CONTROLS [ from SKELETON-UI-ELEMENT ] list of gdl objects, which should be of type 'base-form-control . [Note -- this slot is not really necessary for protecting out-of-bounds sequence references anymore, the form-control processor protects against this by itself now]. These objects are validated and bashed first, in the order given. If the cardinality of one form-control depends on another as in the example below, then you should list those dependent objects first. Default is nil. examples: ...
```

---

## index.html (chunk 12/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/1/index.html
Type: reference

```
:computed-slots ((number-of-nozzles (the number-of-nozzles-form value)) (ordered-form-controls (append (list-elements (the inner-flange-form)) (list (the number-of-nozzles-form))))) :objects ((inner-flange-form :type 'menu-form-control :choice-plist (list :hey "hey" :now "now") :default :hey :sequence (:size (the number-of-nozzles))) (number-of-nozzles-form :type 'text-form-control :prompt "Number of Shell Nozzles Required: " :domain :number :default 0) ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0).
```

---

## index.html (chunk 13/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/1/index.html
Type: reference

```
ng each row is a vector with a magnitude of one (1.0). POSSIBLE-NILS [ from SKELETON-UI-ELEMENT ] list of keyword symbols Messages corresponding to form fields which could be missing from form submission (e.g. checkbox fields). Defaults to the names of any children or hidden-children of type menu-form-control or checkbox-form-control. PRESET-ALL? [ from SKELETON-UI-ELEMENT ] boolean This switch determines whether all form-controls should be preset before the final setting, in order to allow any interdependencies to be detected for validation or detecting changed values. If this is specified as a non-nil value, then any nil values of (the preset?) on individual form controls will be ignored. If this is specified as nil, then (the preset?) of individual form-controls (default of these is also nil) will be respected. Default is nil.
```

---

## index.html (chunk 14/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/1/index.html
Type: reference

```
o nil) will be respected. Default is nil. PROCESS-COOKIES! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in a user specialization of base-html-sheet, to do some processing before the header-plist is evaluated and before the HTTP response is actually initiated, but after the cookies-received have been set. PROJECTION-VECTOR 3d vector This is the normal vector of the view plane onto which to project the 3D objects. Defaults to (getf *standard-views* (the view-selector value)), and (the view-selector value) defaults to :top. RETURN-OBJECT [ from BASE-HTML-SHEET ] gdl object Default object to which control will return with the write-back-link method ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists.
```

---

## index.html (chunk 15/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/1/index.html
Type: reference

```
archy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STANDARD-VIEWS [ from BASE-HTML-GRAPHICS-SHEET ] plist of keywords and 3d vectors . Indicates the views to show in the graphics controls. STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. SVG-CLASS string with css classes These classes will be included in any svg tag outputted from this drawing. TARGET [ from BASE-HTML-SHEET ] string Name of a browser frame or window to display this page. Default of NIL indicates to use the same window.
```

---

## index.html (chunk 16/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/1/index.html
Type: reference

```
ET [ from BASE-HTML-SHEET ] string Name of a browser frame or window to display this page. Default of NIL indicates to use the same window. TITLE [ from BASE-AJAX-SHEET ] string The title of the web page. Defaults to "GendL Application -" .followed by the strings-for-display. TRANSITORY-SLOTS [ from BASE-HTML-SHEET ] list of keyword symbols Messages corresponding to form fields which should not be retained against Updates to the model (e.g. calls to the update! function or hitting the Update button or link in the browser in development mode). Defaults to NIL (the empty list). USE-BSPLINES? [ from BASE-HTML-GRAPHICS-SHEET ] boolean Determines whether to use native bspline data in the vrml USE-RAPHAEL-GRAF? boolean Include raphael graphing library in the page header? Default nil. USE-RAPHAEL? boolean Include raphael javascript library in the page header? Default nil. VIEW-DIRECTION-DEFAULT Default view initially in the view-selector which is automatically included in the view-controls.
```

---

## index.html (chunk 17/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/1/index.html
Type: reference

```
? boolean Include raphael javascript library in the page header? Default nil. VIEW-DIRECTION-DEFAULT Default view initially in the view-selector which is automatically included in the view-controls. VIEWPORT-BORDER-DEFAULT number Thickness of default border around graphics viewport. Default is 1. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. WIDTH [ from GEOMETRY-VIEW-MIXIN ] number Width of the graphics viewport. Default is 300. Input Slots (optional, settable) ADDITIONAL-HEADER-CONTENT [ from BASE-AJAX-SHEET ] string of valid html Additional tag content to go into the page header, if you use the default main-sheet message and just fill in your own main-sheet-body, as is the intended use of the base-ajax-sheet primitive.
```

---

## index.html (chunk 18/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/1/index.html
Type: reference

```
body, as is the intended use of the base-ajax-sheet primitive. ADDITIONAL-HEADER-JS-CONTENT [ from BASE-AJAX-SHEET ] valid javascript This javascript is added to the head of the page, just before the body. DIGITATION-MODE [ from BASE-HTML-GRAPHICS-SHEET ] keyword symbol, one of :zoom-and-center , :report-point , or :measure-distance . If :zoom-and-center , sets the user-center and user-scale accordingly when graphics area is clicked. If :report-point , the slot digitized-point is set with the x y value. If measure-distance , the slot :digitized-distance is set with the resultant distance. Default is :zoom-and-center UI-SPECIFIC-LAYOUT-JS [ from BASE-AJAX-SHEET ] absolute uri in the browser . This is additional JavaScript that needs to be loaded in order to initiate the layout of a user interface. Defaults to nil. USE-JQUERY? [ from BASE-AJAX-SHEET ] boolean Include jquery javascript libraries in the page header? Default nil.
```

---

## index.html (chunk 19/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/1/index.html
Type: reference

```
erface. Defaults to nil. USE-JQUERY? [ from BASE-AJAX-SHEET ] boolean Include jquery javascript libraries in the page header? Default nil. VIEW [ from BASE-HTML-GRAPHICS-SHEET ] keyword symbol Determines the default view from the standard-views . Defaults to :trimetric. ZOOM-FACTOR [ from BASE-HTML-GRAPHICS-SHEET ] number The factor used for zooming in or out. ZOOM-MODE [ from BASE-HTML-GRAPHICS-SHEET ] keyword symbol, one of :in, :out, or :none, or nil If :in, then clicks in the graphics area will increase the zoom factor by (the zoom-factor). If :out, then clicks will decrease the factor by that amount. If :none or nil, then clicks will have no effect. Input Slots (optional, defaulting) DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g.
```

---

## index.html (chunk 20/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/1/index.html
Type: reference

```
eristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. RESPONDENT [ from SKELETON-UI-ELEMENT ] gdl object Object to respond to the form submission. Defaults to self. Computed Slots (settable) DROPPED-HEIGHT-WIDTH plist with :width and :height The dimensions of the bounding-box of the dragged and/or dropped element.
```

---

## index.html (chunk 21/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/1/index.html
Type: reference

```
on. Defaults to self. Computed Slots (settable) DROPPED-HEIGHT-WIDTH plist with :width and :height The dimensions of the bounding-box of the dragged and/or dropped element. DROPPED-OBJECT list representing gdl root-path This is the root path of the dragged and/or dropped object. This is not tested to see if it is part of the same object tree as current self. DROPPED-X-Y 3d point This is the upper-right corner of the bounding box of the dragged and/or dropped element. JS-TO-EVAL [ from SKELETON-UI-ELEMENT ] string of valid javascript This Javascript will be send with the Ajax response, and evaluated after the innerHTML for this section has been replaced. Computed Slots GRAPHICS string of valid html This can be used to include the geometry, in the format currently selected by the image-format-selector. If the include-view-controls? is non-nil, the view-controls will be appended at the bottom of the graphics inside a table.
```

---

## index.html (chunk 22/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/1/index.html
Type: reference

```
t-selector. If the include-view-controls? is non-nil, the view-controls will be appended at the bottom of the graphics inside a table. RASTER-GRAPHICS string of valid html This can be used to include the PNG or JPG raster-graphics of the geometry. VECTOR-GRAPHICS string of valid html This can be used to include the SVG or VML vector-graphics of the geometry. VIEW-CONTROLS string of valid html This includes the image-format-selector, the reset-zoom-button, and the view-selector, in a simple table layout. You can override this to make the view-controls appear any way you want and include different and/or additional form-controls. X3DOM-GRAPHICS string of valid html This can be used to include the x3dom tag content for the geometry. Hidden Objects IMAGE-FORMAT-SELECTOR object of type menu-form-control Its value slot can be used to determine the format of image displayed. VIEW-OBJECT [ from GEOMETRY-VIEW-MIXIN ] gdl web-drawing object This must be overridden in the specialized class.
```

---

## index.html (chunk 23/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/1/index.html
Type: reference

```
m-control Its value slot can be used to determine the format of image displayed. VIEW-OBJECT [ from GEOMETRY-VIEW-MIXIN ] gdl web-drawing object This must be overridden in the specialized class. GDL Functions WRITE-EMBEDDED-X3DOM-WORLD void Writes an embedded X3D tag with content for the view-object child of this object. The view-object child should exist and be of type web-drawing . Examples FLAG -- Fill in!!! Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/20)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/0/index.html
Type: reference

```
GendL Application - APPLICATION-MIXIN Package Documentation Object: APPLICATION-MIXIN (The :GWL Package) Mixins: LAYOUT-MIXIN , VANILLA-MIXIN Description This mixin generates a default GWL user interface, similar to node-mixin , but you should use application-mixin if this is a leaf-level application (i.e. has no children of type node-mixin or application-mixin Input Slots (optional) AFTER-PRESENT! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the respondent of a form, to do some processing after the respondent's write-html-sheet function runs to present the object. AFTER-SET! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the requestor of a form, to do some processing after the requestor's form values are set into the specified bashee. AVAILABLE-IMAGE-FORMATS [ from LAYOUT-MIXIN ] list of keyword symbols Determines which formats are available in the Preferences.
```

---

## index.html (chunk 2/20)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/0/index.html
Type: reference

```
lues are set into the specified bashee. AVAILABLE-IMAGE-FORMATS [ from LAYOUT-MIXIN ] list of keyword symbols Determines which formats are available in the Preferences. Defaults to :png, :jpeg, and :vrml. BASHEE [ from SKELETON-UI-ELEMENT ] gdl object Object to have its settable computed-slots and/or query-plist set from the fields on the form upon submission. Defaults to self. BEFORE-PRESENT! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the respondent of a form, to do some processing before the respondent's write-html-sheet function runs to present the object. This can be useful especially for objects which are subclasses of higher-level mixins such as application-mixin and node-mixin , where you do not have direct access to the write-html-sheet function and typically only define the model-inputs function.
```

---

## index.html (chunk 3/20)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/0/index.html
Type: reference

```
heet function and typically only define the model-inputs function. It is not always reliable to do processing in the model-inputs function, since some slots which depend on your intended modifications may already have been evaluated by the time the model-inputs function runs. BEFORE-RESPONSE! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in a user specialization of base-html-sheet, to do some processing before the header-plist is evaluated and before the HTTP response is actually initiated. BEFORE-SET! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the requestor of a form, to do some processing before the requestor's form values are set into the specified bashee. BODY-BGCOLOR [ from LAYOUT-MIXIN ] keyword symbol Color keyword from *color-table* for the body background. Defaults to :blue-sky .
```

---

## index.html (chunk 4/20)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/0/index.html
Type: reference

```
word symbol Color keyword from *color-table* for the body background. Defaults to :blue-sky . BOUNDING-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object. CENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located. CHECK-SANITY? [ from BASE-HTML-SHEET ] boolean Determines whether a a sanity check is done (with the check-sanity function) before presenting the response page if this page is a respondent. Default is NIL. DOM-ID [ from SKELETON-UI-ELEMENT ] string This is the auto-computed dom-id which should be used for rendering this section.
```

---

## index.html (chunk 5/20)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/0/index.html
Type: reference

```
If you use the main-div HTML string for rendering this object as a page section, then you do not have to generate the :div tag yourself - the main-div will be a string of HTML which is wrapped in the correct :div tag already. FORCE-VALIDATION-FOR [ from SKELETON-UI-ELEMENT ] list of gdl objects of type form-control The validation-function will be forced on these objects when a form is submitted, even if the object's html form-control does not happen to be included in the values submitted with the form. Defaults to nil. HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. HTML-SECTIONS [ from SKELETON-UI-ELEMENT ] List of HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk.
```

---

## index.html (chunk 6/20)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/0/index.html
Type: reference

```
N-UI-ELEMENT ] List of HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded. HTML-SECTIONS-VALID [ from SKELETON-UI-ELEMENT ] List of valid HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil IMAGE-FORMAT [ from BASE-HTML-GRAPHICS-SHEET ] keyword symbol Determines the default image format.
```

---

## index.html (chunk 7/20)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/0/index.html
Type: reference

```
object. Defaults to nil IMAGE-FORMAT [ from BASE-HTML-GRAPHICS-SHEET ] keyword symbol Determines the default image format. Defaults to :png INNER-HTML [ from SKELETON-UI-ELEMENT ] string This can be used with (str .) [in cl-who] or (:princ .) [in htmlGen] to output this section of the page, without the wrapping :div tag [so if you use this, your code would be responsible for wrapping the :div tag with :id (the dom-id).] INPUTS-BGCOLOR [ from LAYOUT-MIXIN ] keyword symbol Color keyword from *color-table* for the model-inputs area background. Defaults to :aquamarine . INPUTS-TITLE [ from LAYOUT-MIXIN ] string Title for the model-inputs section. Defaults to "Model Inputs". JS-TO-EVAL [ from SKELETON-UI-ELEMENT ] string of valid javascript This Javascript will be send with the Ajax response, and evaluated after the innerHTML for this section has been replaced. LENGTH [ from GEOMETRY-VIEW-MIXIN ] number Length ("height" of screen window) of the graphics viewport. Default is 300.
```

---

## index.html (chunk 8/20)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/0/index.html
Type: reference

```
and evaluated after the innerHTML for this section has been replaced. LENGTH [ from GEOMETRY-VIEW-MIXIN ] number Length ("height" of screen window) of the graphics viewport. Default is 300. LOCAL-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object. MULTIPART-FORM? [ from LAYOUT-MIXIN ] boolean Determines whether the embedded form will support multipart MIME parts. Defaults to NIL. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. ORDERED-FORM-CONTROLS [ from SKELETON-UI-ELEMENT ] list of gdl objects, which should be of type 'base-form-control .
```

---

## index.html (chunk 9/20)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/0/index.html
Type: reference

```
ends which support this functionality, e.g. SVG/Raphael and X3DOM. ORDERED-FORM-CONTROLS [ from SKELETON-UI-ELEMENT ] list of gdl objects, which should be of type 'base-form-control . [Note -- this slot is not really necessary for protecting out-of-bounds sequence references anymore, the form-control processor protects against this by itself now]. These objects are validated and bashed first, in the order given. If the cardinality of one form-control depends on another as in the example below, then you should list those dependent objects first. Default is nil. examples: ...
```

---

## index.html (chunk 10/20)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/0/index.html
Type: reference

```
:computed-slots ((number-of-nozzles (the number-of-nozzles-form value)) (ordered-form-controls (append (list-elements (the inner-flange-form)) (list (the number-of-nozzles-form))))) :objects ((inner-flange-form :type 'menu-form-control :choice-plist (list :hey "hey" :now "now") :default :hey :sequence (:size (the number-of-nozzles))) (number-of-nozzles-form :type 'text-form-control :prompt "Number of Shell Nozzles Required: " :domain :number :default 0) ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). OTHER-RULES [ from LAYOUT-MIXIN ] list of gdl objects of type base-rule-object or (preferably) gwl-base-rule-object .
```

---

## index.html (chunk 11/20)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/0/index.html
Type: reference

```
ng each row is a vector with a magnitude of one (1.0). OTHER-RULES [ from LAYOUT-MIXIN ] list of gdl objects of type base-rule-object or (preferably) gwl-base-rule-object . Links to these will be displayed in the other-rules section. Default to the collection of all objects of type base-rule-object from this node in the tree down to the leaves, whose violated? message evaluates to NIL. OTHER-RULES-BGCOLOR [ from LAYOUT-MIXIN ] keyword symbol Color keyword from *color-table* for the other-rules area background. Defaults to :aquamarine . OTHER-RULES-TITLE [ from LAYOUT-MIXIN ] string Title for the other-rules section. Defaults to "Other Rules". PAGE-TITLE [ from LAYOUT-MIXIN ] string The title to display on the page and in the tree. Defaults to (the strings-for-display) . POSSIBLE-NILS [ from SKELETON-UI-ELEMENT ] list of keyword symbols Messages corresponding to form fields which could be missing from form submission (e.g. checkbox fields).
```

---

## index.html (chunk 12/20)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/0/index.html
Type: reference

```
m SKELETON-UI-ELEMENT ] list of keyword symbols Messages corresponding to form fields which could be missing from form submission (e.g. checkbox fields). Defaults to the names of any children or hidden-children of type menu-form-control or checkbox-form-control. PRESET-ALL? [ from SKELETON-UI-ELEMENT ] boolean This switch determines whether all form-controls should be preset before the final setting, in order to allow any interdependencies to be detected for validation or detecting changed values. If this is specified as a non-nil value, then any nil values of (the preset?) on individual form controls will be ignored. If this is specified as nil, then (the preset?) of individual form-controls (default of these is also nil) will be respected. Default is nil.
```

---

## index.html (chunk 13/20)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/0/index.html
Type: reference

```
PROCESS-COOKIES! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in a user specialization of base-html-sheet, to do some processing before the header-plist is evaluated and before the HTTP response is actually initiated, but after the cookies-received have been set. RETURN-OBJECT [ from BASE-HTML-SHEET ] gdl object Default object to which control will return with the write-back-link method ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information SHOW-TITLE? [ from LAYOUT-MIXIN ] boolean Indicates whether to display the title at the top of the page. Defaults to T. STANDARD-VIEWS [ from BASE-HTML-GRAPHICS-SHEET ] plist of keywords and 3d vectors .
```

---

## index.html (chunk 14/20)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/0/index.html
Type: reference

```
MIXIN ] boolean Indicates whether to display the title at the top of the page. Defaults to T. STANDARD-VIEWS [ from BASE-HTML-GRAPHICS-SHEET ] plist of keywords and 3d vectors . Indicates the views to show in the graphics controls. STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. TARGET [ from BASE-HTML-SHEET ] string Name of a browser frame or window to display this page. Default of NIL indicates to use the same window. TRANSITORY-SLOTS [ from BASE-HTML-SHEET ] list of keyword symbols Messages corresponding to form fields which should not be retained against Updates to the model (e.g. calls to the update! function or hitting the Update button or link in the browser in development mode). Defaults to NIL (the empty list).
```

---

## index.html (chunk 15/20)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/0/index.html
Type: reference

```
hould not be retained against Updates to the model (e.g. calls to the update! function or hitting the Update button or link in the browser in development mode). Defaults to NIL (the empty list). TREE-BGCOLOR [ from LAYOUT-MIXIN ] keyword symbol Color keyword from *color-table* for the tree area background. Defaults to :aquamarine . TREE-TITLE [ from LAYOUT-MIXIN ] string Title for the Tree section. Defaults to "Assembly Tree" if the tree-root is only a subclass of application-mixin , and "Assembly Tree" if the tree-root is an actual node with child applications. UI-DISPLAY-LIST-LEAVES [ from LAYOUT-MIXIN ] list of gdl objects This should be overridden with a list of objects of your choice. These objects (not their leaves, but these actual nodes) will be scaled to fit and displayed in the graphics area. Defaults to NIL. UI-DISPLAY-LIST-OBJECTS [ from LAYOUT-MIXIN ] list of gdl objects This should be overridden with a list of objects of your choice.
```

---

## index.html (chunk 16/20)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/0/index.html
Type: reference

```
raphics area. Defaults to NIL. UI-DISPLAY-LIST-OBJECTS [ from LAYOUT-MIXIN ] list of gdl objects This should be overridden with a list of objects of your choice. The leaves of these objects will be scaled to fit and displayed in the graphics area. Defaults to NIL. USE-BSPLINES? [ from BASE-HTML-GRAPHICS-SHEET ] boolean Determines whether to use native bspline data in the vrml VIOLATED-RULES [ from LAYOUT-MIXIN ] list of gdl objects of type base-rule-object or (preferably) gwl-base-rule-object . Links to these will be displayed in the other-rules section. Default to the collection of all objects of type base-rule-object from this node in the tree down to the leaves, whose violated? message evaluates to non-NIL. VIOLATED-RULES-BGCOLOR [ from LAYOUT-MIXIN ] keyword symbol Color keyword from *color-table* for the violated-rules area background. Defaults to :aquamarine . VIOLATED-RULES-TITLE [ from LAYOUT-MIXIN ] string Title for the violated-rules section. Defaults to "Violated Rules".
```

---

## index.html (chunk 17/20)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/0/index.html
Type: reference

```
color-table* for the violated-rules area background. Defaults to :aquamarine . VIOLATED-RULES-TITLE [ from LAYOUT-MIXIN ] string Title for the violated-rules section. Defaults to "Violated Rules". VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. WIDTH [ from GEOMETRY-VIEW-MIXIN ] number Width of the graphics viewport. Default is 300. Input Slots (optional, settable) DIGITATION-MODE [ from BASE-HTML-GRAPHICS-SHEET ] keyword symbol, one of :zoom-and-center , :report-point , or :measure-distance . If :zoom-and-center , sets the user-center and user-scale accordingly when graphics area is clicked. If :report-point , the slot digitized-point is set with the x y value. If measure-distance , the slot :digitized-distance is set with the resultant distance.
```

---

## index.html (chunk 18/20)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/0/index.html
Type: reference

```
If measure-distance , the slot :digitized-distance is set with the resultant distance. Default is :zoom-and-center VIEW [ from BASE-HTML-GRAPHICS-SHEET ] keyword symbol Determines the default view from the standard-views . Defaults to :trimetric. ZOOM-FACTOR [ from BASE-HTML-GRAPHICS-SHEET ] number The factor used for zooming in or out. ZOOM-MODE [ from BASE-HTML-GRAPHICS-SHEET ] keyword symbol, one of :in, :out, or :none, or nil If :in, then clicks in the graphics area will increase the zoom factor by (the zoom-factor). If :out, then clicks will decrease the factor by that amount. If :none or nil, then clicks will have no effect. Input Slots (optional, defaulting) DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white.
```

---

## index.html (chunk 19/20)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/0/index.html
Type: reference

```
. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. DISPLAY-RULES? [ from LAYOUT-MIXIN ] boolean Indicates whether the Rules panel should be displayed. Defaults to T. DISPLAY-TREE? [ from LAYOUT-MIXIN ] boolean Indicates whether the Tree area should be displayed. Defaults to T. GRAPHICS-HEIGHT [ from LAYOUT-MIXIN ] integer Height (top to bottom on screen) in pixels of the graphics area. Defaults to 500.
```

---

## index.html (chunk 20/20)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/0/index.html
Type: reference

```
layed. Defaults to T. GRAPHICS-HEIGHT [ from LAYOUT-MIXIN ] integer Height (top to bottom on screen) in pixels of the graphics area. Defaults to 500. GRAPHICS-WIDTH [ from LAYOUT-MIXIN ] integer Height (left to right on screen) in pixels of the graphics area. Defaults to 500. RESPONDENT [ from SKELETON-UI-ELEMENT ] gdl object Object to respond to the form submission. Defaults to self. USE-STANDARD-SAVED-SLOTS? [ from LAYOUT-MIXIN ] boolean Determines whether the standard-saved-slots are automatically used by default for the saved-slots. This is a trickle-down slot so its value will be passed to descendent objects automatically. The default value is NIL. Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/22)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/3/index.html
Type: reference

```
GendL Application - BASE-FORM-CONTROL Package Documentation Object: BASE-FORM-CONTROL (The :GWL Package) Mixins: SKELETON-FORM-CONTROL , VANILLA-MIXIN Author Dave Cooper, Genworks Description This object can be used to represent a single HTML form control. It captures the initial default value, some display information such as the label, and all the standard HTML tag attributes for the tag e.g. INPUT, SELECT, TEXTAREA. GWL will process the data types according to specific rules, and validate the typed value according to other default rules. A custom validation-function can also be provided by user code. Sequences of these objects (with :size, :indices, :matrix, and :radial) are supported. This facility and its documentation is expected to undergo significant and frequent upgrades in the remainder of GDL 1573 and upcoming 1575. Current to-do list: Currently this works with normal HTTP form submission and full page reloading.
```

---

## index.html (chunk 2/22)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/3/index.html
Type: reference

```
mainder of GDL 1573 and upcoming 1575. Current to-do list: Currently this works with normal HTTP form submission and full page reloading. We intend to make it work with AJAX and surgical page update as well. We intend to provide inputs for all the standard tag attributes for the accompanying LABEL tag for the form control. Additional form control elements to be included, to cover all types of form elements specified in current HTML standard from http://www.w3.org/TR/html401/interact/forms.html#h-17.2.1 button-form-control: submit buttons, reset buttons, push buttons. checkbox-form-control: checkboxes, radio buttons (multiple of these must be able to have same name) menu-form-control: select, along with optgroup and option. text-form-control: single-line text input (including masked passwords) and multi-line (TEXTAREA) text input. file-form-control: file select for submittal with a form. hidden-form-control: input of type hidden.
```

---

## index.html (chunk 3/22)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/3/index.html
Type: reference

```
ds) and multi-line (TEXTAREA) text input. file-form-control: file select for submittal with a form. hidden-form-control: input of type hidden. object-form-control: (not sure how this is supposed to work yet). Also, we have to study and clarify the issue of under what conditions values can possibly take on nil values, and what constitutes a required field as opposed to a non-validated field, and whether a blank string on a text input should be represented as a nil value or as an empty string. Note that checkbox-form-control and menu-form-control currently get automatically included in the possible-nils. Input Slots (optional) ACCEPT string or nil Maps to HTML form control attribute of the same name. Default is nil. ACCESSKEY string or nil Maps to HTML form control attribute of the same name. Default is nil. AJAX-SUBMIT-ON-CHANGE? boolean If set to non-nil, this field's value will be sent to server upon change. Default is nil.
```

---

## index.html (chunk 4/22)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/3/index.html
Type: reference

```
. Default is nil. AJAX-SUBMIT-ON-CHANGE? boolean If set to non-nil, this field's value will be sent to server upon change. Default is nil. AJAX-SUBMIT-ON-ENTER? boolean If set to non-nil, this field's value will be sent to server upon enter. Default is nil. ALIGN string or nil Maps to HTML form control attribute of the same name. Default is nil. ALLOW-INVALID-TYPE? boolean If non-nil, then values which fail the type test will still be allowed to be the value. Default is nil. ALLOW-INVALID? boolean If non-nil, then values which fail the type or validation test will still be allowed to be the value. Default is t. ALLOW-NIL? boolean Regardless of :domain, if this is non-nil, nil values will be accepted. Defaults to t if (the default) is nil, otherwise defaults to nil. ALT string or nil Maps to HTML form control attribute of the same name. Default is nil.
```

---

## index.html (chunk 5/22)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/3/index.html
Type: reference

```
o HTML form control attribute of the same name. Default is nil. APPEND-ERROR-STRING? boolean Determines whether a default error string is appended to string ouput-function for html-format (and therefore html-string computed-slot as well). Defaults to t. BASHEE [ from SKELETON-UI-ELEMENT ] gdl object Object to have its settable computed-slots and/or query-plist set from the fields on the form upon submission. Defaults to self. CLASS [ from SKELETON-FORM-CONTROL ] string You can use this to specify a user-defined class for the form-control. Defaults to nil, which means no class attribute will be generated. DEFAULT lisp value of a type compatible with (the domain) This is the initial default value for the control. This must be specified by user code, or an error will result. DISABLED? boolean Maps to HTML form control attribute of the same name. Default is nil. DOM-ID [ from SKELETON-UI-ELEMENT ] string This is the auto-computed dom-id which should be used for rendering this section.
```

---

## index.html (chunk 6/22)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/3/index.html
Type: reference

```
Maps to HTML form control attribute of the same name. Default is nil. DOM-ID [ from SKELETON-UI-ELEMENT ] string This is the auto-computed dom-id which should be used for rendering this section. If you use the main-div HTML string for rendering this object as a page section, then you do not have to generate the :div tag yourself - the main-div will be a string of HTML which is wrapped in the correct :div tag already. DOMAIN keyword symbol, one of :number, :keyword, :list-of-strings, :list-of-anything, or :string . This specifies the expected and acceptable type for the submitted form value. If possible, the submitted value will be coerced into the specified type. The default is based upon the Lisp type of (the default) provided as input to this object. If the default is nil, the domain will default to :string FIELD-NAME [ from SKELETON-FORM-CONTROL ] keyword symbol The name of this field. Computed from the object name within the tree.
```

---

## index.html (chunk 7/22)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/3/index.html
Type: reference

```
ll default to :string FIELD-NAME [ from SKELETON-FORM-CONTROL ] keyword symbol The name of this field. Computed from the object name within the tree. FORCE-VALIDATION-FOR [ from SKELETON-UI-ELEMENT ] list of gdl objects of type form-control The validation-function will be forced on these objects when a form is submitted, even if the object's html form-control does not happen to be included in the values submitted with the form. Defaults to nil. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. HTML-SECTIONS [ from SKELETON-UI-ELEMENT ] List of HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded.
```

---

## index.html (chunk 8/22)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/3/index.html
Type: reference

```
who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded. HTML-SECTIONS-VALID [ from SKELETON-UI-ELEMENT ] List of valid HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded. ID [ from SKELETON-FORM-CONTROL ] keyword symbol The ID attribute for this tag. Defaults to (the field-name). INNER-HTML [ from SKELETON-UI-ELEMENT ] string This can be used with (str .) [in cl-who] or (:princ .) [in htmlGen] to output this section of the page, without the wrapping :div tag [so if you use this, your code would be responsible for wrapping the :div tag with :id (the dom-id).] ISMAP? boolean Maps to HTML form control attribute of the same name. Default is nil.
```

---

## index.html (chunk 9/22)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/3/index.html
Type: reference

```
this, your code would be responsible for wrapping the :div tag with :id (the dom-id).] ISMAP? boolean Maps to HTML form control attribute of the same name. Default is nil. JS-TO-EVAL [ from SKELETON-UI-ELEMENT ] string of valid javascript This Javascript will be send with the Ajax response, and evaluated after the innerHTML for this section has been replaced. LABEL-POSITION keyword symbol or nil Specifies where the label tag goes, if any. Can be :table-td (label goes in a td before the form control), :table-td-append (label goes in a td after the form control), prepend: (label tag wraps around form control and label text comes before form control), append: (label tag wraps around form control and label text comes after form control), table-with-class: (like :table-td, but adds a class "form-control" to the table), or as-div: (puts label and control inside a div of class "form-control"). Default is :as-div LANG string or nil Maps to HTML form control attribute of the same name.
```

---

## index.html (chunk 10/22)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/3/index.html
Type: reference

```
m-control" to the table), or as-div: (puts label and control inside a div of class "form-control"). Default is :as-div LANG string or nil Maps to HTML form control attribute of the same name. Default is nil. MAXLENGTH number or nil Maps to HTML form control attribute of the same name. Default is nil. NULLIFY-EMPTY-STRING? boolean Regardless of :domain, if this is non-nil, empty strings will convert to nil. Defaults to (the allow-nil?) ONBLUR string or nil Maps to HTML form control attribute of the same name. Default is nil. ONCHANGE string or nil Maps to HTML form control attribute of the same name. Default is nil, unless ajax-submit-on-change? is non-nil, in which case it calls ajax to set current form value. ONCLICK string or nil Maps to HTML form control attribute of the same name. Default is nil. ONDBLCLICK string or nil Maps to HTML form control attribute of the same name. Default is nil. ONENTER string or nil Maps to HTML form control attribute of the same name.
```

---

## index.html (chunk 11/22)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/3/index.html
Type: reference

```
ult is nil. ONDBLCLICK string or nil Maps to HTML form control attribute of the same name. Default is nil. ONENTER string or nil Maps to HTML form control attribute of the same name. Default is nil, unless ajax-submit-on-enter? is non-nil, in which case it calls ajax to set current form value. ONFOCUS string or nil Maps to HTML form control attribute of the same name. Default is nil. ONKEYDOWN string or nil Maps to HTML form control attribute of the same name. Default is nil. ONKEYPRESS string or nil Maps to HTML form control attribute of the same name. Default is nil. ONKEYUP string or nil Maps to HTML form control attribute of the same name. Default is nil. ONMOUSEDOWN string or nil Maps to HTML form control attribute of the same name. Default is nil. ONMOUSEMOVE string or nil Maps to HTML form control attribute of the same name. Default is nil. ONMOUSEOUT string or nil Maps to HTML form control attribute of the same name. Default is nil.
```

---

## index.html (chunk 12/22)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/3/index.html
Type: reference

```
ML form control attribute of the same name. Default is nil. ONMOUSEOUT string or nil Maps to HTML form control attribute of the same name. Default is nil. ONMOUSEOVER string or nil Maps to HTML form control attribute of the same name. Default is nil. ONMOUSEUP string or nil Maps to HTML form control attribute of the same name. Default is nil. ONSELECT string or nil Maps to HTML form control attribute of the same name. Default is nil. ORDERED-FORM-CONTROLS [ from SKELETON-UI-ELEMENT ] list of gdl objects, which should be of type 'base-form-control . [Note -- this slot is not really necessary for protecting out-of-bounds sequence references anymore, the form-control processor protects against this by itself now]. These objects are validated and bashed first, in the order given. If the cardinality of one form-control depends on another as in the example below, then you should list those dependent objects first. Default is nil. examples: ...
```

---

## index.html (chunk 13/22)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/3/index.html
Type: reference

```
ality of one form-control depends on another as in the example below, then you should list those dependent objects first. Default is nil. examples: ... :computed-slots ((number-of-nozzles (the number-of-nozzles-form value)) (ordered-form-controls (append (list-elements (the inner-flange-form)) (list (the number-of-nozzles-form))))) :objects ((inner-flange-form :type 'menu-form-control :choice-plist (list :hey "hey" :now "now") :default :hey :sequence (:size (the number-of-nozzles))) (number-of-nozzles-form :type 'text-form-control :prompt "Number of Shell Nozzles Required: " :domain :number :default 0) PLACEHOLDER string Text to place in the field by default, overwritten as soon as the field is selected. Works only in HTML5. Default is nil. POSSIBLE-NILS [ from SKELETON-UI-ELEMENT ] list of keyword symbols Messages corresponding to form fields which could be missing from form submission (e.g. checkbox fields).
```

---

## index.html (chunk 14/22)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/3/index.html
Type: reference

```
f keyword symbols Messages corresponding to form fields which could be missing from form submission (e.g. checkbox fields). Defaults to the names of any children or hidden-children of type menu-form-control or checkbox-form-control. PRESET-ALL? [ from SKELETON-UI-ELEMENT ] boolean This switch determines whether all form-controls should be preset before the final setting, in order to allow any interdependencies to be detected for validation or detecting changed values. If this is specified as a non-nil value, then any nil values of (the preset?) on individual form controls will be ignored. If this is specified as nil, then (the preset?) of individual form-controls (default of these is also nil) will be respected. Default is nil. PRESET? boolean This switch determines whether this form-control should be preset before the final setting, in order to allow any interdependencies to be detected for validation or detecting changed values. Default is nil.
```

---

## index.html (chunk 15/22)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/3/index.html
Type: reference

```
ol should be preset before the final setting, in order to allow any interdependencies to be detected for validation or detecting changed values. Default is nil. PRIMARY? [ from SKELETON-FORM-CONTROL ] boolean Set this to t if the form-control should always occur first in an outputted snapshot file. Defaults to nil. PROMPT string The prompt used in the label. READONLY? boolean Maps to HTML form control attribute of the same name. Default is nil. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information SIZE number or nil Maps to HTML form control attribute of the same name. Default is nil. SRC string or nil Maps to HTML form control attribute of the same name. Default is nil.
```

---

## index.html (chunk 16/22)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/3/index.html
Type: reference

```
r nil Maps to HTML form control attribute of the same name. Default is nil. SRC string or nil Maps to HTML form control attribute of the same name. Default is nil. STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. STYLE string or nil Maps to HTML form control attribute of the same name. Default is nil. TABINDEX integer or nil Maps to HTML form control attribute of the same name. Default is nil. TITLE string or nil Maps to HTML form control attribute of the same name. Default is nil. USEMAP string or nil Maps to HTML form control attribute of the same name. Default is nil. VALIDATION-FUNCTION function of one argument The argument will be the submitted form value converted to the proper type.
```

---

## index.html (chunk 17/22)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/3/index.html
Type: reference

```
tribute of the same name. Default is nil. VALIDATION-FUNCTION function of one argument The argument will be the submitted form value converted to the proper type. The return value from this function can be nil, any non-nil value, or a plist with keys :validated-value and :error. The following behavior applies: If the function returns nil, error is set to :unspecified-validation-fail. If the function returns a plist with keys :validated-value and :error, and if :error is non-nil, it means the value is not acceptable, the form-controls error message is set to this error (usually a keyword symbol), and the error string will be appended to the html-string by default. If the function returns any other value, then the properly typed submitted form value is considered valid and is used. In the case of an error, the form-control's failed-value message is set to the properly typed submitted form value.
```

---

## index.html (chunk 18/22)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/3/index.html
Type: reference

```
se of an error, the form-control's failed-value message is set to the properly typed submitted form value. If allow-invalid? is non-nil, then the form-control's value message is also set to this value (i.e. the invalid value is still accepted, even though a non-nil error is present). Default is (list :validated-value value :error nil). VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, defaulting) RESPONDENT [ from SKELETON-UI-ELEMENT ] gdl object Object to respond to the form submission. Defaults to self. Computed Slots (settable) ERROR string or error object This will be set to a validation error if any, and cleared when the error is gone. FAILED-VALUE lisp value The value which was attempted to be set but failed validation. VALUE lisp value The current value of this form control.
```

---

## index.html (chunk 19/22)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/3/index.html
Type: reference

```
AILED-VALUE lisp value The value which was attempted to be set but failed validation. VALUE lisp value The current value of this form control. GDL Functions RESTORE-DEFAULTS! void Restores the default for the value, the failed-value, and the error. Examples (in-package :gwl-user) (define-object test-form (base-html-sheet) :objects ((username :type 'text-form-control :size 35 :maxlength 30 :allow-nil? t :default "Ron Paul") (age :type 'text-form-control :size 5 :validation-function #'(lambda(input) (or (null input) (> 80 input 70))) :domain :number ;;:default 72 :default nil ) (bio :type 'text-form-control :rows 8 :size 120 :default " Congressman Ron Paul is the leading advocate for freedom in our nation's capital. As a member of the U.S. House of Representatives, Dr. Paul tirelessly works for limited constitutional government, low taxes, free markets, and a return to sound monetary policies.
```

---

## index.html (chunk 20/22)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/3/index.html
Type: reference

```
for limited constitutional government, low taxes, free markets, and a return to sound monetary policies. He is known among his congressional colleagues and his constituents for his consistent voting record. Dr. Paul never votes for legislation unless the proposed measure is expressly authorized by the Constitution. In the words of former Treasury Secretary William Simon, Dr. Paul is the one exception to the Gang of 535 on Capitol Hill.") (issues :type 'menu-form-control :choice-list (list "Taxes" "Health Care" "Foreign Policy") :default "Taxes" :multiple? t) (color :type 'menu-form-control :size 7 :choice-plist (list :red "red" :green "green" :blue "blue" :magenta "magenta" :cyan "cyan" :yellow "yellow" :orange "orange") :validation-function #'(lambda(color) (if (intersection (ensure-list color) (list :yellow :magenta)) (list :error :disallowed-color-choice) t)) ;;:append-error-string? nil :multiple? t :default :red ;;:onchange "alert('hey now');" ) (early-riser? :type
```

---

## index.html (chunk 21/22)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/3/index.html
Type: reference

```
color) (list :yellow :magenta)) (list :error :disallowed-color-choice) t)) ;;:append-error-string? nil :multiple? t :default :red ;;:onchange "alert('hey now');" ) (early-riser? :type 'checkbox-form-control :default nil) (favorite-links :type 'text-form-control :sequence (:size 3) :size 70 :default "http://"))) (define-lens (html-format test-form)() :output-functions ((main-sheet () (with-html-output (*html-stream* nil :indent t) (:html (:head (:title "Test Form")) (:body (:h2 (:center "Test Form")) (the write-development-links) (with-html-form (:cl-who? t) (:p (str (the username html-string))) (:p "(internal value is: " (fmt "~s" (the username value)) ")") (:p (str (the age html-string))) (:p "(internal value is: " (fmt "~s" (the age value)) ")") (:p (str (the bio html-string))) (:p (:table (:tr (:td (str (the issues html-string)))) (:tr (:td (str (the color html-string)))))) (:p (str (the early-riser? html-string))) (dolist (link (list-elements (the favorite-links))) (htm (str
```

---

## index.html (chunk 22/22)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/3/index.html
Type: reference

```
le (:tr (:td (str (the issues html-string)))) (:tr (:td (str (the color html-string)))))) (:p (str (the early-riser? html-string))) (dolist (link (list-elements (the favorite-links))) (htm (str (the-object link html-string)))) (:p ((:input :type :submit :value " OK ")))))))))) (publish :path "/fe" :function #'(lambda(req ent) (gwl-make-object req ent "gwl-user::test-form"))) Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/13)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/20/index.html
Type: reference

```
GendL Application - WEB-DRAWING Package Documentation Object: WEB-DRAWING (The :GWL Package) Mixins: RENDERER-MIXIN , BASE-DRAWING Description Container object for displaying a view of geometric or text-based entities in a web application. This is supposed to be the type of the view-object hidden-child of base-html-graphics-sheet. Also, in a GWL application using application-mixin, you can include one object of this type in the ui-display-list-leaves. Input Slots (optional) 3D-BOX [ from RENDERER-MIXIN ] list of two 3d points The left-front-lower and right-rear-upper corners of the axis-aligned bounding box of the object-roots and objects . 3D-BOX-CENTER [ from RENDERER-MIXIN ] 3d point The effective view center for the scene contained in this view object. Defaults to the center of the bounding sphere of all the objects in the scene, consisting of the object-roots and the objects .
```

---

## index.html (chunk 2/13)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/20/index.html
Type: reference

```
nding sphere of all the objects in the scene, consisting of the object-roots and the objects . BOUNDING-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object. BOUNDING-SPHERE [ from RENDERER-MIXIN ] plist containing keys: :center and :radius This plist represents the tightest-fitting sphere around all the objects listed in the object-roots and the objects FIELD-OF-VIEW-DEFAULT [ from RENDERER-MIXIN ] number in angular degrees The maximum angle of the view frustrum for perspective views. Defaults to 0.1 (which results in a near parallel projection with virtually no perspective effect). HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil.
```

---

## index.html (chunk 3/13)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/20/index.html
Type: reference

```
the reference box. Defaults to zero. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. IMMUNE-OBJECTS list of gdl objects These objects are not used in computing the scale or centering for the display list. Defaults to nil. LENGTH [ from BASE-OBJECT ] number Y-axis dimension of the reference box. Defaults to zero. LOCAL-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object. OBJECT-ROOTS list of gdl objects The leaves of each of these objects will be included in the geometry display. Defaults to nil. OBJECTS list of gdl objects These nodes (not their leaves but the actual objects) will be included in the geometry display. Defaults to nil. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation .
```

---

## index.html (chunk 4/13)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/20/index.html
Type: reference

```
tual objects) will be included in the geometry display. Defaults to nil. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. PAGE-LENGTH [ from BASE-DRAWING ] number in pdf points Front-to-back (or top-to-bottom) length of the paper being represented by this drawing. The default is (* 11 72) points, or 11 inches, corresponding to US standard letter-size paper. PAGE-WIDTH [ from BASE-DRAWING ] number in pdf points Left-to-right width of the paper being represented by this drawing. The default is (* 8.5 72) points, or 8.5 inches, corresponding to US standard letter-size paper. PROJECTION-VECTOR 3d vector This is the normal vector of the view plane onto which to project the 3D objects.
```

---

## index.html (chunk 5/13)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/20/index.html
Type: reference

```
lt is (* 8.5 72) points, or 8.5 inches, corresponding to US standard letter-size paper. PROJECTION-VECTOR 3d vector This is the normal vector of the view plane onto which to project the 3D objects. Defaults to (getf *standard-views* :top). RAPHAEL-CANVAS-ID string Unique ID on the page for the raphael canvas div. By default this is passed in from the base-ajax-graphics-sheet and based on its root-path, but can be specified manually if you are making a web-drawing on your own. Defaults (in the standalone case) to "RaphaelCanvas" ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists.
```

---

## index.html (chunk 6/13)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/20/index.html
Type: reference

```
Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. SVG-CLASS [ from BASE-DRAWING ] string with css classes These classes will be included in any svg tag outputted from this drawing. VIEW-VECTORS [ from RENDERER-MIXIN ] plist Keys indicate view vector names (e.g. :trimetric ), and values contain the 3D vectors. Defaults to the parameter *standard-views* , but with the key corresponding to current (the view) ordered first in the plist. This list of view-vectors is used to construct the default viewpoints .
```

---

## index.html (chunk 7/13)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/20/index.html
Type: reference

```
rs is used to construct the default viewpoints . VIEWPOINTS [ from RENDERER-MIXIN ] list of plists Each plist contains, based on each entry in the view-vectors , keys: :point (camera location, defaults to the 3d-box-center translated along the corresponding element of view-vectors ) by the local camera distance. The camera distance is computed based on the field-of-view angle and the bounding-sphere :orientation (3d matrix indicating camera orientation) field-of-view Angle in degrees of the view frustrum (i.e. lens angle of the virtual camera). VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. WIDTH [ from BASE-OBJECT ] number X-axis dimension of the reference box. Defaults to zero. Input Slots (optional, defaulting) DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object.
```

---

## index.html (chunk 8/13)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/20/index.html
Type: reference

```
Defaults to zero. Input Slots (optional, defaulting) DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e.
```

---

## index.html (chunk 9/13)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/20/index.html
Type: reference

```
BJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). Computed Slots CENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil Objects MAIN-VIEW gdl object of type geom-base:base-view This is the actual drawing view which is used to present the geometry. Defaults to an internally-computed object, this should not be overridden in user code.
```

---

## index.html (chunk 10/13)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/20/index.html
Type: reference

```
ent the geometry. Defaults to an internally-computed object, this should not be overridden in user code. Examples (in-package :gwl-user) (define-object test-html-graphics-sheet (base-html-graphics-sheet) :objects ((b-splines :type 'test-b-spline-curves) (boxed-spline :type 'surf:boxed-curve :curve-in (the b-splines (curves 0)) :orientation (alignment :top (the (face-normal-vector :rear))) :show-box? t) (view-object :type 'web-drawing :page-length (the graphics-height value) :page-width (the graphics-width value) :projection-vector (getf *standard-views* (the view)) :object-roots (the ui-display-roots)) (graphics-height :type 'text-form-control :default 350) (graphics-width :type 'text-form-control :default 500) (bg-color :type 'text-form-control :default :black) (fg-color :type 'text-form-control :default :white)) :computed-slots ((background-color (lookup-color (the :bg-color value) :format :decimal)) (foreground-color (lookup-color (the :fg-color value) :format :decimal)) (view
```

---

## index.html (chunk 11/13)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/20/index.html
Type: reference

```
control :default :white)) :computed-slots ((background-color (lookup-color (the :bg-color value) :format :decimal)) (foreground-color (lookup-color (the :fg-color value) :format :decimal)) (view :trimetric :settable) ("list of gdl objects. Objects to be displayed in the graphics window." ui-display-roots (list (the b-splines) (the boxed-spline))))) (define-lens (html-format test-html-graphics-sheet)() :output-functions ((main-sheet () (with-html-output (*html-stream* nil :indent t) (:html (:head (:title "Test HTML Graphics Sheet")) (:body (when gwl:*developing?* (the write-development-links)) (:h2 (:center "Test HTML Graphics Sheet")) (with-html-form (:cl-who? t) (:table (:tr (:td (:ul (:li (str (the graphics-height html-string))) (:li (str (the graphics-width html-string))) (:li (str (the bg-color html-string))) (:li (str (the fg-color html-string)))) (:p (:input :type :submit :value " OK "))) (:td (write-the geometry))))))))))) (publish :path "/t-h-g-s" :function #'(lambda(req ent)
```

---

## index.html (chunk 12/13)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/20/index.html
Type: reference

```
bg-color html-string))) (:li (str (the fg-color html-string)))) (:p (:input :type :submit :value " OK "))) (:td (write-the geometry))))))))))) (publish :path "/t-h-g-s" :function #'(lambda(req ent) (gwl-make-object req ent "gwl-user::test-html-graphics-sheet"))) (define-object test-b-spline-curves (base-object) :input-slots ((control-points (list (make-point 0 0 0) (make-point 2 3.0 0.0) (make-point 4 2.0 0.0) (make-point 5 0.0 0.0) (make-point 4 -2.0 0.0) (make-point 2 -3.0 0.0) (make-point 0 0 0)))) :objects ((curves :type 'surf:b-spline-curve :sequence (:size 6) :control-points (the control-points) :degree (1+ (the-child :index)) :display-controls (list :line-thickness (* 0.3 (the-child index)) :color (ecase (the-child index) (0 :red) (1 :orange) (2 :yellow) (3 :green) (4 :blue) (5 :red-violet)))) (points :type 'point :sequence (:size (length (rest (the control-points)))) :center (nth (the-child index) (rest (the control-points))) :display-controls (list :color :green)))) Package
```

---

## index.html (chunk 13/13)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/20/index.html
Type: reference

```
d-violet)))) (points :type 'point :sequence (:size (length (rest (the control-points)))) :center (nth (the-child index) (rest (the control-points))) :display-controls (list :color :green)))) Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/11)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/7/index.html
Type: reference

```
GendL Application - COLOR-MAP Package Documentation Object: COLOR-MAP (The :GWL Package) Mixins: BASE-HTML-SHEET Description Shows a list of the default colors. This is published as the URI "/color-map" of the running GWL webserver. Input Slots (optional) AFTER-PRESENT! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the respondent of a form, to do some processing after the respondent's write-html-sheet function runs to present the object. AFTER-SET! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the requestor of a form, to do some processing after the requestor's form values are set into the specified bashee. BASHEE [ from SKELETON-UI-ELEMENT ] gdl object Object to have its settable computed-slots and/or query-plist set from the fields on the form upon submission. Defaults to self.
```

---

## index.html (chunk 2/11)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/7/index.html
Type: reference

```
nd/or query-plist set from the fields on the form upon submission. Defaults to self. BEFORE-PRESENT! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the respondent of a form, to do some processing before the respondent's write-html-sheet function runs to present the object. This can be useful especially for objects which are subclasses of higher-level mixins such as application-mixin and node-mixin , where you do not have direct access to the write-html-sheet function and typically only define the model-inputs function. It is not always reliable to do processing in the model-inputs function, since some slots which depend on your intended modifications may already have been evaluated by the time the model-inputs function runs.
```

---

## index.html (chunk 3/11)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/7/index.html
Type: reference

```
BEFORE-RESPONSE! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in a user specialization of base-html-sheet, to do some processing before the header-plist is evaluated and before the HTTP response is actually initiated. BEFORE-SET! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the requestor of a form, to do some processing before the requestor's form values are set into the specified bashee. CHECK-SANITY? [ from BASE-HTML-SHEET ] boolean Determines whether a a sanity check is done (with the check-sanity function) before presenting the response page if this page is a respondent. Default is NIL. DOM-ID [ from SKELETON-UI-ELEMENT ] string This is the auto-computed dom-id which should be used for rendering this section.
```

---

## index.html (chunk 4/11)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/7/index.html
Type: reference

```
ng this section. If you use the main-div HTML string for rendering this object as a page section, then you do not have to generate the :div tag yourself - the main-div will be a string of HTML which is wrapped in the correct :div tag already. FORCE-VALIDATION-FOR [ from SKELETON-UI-ELEMENT ] list of gdl objects of type form-control The validation-function will be forced on these objects when a form is submitted, even if the object's html form-control does not happen to be included in the values submitted with the form. Defaults to nil. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. HTML-SECTIONS [ from SKELETON-UI-ELEMENT ] List of HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk.
```

---

## index.html (chunk 5/11)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/7/index.html
Type: reference

```
response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded. HTML-SECTIONS-VALID [ from SKELETON-UI-ELEMENT ] List of valid HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded.
```

---

## index.html (chunk 6/11)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/7/index.html
Type: reference

```
INNER-HTML [ from SKELETON-UI-ELEMENT ] string This can be used with (str .) [in cl-who] or (:princ .) [in htmlGen] to output this section of the page, without the wrapping :div tag [so if you use this, your code would be responsible for wrapping the :div tag with :id (the dom-id).] JS-TO-EVAL [ from SKELETON-UI-ELEMENT ] string of valid javascript This Javascript will be send with the Ajax response, and evaluated after the innerHTML for this section has been replaced. ORDERED-FORM-CONTROLS [ from SKELETON-UI-ELEMENT ] list of gdl objects, which should be of type 'base-form-control . [Note -- this slot is not really necessary for protecting out-of-bounds sequence references anymore, the form-control processor protects against this by itself now]. These objects are validated and bashed first, in the order given. If the cardinality of one form-control depends on another as in the example below, then you should list those dependent objects first. Default is nil. examples: ...
```

---

## index.html (chunk 7/11)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/7/index.html
Type: reference

```
st, in the order given. If the cardinality of one form-control depends on another as in the example below, then you should list those dependent objects first. Default is nil. examples: ... :computed-slots ((number-of-nozzles (the number-of-nozzles-form value)) (ordered-form-controls (append (list-elements (the inner-flange-form)) (list (the number-of-nozzles-form))))) :objects ((inner-flange-form :type 'menu-form-control :choice-plist (list :hey "hey" :now "now") :default :hey :sequence (:size (the number-of-nozzles))) (number-of-nozzles-form :type 'text-form-control :prompt "Number of Shell Nozzles Required: " :domain :number :default 0) POSSIBLE-NILS [ from SKELETON-UI-ELEMENT ] list of keyword symbols Messages corresponding to form fields which could be missing from form submission (e.g. checkbox fields). Defaults to the names of any children or hidden-children of type menu-form-control or checkbox-form-control.
```

---

## index.html (chunk 8/11)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/7/index.html
Type: reference

```
. checkbox fields). Defaults to the names of any children or hidden-children of type menu-form-control or checkbox-form-control. PRESET-ALL? [ from SKELETON-UI-ELEMENT ] boolean This switch determines whether all form-controls should be preset before the final setting, in order to allow any interdependencies to be detected for validation or detecting changed values. If this is specified as a non-nil value, then any nil values of (the preset?) on individual form controls will be ignored. If this is specified as nil, then (the preset?) of individual form-controls (default of these is also nil) will be respected. Default is nil. PROCESS-COOKIES! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in a user specialization of base-html-sheet, to do some processing before the header-plist is evaluated and before the HTTP response is actually initiated, but after the cookies-received have been set.
```

---

## index.html (chunk 9/11)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/7/index.html
Type: reference

```
cessing before the header-plist is evaluated and before the HTTP response is actually initiated, but after the cookies-received have been set. RETURN-OBJECT [ from BASE-HTML-SHEET ] gdl object Default object to which control will return with the write-back-link method ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence.
```

---

## index.html (chunk 10/11)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/7/index.html
Type: reference

```
rally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. TARGET [ from BASE-HTML-SHEET ] string Name of a browser frame or window to display this page. Default of NIL indicates to use the same window. TRANSITORY-SLOTS [ from BASE-HTML-SHEET ] list of keyword symbols Messages corresponding to form fields which should not be retained against Updates to the model (e.g. calls to the update! function or hitting the Update button or link in the browser in development mode). Defaults to NIL (the empty list). VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, defaulting) RESPONDENT [ from SKELETON-UI-ELEMENT ] gdl object Object to respond to the form submission. Defaults to self.
```

---

## index.html (chunk 11/11)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/7/index.html
Type: reference

```
SKELETON-UI-ELEMENT ] gdl object Object to respond to the form submission. Defaults to self. GDL Functions WRITE-HTML-SHEET [ from BASE-HTML-SHEET ] void This GDL function should be redefined to generate the HTML page corresponding to this object. It can be specified here, or as the main-sheet output-function in an html-format lens for this object's type. This write-html-sheet function, if defined, will override any main-sheet function defined in the lens. Typically a write-html-sheet function would look as follows: example: (write-html-sheet () (html (:html (:head (:title (:princ (the :page-title)))) (:body ;;; fill in your body here )))) Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/2/index.html
Type: reference

```
GendL Application - BASE-AJAX-SHEET Package Documentation Object: BASE-AJAX-SHEET (The :GWL Package) Mixins: BASE-HTML-SHEET Description (Note: this documentation will be moved to the specific docs for the html-format/base-ajax-sheet lens, when we have lens documentation working properly) Produces a standard main-sheet for html-format which includes the standard GDL Javascript to enable code produced with gdl-ajax-call to work, and optionally to include the standard JQuery library. If you want to define your own main-sheet, then there is no use for base-ajax-sheet, you can just use base-html-sheet. But then you have to include any needed Javascript yourself, e.g. for gdl-ajax-call support or jQuery. The html-format lens for base-ajax-sheet also defines a user hook function, main-sheet-body, which produces a "No Body has been defined" message by default, but which you can fill in your own specific lens to do something useful for the body.
```

---

## index.html (chunk 2/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/2/index.html
Type: reference

```
, which produces a "No Body has been defined" message by default, but which you can fill in your own specific lens to do something useful for the body. Input Slots (optional) AFTER-PRESENT! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the respondent of a form, to do some processing after the respondent's write-html-sheet function runs to present the object. AFTER-SET! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the requestor of a form, to do some processing after the requestor's form values are set into the specified bashee. BASHEE [ from SKELETON-UI-ELEMENT ] gdl object Object to have its settable computed-slots and/or query-plist set from the fields on the form upon submission. Defaults to self.
```

---

## index.html (chunk 3/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/2/index.html
Type: reference

```
lf. BEFORE-PRESENT! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the respondent of a form, to do some processing before the respondent's write-html-sheet function runs to present the object. This can be useful especially for objects which are subclasses of higher-level mixins such as application-mixin and node-mixin , where you do not have direct access to the write-html-sheet function and typically only define the model-inputs function. It is not always reliable to do processing in the model-inputs function, since some slots which depend on your intended modifications may already have been evaluated by the time the model-inputs function runs. BEFORE-RESPONSE! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in a user specialization of base-html-sheet, to do some processing before the header-plist is evaluated and before the HTTP response is actually initiated.
```

---

## index.html (chunk 4/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/2/index.html
Type: reference

```
ridden in a user specialization of base-html-sheet, to do some processing before the header-plist is evaluated and before the HTTP response is actually initiated. BEFORE-SET! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the requestor of a form, to do some processing before the requestor's form values are set into the specified bashee. BODY-CLASS string or nil Names the value of class attribute for the body tag. Default is nil. BODY-ONLOAD string of javascript or nil This Javascript will go into the :onload event of the body. Default is nil. BODY-ONPAGESHOW string of javascript or nil This Javascript will go into the :onpageshow event of the body. Default is nil. BODY-ONRESIZE string of javascript or nil This Javascript will go into the :onresize event of the body. Default is nil.
```

---

## index.html (chunk 5/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/2/index.html
Type: reference

```
ize event of the body. Default is nil. CHECK-SANITY? [ from BASE-HTML-SHEET ] boolean Determines whether a a sanity check is done (with the check-sanity function) before presenting the response page if this page is a respondent. Default is NIL. DOCTYPE-STRING string or nil Contains the string for the doctype at the top of the document. Default is the standard doctype for HTML5 and later. DOM-ID [ from SKELETON-UI-ELEMENT ] string This is the auto-computed dom-id which should be used for rendering this section. If you use the main-div HTML string for rendering this object as a page section, then you do not have to generate the :div tag yourself - the main-div will be a string of HTML which is wrapped in the correct :div tag already.
```

---

## index.html (chunk 6/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/2/index.html
Type: reference

```
FORCE-VALIDATION-FOR [ from SKELETON-UI-ELEMENT ] list of gdl objects of type form-control The validation-function will be forced on these objects when a form is submitted, even if the object's html form-control does not happen to be included in the values submitted with the form. Defaults to nil. HEAD-CLASS string or nil Names the value of class attribute for the head tag. Default is nil. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. HTML-CLASS string or nil Names the value of class attribute for the html tag. Default is nil. HTML-SECTIONS [ from SKELETON-UI-ELEMENT ] List of HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded.
```

---

## index.html (chunk 7/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/2/index.html
Type: reference

```
own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded. HTML-SECTIONS-VALID [ from SKELETON-UI-ELEMENT ] List of valid HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded.
```

---

## index.html (chunk 8/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/2/index.html
Type: reference

```
INNER-HTML [ from SKELETON-UI-ELEMENT ] string This can be used with (str .) [in cl-who] or (:princ .) [in htmlGen] to output this section of the page, without the wrapping :div tag [so if you use this, your code would be responsible for wrapping the :div tag with :id (the dom-id).] JS-TO-EVAL [ from SKELETON-UI-ELEMENT ] string of valid javascript This Javascript will be send with the Ajax response, and evaluated after the innerHTML for this section has been replaced. MAIN-SHEET-BODY string of html The main body of the page. This can be specified as input or overridden in subclass, otherwise it defaults to the content produced by the :output-function of the same name in the applicable lens for html-format. ORDERED-FORM-CONTROLS [ from SKELETON-UI-ELEMENT ] list of gdl objects, which should be of type 'base-form-control .
```

---

## index.html (chunk 9/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/2/index.html
Type: reference

```
ld be of type 'base-form-control . [Note -- this slot is not really necessary for protecting out-of-bounds sequence references anymore, the form-control processor protects against this by itself now]. These objects are validated and bashed first, in the order given. If the cardinality of one form-control depends on another as in the example below, then you should list those dependent objects first. Default is nil. examples: ... :computed-slots ((number-of-nozzles (the number-of-nozzles-form value)) (ordered-form-controls (append (list-elements (the inner-flange-form)) (list (the number-of-nozzles-form))))) :objects ((inner-flange-form :type 'menu-form-control :choice-plist (list :hey "hey" :now "now") :default :hey :sequence (:size (the number-of-nozzles))) (number-of-nozzles-form :type 'text-form-control :prompt "Number of Shell Nozzles Required: " :domain :number :default 0) POSSIBLE-NILS [ from SKELETON-UI-ELEMENT ] list of keyword symbols Messages corresponding to form fields
```

---

## index.html (chunk 10/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/2/index.html
Type: reference

```
ext-form-control :prompt "Number of Shell Nozzles Required: " :domain :number :default 0) POSSIBLE-NILS [ from SKELETON-UI-ELEMENT ] list of keyword symbols Messages corresponding to form fields which could be missing from form submission (e.g. checkbox fields). Defaults to the names of any children or hidden-children of type menu-form-control or checkbox-form-control. PRESET-ALL? [ from SKELETON-UI-ELEMENT ] boolean This switch determines whether all form-controls should be preset before the final setting, in order to allow any interdependencies to be detected for validation or detecting changed values. If this is specified as a non-nil value, then any nil values of (the preset?) on individual form controls will be ignored. If this is specified as nil, then (the preset?) of individual form-controls (default of these is also nil) will be respected. Default is nil.
```

---

## index.html (chunk 11/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/2/index.html
Type: reference

```
m-controls (default of these is also nil) will be respected. Default is nil. PROCESS-COOKIES! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in a user specialization of base-html-sheet, to do some processing before the header-plist is evaluated and before the HTTP response is actually initiated, but after the cookies-received have been set. RESPONDENT [ from SKELETON-UI-ELEMENT ] gdl object Object to respond to the form submission. Defaults to self. RETURN-OBJECT [ from BASE-HTML-SHEET ] gdl object Default object to which control will return with the write-back-link method ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists.
```

---

## index.html (chunk 12/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/2/index.html
Type: reference

```
ects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. TARGET [ from BASE-HTML-SHEET ] string Name of a browser frame or window to display this page. Default of NIL indicates to use the same window. TITLE string The title of the web page. Defaults to "GendL Application -" .followed by the strings-for-display. TRANSITORY-SLOTS [ from BASE-HTML-SHEET ] list of keyword symbols Messages corresponding to form fields which should not be retained against Updates to the model (e.g.
```

---

## index.html (chunk 13/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/2/index.html
Type: reference

```
ages corresponding to form fields which should not be retained against Updates to the model (e.g. calls to the update! function or hitting the Update button or link in the browser in development mode). Defaults to NIL (the empty list). VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, settable) ADDITIONAL-HEADER-CONTENT string of valid html Additional tag content to go into the page header, if you use the default main-sheet message and just fill in your own main-sheet-body, as is the intended use of the base-ajax-sheet primitive. ADDITIONAL-HEADER-JS-CONTENT valid javascript This javascript is added to the head of the page, just before the body. UI-SPECIFIC-LAYOUT-JS absolute uri in the browser . This is additional JavaScript that needs to be loaded in order to initiate the layout of a user interface. Defaults to nil.
```

---

## index.html (chunk 14/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/2/index.html
Type: reference

```
dy. UI-SPECIFIC-LAYOUT-JS absolute uri in the browser . This is additional JavaScript that needs to be loaded in order to initiate the layout of a user interface. Defaults to nil. USE-JQUERY? boolean Include jquery javascript libraries in the page header? Default nil. Computed Slots DEVELOPMENT-LINKS string of html Provides the developer control links for current sheet. GDL Functions CUSTOM-SNAP-RESTORE! void This is a hook function which applications can use to restore automatically from a saved snapshot file.
```

---

## index.html (chunk 15/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/2/index.html
Type: reference

```
Examples (in-package :gdl-user) (gwl:define-package :ajax-test (:export #:assembly)) (in-package :ajax-test) (define-object assembly (base-ajax-sheet) :objects ((inputs-section :type 'inputs-section) (outputs-section :type 'outputs-section :box (the viewport box) :color (the inputs-section color)) (viewport :type 'viewport :box-color (the inputs-section color)))) (define-lens (html-format assembly)() :output-functions ((main-sheet-body () (with-cl-who () (:table (:tr (:td (str (the inputs-section main-div))) (:td (str (the outputs-section main-div))) (:td (str (the viewport main-div))))))))) (define-object inputs-section (sheet-section) :computed-slots ((color (the menu-control value))) :objects ((menu-control :type 'menu-form-control :choice-list (list :red :green :blue) :default :red :onchange (the (gdl-ajax-call :form-controls (list (the-child))))) (little-grid :type 'grid-form-control :form-control-types '(text-form-control text-form-control button-form-control)
```

---

## index.html (chunk 16/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/2/index.html
Type: reference

```
nchange (the (gdl-ajax-call :form-controls (list (the-child))))) (little-grid :type 'grid-form-control :form-control-types '(text-form-control text-form-control button-form-control) :form-control-attributes '((:ajax-submit-on-change? t) (:ajax-submit-on-change? t)) :form-control-inputs (mapcar #'(lambda(row) (list nil nil (list :onclick (the (gdl-ajax-call :function-key :do-something! :arguments (list (the-object row index))))))) (list-elements (the-child rows))) :default '((:color :number :press-me) (:red 42 "OK") (:blue 50 "OK")))) :computed-slots ((inner-html (with-cl-who-string () (str (the little-grid form-control-string)) (str (the menu-control html-string))))) :functions ((do-something! (index) (format t "Processing row ~a...~%" index)))) (define-object outputs-section (sheet-section) :input-slots (color box) :computed-slots ((inner-html (with-cl-who-string () (:p "The box volume is: " (fmt "~a" (the box volume))) (:p "The box color is: " ((:span :style (format nil "color: ~a"
```

---

## index.html (chunk 17/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/2/index.html
Type: reference

```
n) :input-slots (color box) :computed-slots ((inner-html (with-cl-who-string () (:p "The box volume is: " (fmt "~a" (the box volume))) (:p "The box color is: " ((:span :style (format nil "color: ~a" (the color))) (str (the color)))))))) (define-object viewport (base-ajax-graphics-sheet) :input-slots (box-color) :computed-slots ((length 300) (width 300) (display-list-objects (list (the box))) (projection-vector (getf *standard-views* (the view-selector value))) (inner-html (with-cl-who-string () (str (the view-selector html-string)) (str (the reset-zoom-button form-control-string)) (str (the raster-graphics))))) :objects ((box :type 'box :length 20 :width 25 :height 30 :display-controls (list :color (the box-color))))) (publish-gwl-app "/ajax-test" "ajax-test:assembly") Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 18/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/2/index.html
Type: reference

```
n Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/9)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/17/index.html
Type: reference

```
GendL Application - SKELETON-FORM-CONTROL Package Documentation Object: SKELETON-FORM-CONTROL (The :GWL Package) Mixins: SKELETON-UI-ELEMENT , VANILLA-MIXIN Author Dave Cooper, Genworks Description Computes standard values for base-form-control and similar container objects, e.g. grid-form-control. Does not perform the actual bashing and computation of result value, should be mixed in to something which does this. Input Slots (optional) BASHEE [ from SKELETON-UI-ELEMENT ] gdl object Object to have its settable computed-slots and/or query-plist set from the fields on the form upon submission. Defaults to self. CLASS string You can use this to specify a user-defined class for the form-control. Defaults to nil, which means no class attribute will be generated. DOM-ID [ from SKELETON-UI-ELEMENT ] string This is the auto-computed dom-id which should be used for rendering this section.
```

---

## index.html (chunk 2/9)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/17/index.html
Type: reference

```
T ] string This is the auto-computed dom-id which should be used for rendering this section. If you use the main-div HTML string for rendering this object as a page section, then you do not have to generate the :div tag yourself - the main-div will be a string of HTML which is wrapped in the correct :div tag already. FIELD-NAME keyword symbol The name of this field. Computed from the object name within the tree. FORCE-VALIDATION-FOR [ from SKELETON-UI-ELEMENT ] list of gdl objects of type form-control The validation-function will be forced on these objects when a form is submitted, even if the object's html form-control does not happen to be included in the values submitted with the form. Defaults to nil. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil.
```

---

## index.html (chunk 3/9)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/17/index.html
Type: reference

```
ly be a hidden-object even if specified in :objects. Default is nil. HTML-SECTIONS [ from SKELETON-UI-ELEMENT ] List of HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded. HTML-SECTIONS-VALID [ from SKELETON-UI-ELEMENT ] List of valid HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded. ID keyword symbol The ID attribute for this tag. Defaults to (the field-name).
```

---

## index.html (chunk 4/9)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/17/index.html
Type: reference

```
is tag. Defaults to (the field-name). INNER-HTML [ from SKELETON-UI-ELEMENT ] string This can be used with (str .) [in cl-who] or (:princ .) [in htmlGen] to output this section of the page, without the wrapping :div tag [so if you use this, your code would be responsible for wrapping the :div tag with :id (the dom-id).] JS-TO-EVAL [ from SKELETON-UI-ELEMENT ] string of valid javascript This Javascript will be send with the Ajax response, and evaluated after the innerHTML for this section has been replaced. ORDERED-FORM-CONTROLS [ from SKELETON-UI-ELEMENT ] list of gdl objects, which should be of type 'base-form-control . [Note -- this slot is not really necessary for protecting out-of-bounds sequence references anymore, the form-control processor protects against this by itself now]. These objects are validated and bashed first, in the order given. If the cardinality of one form-control depends on another as in the example below, then you should list those dependent objects first.
```

---

## index.html (chunk 5/9)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/17/index.html
Type: reference

```
objects are validated and bashed first, in the order given. If the cardinality of one form-control depends on another as in the example below, then you should list those dependent objects first. Default is nil. examples: ... :computed-slots ((number-of-nozzles (the number-of-nozzles-form value)) (ordered-form-controls (append (list-elements (the inner-flange-form)) (list (the number-of-nozzles-form))))) :objects ((inner-flange-form :type 'menu-form-control :choice-plist (list :hey "hey" :now "now") :default :hey :sequence (:size (the number-of-nozzles))) (number-of-nozzles-form :type 'text-form-control :prompt "Number of Shell Nozzles Required: " :domain :number :default 0) POSSIBLE-NILS [ from SKELETON-UI-ELEMENT ] list of keyword symbols Messages corresponding to form fields which could be missing from form submission (e.g. checkbox fields). Defaults to the names of any children or hidden-children of type menu-form-control or checkbox-form-control.
```

---

## index.html (chunk 6/9)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/17/index.html
Type: reference

```
be missing from form submission (e.g. checkbox fields). Defaults to the names of any children or hidden-children of type menu-form-control or checkbox-form-control. PRESET-ALL? [ from SKELETON-UI-ELEMENT ] boolean This switch determines whether all form-controls should be preset before the final setting, in order to allow any interdependencies to be detected for validation or detecting changed values. If this is specified as a non-nil value, then any nil values of (the preset?) on individual form controls will be ignored. If this is specified as nil, then (the preset?) of individual form-controls (default of these is also nil) will be respected. Default is nil. PRIMARY? boolean Set this to t if the form-control should always occur first in an outputted snapshot file. Defaults to nil. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy).
```

---

## index.html (chunk 7/9)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/17/index.html
Type: reference

```
[ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, defaulting) RESPONDENT [ from SKELETON-UI-ELEMENT ] gdl object Object to respond to the form submission. Defaults to self.
```

---

## index.html (chunk 8/9)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/17/index.html
Type: reference

```
ldren. Defaults to NIL. Input Slots (optional, defaulting) RESPONDENT [ from SKELETON-UI-ELEMENT ] gdl object Object to respond to the form submission. Defaults to self. Computed Slots FORM-CONTROL string of valid html This is the default HTML which can be included in a form in a web page to display this form control. Previously known as form-control-string. Default is the form-control-string. FORM-CONTROL-STRING string of valid html Also known as simply form-control. This is the default HTML which can be included in a form in a web page to display this form control. Default is the output from form-control method of the lens for html-format and the specific type of this object, returned as a string. FORM-CONTROLS [ from SKELETON-UI-ELEMENT ] list of gdl objects All the children or hidden-children of type base-form-control.
```

---

## index.html (chunk 9/9)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/17/index.html
Type: reference

```
hildren of type base-form-control. HTML-STRING string of valid html This is the default HTML which can be included in a form in a web page to display this form control, wrapped with labels and table cells. Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/16)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/5/index.html
Type: reference

```
GendL Application - BASE-HTML-SHEET Package Documentation Object: BASE-HTML-SHEET (The :GWL Package) Mixins: SHEET-SECTION , VANILLA-MIXIN Description This mixin allows a part to be displayed as a web page in GWL. The main output can be specified either in a write-html-sheet function in the object which mixes this in, or in a main-sheet output-function in an html-format view of the object. Input Slots (optional) AFTER-PRESENT! void This is an empty function by default, but can be overridden in the respondent of a form, to do some processing after the respondent's write-html-sheet function runs to present the object. AFTER-SET! void This is an empty function by default, but can be overridden in the requestor of a form, to do some processing after the requestor's form values are set into the specified bashee. BASHEE [ from SKELETON-UI-ELEMENT ] gdl object Object to have its settable computed-slots and/or query-plist set from the fields on the form upon submission. Defaults to self.
```

---

## index.html (chunk 2/16)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/5/index.html
Type: reference

```
specified bashee. BASHEE [ from SKELETON-UI-ELEMENT ] gdl object Object to have its settable computed-slots and/or query-plist set from the fields on the form upon submission. Defaults to self. BEFORE-PRESENT! void This is an empty function by default, but can be overridden in the respondent of a form, to do some processing before the respondent's write-html-sheet function runs to present the object. This can be useful especially for objects which are subclasses of higher-level mixins such as application-mixin and node-mixin , where you do not have direct access to the write-html-sheet function and typically only define the model-inputs function. It is not always reliable to do processing in the model-inputs function, since some slots which depend on your intended modifications may already have been evaluated by the time the model-inputs function runs.
```

---

## index.html (chunk 3/16)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/5/index.html
Type: reference

```
y have been evaluated by the time the model-inputs function runs. BEFORE-RESPONSE! void This is an empty function by default, but can be overridden in a user specialization of base-html-sheet, to do some processing before the header-plist is evaluated and before the HTTP response is actually initiated. BEFORE-SET! void This is an empty function by default, but can be overridden in the requestor of a form, to do some processing before the requestor's form values are set into the specified bashee. CHECK-SANITY? boolean Determines whether a a sanity check is done (with the check-sanity function) before presenting the response page if this page is a respondent. Default is NIL. DOM-ID [ from SKELETON-UI-ELEMENT ] string This is the auto-computed dom-id which should be used for rendering this section.
```

---

## index.html (chunk 4/16)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/5/index.html
Type: reference

```
ction. If you use the main-div HTML string for rendering this object as a page section, then you do not have to generate the :div tag yourself - the main-div will be a string of HTML which is wrapped in the correct :div tag already. FORCE-VALIDATION-FOR [ from SKELETON-UI-ELEMENT ] list of gdl objects of type form-control The validation-function will be forced on these objects when a form is submitted, even if the object's html form-control does not happen to be included in the values submitted with the form. Defaults to nil. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. HTML-SECTIONS [ from SKELETON-UI-ELEMENT ] List of HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk.
```

---

## index.html (chunk 5/16)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/5/index.html
Type: reference

```
o GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded. HTML-SECTIONS-VALID [ from SKELETON-UI-ELEMENT ] List of valid HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded.
```

---

## index.html (chunk 6/16)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/5/index.html
Type: reference

```
INNER-HTML [ from SKELETON-UI-ELEMENT ] string This can be used with (str .) [in cl-who] or (:princ .) [in htmlGen] to output this section of the page, without the wrapping :div tag [so if you use this, your code would be responsible for wrapping the :div tag with :id (the dom-id).] JS-TO-EVAL [ from SKELETON-UI-ELEMENT ] string of valid javascript This Javascript will be send with the Ajax response, and evaluated after the innerHTML for this section has been replaced. ORDERED-FORM-CONTROLS [ from SKELETON-UI-ELEMENT ] list of gdl objects, which should be of type 'base-form-control . [Note -- this slot is not really necessary for protecting out-of-bounds sequence references anymore, the form-control processor protects against this by itself now]. These objects are validated and bashed first, in the order given. If the cardinality of one form-control depends on another as in the example below, then you should list those dependent objects first. Default is nil. examples: ...
```

---

## index.html (chunk 7/16)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/5/index.html
Type: reference

```
st, in the order given. If the cardinality of one form-control depends on another as in the example below, then you should list those dependent objects first. Default is nil. examples: ... :computed-slots ((number-of-nozzles (the number-of-nozzles-form value)) (ordered-form-controls (append (list-elements (the inner-flange-form)) (list (the number-of-nozzles-form))))) :objects ((inner-flange-form :type 'menu-form-control :choice-plist (list :hey "hey" :now "now") :default :hey :sequence (:size (the number-of-nozzles))) (number-of-nozzles-form :type 'text-form-control :prompt "Number of Shell Nozzles Required: " :domain :number :default 0) POSSIBLE-NILS [ from SKELETON-UI-ELEMENT ] list of keyword symbols Messages corresponding to form fields which could be missing from form submission (e.g. checkbox fields). Defaults to the names of any children or hidden-children of type menu-form-control or checkbox-form-control.
```

---

## index.html (chunk 8/16)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/5/index.html
Type: reference

```
. checkbox fields). Defaults to the names of any children or hidden-children of type menu-form-control or checkbox-form-control. PRESET-ALL? [ from SKELETON-UI-ELEMENT ] boolean This switch determines whether all form-controls should be preset before the final setting, in order to allow any interdependencies to be detected for validation or detecting changed values. If this is specified as a non-nil value, then any nil values of (the preset?) on individual form controls will be ignored. If this is specified as nil, then (the preset?) of individual form-controls (default of these is also nil) will be respected. Default is nil. PROCESS-COOKIES! void This is an empty function by default, but can be overridden in a user specialization of base-html-sheet, to do some processing before the header-plist is evaluated and before the HTTP response is actually initiated, but after the cookies-received have been set.
```

---

## index.html (chunk 9/16)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/5/index.html
Type: reference

```
-plist is evaluated and before the HTTP response is actually initiated, but after the cookies-received have been set. RETURN-OBJECT gdl object Default object to which control will return with the write-back-link method ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. TARGET string Name of a browser frame or window to display this page. Default of NIL indicates to use the same window.
```

---

## index.html (chunk 10/16)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/5/index.html
Type: reference

```
followed by an index number if the part is an element of a sequence. TARGET string Name of a browser frame or window to display this page. Default of NIL indicates to use the same window. TRANSITORY-SLOTS list of keyword symbols Messages corresponding to form fields which should not be retained against Updates to the model (e.g. calls to the update! function or hitting the Update button or link in the browser in development mode). Defaults to NIL (the empty list). VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, defaulting) RESPONDENT [ from SKELETON-UI-ELEMENT ] gdl object Object to respond to the form submission. Defaults to self. Computed Slots (settable) QUERY-PLIST plist Contains submitted form field names and values for which no corresponding settable computed-slots exist.
```

---

## index.html (chunk 11/16)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/5/index.html
Type: reference

```
d Slots (settable) QUERY-PLIST plist Contains submitted form field names and values for which no corresponding settable computed-slots exist. Where corresponding settable computed-slots exist, their values are set from the submitted form fields automatically. Computed Slots HEADER-PLIST plist Extra http headers to be published with the URI for this page. URL string The canonical web address in the current session which points at this page. Also see `full-url`. Published on demand. GDL Functions CHECK-SANITY nil or error object This function checks the "sanity" of this object. By default, it checks that following the object's root-path from the root resolves to this object. If the act of following the root-path throws an error, this error will be returned. Otherwise, if the result of following the root-path does not match the identity of this object, an error is thrown indicating this. Otherwise, NIL is returned and no error is thrown.
```

---

## index.html (chunk 12/16)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/5/index.html
Type: reference

```
ing the root-path does not match the identity of this object, an error is thrown indicating this. Otherwise, NIL is returned and no error is thrown. You can override this function to do what you wish. It should return NIL if the object is found to be "sane" and an throw an error otherwise. If check-sanity? is set to T in this object, this function will be invoked automatically within an ignore-errors by the function handling the GWL "/answer" form action URI when this object is a respondent, before the main-sheet is presented. RESTORE-FORM-CONTROLS! void Calls restore-defaults! on all the form-controls in this sheet. SANITY-ERROR void Emits a page explaining the sanity error. This will be invoked instead of the write-main-sheet if check-sanity? is set to T and the check-sanity throws an error. You may override this function to do what you wish. By default a minimal error message is displayed and a link to the root object is presented.
```

---

## index.html (chunk 13/16)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/5/index.html
Type: reference

```
ror. You may override this function to do what you wish. By default a minimal error message is displayed and a link to the root object is presented. arguments: error an error object, presumably from the check-sanity function SELECT-CHOICES void Writes an HTML Select field with Options. keyword arguments: size Integer , Default Value: 1 determines size of selection list. Default of 1 is a pulldown menu name Keyword symbol or string , Default Value: NIL Determines the name of the field, which should probably match a settable computed-slot keys List of strings, numbers, or symbols , Default Value: NIL Values, the selected one of which will be returned as the value of the field values List of strings , Default Value: KEYS Keys to display in the selection-list tabindex Integer , Default Value: NIL If given, this will generate the tabindex tag for this HTML input field WRITE-CHILD-LINKS void Creates a default unordered list with links to each child part of self.
```

---

## index.html (chunk 14/16)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/5/index.html
Type: reference

```
NIL If given, this will generate the tabindex tag for this HTML input field WRITE-CHILD-LINKS void Creates a default unordered list with links to each child part of self. The text of the links will come from each child's strings-for-display. WRITE-DEVELOPMENT-LINKS void Writes links for access to the standard developer views of the object, currently consisting of an update (Refresh!) link, a Break link, and a ta2 link. WRITE-HTML-SHEET void This GDL function should be redefined to generate the HTML page corresponding to this object. It can be specified here, or as the main-sheet output-function in an html-format lens for this object's type. This write-html-sheet function, if defined, will override any main-sheet function defined in the lens. Typically a write-html-sheet function would look as follows: example: (write-html-sheet () (html (:html (:head (:title (:princ (the :page-title)))) (:body ;;; fill in your body here )))) WRITE-SELF-LINK void Emits a hyperlink pointing to self.
```

---

## index.html (chunk 15/16)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/5/index.html
Type: reference

```
as follows: example: (write-html-sheet () (html (:html (:head (:title (:princ (the :page-title)))) (:body ;;; fill in your body here )))) WRITE-SELF-LINK void Emits a hyperlink pointing to self. Note that if you need extra customization on the display-string (e.g. to include an image tag or other arbitrary markup), use with-output-to-string in conjunction with the html-stream macro. keyword arguments: display-string String , Default Value: (THE :STRINGS-FOR-DISPLAY) String to be displayed display-color Keyword symbol or HTML color string , Default Value: NIL Determines the color of the displayed link text.
```

---

## index.html (chunk 16/16)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/5/index.html
Type: reference

```
Default of NIL indicates web browser default (usually blue) target String , Default Value: (THE :TARGET) Names a frame or window to open the link when clicked class String , Default Value: NIL Names a stylesheet class id String , Default Value: NIL Names a stylesheet id on-mouse-over String , Default Value: NIL Javascript code to run on mouse over on-mouse-out String , Default Value: NIL Javascript code to run on mouse out WRITE-STANDARD-FOOTER void Writes some standard footer information. Defaults to writing Genworks and Franz copyright and product links. Note that VAR agreements often require that you include a ``powered by'' link to the vendor on public web pages. Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/10)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/18/index.html
Type: reference

```
GendL Application - SKELETON-UI-ELEMENT Package Documentation Object: SKELETON-UI-ELEMENT (The :GWL Package) Mixins: VANILLA-MIXIN Description Basic mixin to support constructing a gdl ajax call relative to this node. Note that in order for a node to represent a section of a web page, you should use sheet-section (which mixes this in), rather than this raw primitive. This is a mixin into base-html-sheet, and some of the previous base-html-sheet functionality has been factored out into this mixin. Of special note in this object is the function gdl-ajax-call which generates Javascript appropriate for attaching with a UI event, e.g. onclick, onchange, onblur, etc. In this Javascript you can specify a GDL function (on this object, self) to be run, and/or specify a list of form-control objects which are rendered on the current page, whose values should be submitted and processed ("bashed") into the server.
```

---

## index.html (chunk 2/10)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/18/index.html
Type: reference

```
which are rendered on the current page, whose values should be submitted and processed ("bashed") into the server. Input Slots (optional) BASHEE gdl object Object to have its settable computed-slots and/or query-plist set from the fields on the form upon submission. Defaults to self. DOM-ID string This is the auto-computed dom-id which should be used for rendering this section. If you use the main-div HTML string for rendering this object as a page section, then you do not have to generate the :div tag yourself - the main-div will be a string of HTML which is wrapped in the correct :div tag already. FORCE-VALIDATION-FOR list of gdl objects of type form-control The validation-function will be forced on these objects when a form is submitted, even if the object's html form-control does not happen to be included in the values submitted with the form. Defaults to nil.
```

---

## index.html (chunk 3/10)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/18/index.html
Type: reference

```
appen to be included in the values submitted with the form. Defaults to nil. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. HTML-SECTIONS List of HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded. HTML-SECTIONS-VALID List of valid HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded.
```

---

## index.html (chunk 4/10)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/18/index.html
Type: reference

```
matically every time the sheet section's main-div is demanded. INNER-HTML string This can be used with (str .) [in cl-who] or (:princ .) [in htmlGen] to output this section of the page, without the wrapping :div tag [so if you use this, your code would be responsible for wrapping the :div tag with :id (the dom-id).] JS-TO-EVAL string of valid javascript This Javascript will be send with the Ajax response, and evaluated after the innerHTML for this section has been replaced. ORDERED-FORM-CONTROLS list of gdl objects, which should be of type 'base-form-control . [Note -- this slot is not really necessary for protecting out-of-bounds sequence references anymore, the form-control processor protects against this by itself now]. These objects are validated and bashed first, in the order given. If the cardinality of one form-control depends on another as in the example below, then you should list those dependent objects first. Default is nil. examples: ...
```

---

## index.html (chunk 5/10)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/18/index.html
Type: reference

```
f the cardinality of one form-control depends on another as in the example below, then you should list those dependent objects first. Default is nil. examples: ... :computed-slots ((number-of-nozzles (the number-of-nozzles-form value)) (ordered-form-controls (append (list-elements (the inner-flange-form)) (list (the number-of-nozzles-form))))) :objects ((inner-flange-form :type 'menu-form-control :choice-plist (list :hey "hey" :now "now") :default :hey :sequence (:size (the number-of-nozzles))) (number-of-nozzles-form :type 'text-form-control :prompt "Number of Shell Nozzles Required: " :domain :number :default 0) POSSIBLE-NILS list of keyword symbols Messages corresponding to form fields which could be missing from form submission (e.g. checkbox fields). Defaults to the names of any children or hidden-children of type menu-form-control or checkbox-form-control.
```

---

## index.html (chunk 6/10)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/18/index.html
Type: reference

```
ren or hidden-children of type menu-form-control or checkbox-form-control. PRESET-ALL? boolean This switch determines whether all form-controls should be preset before the final setting, in order to allow any interdependencies to be detected for validation or detecting changed values. If this is specified as a non-nil value, then any nil values of (the preset?) on individual form controls will be ignored. If this is specified as nil, then (the preset?) of individual form-controls (default of these is also nil) will be respected. Default is nil. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists.
```

---

## index.html (chunk 7/10)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/18/index.html
Type: reference

```
flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, defaulting) RESPONDENT gdl object Object to respond to the form submission. Defaults to self. Computed Slots FAILED-FORM-CONTROLS list of gdl objects All the form-controls which do not pass validation. FORM-CONTROLS list of gdl objects All the children or hidden-children of type base-form-control.
```

---

## index.html (chunk 8/10)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/18/index.html
Type: reference

```
on. FORM-CONTROLS list of gdl objects All the children or hidden-children of type base-form-control. MAIN-DIV% string This should be used with (str .) [in cl-who] or (:princ .) [in htmlGen] to output this section of the page, including the wrapping :div tag. GDL Functions GDL-AJAX-CALL string . This function returns a string of Javascript, appropriate to use for events such as :onclick, :onchange, etc, which will invoke an Ajax request to the server, which will respond by replacing the innerHTML of affected :div's, and running the Javascript interpreter to evaluate (the js-to-eval), if any.
```

---

## index.html (chunk 9/10)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/18/index.html
Type: reference

```
examples: " FLAG -- Fill in!!! " keyword arguments: bashee GDL Object , Default Value: (THE BASHEE) This object will have the function-key called on it, if any respondent GDL Object , Default Value: (THE RESPONDENT) This must be the object which represents the actual web page being used function-key Keyword symbol , Default Value: NIL This keyword symbol must name a GDL function or method which is to be invoked with the Ajax call arguments List of values , Default Value: NIL This is the argument list on which the function named by function-key will be applied form-controls List of GDL objects of type base-form-control , Default Value: NIL Each of the objects in this list will have its current value (as entered by the user) scraped from the web page and its value in the model bashed to reflect what has been entered on the page , Default Value: NIL Examples FLAG -- Fill in!!! Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 10/10)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/18/index.html
Type: reference

```
lect what has been entered on the page , Default Value: NIL Examples FLAG -- Fill in!!! Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/19)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/12/index.html
Type: reference

```
GendL Application - MENU-FORM-CONTROL Package Documentation Object: MENU-FORM-CONTROL (The :GWL Package) Mixins: BASE-FORM-CONTROL , VANILLA-MIXIN Author Dave Cooper, Genworks Description This represents a SELECT form control tag wrapping some OPTION tags. OPTIONGROUP is not yet implemented, but will be. Input Slots (optional) ACCEPT [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ACCESSKEY [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. AJAX-SUBMIT-ON-CHANGE? [ from BASE-FORM-CONTROL ] boolean If set to non-nil, this field's value will be sent to server upon change. Default is nil. AJAX-SUBMIT-ON-ENTER? [ from BASE-FORM-CONTROL ] boolean If set to non-nil, this field's value will be sent to server upon enter. Default is nil. ALIGN [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil.
```

---

## index.html (chunk 2/19)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/12/index.html
Type: reference

```
ld's value will be sent to server upon enter. Default is nil. ALIGN [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ALLOW-INVALID-TYPE? [ from BASE-FORM-CONTROL ] boolean If non-nil, then values which fail the type test will still be allowed to be the value. Default is nil. ALLOW-INVALID? [ from BASE-FORM-CONTROL ] boolean If non-nil, then values which fail the type or validation test will still be allowed to be the value. Default is t. ALLOW-NIL? [ from BASE-FORM-CONTROL ] boolean Regardless of :domain, if this is non-nil, nil values will be accepted. Defaults to t if (the default) is nil, otherwise defaults to nil. ALT [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil.
```

---

## index.html (chunk 3/19)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/12/index.html
Type: reference

```
l. APPEND-ERROR-STRING? [ from BASE-FORM-CONTROL ] boolean Determines whether a default error string is appended to string ouput-function for html-format (and therefore html-string computed-slot as well). Defaults to t. BASHEE [ from SKELETON-UI-ELEMENT ] gdl object Object to have its settable computed-slots and/or query-plist set from the fields on the form upon submission. Defaults to self. CHOICE-LIST list Display values, also used as return values, for selection list. Specify this or choice-plist, not both. CHOICE-PLIST plist Keywords and display values for the selection list. Specify this or choice-list, not both. CHOICE-STYLES plist Keywords and CSS style for display of each choice. The keys should correspond to the keys in choice-plist, or the items in choice-list if no choice-plist is given. CLASS [ from SKELETON-FORM-CONTROL ] string You can use this to specify a user-defined class for the form-control. Defaults to nil, which means no class attribute will be generated.
```

---

## index.html (chunk 4/19)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/12/index.html
Type: reference

```
is given. CLASS [ from SKELETON-FORM-CONTROL ] string You can use this to specify a user-defined class for the form-control. Defaults to nil, which means no class attribute will be generated. DEFAULT [ from BASE-FORM-CONTROL ] lisp value of a type compatible with (the domain) This is the initial default value for the control. This must be specified by user code, or an error will result. DISABLED-KEYS list of keyword symbols Each of these should match a key in the choice-plist, and where there is a match, that key will be disabled in the rendering. DISABLED? [ from BASE-FORM-CONTROL ] boolean Maps to HTML form control attribute of the same name. Default is nil. DOM-ID [ from SKELETON-UI-ELEMENT ] string This is the auto-computed dom-id which should be used for rendering this section.
```

---

## index.html (chunk 5/19)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/12/index.html
Type: reference

```
If you use the main-div HTML string for rendering this object as a page section, then you do not have to generate the :div tag yourself - the main-div will be a string of HTML which is wrapped in the correct :div tag already. DOMAIN [ from BASE-FORM-CONTROL ] keyword symbol, one of :number, :keyword, :list-of-strings, :list-of-anything, or :string . This specifies the expected and acceptable type for the submitted form value. If possible, the submitted value will be coerced into the specified type. The default is based upon the Lisp type of (the default) provided as input to this object. If the default is nil, the domain will default to :string FIELD-NAME [ from SKELETON-FORM-CONTROL ] keyword symbol The name of this field. Computed from the object name within the tree.
```

---

## index.html (chunk 6/19)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/12/index.html
Type: reference

```
FORCE-VALIDATION-FOR [ from SKELETON-UI-ELEMENT ] list of gdl objects of type form-control The validation-function will be forced on these objects when a form is submitted, even if the object's html form-control does not happen to be included in the values submitted with the form. Defaults to nil. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. HTML-SECTIONS [ from SKELETON-UI-ELEMENT ] List of HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded. HTML-SECTIONS-VALID [ from SKELETON-UI-ELEMENT ] List of valid HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk.
```

---

## index.html (chunk 7/19)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/12/index.html
Type: reference

```
CTIONS-VALID [ from SKELETON-UI-ELEMENT ] List of valid HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded. ID [ from SKELETON-FORM-CONTROL ] keyword symbol The ID attribute for this tag. Defaults to (the field-name). INNER-HTML [ from SKELETON-UI-ELEMENT ] string This can be used with (str .) [in cl-who] or (:princ .) [in htmlGen] to output this section of the page, without the wrapping :div tag [so if you use this, your code would be responsible for wrapping the :div tag with :id (the dom-id).] ISMAP? [ from BASE-FORM-CONTROL ] boolean Maps to HTML form control attribute of the same name. Default is nil.
```

---

## index.html (chunk 8/19)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/12/index.html
Type: reference

```
ntrol attribute of the same name. Default is nil. JS-TO-EVAL [ from SKELETON-UI-ELEMENT ] string of valid javascript This Javascript will be send with the Ajax response, and evaluated after the innerHTML for this section has been replaced. LABEL-POSITION [ from BASE-FORM-CONTROL ] keyword symbol or nil Specifies where the label tag goes, if any. Can be :table-td (label goes in a td before the form control), :table-td-append (label goes in a td after the form control), prepend: (label tag wraps around form control and label text comes before form control), append: (label tag wraps around form control and label text comes after form control), table-with-class: (like :table-td, but adds a class "form-control" to the table), or as-div: (puts label and control inside a div of class "form-control"). Default is :as-div LANG [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil.
```

---

## index.html (chunk 9/19)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/12/index.html
Type: reference

```
l"). Default is :as-div LANG [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. MAXLENGTH [ from BASE-FORM-CONTROL ] number or nil Maps to HTML form control attribute of the same name. Default is nil. MULTIPLE? boolean Are multiple selections allowed? Default is nil. NULLIFY-EMPTY-STRING? [ from BASE-FORM-CONTROL ] boolean Regardless of :domain, if this is non-nil, empty strings will convert to nil. Defaults to (the allow-nil?) ONBLUR [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONCHANGE [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil, unless ajax-submit-on-change? is non-nil, in which case it calls ajax to set current form value. ONCLICK [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil.
```

---

## index.html (chunk 10/19)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/12/index.html
Type: reference

```
to set current form value. ONCLICK [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONDBLCLICK [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONENTER [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil, unless ajax-submit-on-enter? is non-nil, in which case it calls ajax to set current form value. ONFOCUS [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONKEYDOWN [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONKEYPRESS [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONKEYUP [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil.
```

---

## index.html (chunk 11/19)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/12/index.html
Type: reference

```
attribute of the same name. Default is nil. ONKEYUP [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONMOUSEDOWN [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONMOUSEMOVE [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONMOUSEOUT [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONMOUSEOVER [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONMOUSEUP [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONSELECT [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil.
```

---

## index.html (chunk 12/19)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/12/index.html
Type: reference

```
M-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ORDERED-FORM-CONTROLS [ from SKELETON-UI-ELEMENT ] list of gdl objects, which should be of type 'base-form-control . [Note -- this slot is not really necessary for protecting out-of-bounds sequence references anymore, the form-control processor protects against this by itself now]. These objects are validated and bashed first, in the order given. If the cardinality of one form-control depends on another as in the example below, then you should list those dependent objects first. Default is nil. examples: ...
```

---

## index.html (chunk 13/19)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/12/index.html
Type: reference

```
:computed-slots ((number-of-nozzles (the number-of-nozzles-form value)) (ordered-form-controls (append (list-elements (the inner-flange-form)) (list (the number-of-nozzles-form))))) :objects ((inner-flange-form :type 'menu-form-control :choice-plist (list :hey "hey" :now "now") :default :hey :sequence (:size (the number-of-nozzles))) (number-of-nozzles-form :type 'text-form-control :prompt "Number of Shell Nozzles Required: " :domain :number :default 0) PLACEHOLDER [ from BASE-FORM-CONTROL ] string Text to place in the field by default, overwritten as soon as the field is selected. Works only in HTML5. Default is nil. POSSIBLE-NIL? boolean Indicates whether this should be included in possible-nils. Defaults to (the multiple?) POSSIBLE-NILS [ from SKELETON-UI-ELEMENT ] list of keyword symbols Messages corresponding to form fields which could be missing from form submission (e.g. checkbox fields).
```

---

## index.html (chunk 14/19)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/12/index.html
Type: reference

```
ols Messages corresponding to form fields which could be missing from form submission (e.g. checkbox fields). Defaults to the names of any children or hidden-children of type menu-form-control or checkbox-form-control. PRESET-ALL? [ from SKELETON-UI-ELEMENT ] boolean This switch determines whether all form-controls should be preset before the final setting, in order to allow any interdependencies to be detected for validation or detecting changed values. If this is specified as a non-nil value, then any nil values of (the preset?) on individual form controls will be ignored. If this is specified as nil, then (the preset?) of individual form-controls (default of these is also nil) will be respected. Default is nil. PRESET? [ from BASE-FORM-CONTROL ] boolean This switch determines whether this form-control should be preset before the final setting, in order to allow any interdependencies to be detected for validation or detecting changed values. Default is nil.
```

---

## index.html (chunk 15/19)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/12/index.html
Type: reference

```
is form-control should be preset before the final setting, in order to allow any interdependencies to be detected for validation or detecting changed values. Default is nil. PRIMARY? [ from SKELETON-FORM-CONTROL ] boolean Set this to t if the form-control should always occur first in an outputted snapshot file. Defaults to nil. PROMPT [ from BASE-FORM-CONTROL ] string The prompt used in the label. READONLY? [ from BASE-FORM-CONTROL ] boolean Maps to HTML form control attribute of the same name. Default is nil. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information SIZE number How many choices to display SRC [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name.
```

---

## index.html (chunk 16/19)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/12/index.html
Type: reference

```
ow errors come back as a plist with error information SIZE number How many choices to display SRC [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. STYLE [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. TABINDEX [ from BASE-FORM-CONTROL ] integer or nil Maps to HTML form control attribute of the same name. Default is nil. TEST predicate function of two arguments Defaults based on type of first in choice-plist: eql for keywords, string-equal for strings, and equalp otherwise.
```

---

## index.html (chunk 17/19)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/12/index.html
Type: reference

```
Defaults based on type of first in choice-plist: eql for keywords, string-equal for strings, and equalp otherwise. TITLE [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. USEMAP [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. VALIDATION-FUNCTION [ from BASE-FORM-CONTROL ] function of one argument The argument will be the submitted form value converted to the proper type. The return value from this function can be nil, any non-nil value, or a plist with keys :validated-value and :error. The following behavior applies: If the function returns nil, error is set to :unspecified-validation-fail.
```

---

## index.html (chunk 18/19)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/12/index.html
Type: reference

```
If the function returns a plist with keys :validated-value and :error, and if :error is non-nil, it means the value is not acceptable, the form-controls error message is set to this error (usually a keyword symbol), and the error string will be appended to the html-string by default. If the function returns any other value, then the properly typed submitted form value is considered valid and is used. In the case of an error, the form-control's failed-value message is set to the properly typed submitted form value. If allow-invalid? is non-nil, then the form-control's value message is also set to this value (i.e. the invalid value is still accepted, even though a non-nil error is present). Default is (list :validated-value value :error nil). VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL.
```

---

## index.html (chunk 19/19)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/12/index.html
Type: reference

```
gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, defaulting) RESPONDENT [ from SKELETON-UI-ELEMENT ] gdl object Object to respond to the form submission. Defaults to self. Examples ... :objects ((menu-1 :type 'menu-form-control :choice-plist (list 1 "one" 2 "two"))) ... Please see base-form-control for a broader example which uses more form-control primitives together. Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/20)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/14/index.html
Type: reference

```
GendL Application - RADIO-FORM-CONTROL Package Documentation Object: RADIO-FORM-CONTROL (The :GWL Package) Mixins: MENU-FORM-CONTROL , VANILLA-MIXIN Description Produces a standard radio-button form control. Input Slots (optional) ACCEPT [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ACCESSKEY [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. AJAX-SUBMIT-ON-CHANGE? [ from BASE-FORM-CONTROL ] boolean If set to non-nil, this field's value will be sent to server upon change. Default is nil. AJAX-SUBMIT-ON-ENTER? [ from BASE-FORM-CONTROL ] boolean If set to non-nil, this field's value will be sent to server upon enter. Default is nil. ALIGN [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil.
```

---

## index.html (chunk 2/20)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/14/index.html
Type: reference

```
ing or nil Maps to HTML form control attribute of the same name. Default is nil. ALLOW-INVALID-TYPE? [ from BASE-FORM-CONTROL ] boolean If non-nil, then values which fail the type test will still be allowed to be the value. Default is nil. ALLOW-INVALID? [ from BASE-FORM-CONTROL ] boolean If non-nil, then values which fail the type or validation test will still be allowed to be the value. Default is t. ALLOW-NIL? [ from BASE-FORM-CONTROL ] boolean Regardless of :domain, if this is non-nil, nil values will be accepted. Defaults to t if (the default) is nil, otherwise defaults to nil. ALT [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. APPEND-ERROR-STRING? [ from BASE-FORM-CONTROL ] boolean Determines whether a default error string is appended to string ouput-function for html-format (and therefore html-string computed-slot as well). Defaults to t.
```

---

## index.html (chunk 3/20)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/14/index.html
Type: reference

```
ng is appended to string ouput-function for html-format (and therefore html-string computed-slot as well). Defaults to t. BASHEE [ from SKELETON-UI-ELEMENT ] gdl object Object to have its settable computed-slots and/or query-plist set from the fields on the form upon submission. Defaults to self. CHOICE-LIST [ from MENU-FORM-CONTROL ] list Display values, also used as return values, for selection list. Specify this or choice-plist, not both. CHOICE-PLIST [ from MENU-FORM-CONTROL ] plist Keywords and display values for the selection list. Specify this or choice-list, not both. CHOICE-STYLES [ from MENU-FORM-CONTROL ] plist Keywords and CSS style for display of each choice. The keys should correspond to the keys in choice-plist, or the items in choice-list if no choice-plist is given. CLASS [ from SKELETON-FORM-CONTROL ] string You can use this to specify a user-defined class for the form-control. Defaults to nil, which means no class attribute will be generated.
```

---

## index.html (chunk 4/20)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/14/index.html
Type: reference

```
[ from SKELETON-FORM-CONTROL ] string You can use this to specify a user-defined class for the form-control. Defaults to nil, which means no class attribute will be generated. DEFAULT [ from BASE-FORM-CONTROL ] lisp value of a type compatible with (the domain) This is the initial default value for the control. This must be specified by user code, or an error will result. DESCRIPTION-POSITION keyword symbol or nil Specifies where the description for each radio goes, if any. Can be: :paragraph-prepend (or :p-prepend or :p) Description goes in a paragraph tag before the input tag.
```

---

## index.html (chunk 5/20)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/14/index.html
Type: reference

```
:paragraph-append (or :p-append) Description goes in a paragraph tag after the input tag :table-row-prepend (or :table-tr or :table-tr-prepend) Description goes in a table cell wrapped in a table row before the input tag table cell :table-row-append (or :table-tr-append) Description goes in a table cell wrapped in a table row after the input tag table cell nil (or any other value) No description, only the bare input tag for the radio Default is :paragraph-append. DISABLED-KEYS [ from MENU-FORM-CONTROL ] list of keyword symbols Each of these should match a key in the choice-plist, and where there is a match, that key will be disabled in the rendering. DISABLED? [ from BASE-FORM-CONTROL ] boolean Maps to HTML form control attribute of the same name. Default is nil. DOM-ID [ from SKELETON-UI-ELEMENT ] string This is the auto-computed dom-id which should be used for rendering this section.
```

---

## index.html (chunk 6/20)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/14/index.html
Type: reference

```
-ELEMENT ] string This is the auto-computed dom-id which should be used for rendering this section. If you use the main-div HTML string for rendering this object as a page section, then you do not have to generate the :div tag yourself - the main-div will be a string of HTML which is wrapped in the correct :div tag already. DOMAIN [ from BASE-FORM-CONTROL ] keyword symbol, one of :number, :keyword, :list-of-strings, :list-of-anything, or :string . This specifies the expected and acceptable type for the submitted form value. If possible, the submitted value will be coerced into the specified type. The default is based upon the Lisp type of (the default) provided as input to this object. If the default is nil, the domain will default to :string FIELD-NAME [ from SKELETON-FORM-CONTROL ] keyword symbol The name of this field. Computed from the object name within the tree.
```

---

## index.html (chunk 7/20)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/14/index.html
Type: reference

```
rd symbol The name of this field. Computed from the object name within the tree. FORCE-VALIDATION-FOR [ from SKELETON-UI-ELEMENT ] list of gdl objects of type form-control The validation-function will be forced on these objects when a form is submitted, even if the object's html form-control does not happen to be included in the values submitted with the form. Defaults to nil. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. HTML-SECTIONS [ from SKELETON-UI-ELEMENT ] List of HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded.
```

---

## index.html (chunk 8/20)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/14/index.html
Type: reference

```
et programmatically every time the sheet section's main-div is demanded. HTML-SECTIONS-VALID [ from SKELETON-UI-ELEMENT ] List of valid HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded. ID [ from SKELETON-FORM-CONTROL ] keyword symbol The ID attribute for this tag. Defaults to (the field-name). INNER-HTML [ from SKELETON-UI-ELEMENT ] string This can be used with (str .) [in cl-who] or (:princ .) [in htmlGen] to output this section of the page, without the wrapping :div tag [so if you use this, your code would be responsible for wrapping the :div tag with :id (the dom-id).] ISMAP? [ from BASE-FORM-CONTROL ] boolean Maps to HTML form control attribute of the same name. Default is nil.
```

---

## index.html (chunk 9/20)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/14/index.html
Type: reference

```
id (the dom-id).] ISMAP? [ from BASE-FORM-CONTROL ] boolean Maps to HTML form control attribute of the same name. Default is nil. JS-TO-EVAL [ from SKELETON-UI-ELEMENT ] string of valid javascript This Javascript will be send with the Ajax response, and evaluated after the innerHTML for this section has been replaced. LABEL-POSITION [ from BASE-FORM-CONTROL ] keyword symbol or nil Specifies where the label tag goes, if any. Can be :table-td (label goes in a td before the form control), :table-td-append (label goes in a td after the form control), prepend: (label tag wraps around form control and label text comes before form control), append: (label tag wraps around form control and label text comes after form control), table-with-class: (like :table-td, but adds a class "form-control" to the table), or as-div: (puts label and control inside a div of class "form-control").
```

---

## index.html (chunk 10/20)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/14/index.html
Type: reference

```
he table), or as-div: (puts label and control inside a div of class "form-control"). Default is :as-div LANG [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. MAXLENGTH [ from BASE-FORM-CONTROL ] number or nil Maps to HTML form control attribute of the same name. Default is nil. NULLIFY-EMPTY-STRING? [ from BASE-FORM-CONTROL ] boolean Regardless of :domain, if this is non-nil, empty strings will convert to nil. Defaults to (the allow-nil?) ONBLUR [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONCHANGE [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil, unless ajax-submit-on-change? is non-nil, in which case it calls ajax to set current form value. ONCLICK [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil.
```

---

## index.html (chunk 11/20)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/14/index.html
Type: reference

```
t calls ajax to set current form value. ONCLICK [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONDBLCLICK [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONENTER [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil, unless ajax-submit-on-enter? is non-nil, in which case it calls ajax to set current form value. ONFOCUS [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONKEYDOWN [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONKEYPRESS [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONKEYUP [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil.
```

---

## index.html (chunk 12/20)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/14/index.html
Type: reference

```
form control attribute of the same name. Default is nil. ONKEYUP [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONMOUSEDOWN [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONMOUSEMOVE [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONMOUSEOUT [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONMOUSEOVER [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONMOUSEUP [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONSELECT [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil.
```

---

## index.html (chunk 13/20)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/14/index.html
Type: reference

```
from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ORDERED-FORM-CONTROLS [ from SKELETON-UI-ELEMENT ] list of gdl objects, which should be of type 'base-form-control . [Note -- this slot is not really necessary for protecting out-of-bounds sequence references anymore, the form-control processor protects against this by itself now]. These objects are validated and bashed first, in the order given. If the cardinality of one form-control depends on another as in the example below, then you should list those dependent objects first. Default is nil. examples: ...
```

---

## index.html (chunk 14/20)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/14/index.html
Type: reference

```
:computed-slots ((number-of-nozzles (the number-of-nozzles-form value)) (ordered-form-controls (append (list-elements (the inner-flange-form)) (list (the number-of-nozzles-form))))) :objects ((inner-flange-form :type 'menu-form-control :choice-plist (list :hey "hey" :now "now") :default :hey :sequence (:size (the number-of-nozzles))) (number-of-nozzles-form :type 'text-form-control :prompt "Number of Shell Nozzles Required: " :domain :number :default 0) PLACEHOLDER [ from BASE-FORM-CONTROL ] string Text to place in the field by default, overwritten as soon as the field is selected. Works only in HTML5. Default is nil. POSSIBLE-NIL? [ from MENU-FORM-CONTROL ] boolean Indicates whether this should be included in possible-nils. Defaults to (the multiple?) POSSIBLE-NILS [ from SKELETON-UI-ELEMENT ] list of keyword symbols Messages corresponding to form fields which could be missing from form submission (e.g. checkbox fields).
```

---

## index.html (chunk 15/20)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/14/index.html
Type: reference

```
MENT ] list of keyword symbols Messages corresponding to form fields which could be missing from form submission (e.g. checkbox fields). Defaults to the names of any children or hidden-children of type menu-form-control or checkbox-form-control. PRESET-ALL? [ from SKELETON-UI-ELEMENT ] boolean This switch determines whether all form-controls should be preset before the final setting, in order to allow any interdependencies to be detected for validation or detecting changed values. If this is specified as a non-nil value, then any nil values of (the preset?) on individual form controls will be ignored. If this is specified as nil, then (the preset?) of individual form-controls (default of these is also nil) will be respected. Default is nil. PRESET? [ from BASE-FORM-CONTROL ] boolean This switch determines whether this form-control should be preset before the final setting, in order to allow any interdependencies to be detected for validation or detecting changed values.
```

---

## index.html (chunk 16/20)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/14/index.html
Type: reference

```
witch determines whether this form-control should be preset before the final setting, in order to allow any interdependencies to be detected for validation or detecting changed values. Default is nil. PRIMARY? [ from SKELETON-FORM-CONTROL ] boolean Set this to t if the form-control should always occur first in an outputted snapshot file. Defaults to nil. PROMPT [ from BASE-FORM-CONTROL ] string The prompt used in the label. READONLY? [ from BASE-FORM-CONTROL ] boolean Maps to HTML form control attribute of the same name. Default is nil. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists.
```

---

## index.html (chunk 17/20)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/14/index.html
Type: reference

```
sts. Any children which throw errors come back as a plist with error information SIZE [ from MENU-FORM-CONTROL ] number How many choices to display SRC [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. STYLE [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. TABINDEX [ from BASE-FORM-CONTROL ] integer or nil Maps to HTML form control attribute of the same name. Default is nil. TABLE-CLASS string Allows you to specify a class for the table surrounding the radio input elements. Defaults to empty string.
```

---

## index.html (chunk 18/20)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/14/index.html
Type: reference

```
fault is nil. TABLE-CLASS string Allows you to specify a class for the table surrounding the radio input elements. Defaults to empty string. TEST [ from MENU-FORM-CONTROL ] predicate function of two arguments Defaults based on type of first in choice-plist: eql for keywords, string-equal for strings, and equalp otherwise. TITLE [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. USEMAP [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. VALIDATION-FUNCTION [ from BASE-FORM-CONTROL ] function of one argument The argument will be the submitted form value converted to the proper type. The return value from this function can be nil, any non-nil value, or a plist with keys :validated-value and :error. The following behavior applies: If the function returns nil, error is set to :unspecified-validation-fail.
```

---

## index.html (chunk 19/20)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/14/index.html
Type: reference

```
dated-value and :error. The following behavior applies: If the function returns nil, error is set to :unspecified-validation-fail. If the function returns a plist with keys :validated-value and :error, and if :error is non-nil, it means the value is not acceptable, the form-controls error message is set to this error (usually a keyword symbol), and the error string will be appended to the html-string by default. If the function returns any other value, then the properly typed submitted form value is considered valid and is used. In the case of an error, the form-control's failed-value message is set to the properly typed submitted form value. If allow-invalid? is non-nil, then the form-control's value message is also set to this value (i.e. the invalid value is still accepted, even though a non-nil error is present). Default is (list :validated-value value :error nil). VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree.
```

---

## index.html (chunk 20/20)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/14/index.html
Type: reference

```
a non-nil error is present). Default is (list :validated-value value :error nil). VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, defaulting) RESPONDENT [ from SKELETON-UI-ELEMENT ] gdl object Object to respond to the form submission. Defaults to self. Computed Slots MULTIPLE? [ from MENU-FORM-CONTROL ] boolean Are multiple selections allowed? Default is nil. Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/11/index.html
Type: reference

```
GendL Application - LAYOUT-MIXIN Package Documentation Object: LAYOUT-MIXIN (The :GWL Package) Mixins: BASE-HTML-GRAPHICS-SHEET Description This is mixed into both node-mixin and application-mixin . It contains the common messages for nodes in a GWL application tree. For any node-mixin or application-mixin , you may override the default (empty) model-inputs output-function of the corresponding html-format view to make specific model-inputs for that node. Input Slots (optional) AFTER-PRESENT! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the respondent of a form, to do some processing after the respondent's write-html-sheet function runs to present the object. AFTER-SET! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the requestor of a form, to do some processing after the requestor's form values are set into the specified bashee.
```

---

## index.html (chunk 2/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/11/index.html
Type: reference

```
n be overridden in the requestor of a form, to do some processing after the requestor's form values are set into the specified bashee. AVAILABLE-IMAGE-FORMATS list of keyword symbols Determines which formats are available in the Preferences. Defaults to :png, :jpeg, and :vrml. BASHEE [ from SKELETON-UI-ELEMENT ] gdl object Object to have its settable computed-slots and/or query-plist set from the fields on the form upon submission. Defaults to self. BEFORE-PRESENT! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the respondent of a form, to do some processing before the respondent's write-html-sheet function runs to present the object. This can be useful especially for objects which are subclasses of higher-level mixins such as application-mixin and node-mixin , where you do not have direct access to the write-html-sheet function and typically only define the model-inputs function.
```

---

## index.html (chunk 3/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/11/index.html
Type: reference

```
and node-mixin , where you do not have direct access to the write-html-sheet function and typically only define the model-inputs function. It is not always reliable to do processing in the model-inputs function, since some slots which depend on your intended modifications may already have been evaluated by the time the model-inputs function runs. BEFORE-RESPONSE! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in a user specialization of base-html-sheet, to do some processing before the header-plist is evaluated and before the HTTP response is actually initiated. BEFORE-SET! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the requestor of a form, to do some processing before the requestor's form values are set into the specified bashee. BODY-BGCOLOR keyword symbol Color keyword from *color-table* for the body background. Defaults to :blue-sky .
```

---

## index.html (chunk 4/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/11/index.html
Type: reference

```
are set into the specified bashee. BODY-BGCOLOR keyword symbol Color keyword from *color-table* for the body background. Defaults to :blue-sky . BOUNDING-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object. CENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located. CHECK-SANITY? [ from BASE-HTML-SHEET ] boolean Determines whether a a sanity check is done (with the check-sanity function) before presenting the response page if this page is a respondent. Default is NIL. DOM-ID [ from SKELETON-UI-ELEMENT ] string This is the auto-computed dom-id which should be used for rendering this section.
```

---

## index.html (chunk 5/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/11/index.html
Type: reference

```
used for rendering this section. If you use the main-div HTML string for rendering this object as a page section, then you do not have to generate the :div tag yourself - the main-div will be a string of HTML which is wrapped in the correct :div tag already. FORCE-VALIDATION-FOR [ from SKELETON-UI-ELEMENT ] list of gdl objects of type form-control The validation-function will be forced on these objects when a form is submitted, even if the object's html form-control does not happen to be included in the values submitted with the form. Defaults to nil. HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. HTML-SECTIONS [ from SKELETON-UI-ELEMENT ] List of HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk.
```

---

## index.html (chunk 6/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/11/index.html
Type: reference

```
nil. HTML-SECTIONS [ from SKELETON-UI-ELEMENT ] List of HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded. HTML-SECTIONS-VALID [ from SKELETON-UI-ELEMENT ] List of valid HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil IMAGE-FORMAT [ from BASE-HTML-GRAPHICS-SHEET ] keyword symbol Determines the default image format.
```

---

## index.html (chunk 7/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/11/index.html
Type: reference

```
stead of actual geometry for this object. Defaults to nil IMAGE-FORMAT [ from BASE-HTML-GRAPHICS-SHEET ] keyword symbol Determines the default image format. Defaults to :png INNER-HTML [ from SKELETON-UI-ELEMENT ] string This can be used with (str .) [in cl-who] or (:princ .) [in htmlGen] to output this section of the page, without the wrapping :div tag [so if you use this, your code would be responsible for wrapping the :div tag with :id (the dom-id).] INPUTS-BGCOLOR keyword symbol Color keyword from *color-table* for the model-inputs area background. Defaults to :aquamarine . INPUTS-TITLE string Title for the model-inputs section. Defaults to "Model Inputs". JS-TO-EVAL [ from SKELETON-UI-ELEMENT ] string of valid javascript This Javascript will be send with the Ajax response, and evaluated after the innerHTML for this section has been replaced. LENGTH [ from GEOMETRY-VIEW-MIXIN ] number Length ("height" of screen window) of the graphics viewport. Default is 300.
```

---

## index.html (chunk 8/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/11/index.html
Type: reference

```
ed after the innerHTML for this section has been replaced. LENGTH [ from GEOMETRY-VIEW-MIXIN ] number Length ("height" of screen window) of the graphics viewport. Default is 300. LOCAL-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object. MULTIPART-FORM? boolean Determines whether the embedded form will support multipart MIME parts. Defaults to NIL. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. ORDERED-FORM-CONTROLS [ from SKELETON-UI-ELEMENT ] list of gdl objects, which should be of type 'base-form-control .
```

---

## index.html (chunk 9/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/11/index.html
Type: reference

```
lity, e.g. SVG/Raphael and X3DOM. ORDERED-FORM-CONTROLS [ from SKELETON-UI-ELEMENT ] list of gdl objects, which should be of type 'base-form-control . [Note -- this slot is not really necessary for protecting out-of-bounds sequence references anymore, the form-control processor protects against this by itself now]. These objects are validated and bashed first, in the order given. If the cardinality of one form-control depends on another as in the example below, then you should list those dependent objects first. Default is nil. examples: ...
```

---

## index.html (chunk 10/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/11/index.html
Type: reference

```
:computed-slots ((number-of-nozzles (the number-of-nozzles-form value)) (ordered-form-controls (append (list-elements (the inner-flange-form)) (list (the number-of-nozzles-form))))) :objects ((inner-flange-form :type 'menu-form-control :choice-plist (list :hey "hey" :now "now") :default :hey :sequence (:size (the number-of-nozzles))) (number-of-nozzles-form :type 'text-form-control :prompt "Number of Shell Nozzles Required: " :domain :number :default 0) ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). OTHER-RULES list of gdl objects of type base-rule-object or (preferably) gwl-base-rule-object .
```

---

## index.html (chunk 11/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/11/index.html
Type: reference

```
ng each row is a vector with a magnitude of one (1.0). OTHER-RULES list of gdl objects of type base-rule-object or (preferably) gwl-base-rule-object . Links to these will be displayed in the other-rules section. Default to the collection of all objects of type base-rule-object from this node in the tree down to the leaves, whose violated? message evaluates to NIL. OTHER-RULES-BGCOLOR keyword symbol Color keyword from *color-table* for the other-rules area background. Defaults to :aquamarine . OTHER-RULES-TITLE string Title for the other-rules section. Defaults to "Other Rules". PAGE-TITLE string The title to display on the page and in the tree. Defaults to (the strings-for-display) . POSSIBLE-NILS [ from SKELETON-UI-ELEMENT ] list of keyword symbols Messages corresponding to form fields which could be missing from form submission (e.g. checkbox fields). Defaults to the names of any children or hidden-children of type menu-form-control or checkbox-form-control.
```

---

## index.html (chunk 12/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/11/index.html
Type: reference

```
ich could be missing from form submission (e.g. checkbox fields). Defaults to the names of any children or hidden-children of type menu-form-control or checkbox-form-control. PRESET-ALL? [ from SKELETON-UI-ELEMENT ] boolean This switch determines whether all form-controls should be preset before the final setting, in order to allow any interdependencies to be detected for validation or detecting changed values. If this is specified as a non-nil value, then any nil values of (the preset?) on individual form controls will be ignored. If this is specified as nil, then (the preset?) of individual form-controls (default of these is also nil) will be respected. Default is nil. PROCESS-COOKIES! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in a user specialization of base-html-sheet, to do some processing before the header-plist is evaluated and before the HTTP response is actually initiated, but after the cookies-received have been set.
```

---

## index.html (chunk 13/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/11/index.html
Type: reference

```
cialization of base-html-sheet, to do some processing before the header-plist is evaluated and before the HTTP response is actually initiated, but after the cookies-received have been set. RETURN-OBJECT [ from BASE-HTML-SHEET ] gdl object Default object to which control will return with the write-back-link method ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information SHOW-TITLE? boolean Indicates whether to display the title at the top of the page. Defaults to T. STANDARD-VIEWS [ from BASE-HTML-GRAPHICS-SHEET ] plist of keywords and 3d vectors . Indicates the views to show in the graphics controls.
```

---

## index.html (chunk 14/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/11/index.html
Type: reference

```
plist of keywords and 3d vectors . Indicates the views to show in the graphics controls. STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. TARGET [ from BASE-HTML-SHEET ] string Name of a browser frame or window to display this page. Default of NIL indicates to use the same window. TRANSITORY-SLOTS [ from BASE-HTML-SHEET ] list of keyword symbols Messages corresponding to form fields which should not be retained against Updates to the model (e.g. calls to the update! function or hitting the Update button or link in the browser in development mode). Defaults to NIL (the empty list). TREE-BGCOLOR keyword symbol Color keyword from *color-table* for the tree area background. Defaults to :aquamarine .
```

---

## index.html (chunk 15/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/11/index.html
Type: reference

```
evelopment mode). Defaults to NIL (the empty list). TREE-BGCOLOR keyword symbol Color keyword from *color-table* for the tree area background. Defaults to :aquamarine . TREE-TITLE string Title for the Tree section. Defaults to "Assembly Tree" if the tree-root is only a subclass of application-mixin , and "Assembly Tree" if the tree-root is an actual node with child applications. UI-DISPLAY-LIST-LEAVES list of gdl objects This should be overridden with a list of objects of your choice. These objects (not their leaves, but these actual nodes) will be scaled to fit and displayed in the graphics area. Defaults to NIL. UI-DISPLAY-LIST-OBJECTS list of gdl objects This should be overridden with a list of objects of your choice. The leaves of these objects will be scaled to fit and displayed in the graphics area. Defaults to NIL.
```

---

## index.html (chunk 16/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/11/index.html
Type: reference

```
e graphics area. Defaults to NIL. USE-BSPLINES? [ from BASE-HTML-GRAPHICS-SHEET ] boolean Determines whether to use native bspline data in the vrml VIOLATED-RULES list of gdl objects of type base-rule-object or (preferably) gwl-base-rule-object . Links to these will be displayed in the other-rules section. Default to the collection of all objects of type base-rule-object from this node in the tree down to the leaves, whose violated? message evaluates to non-NIL. VIOLATED-RULES-BGCOLOR keyword symbol Color keyword from *color-table* for the violated-rules area background. Defaults to :aquamarine . VIOLATED-RULES-TITLE string Title for the violated-rules section. Defaults to "Violated Rules". VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. WIDTH [ from GEOMETRY-VIEW-MIXIN ] number Width of the graphics viewport. Default is 300.
```

---

## index.html (chunk 17/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/11/index.html
Type: reference

```
tree. Typically this would be a subset of hidden-children. Defaults to NIL. WIDTH [ from GEOMETRY-VIEW-MIXIN ] number Width of the graphics viewport. Default is 300. Input Slots (optional, settable) DIGITATION-MODE [ from BASE-HTML-GRAPHICS-SHEET ] keyword symbol, one of :zoom-and-center , :report-point , or :measure-distance . If :zoom-and-center , sets the user-center and user-scale accordingly when graphics area is clicked. If :report-point , the slot digitized-point is set with the x y value. If measure-distance , the slot :digitized-distance is set with the resultant distance. Default is :zoom-and-center VIEW [ from BASE-HTML-GRAPHICS-SHEET ] keyword symbol Determines the default view from the standard-views . Defaults to :trimetric. ZOOM-FACTOR [ from BASE-HTML-GRAPHICS-SHEET ] number The factor used for zooming in or out.
```

---

## index.html (chunk 18/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/11/index.html
Type: reference

```
er The factor used for zooming in or out. ZOOM-MODE [ from BASE-HTML-GRAPHICS-SHEET ] keyword symbol, one of :in, :out, or :none, or nil If :in, then clicks in the graphics area will increase the zoom factor by (the zoom-factor). If :out, then clicks will decrease the factor by that amount. If :none or nil, then clicks will have no effect. Input Slots (optional, defaulting) DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only).
```

---

## index.html (chunk 19/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/11/index.html
Type: reference

```
representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. DISPLAY-RULES? boolean Indicates whether the Rules panel should be displayed. Defaults to T. DISPLAY-TREE? boolean Indicates whether the Tree area should be displayed. Defaults to T. GRAPHICS-HEIGHT integer Height (top to bottom on screen) in pixels of the graphics area. Defaults to 500. GRAPHICS-WIDTH integer Height (left to right on screen) in pixels of the graphics area. Defaults to 500. RESPONDENT [ from SKELETON-UI-ELEMENT ] gdl object Object to respond to the form submission. Defaults to self. USE-STANDARD-SAVED-SLOTS? boolean Determines whether the standard-saved-slots are automatically used by default for the saved-slots.
```

---

## index.html (chunk 20/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/11/index.html
Type: reference

```
f. USE-STANDARD-SAVED-SLOTS? boolean Determines whether the standard-saved-slots are automatically used by default for the saved-slots. This is a trickle-down slot so its value will be passed to descendent objects automatically. The default value is NIL. Computed Slots SAVED-SLOTS list of keyword symbols or lists . The first of this list should be the unique name for this tree node for the purposes of saving slots. The rest of this list is made up of either keyword symbols or lists. A keyword symbol indicates the name of a slot to be saved in the current object. These slot names should correspond to :settable slots of this object. A list indicates slots to be saved in a child object, specified as follows: the first of the list is the name of the child part, and the rest is made up of keywords naming the slots in the child part to be saved. These should correspond to :settable slots in the child object.
```

---

## index.html (chunk 21/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/11/index.html
Type: reference

```
rds naming the slots in the child part to be saved. These should correspond to :settable slots in the child object. The default value is the standard-saved-slots if the use-standard-saved-slots? is non-NIL, NIL otherwise. STANDARD-SAVED-SLOTS list of keyword symbols The first of this list is the name-for-display of this object. The rest of the list are all the keyword symbols representing the settable computed-slots and input-slots which have a default value. Required input-slots (i.e. input-slots without a default value) are not included in this list. If you wish to include required inputs with the saved-slots, you should explicitly append them to this list when specifying the saved-slots . Hidden Objects VIEW-OBJECT [ from GEOMETRY-VIEW-MIXIN ] gdl web-drawing object This must be overridden in the specialized class.
```

---

## index.html (chunk 22/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/11/index.html
Type: reference

```
den in the specialized class. GDL Functions READ-SAVED-SLOTS void Reads the slots data from filename , restores the corresponding slots in this object and matching descendant objects, and calls the restore! function on each object. keyword arguments: filename String or pathname , Default Value: "/tmp/<object type>" Name of file to be read WRITE-HTML-SHEET [ from BASE-HTML-SHEET ] void This GDL function should be redefined to generate the HTML page corresponding to this object. It can be specified here, or as the main-sheet output-function in an html-format lens for this object's type. This write-html-sheet function, if defined, will override any main-sheet function defined in the lens. Typically a write-html-sheet function would look as follows: example: (write-html-sheet () (html (:html (:head (:title (:princ (the :page-title)))) (:body ;;
```

---

## index.html (chunk 23/23)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/11/index.html
Type: reference

```
:head (:title (:princ (the :page-title)))) (:body ;;; fill in your body here )))) WRITE-SAVED-SLOTS void Writes the unique application name names and values of all saved-slots in this and all descendants which are of type node-mixin or application-mixin. keyword arguments: filename-or-stream String, pathname, or stream , Default Value: "/tmp/<object type>" Name or stream for file to be written Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/19)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/19/index.html
Type: reference

```
GendL Application - TEXT-FORM-CONTROL Package Documentation Object: TEXT-FORM-CONTROL (The :GWL Package) Mixins: BASE-FORM-CONTROL , VANILLA-MIXIN Author Dave Cooper, Genworks Description This represents a INPUT TYPE=TEXT or TEXTAREA form control tag. Input Slots (optional) ACCEPT [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ACCESSKEY [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. AJAX-SUBMIT-ON-CHANGE? [ from BASE-FORM-CONTROL ] boolean If set to non-nil, this field's value will be sent to server upon change. Default is nil. AJAX-SUBMIT-ON-ENTER? [ from BASE-FORM-CONTROL ] boolean If set to non-nil, this field's value will be sent to server upon enter. Default is nil. ALIGN [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil.
```

---

## index.html (chunk 2/19)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/19/index.html
Type: reference

```
is nil. ALIGN [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ALLOW-INVALID-TYPE? [ from BASE-FORM-CONTROL ] boolean If non-nil, then values which fail the type test will still be allowed to be the value. Default is nil. ALLOW-INVALID? [ from BASE-FORM-CONTROL ] boolean If non-nil, then values which fail the type or validation test will still be allowed to be the value. Default is t. ALLOW-NIL? [ from BASE-FORM-CONTROL ] boolean Regardless of :domain, if this is non-nil, nil values will be accepted. Defaults to t if (the default) is nil, otherwise defaults to nil. ALT [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. APPEND-ERROR-STRING? [ from BASE-FORM-CONTROL ] boolean Determines whether a default error string is appended to string ouput-function for html-format (and therefore html-string computed-slot as well). Defaults to t.
```

---

## index.html (chunk 3/19)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/19/index.html
Type: reference

```
lean Determines whether a default error string is appended to string ouput-function for html-format (and therefore html-string computed-slot as well). Defaults to t. BASHEE [ from SKELETON-UI-ELEMENT ] gdl object Object to have its settable computed-slots and/or query-plist set from the fields on the form upon submission. Defaults to self. CLASS [ from SKELETON-FORM-CONTROL ] string You can use this to specify a user-defined class for the form-control. Defaults to nil, which means no class attribute will be generated. COLS integer The number of columns for a TEXTAREA (if rows is > 1). Defaults to (the size). DEFAULT [ from BASE-FORM-CONTROL ] lisp value of a type compatible with (the domain) This is the initial default value for the control. This must be specified by user code, or an error will result. DISABLED? [ from BASE-FORM-CONTROL ] boolean Maps to HTML form control attribute of the same name. Default is nil.
```

---

## index.html (chunk 4/19)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/19/index.html
Type: reference

```
will result. DISABLED? [ from BASE-FORM-CONTROL ] boolean Maps to HTML form control attribute of the same name. Default is nil. DOM-ID [ from SKELETON-UI-ELEMENT ] string This is the auto-computed dom-id which should be used for rendering this section. If you use the main-div HTML string for rendering this object as a page section, then you do not have to generate the :div tag yourself - the main-div will be a string of HTML which is wrapped in the correct :div tag already. DOMAIN [ from BASE-FORM-CONTROL ] keyword symbol, one of :number, :keyword, :list-of-strings, :list-of-anything, or :string . This specifies the expected and acceptable type for the submitted form value. If possible, the submitted value will be coerced into the specified type. The default is based upon the Lisp type of (the default) provided as input to this object. If the default is nil, the domain will default to :string FIELD-NAME [ from SKELETON-FORM-CONTROL ] keyword symbol The name of this field.
```

---

## index.html (chunk 5/19)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/19/index.html
Type: reference

```
(the default) provided as input to this object. If the default is nil, the domain will default to :string FIELD-NAME [ from SKELETON-FORM-CONTROL ] keyword symbol The name of this field. Computed from the object name within the tree. FORCE-VALIDATION-FOR [ from SKELETON-UI-ELEMENT ] list of gdl objects of type form-control The validation-function will be forced on these objects when a form is submitted, even if the object's html form-control does not happen to be included in the values submitted with the form. Defaults to nil. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. HTML-SECTIONS [ from SKELETON-UI-ELEMENT ] List of HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk.
```

---

## index.html (chunk 6/19)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/19/index.html
Type: reference

```
to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded. HTML-SECTIONS-VALID [ from SKELETON-UI-ELEMENT ] List of valid HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded. ID [ from SKELETON-FORM-CONTROL ] keyword symbol The ID attribute for this tag. Defaults to (the field-name).
```

---

## index.html (chunk 7/19)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/19/index.html
Type: reference

```
INNER-HTML [ from SKELETON-UI-ELEMENT ] string This can be used with (str .) [in cl-who] or (:princ .) [in htmlGen] to output this section of the page, without the wrapping :div tag [so if you use this, your code would be responsible for wrapping the :div tag with :id (the dom-id).] ISMAP? [ from BASE-FORM-CONTROL ] boolean Maps to HTML form control attribute of the same name. Default is nil. JS-TO-EVAL [ from SKELETON-UI-ELEMENT ] string of valid javascript This Javascript will be send with the Ajax response, and evaluated after the innerHTML for this section has been replaced. LABEL-POSITION [ from BASE-FORM-CONTROL ] keyword symbol or nil Specifies where the label tag goes, if any.
```

---

## index.html (chunk 8/19)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/19/index.html
Type: reference

```
Can be :table-td (label goes in a td before the form control), :table-td-append (label goes in a td after the form control), prepend: (label tag wraps around form control and label text comes before form control), append: (label tag wraps around form control and label text comes after form control), table-with-class: (like :table-td, but adds a class "form-control" to the table), or as-div: (puts label and control inside a div of class "form-control"). Default is :as-div LANG [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. MAXLENGTH [ from BASE-FORM-CONTROL ] number or nil Maps to HTML form control attribute of the same name. Default is nil. NULLIFY-EMPTY-STRING? [ from BASE-FORM-CONTROL ] boolean Regardless of :domain, if this is non-nil, empty strings will convert to nil. Defaults to (the allow-nil?) NUMBER? boolean Specifies whether this should be a number form control with support for numerical input. Defaults to nil.
```

---

## index.html (chunk 9/19)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/19/index.html
Type: reference

```
s is non-nil, empty strings will convert to nil. Defaults to (the allow-nil?) NUMBER? boolean Specifies whether this should be a number form control with support for numerical input. Defaults to nil. Use number-form-control to get a default of t. ONBLUR [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONCHANGE [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil, unless ajax-submit-on-change? is non-nil, in which case it calls ajax to set current form value. ONCLICK [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONDBLCLICK [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONENTER [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name.
```

---

## index.html (chunk 10/19)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/19/index.html
Type: reference

```
name. Default is nil. ONENTER [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil, unless ajax-submit-on-enter? is non-nil, in which case it calls ajax to set current form value. ONFOCUS [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONKEYDOWN [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONKEYPRESS [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONKEYUP [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONMOUSEDOWN [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONMOUSEMOVE [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil.
```

---

## index.html (chunk 11/19)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/19/index.html
Type: reference

```
tribute of the same name. Default is nil. ONMOUSEMOVE [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONMOUSEOUT [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONMOUSEOVER [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONMOUSEUP [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONSELECT [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ORDERED-FORM-CONTROLS [ from SKELETON-UI-ELEMENT ] list of gdl objects, which should be of type 'base-form-control . [Note -- this slot is not really necessary for protecting out-of-bounds sequence references anymore, the form-control processor protects against this by itself now]. These objects are validated and bashed first, in the order given.
```

---

## index.html (chunk 12/19)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/19/index.html
Type: reference

```
necessary for protecting out-of-bounds sequence references anymore, the form-control processor protects against this by itself now]. These objects are validated and bashed first, in the order given. If the cardinality of one form-control depends on another as in the example below, then you should list those dependent objects first. Default is nil. examples: ... :computed-slots ((number-of-nozzles (the number-of-nozzles-form value)) (ordered-form-controls (append (list-elements (the inner-flange-form)) (list (the number-of-nozzles-form))))) :objects ((inner-flange-form :type 'menu-form-control :choice-plist (list :hey "hey" :now "now") :default :hey :sequence (:size (the number-of-nozzles))) (number-of-nozzles-form :type 'text-form-control :prompt "Number of Shell Nozzles Required: " :domain :number :default 0) PASSWORD? boolean Specifies whether this should be a password form control with obscured screen text.
```

---

## index.html (chunk 13/19)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/19/index.html
Type: reference

```
in :number :default 0) PASSWORD? boolean Specifies whether this should be a password form control with obscured screen text. Note that this does not automatically give encrypted transmission to the server - you need SSL for that. Defaults to nil. Use password-form-control to get a default of t. PLACEHOLDER [ from BASE-FORM-CONTROL ] string Text to place in the field by default, overwritten as soon as the field is selected. Works only in HTML5. Default is nil. POSSIBLE-NILS [ from SKELETON-UI-ELEMENT ] list of keyword symbols Messages corresponding to form fields which could be missing from form submission (e.g. checkbox fields). Defaults to the names of any children or hidden-children of type menu-form-control or checkbox-form-control. PRESET-ALL? [ from SKELETON-UI-ELEMENT ] boolean This switch determines whether all form-controls should be preset before the final setting, in order to allow any interdependencies to be detected for validation or detecting changed values.
```

---

## index.html (chunk 14/19)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/19/index.html
Type: reference

```
switch determines whether all form-controls should be preset before the final setting, in order to allow any interdependencies to be detected for validation or detecting changed values. If this is specified as a non-nil value, then any nil values of (the preset?) on individual form controls will be ignored. If this is specified as nil, then (the preset?) of individual form-controls (default of these is also nil) will be respected. Default is nil. PRESET? [ from BASE-FORM-CONTROL ] boolean This switch determines whether this form-control should be preset before the final setting, in order to allow any interdependencies to be detected for validation or detecting changed values. Default is nil. PRIMARY? [ from SKELETON-FORM-CONTROL ] boolean Set this to t if the form-control should always occur first in an outputted snapshot file. Defaults to nil. PROMPT [ from BASE-FORM-CONTROL ] string The prompt used in the label.
```

---

## index.html (chunk 15/19)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/19/index.html
Type: reference

```
ur first in an outputted snapshot file. Defaults to nil. PROMPT [ from BASE-FORM-CONTROL ] string The prompt used in the label. READONLY? [ from BASE-FORM-CONTROL ] boolean Maps to HTML form control attribute of the same name. Default is nil. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). ROWS integer The number of rows. If more than 1, this will be a TEXTAREA. Defaults to 1. SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information SIZE [ from BASE-FORM-CONTROL ] number or nil Maps to HTML form control attribute of the same name. Default is nil. SRC [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil.
```

---

## index.html (chunk 16/19)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/19/index.html
Type: reference

```
E-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. STYLE [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. TABINDEX [ from BASE-FORM-CONTROL ] integer or nil Maps to HTML form control attribute of the same name. Default is nil. TITLE [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. USEMAP [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil.
```

---

## index.html (chunk 17/19)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/19/index.html
Type: reference

```
] string or nil Maps to HTML form control attribute of the same name. Default is nil. VALIDATION-FUNCTION [ from BASE-FORM-CONTROL ] function of one argument The argument will be the submitted form value converted to the proper type. The return value from this function can be nil, any non-nil value, or a plist with keys :validated-value and :error. The following behavior applies: If the function returns nil, error is set to :unspecified-validation-fail. If the function returns a plist with keys :validated-value and :error, and if :error is non-nil, it means the value is not acceptable, the form-controls error message is set to this error (usually a keyword symbol), and the error string will be appended to the html-string by default. If the function returns any other value, then the properly typed submitted form value is considered valid and is used. In the case of an error, the form-control's failed-value message is set to the properly typed submitted form value.
```

---

## index.html (chunk 18/19)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/19/index.html
Type: reference

```
ly typed submitted form value is considered valid and is used. In the case of an error, the form-control's failed-value message is set to the properly typed submitted form value. If allow-invalid? is non-nil, then the form-control's value message is also set to this value (i.e. the invalid value is still accepted, even though a non-nil error is present). Default is (list :validated-value value :error nil). VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, defaulting) RESPONDENT [ from SKELETON-UI-ELEMENT ] gdl object Object to respond to the form submission. Defaults to self. Examples Please see base-form-control for all the examples. Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 19/19)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/19/index.html
Type: reference

```
Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/2)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/8/index.html
Type: reference

```
GendL Application - GEOMETRY-VIEW-MIXIN Package Documentation Object: GEOMETRY-VIEW-MIXIN (The :GWL Package) Mixins: VANILLA-MIXIN Description Internal mixin for use inside e.g. base-html-graphics-sheet. Input Slots (optional) HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. LENGTH number Length ("height" of screen window) of the graphics viewport. Default is 300. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places.
```

---

## index.html (chunk 2/2)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/8/index.html
Type: reference

```
formation STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VIEW-OBJECT gdl web-drawing object This must be overridden in the specialized class. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. WIDTH number Width of the graphics viewport. Default is 300. Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/6/index.html
Type: reference

```
GendL Application - CHECKBOX-FORM-CONTROL Package Documentation Object: CHECKBOX-FORM-CONTROL (The :GWL Package) Mixins: BASE-FORM-CONTROL , VANILLA-MIXIN Author Dave Cooper, Genworks Description This represents a INPUT of TYPE CHECKBOX Input Slots (optional) ACCEPT [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ACCESSKEY [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. AJAX-SUBMIT-ON-CHANGE? [ from BASE-FORM-CONTROL ] boolean If set to non-nil, this field's value will be sent to server upon change. Default is nil. AJAX-SUBMIT-ON-ENTER? [ from BASE-FORM-CONTROL ] boolean If set to non-nil, this field's value will be sent to server upon enter. Default is nil. ALIGN [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil.
```

---

## index.html (chunk 2/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/6/index.html
Type: reference

```
from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ALLOW-INVALID-TYPE? [ from BASE-FORM-CONTROL ] boolean If non-nil, then values which fail the type test will still be allowed to be the value. Default is nil. ALLOW-INVALID? [ from BASE-FORM-CONTROL ] boolean If non-nil, then values which fail the type or validation test will still be allowed to be the value. Default is t. ALLOW-NIL? [ from BASE-FORM-CONTROL ] boolean Regardless of :domain, if this is non-nil, nil values will be accepted. Defaults to t if (the default) is nil, otherwise defaults to nil. ALT [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. APPEND-ERROR-STRING? [ from BASE-FORM-CONTROL ] boolean Determines whether a default error string is appended to string ouput-function for html-format (and therefore html-string computed-slot as well). Defaults to t.
```

---

## index.html (chunk 3/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/6/index.html
Type: reference

```
whether a default error string is appended to string ouput-function for html-format (and therefore html-string computed-slot as well). Defaults to t. BASHEE [ from SKELETON-UI-ELEMENT ] gdl object Object to have its settable computed-slots and/or query-plist set from the fields on the form upon submission. Defaults to self. CLASS [ from SKELETON-FORM-CONTROL ] string You can use this to specify a user-defined class for the form-control. Defaults to nil, which means no class attribute will be generated. DEFAULT [ from BASE-FORM-CONTROL ] lisp value of a type compatible with (the domain) This is the initial default value for the control. This must be specified by user code, or an error will result. DISABLED? [ from BASE-FORM-CONTROL ] boolean Maps to HTML form control attribute of the same name. Default is nil. DOM-ID [ from SKELETON-UI-ELEMENT ] string This is the auto-computed dom-id which should be used for rendering this section.
```

---

## index.html (chunk 4/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/6/index.html
Type: reference

```
name. Default is nil. DOM-ID [ from SKELETON-UI-ELEMENT ] string This is the auto-computed dom-id which should be used for rendering this section. If you use the main-div HTML string for rendering this object as a page section, then you do not have to generate the :div tag yourself - the main-div will be a string of HTML which is wrapped in the correct :div tag already. DOMAIN keyword symbol The domain defaults to :boolean for the checkbox-form-control. However, this can be overridden in user code if the checkbox is supposed to return a meaningful value other than nil or t (e.g. for a group of checkboxes with the same name, where each can return a different value). FIELD-NAME [ from SKELETON-FORM-CONTROL ] keyword symbol The name of this field. Computed from the object name within the tree.
```

---

## index.html (chunk 5/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/6/index.html
Type: reference

```
. FORCE-VALIDATION-FOR [ from SKELETON-UI-ELEMENT ] list of gdl objects of type form-control The validation-function will be forced on these objects when a form is submitted, even if the object's html form-control does not happen to be included in the values submitted with the form. Defaults to nil. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. HTML-SECTIONS [ from SKELETON-UI-ELEMENT ] List of HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded. HTML-SECTIONS-VALID [ from SKELETON-UI-ELEMENT ] List of valid HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk.
```

---

## index.html (chunk 6/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/6/index.html
Type: reference

```
ECTIONS-VALID [ from SKELETON-UI-ELEMENT ] List of valid HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded. ID [ from SKELETON-FORM-CONTROL ] keyword symbol The ID attribute for this tag. Defaults to (the field-name). INNER-HTML [ from SKELETON-UI-ELEMENT ] string This can be used with (str .) [in cl-who] or (:princ .) [in htmlGen] to output this section of the page, without the wrapping :div tag [so if you use this, your code would be responsible for wrapping the :div tag with :id (the dom-id).] ISMAP? [ from BASE-FORM-CONTROL ] boolean Maps to HTML form control attribute of the same name. Default is nil.
```

---

## index.html (chunk 7/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/6/index.html
Type: reference

```
ontrol attribute of the same name. Default is nil. JS-TO-EVAL [ from SKELETON-UI-ELEMENT ] string of valid javascript This Javascript will be send with the Ajax response, and evaluated after the innerHTML for this section has been replaced. LABEL-POSITION [ from BASE-FORM-CONTROL ] keyword symbol or nil Specifies where the label tag goes, if any. Can be :table-td (label goes in a td before the form control), :table-td-append (label goes in a td after the form control), prepend: (label tag wraps around form control and label text comes before form control), append: (label tag wraps around form control and label text comes after form control), table-with-class: (like :table-td, but adds a class "form-control" to the table), or as-div: (puts label and control inside a div of class "form-control"). Default is :as-div LANG [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil.
```

---

## index.html (chunk 8/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/6/index.html
Type: reference

```
ol"). Default is :as-div LANG [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. MAXLENGTH [ from BASE-FORM-CONTROL ] number or nil Maps to HTML form control attribute of the same name. Default is nil. NULLIFY-EMPTY-STRING? [ from BASE-FORM-CONTROL ] boolean Regardless of :domain, if this is non-nil, empty strings will convert to nil. Defaults to (the allow-nil?) ONBLUR [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONCHANGE [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil, unless ajax-submit-on-change? is non-nil, in which case it calls ajax to set current form value. ONCLICK [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONDBLCLICK [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name.
```

---

## index.html (chunk 9/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/6/index.html
Type: reference

```
ng or nil Maps to HTML form control attribute of the same name. Default is nil. ONDBLCLICK [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONENTER [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil, unless ajax-submit-on-enter? is non-nil, in which case it calls ajax to set current form value. ONFOCUS [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONKEYDOWN [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONKEYPRESS [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONKEYUP [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil.
```

---

## index.html (chunk 10/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/6/index.html
Type: reference

```
ORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONMOUSEDOWN [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONMOUSEMOVE [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONMOUSEOUT [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONMOUSEOVER [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONMOUSEUP [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ONSELECT [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. ORDERED-FORM-CONTROLS [ from SKELETON-UI-ELEMENT ] list of gdl objects, which should be of type 'base-form-control .
```

---

## index.html (chunk 11/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/6/index.html
Type: reference

```
he same name. Default is nil. ORDERED-FORM-CONTROLS [ from SKELETON-UI-ELEMENT ] list of gdl objects, which should be of type 'base-form-control . [Note -- this slot is not really necessary for protecting out-of-bounds sequence references anymore, the form-control processor protects against this by itself now]. These objects are validated and bashed first, in the order given. If the cardinality of one form-control depends on another as in the example below, then you should list those dependent objects first. Default is nil. examples: ...
```

---

## index.html (chunk 12/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/6/index.html
Type: reference

```
:computed-slots ((number-of-nozzles (the number-of-nozzles-form value)) (ordered-form-controls (append (list-elements (the inner-flange-form)) (list (the number-of-nozzles-form))))) :objects ((inner-flange-form :type 'menu-form-control :choice-plist (list :hey "hey" :now "now") :default :hey :sequence (:size (the number-of-nozzles))) (number-of-nozzles-form :type 'text-form-control :prompt "Number of Shell Nozzles Required: " :domain :number :default 0) PLACEHOLDER [ from BASE-FORM-CONTROL ] string Text to place in the field by default, overwritten as soon as the field is selected. Works only in HTML5. Default is nil. POSSIBLE-NIL? boolean Indicates whether this should be included in possible-nils. Defaults to t. POSSIBLE-NILS [ from SKELETON-UI-ELEMENT ] list of keyword symbols Messages corresponding to form fields which could be missing from form submission (e.g. checkbox fields).
```

---

## index.html (chunk 13/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/6/index.html
Type: reference

```
corresponding to form fields which could be missing from form submission (e.g. checkbox fields). Defaults to the names of any children or hidden-children of type menu-form-control or checkbox-form-control. PRESET-ALL? [ from SKELETON-UI-ELEMENT ] boolean This switch determines whether all form-controls should be preset before the final setting, in order to allow any interdependencies to be detected for validation or detecting changed values. If this is specified as a non-nil value, then any nil values of (the preset?) on individual form controls will be ignored. If this is specified as nil, then (the preset?) of individual form-controls (default of these is also nil) will be respected. Default is nil. PRESET? [ from BASE-FORM-CONTROL ] boolean This switch determines whether this form-control should be preset before the final setting, in order to allow any interdependencies to be detected for validation or detecting changed values. Default is nil.
```

---

## index.html (chunk 14/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/6/index.html
Type: reference

```
ol should be preset before the final setting, in order to allow any interdependencies to be detected for validation or detecting changed values. Default is nil. PRIMARY? [ from SKELETON-FORM-CONTROL ] boolean Set this to t if the form-control should always occur first in an outputted snapshot file. Defaults to nil. PROMPT [ from BASE-FORM-CONTROL ] string The prompt used in the label. READONLY? [ from BASE-FORM-CONTROL ] boolean Maps to HTML form control attribute of the same name. Default is nil. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information SIZE [ from BASE-FORM-CONTROL ] number or nil Maps to HTML form control attribute of the same name. Default is nil.
```

---

## index.html (chunk 15/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/6/index.html
Type: reference

```
e back as a plist with error information SIZE [ from BASE-FORM-CONTROL ] number or nil Maps to HTML form control attribute of the same name. Default is nil. SRC [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. STYLE [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. TABINDEX [ from BASE-FORM-CONTROL ] integer or nil Maps to HTML form control attribute of the same name. Default is nil. TITLE [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil.
```

---

## index.html (chunk 16/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/6/index.html
Type: reference

```
name. Default is nil. TITLE [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. USEMAP [ from BASE-FORM-CONTROL ] string or nil Maps to HTML form control attribute of the same name. Default is nil. VALIDATION-FUNCTION [ from BASE-FORM-CONTROL ] function of one argument The argument will be the submitted form value converted to the proper type. The return value from this function can be nil, any non-nil value, or a plist with keys :validated-value and :error. The following behavior applies: If the function returns nil, error is set to :unspecified-validation-fail. If the function returns a plist with keys :validated-value and :error, and if :error is non-nil, it means the value is not acceptable, the form-controls error message is set to this error (usually a keyword symbol), and the error string will be appended to the html-string by default.
```

---

## index.html (chunk 17/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/6/index.html
Type: reference

```
set to this error (usually a keyword symbol), and the error string will be appended to the html-string by default. If the function returns any other value, then the properly typed submitted form value is considered valid and is used. In the case of an error, the form-control's failed-value message is set to the properly typed submitted form value. If allow-invalid? is non-nil, then the form-control's value message is also set to this value (i.e. the invalid value is still accepted, even though a non-nil error is present). Default is (list :validated-value value :error nil). VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, defaulting) RESPONDENT [ from SKELETON-UI-ELEMENT ] gdl object Object to respond to the form submission. Defaults to self. Examples Please see base-form-control for all the examples.
```

---

## index.html (chunk 18/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/6/index.html
Type: reference

```
ENT [ from SKELETON-UI-ELEMENT ] gdl object Object to respond to the form submission. Defaults to self. Examples Please see base-form-control for all the examples. Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/4/index.html
Type: reference

```
GendL Application - BASE-HTML-GRAPHICS-SHEET Package Documentation Object: BASE-HTML-GRAPHICS-SHEET (The :GWL Package) Mixins: BASE-HTML-SHEET , GEOMETRY-VIEW-MIXIN , BASE-OBJECT Description This mixin allows a part to be displayed as a web page in GWL, and to contain one graphics area. It requires the geom-base module to be loaded. This will probably be extended to allow more than one graphics area. This mixin inherits from base-html-sheet, so just like with base-html-sheet you can prepare the output with the write-html-sheet function in a the object which mixes this in, or in a main-sheet output-function in an html-format view of the object. Input Slots (optional) AFTER-PRESENT! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the respondent of a form, to do some processing after the respondent's write-html-sheet function runs to present the object.
```

---

## index.html (chunk 2/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/4/index.html
Type: reference

```
a form, to do some processing after the respondent's write-html-sheet function runs to present the object. AFTER-SET! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the requestor of a form, to do some processing after the requestor's form values are set into the specified bashee. BASHEE [ from SKELETON-UI-ELEMENT ] gdl object Object to have its settable computed-slots and/or query-plist set from the fields on the form upon submission. Defaults to self. BEFORE-PRESENT! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the respondent of a form, to do some processing before the respondent's write-html-sheet function runs to present the object. This can be useful especially for objects which are subclasses of higher-level mixins such as application-mixin and node-mixin , where you do not have direct access to the write-html-sheet function and typically only define the model-inputs function.
```

---

## index.html (chunk 3/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/4/index.html
Type: reference

```
s of higher-level mixins such as application-mixin and node-mixin , where you do not have direct access to the write-html-sheet function and typically only define the model-inputs function. It is not always reliable to do processing in the model-inputs function, since some slots which depend on your intended modifications may already have been evaluated by the time the model-inputs function runs. BEFORE-RESPONSE! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in a user specialization of base-html-sheet, to do some processing before the header-plist is evaluated and before the HTTP response is actually initiated. BEFORE-SET! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the requestor of a form, to do some processing before the requestor's form values are set into the specified bashee.
```

---

## index.html (chunk 4/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/4/index.html
Type: reference

```
ome processing before the requestor's form values are set into the specified bashee. BOUNDING-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object. CENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located. CHECK-SANITY? [ from BASE-HTML-SHEET ] boolean Determines whether a a sanity check is done (with the check-sanity function) before presenting the response page if this page is a respondent. Default is NIL. DOM-ID [ from SKELETON-UI-ELEMENT ] string This is the auto-computed dom-id which should be used for rendering this section. If you use the main-div HTML string for rendering this object as a page section, then you do not have to generate the :div tag yourself - the main-div will be a string of HTML which is wrapped in the correct :div tag already.
```

---

## index.html (chunk 5/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/4/index.html
Type: reference

```
ML string for rendering this object as a page section, then you do not have to generate the :div tag yourself - the main-div will be a string of HTML which is wrapped in the correct :div tag already. FORCE-VALIDATION-FOR [ from SKELETON-UI-ELEMENT ] list of gdl objects of type form-control The validation-function will be forced on these objects when a form is submitted, even if the object's html form-control does not happen to be included in the values submitted with the form. Defaults to nil. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. HTML-SECTIONS [ from SKELETON-UI-ELEMENT ] List of HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded.
```

---

## index.html (chunk 6/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/4/index.html
Type: reference

```
lot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded. HTML-SECTIONS-VALID [ from SKELETON-UI-ELEMENT ] List of valid HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object.
```

---

## index.html (chunk 7/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/4/index.html
Type: reference

```
Defaults to nil INNER-HTML [ from SKELETON-UI-ELEMENT ] string This can be used with (str .) [in cl-who] or (:princ .) [in htmlGen] to output this section of the page, without the wrapping :div tag [so if you use this, your code would be responsible for wrapping the :div tag with :id (the dom-id).] JS-TO-EVAL [ from SKELETON-UI-ELEMENT ] string of valid javascript This Javascript will be send with the Ajax response, and evaluated after the innerHTML for this section has been replaced. LENGTH [ from GEOMETRY-VIEW-MIXIN ] number Length ("height" of screen window) of the graphics viewport. Default is 300. LOCAL-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation .
```

---

## index.html (chunk 8/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/4/index.html
Type: reference

```
UENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. ORDERED-FORM-CONTROLS [ from SKELETON-UI-ELEMENT ] list of gdl objects, which should be of type 'base-form-control . [Note -- this slot is not really necessary for protecting out-of-bounds sequence references anymore, the form-control processor protects against this by itself now]. These objects are validated and bashed first, in the order given. If the cardinality of one form-control depends on another as in the example below, then you should list those dependent objects first. Default is nil. examples: ...
```

---

## index.html (chunk 9/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/4/index.html
Type: reference

```
list those dependent objects first. Default is nil. examples: ... :computed-slots ((number-of-nozzles (the number-of-nozzles-form value)) (ordered-form-controls (append (list-elements (the inner-flange-form)) (list (the number-of-nozzles-form))))) :objects ((inner-flange-form :type 'menu-form-control :choice-plist (list :hey "hey" :now "now") :default :hey :sequence (:size (the number-of-nozzles))) (number-of-nozzles-form :type 'text-form-control :prompt "Number of Shell Nozzles Required: " :domain :number :default 0) ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0).
```

---

## index.html (chunk 10/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/4/index.html
Type: reference

```
the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). POSSIBLE-NILS [ from SKELETON-UI-ELEMENT ] list of keyword symbols Messages corresponding to form fields which could be missing from form submission (e.g. checkbox fields). Defaults to the names of any children or hidden-children of type menu-form-control or checkbox-form-control. PRESET-ALL? [ from SKELETON-UI-ELEMENT ] boolean This switch determines whether all form-controls should be preset before the final setting, in order to allow any interdependencies to be detected for validation or detecting changed values. If this is specified as a non-nil value, then any nil values of (the preset?) on individual form controls will be ignored. If this is specified as nil, then (the preset?) of individual form-controls (default of these is also nil) will be respected. Default is nil.
```

---

## index.html (chunk 11/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/4/index.html
Type: reference

```
(the preset?) of individual form-controls (default of these is also nil) will be respected. Default is nil. PROCESS-COOKIES! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in a user specialization of base-html-sheet, to do some processing before the header-plist is evaluated and before the HTTP response is actually initiated, but after the cookies-received have been set. RETURN-OBJECT [ from BASE-HTML-SHEET ] gdl object Default object to which control will return with the write-back-link method ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STANDARD-VIEWS plist of keywords and 3d vectors . Indicates the views to show in the graphics controls.
```

---

## index.html (chunk 12/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/4/index.html
Type: reference

```
sts. Any children which throw errors come back as a plist with error information STANDARD-VIEWS plist of keywords and 3d vectors . Indicates the views to show in the graphics controls. STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. TARGET [ from BASE-HTML-SHEET ] string Name of a browser frame or window to display this page. Default of NIL indicates to use the same window. TRANSITORY-SLOTS [ from BASE-HTML-SHEET ] list of keyword symbols Messages corresponding to form fields which should not be retained against Updates to the model (e.g. calls to the update! function or hitting the Update button or link in the browser in development mode). Defaults to NIL (the empty list).
```

---

## index.html (chunk 13/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/4/index.html
Type: reference

```
del (e.g. calls to the update! function or hitting the Update button or link in the browser in development mode). Defaults to NIL (the empty list). USE-BSPLINES? boolean Determines whether to use native bspline data in the vrml VIEW-OBJECT [ from GEOMETRY-VIEW-MIXIN ] gdl web-drawing object This must be overridden in the specialized class. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. WIDTH [ from GEOMETRY-VIEW-MIXIN ] number Width of the graphics viewport. Default is 300. Input Slots (optional, settable) DIGITATION-MODE keyword symbol, one of :zoom-and-center , :report-point , or :measure-distance . If :zoom-and-center , sets the user-center and user-scale accordingly when graphics area is clicked. If :report-point , the slot digitized-point is set with the x y value. If measure-distance , the slot :digitized-distance is set with the resultant distance.
```

---

## index.html (chunk 14/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/4/index.html
Type: reference

```
ordingly when graphics area is clicked. If :report-point , the slot digitized-point is set with the x y value. If measure-distance , the slot :digitized-distance is set with the resultant distance. Default is :zoom-and-center IMAGE-FORMAT keyword symbol Determines the default image format. Defaults to :png VIEW keyword symbol Determines the default view from the standard-views . Defaults to :trimetric. ZOOM-FACTOR number The factor used for zooming in or out. ZOOM-MODE keyword symbol, one of :in, :out, or :none, or nil If :in, then clicks in the graphics area will increase the zoom factor by (the zoom-factor). If :out, then clicks will decrease the factor by that amount. If :none or nil, then clicks will have no effect. Input Slots (optional, defaulting) DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object.
```

---

## index.html (chunk 15/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/4/index.html
Type: reference

```
] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. RESPONDENT [ from SKELETON-UI-ELEMENT ] gdl object Object to respond to the form submission. Defaults to self.
```

---

## index.html (chunk 16/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/4/index.html
Type: reference

```
ETON-UI-ELEMENT ] gdl object Object to respond to the form submission. Defaults to self. GDL Functions BACKGROUND-COLOR keyword symbol, string, list, or vector Default background for the graphics viewport. Can be specified as a name (keyword or string) in *color-table*, an html-style hex string (starting with #), or a decimal RGB triplet in a list or vector. The default comes from the :background entry in *colors-default* . FOREGROUND-COLOR keyword symbol, string, list, or vector Default foreground for the graphics viewport. Can be specified as a name (keyword or string) in *color-table*, an html-style hex string (starting with #), or a decimal RGB triplet in a list or vector. The default comes from the :foreground entry in *colors-default* . REPORT-POINT void Process the points selected by digitizing in the graphics. You can override this function to do your own processing. By default, it prints the information to the console.
```

---

## index.html (chunk 17/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/4/index.html
Type: reference

```
y digitizing in the graphics. You can override this function to do your own processing. By default, it prints the information to the console. arguments: x Number The X Coordinate of the digitized point y Number The Y Coordinate of the digitized point WRITE-EMBEDDED-VRML-WORLD void Writes an EMBED tag and publishes a VRML world for the view-object child of this object. The view-object child should exist and be of type web-drawing . WRITE-EMBEDDED-X3D-WORLD void Writes an OBJECT tag and publishes an X3D world for the view-object child of this object. The view-object child should exist and be of type web-drawing . WRITE-GEOMETRY void Writes an image tag and publishes an image for the view-object child of this object. The view-object child should exist and be of type web-drawing . For objects of type gwl:application-mixin or gwl:node-mixin , this is done automatically.
```

---

## index.html (chunk 18/18)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/4/index.html
Type: reference

```
of type gwl:application-mixin or gwl:node-mixin , this is done automatically. For the time being, we recommend that you use gwl:application-mixin or gwl:node-mixin if you want to display geometric parts in a GWL application. keyword arguments: include-view-controls? Boolean , Default Value: T Determines whether the standard view controls are displayed below the image Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/21)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/13/index.html
Type: reference

```
GendL Application - NODE-MIXIN Package Documentation Object: NODE-MIXIN (The :GWL Package) Mixins: LAYOUT-MIXIN , VANILLA-MIXIN Description Generates a default GWL user interface with a model-inputs area, user-navigable tree with child applications, graphics view with controls, and rule display. Child objects should be of type node-mixin or application-mixin . Child hidden-objects may be of any type. The ui-display-list-objects is appended up automatically from those of the children. Input Slots (optional) AFTER-PRESENT! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the respondent of a form, to do some processing after the respondent's write-html-sheet function runs to present the object. AFTER-SET! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the requestor of a form, to do some processing after the requestor's form values are set into the specified bashee.
```

---

## index.html (chunk 2/21)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/13/index.html
Type: reference

```
ty function by default, but can be overridden in the requestor of a form, to do some processing after the requestor's form values are set into the specified bashee. AVAILABLE-IMAGE-FORMATS [ from LAYOUT-MIXIN ] list of keyword symbols Determines which formats are available in the Preferences. Defaults to :png, :jpeg, and :vrml. BASHEE [ from SKELETON-UI-ELEMENT ] gdl object Object to have its settable computed-slots and/or query-plist set from the fields on the form upon submission. Defaults to self. BEFORE-PRESENT! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the respondent of a form, to do some processing before the respondent's write-html-sheet function runs to present the object. This can be useful especially for objects which are subclasses of higher-level mixins such as application-mixin and node-mixin , where you do not have direct access to the write-html-sheet function and typically only define the model-inputs function.
```

---

## index.html (chunk 3/21)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/13/index.html
Type: reference

```
ses of higher-level mixins such as application-mixin and node-mixin , where you do not have direct access to the write-html-sheet function and typically only define the model-inputs function. It is not always reliable to do processing in the model-inputs function, since some slots which depend on your intended modifications may already have been evaluated by the time the model-inputs function runs. BEFORE-RESPONSE! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in a user specialization of base-html-sheet, to do some processing before the header-plist is evaluated and before the HTTP response is actually initiated. BEFORE-SET! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the requestor of a form, to do some processing before the requestor's form values are set into the specified bashee. BODY-BGCOLOR [ from LAYOUT-MIXIN ] keyword symbol Color keyword from *color-table* for the body background.
```

---

## index.html (chunk 4/21)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/13/index.html
Type: reference

```
some processing before the requestor's form values are set into the specified bashee. BODY-BGCOLOR [ from LAYOUT-MIXIN ] keyword symbol Color keyword from *color-table* for the body background. Defaults to :blue-sky . BOUNDING-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object. CENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located. CHECK-SANITY? [ from BASE-HTML-SHEET ] boolean Determines whether a a sanity check is done (with the check-sanity function) before presenting the response page if this page is a respondent. Default is NIL. DEFAULT-TREE-DEPTH integer Determines how many descendant levels to show in the tree initially. Default is 1.
```

---

## index.html (chunk 5/21)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/13/index.html
Type: reference

```
H integer Determines how many descendant levels to show in the tree initially. Default is 1. DOM-ID [ from SKELETON-UI-ELEMENT ] string This is the auto-computed dom-id which should be used for rendering this section. If you use the main-div HTML string for rendering this object as a page section, then you do not have to generate the :div tag yourself - the main-div will be a string of HTML which is wrapped in the correct :div tag already. FORCE-VALIDATION-FOR [ from SKELETON-UI-ELEMENT ] list of gdl objects of type form-control The validation-function will be forced on these objects when a form is submitted, even if the object's html form-control does not happen to be included in the values submitted with the form. Defaults to nil. HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil.
```

---

## index.html (chunk 6/21)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/13/index.html
Type: reference

```
e reference box. Defaults to zero. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. HTML-SECTIONS [ from SKELETON-UI-ELEMENT ] List of HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded. HTML-SECTIONS-VALID [ from SKELETON-UI-ELEMENT ] List of valid HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded.
```

---

## index.html (chunk 7/21)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/13/index.html
Type: reference

```
s is set programmatically every time the sheet section's main-div is demanded. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil IMAGE-FORMAT [ from BASE-HTML-GRAPHICS-SHEET ] keyword symbol Determines the default image format. Defaults to :png INNER-HTML [ from SKELETON-UI-ELEMENT ] string This can be used with (str .) [in cl-who] or (:princ .) [in htmlGen] to output this section of the page, without the wrapping :div tag [so if you use this, your code would be responsible for wrapping the :div tag with :id (the dom-id).] INPUTS-BGCOLOR [ from LAYOUT-MIXIN ] keyword symbol Color keyword from *color-table* for the model-inputs area background. Defaults to :aquamarine . INPUTS-TITLE [ from LAYOUT-MIXIN ] string Title for the model-inputs section. Defaults to "Model Inputs".
```

---

## index.html (chunk 8/21)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/13/index.html
Type: reference

```
TLE [ from LAYOUT-MIXIN ] string Title for the model-inputs section. Defaults to "Model Inputs". JS-TO-EVAL [ from SKELETON-UI-ELEMENT ] string of valid javascript This Javascript will be send with the Ajax response, and evaluated after the innerHTML for this section has been replaced. LENGTH [ from GEOMETRY-VIEW-MIXIN ] number Length ("height" of screen window) of the graphics viewport. Default is 300. LOCAL-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object. MULTIPART-FORM? [ from LAYOUT-MIXIN ] boolean Determines whether the embedded form will support multipart MIME parts. Defaults to NIL. NODE-UI-DISPLAY-LIST-OBJECTS gdl object list Appends additional objects to the automatically-appended ui-display-list-objects from the children. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation .
```

---

## index.html (chunk 9/21)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/13/index.html
Type: reference

```
the automatically-appended ui-display-list-objects from the children. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. ORDERED-FORM-CONTROLS [ from SKELETON-UI-ELEMENT ] list of gdl objects, which should be of type 'base-form-control . [Note -- this slot is not really necessary for protecting out-of-bounds sequence references anymore, the form-control processor protects against this by itself now]. These objects are validated and bashed first, in the order given. If the cardinality of one form-control depends on another as in the example below, then you should list those dependent objects first. Default is nil. examples: ...
```

---

## index.html (chunk 10/21)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/13/index.html
Type: reference

```
one form-control depends on another as in the example below, then you should list those dependent objects first. Default is nil. examples: ... :computed-slots ((number-of-nozzles (the number-of-nozzles-form value)) (ordered-form-controls (append (list-elements (the inner-flange-form)) (list (the number-of-nozzles-form))))) :objects ((inner-flange-form :type 'menu-form-control :choice-plist (list :hey "hey" :now "now") :default :hey :sequence (:size (the number-of-nozzles))) (number-of-nozzles-form :type 'text-form-control :prompt "Number of Shell Nozzles Required: " :domain :number :default 0) ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0).
```

---

## index.html (chunk 11/21)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/13/index.html
Type: reference

```
(i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). OTHER-RULES [ from LAYOUT-MIXIN ] list of gdl objects of type base-rule-object or (preferably) gwl-base-rule-object . Links to these will be displayed in the other-rules section. Default to the collection of all objects of type base-rule-object from this node in the tree down to the leaves, whose violated? message evaluates to NIL. OTHER-RULES-BGCOLOR [ from LAYOUT-MIXIN ] keyword symbol Color keyword from *color-table* for the other-rules area background. Defaults to :aquamarine . OTHER-RULES-TITLE [ from LAYOUT-MIXIN ] string Title for the other-rules section. Defaults to "Other Rules". PAGE-TITLE [ from LAYOUT-MIXIN ] string The title to display on the page and in the tree. Defaults to (the strings-for-display) .
```

---

## index.html (chunk 12/21)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/13/index.html
Type: reference

```
TLE [ from LAYOUT-MIXIN ] string The title to display on the page and in the tree. Defaults to (the strings-for-display) . POSSIBLE-NILS [ from SKELETON-UI-ELEMENT ] list of keyword symbols Messages corresponding to form fields which could be missing from form submission (e.g. checkbox fields). Defaults to the names of any children or hidden-children of type menu-form-control or checkbox-form-control. PRESET-ALL? [ from SKELETON-UI-ELEMENT ] boolean This switch determines whether all form-controls should be preset before the final setting, in order to allow any interdependencies to be detected for validation or detecting changed values. If this is specified as a non-nil value, then any nil values of (the preset?) on individual form controls will be ignored. If this is specified as nil, then (the preset?) of individual form-controls (default of these is also nil) will be respected. Default is nil.
```

---

## index.html (chunk 13/21)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/13/index.html
Type: reference

```
n (the preset?) of individual form-controls (default of these is also nil) will be respected. Default is nil. PROCESS-COOKIES! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in a user specialization of base-html-sheet, to do some processing before the header-plist is evaluated and before the HTTP response is actually initiated, but after the cookies-received have been set. RETURN-OBJECT [ from BASE-HTML-SHEET ] gdl object Default object to which control will return with the write-back-link method ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information SHOW-TITLE? [ from LAYOUT-MIXIN ] boolean Indicates whether to display the title at the top of the page.
```

---

## index.html (chunk 14/21)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/13/index.html
Type: reference

```
lists. Any children which throw errors come back as a plist with error information SHOW-TITLE? [ from LAYOUT-MIXIN ] boolean Indicates whether to display the title at the top of the page. Defaults to T. STANDARD-VIEWS [ from BASE-HTML-GRAPHICS-SHEET ] plist of keywords and 3d vectors . Indicates the views to show in the graphics controls. STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. TARGET [ from BASE-HTML-SHEET ] string Name of a browser frame or window to display this page. Default of NIL indicates to use the same window. TRANSITORY-SLOTS [ from BASE-HTML-SHEET ] list of keyword symbols Messages corresponding to form fields which should not be retained against Updates to the model (e.g.
```

---

## index.html (chunk 15/21)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/13/index.html
Type: reference

```
ANSITORY-SLOTS [ from BASE-HTML-SHEET ] list of keyword symbols Messages corresponding to form fields which should not be retained against Updates to the model (e.g. calls to the update! function or hitting the Update button or link in the browser in development mode). Defaults to NIL (the empty list). TREE-BGCOLOR [ from LAYOUT-MIXIN ] keyword symbol Color keyword from *color-table* for the tree area background. Defaults to :aquamarine . TREE-TITLE [ from LAYOUT-MIXIN ] string Title for the Tree section. Defaults to "Assembly Tree" if the tree-root is only a subclass of application-mixin , and "Assembly Tree" if the tree-root is an actual node with child applications. USE-BSPLINES? [ from BASE-HTML-GRAPHICS-SHEET ] boolean Determines whether to use native bspline data in the vrml VIOLATED-RULES [ from LAYOUT-MIXIN ] list of gdl objects of type base-rule-object or (preferably) gwl-base-rule-object . Links to these will be displayed in the other-rules section.
```

---

## index.html (chunk 16/21)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/13/index.html
Type: reference

```
-RULES [ from LAYOUT-MIXIN ] list of gdl objects of type base-rule-object or (preferably) gwl-base-rule-object . Links to these will be displayed in the other-rules section. Default to the collection of all objects of type base-rule-object from this node in the tree down to the leaves, whose violated? message evaluates to non-NIL. VIOLATED-RULES-BGCOLOR [ from LAYOUT-MIXIN ] keyword symbol Color keyword from *color-table* for the violated-rules area background. Defaults to :aquamarine . VIOLATED-RULES-TITLE [ from LAYOUT-MIXIN ] string Title for the violated-rules section. Defaults to "Violated Rules". VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. WIDTH [ from GEOMETRY-VIEW-MIXIN ] number Width of the graphics viewport. Default is 300.
```

---

## index.html (chunk 17/21)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/13/index.html
Type: reference

```
GEOMETRY-VIEW-MIXIN ] number Width of the graphics viewport. Default is 300. Input Slots (optional, settable) DIGITATION-MODE [ from BASE-HTML-GRAPHICS-SHEET ] keyword symbol, one of :zoom-and-center , :report-point , or :measure-distance . If :zoom-and-center , sets the user-center and user-scale accordingly when graphics area is clicked. If :report-point , the slot digitized-point is set with the x y value. If measure-distance , the slot :digitized-distance is set with the resultant distance. Default is :zoom-and-center VIEW [ from BASE-HTML-GRAPHICS-SHEET ] keyword symbol Determines the default view from the standard-views . Defaults to :trimetric. ZOOM-FACTOR [ from BASE-HTML-GRAPHICS-SHEET ] number The factor used for zooming in or out. ZOOM-MODE [ from BASE-HTML-GRAPHICS-SHEET ] keyword symbol, one of :in, :out, or :none, or nil If :in, then clicks in the graphics area will increase the zoom factor by (the zoom-factor).
```

---

## index.html (chunk 18/21)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/13/index.html
Type: reference

```
ord symbol, one of :in, :out, or :none, or nil If :in, then clicks in the graphics area will increase the zoom factor by (the zoom-factor). If :out, then clicks will decrease the factor by that amount. If :none or nil, then clicks will have no effect. Input Slots (optional, defaulting) DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line.
```

---

## index.html (chunk 19/21)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/13/index.html
Type: reference

```
st of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. DISPLAY-RULES? [ from LAYOUT-MIXIN ] boolean Indicates whether the Rules panel should be displayed. Defaults to T. DISPLAY-TREE? [ from LAYOUT-MIXIN ] boolean Indicates whether the Tree area should be displayed. Defaults to T. GRAPHICS-HEIGHT [ from LAYOUT-MIXIN ] integer Height (top to bottom on screen) in pixels of the graphics area. Defaults to 500. GRAPHICS-WIDTH [ from LAYOUT-MIXIN ] integer Height (left to right on screen) in pixels of the graphics area. Defaults to 500. RESPONDENT [ from SKELETON-UI-ELEMENT ] gdl object Object to respond to the form submission. Defaults to self. USE-STANDARD-SAVED-SLOTS? [ from LAYOUT-MIXIN ] boolean Determines whether the standard-saved-slots are automatically used by default for the saved-slots.
```

---

## index.html (chunk 20/21)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/13/index.html
Type: reference

```
USE-STANDARD-SAVED-SLOTS? [ from LAYOUT-MIXIN ] boolean Determines whether the standard-saved-slots are automatically used by default for the saved-slots. This is a trickle-down slot so its value will be passed to descendent objects automatically. The default value is NIL. Computed Slots UI-DISPLAY-LIST-LEAVES [ from LAYOUT-MIXIN ] list of gdl objects This should be overridden with a list of objects of your choice. These objects (not their leaves, but these actual nodes) will be scaled to fit and displayed in the graphics area. Defaults to NIL. UI-DISPLAY-LIST-OBJECTS list of gdl object roots The leaves of these objects will be displayed in the graphics. Defaults to the appended result of children's ui-display-list-objects . Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 21/21)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/13/index.html
Type: reference

```
. All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/9)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/9/index.html
Type: reference

```
GendL Application - GRID-FORM-CONTROL Package Documentation Object: GRID-FORM-CONTROL (The :GWL Package) Mixins: SKELETON-FORM-CONTROL , VANILLA-MIXIN Description Beginnings of spread-sheet-like grid control. To do: Add row button, sort by column values, save & restore snapshot. Easy way for user to customize layout and markup. Allow for all types of form-control for each column. Input Slots (optional) BASHEE [ from SKELETON-UI-ELEMENT ] gdl object Object to have its settable computed-slots and/or query-plist set from the fields on the form upon submission. Defaults to self. CLASS [ from SKELETON-FORM-CONTROL ] string You can use this to specify a user-defined class for the form-control. Defaults to nil, which means no class attribute will be generated. DEFAULT list of lists These values become the default row and column values for the grid. DOM-ID [ from SKELETON-UI-ELEMENT ] string This is the auto-computed dom-id which should be used for rendering this section.
```

---

## index.html (chunk 2/9)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/9/index.html
Type: reference

```
ecome the default row and column values for the grid. DOM-ID [ from SKELETON-UI-ELEMENT ] string This is the auto-computed dom-id which should be used for rendering this section. If you use the main-div HTML string for rendering this object as a page section, then you do not have to generate the :div tag yourself - the main-div will be a string of HTML which is wrapped in the correct :div tag already. FIELD-NAME [ from SKELETON-FORM-CONTROL ] keyword symbol The name of this field. Computed from the object name within the tree. FORCE-VALIDATION-FOR [ from SKELETON-UI-ELEMENT ] list of gdl objects of type form-control The validation-function will be forced on these objects when a form is submitted, even if the object's html form-control does not happen to be included in the values submitted with the form. Defaults to nil. FORM-CONTROL-ATTRIBUTES list of plists Each plist contains the desired form-control inputs for the respective column in the table.
```

---

## index.html (chunk 3/9)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/9/index.html
Type: reference

```
with the form. Defaults to nil. FORM-CONTROL-ATTRIBUTES list of plists Each plist contains the desired form-control inputs for the respective column in the table. FORM-CONTROL-INPUTS list of lists plists Each list corresponds to one row and contains plists desired form-control inputs for the respective column in the table. FORM-CONTROL-TYPES list of symbols naming gdl object types This must be the same length as a row of the table. The corresponding form-element in the grid will be of the specified type. Default is nil, which means all the form-controls will be of type 'text-form-control. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. HTML-SECTIONS [ from SKELETON-UI-ELEMENT ] List of HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk.
```

---

## index.html (chunk 4/9)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/9/index.html
Type: reference

```
HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded. HTML-SECTIONS-VALID [ from SKELETON-UI-ELEMENT ] List of valid HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded. ID [ from SKELETON-FORM-CONTROL ] keyword symbol The ID attribute for this tag. Defaults to (the field-name). INCLUDE-DELETE-BUTTONS? boolean Should each row have a delete button? Default is nil.
```

---

## index.html (chunk 5/9)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/9/index.html
Type: reference

```
ve a delete button? Default is nil. INNER-HTML [ from SKELETON-UI-ELEMENT ] string This can be used with (str .) [in cl-who] or (:princ .) [in htmlGen] to output this section of the page, without the wrapping :div tag [so if you use this, your code would be responsible for wrapping the :div tag with :id (the dom-id).] JS-TO-EVAL [ from SKELETON-UI-ELEMENT ] string of valid javascript This Javascript will be send with the Ajax response, and evaluated after the innerHTML for this section has been replaced. ORDERED-FORM-CONTROLS [ from SKELETON-UI-ELEMENT ] list of gdl objects, which should be of type 'base-form-control . [Note -- this slot is not really necessary for protecting out-of-bounds sequence references anymore, the form-control processor protects against this by itself now]. These objects are validated and bashed first, in the order given. If the cardinality of one form-control depends on another as in the example below, then you should list those dependent objects first.
```

---

## index.html (chunk 6/9)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/9/index.html
Type: reference

```
bjects are validated and bashed first, in the order given. If the cardinality of one form-control depends on another as in the example below, then you should list those dependent objects first. Default is nil. examples: ... :computed-slots ((number-of-nozzles (the number-of-nozzles-form value)) (ordered-form-controls (append (list-elements (the inner-flange-form)) (list (the number-of-nozzles-form))))) :objects ((inner-flange-form :type 'menu-form-control :choice-plist (list :hey "hey" :now "now") :default :hey :sequence (:size (the number-of-nozzles))) (number-of-nozzles-form :type 'text-form-control :prompt "Number of Shell Nozzles Required: " :domain :number :default 0) POSSIBLE-NILS [ from SKELETON-UI-ELEMENT ] list of keyword symbols Messages corresponding to form fields which could be missing from form submission (e.g. checkbox fields). Defaults to the names of any children or hidden-children of type menu-form-control or checkbox-form-control.
```

---

## index.html (chunk 7/9)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/9/index.html
Type: reference

```
e missing from form submission (e.g. checkbox fields). Defaults to the names of any children or hidden-children of type menu-form-control or checkbox-form-control. PRESET-ALL? [ from SKELETON-UI-ELEMENT ] boolean This switch determines whether all form-controls should be preset before the final setting, in order to allow any interdependencies to be detected for validation or detecting changed values. If this is specified as a non-nil value, then any nil values of (the preset?) on individual form controls will be ignored. If this is specified as nil, then (the preset?) of individual form-controls (default of these is also nil) will be respected. Default is nil. PRIMARY? [ from SKELETON-FORM-CONTROL ] boolean Set this to t if the form-control should always occur first in an outputted snapshot file. Defaults to nil. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). ROW-LABELS list of strings One for each row.
```

---

## index.html (chunk 8/9)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/9/index.html
Type: reference

```
t file. Defaults to nil. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). ROW-LABELS list of strings One for each row. SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL.
```

---

## index.html (chunk 9/9)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/9/index.html
Type: reference

```
ts to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, defaulting) RESPONDENT [ from SKELETON-UI-ELEMENT ] gdl object Object to respond to the form submission. Defaults to self. Computed Slots FORM-CONTROLS [ from SKELETON-UI-ELEMENT ] list of gdl objects All the children or hidden-children of type base-form-control. Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/8)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/16/index.html
Type: reference

```
GendL Application - SHEET-SECTION Package Documentation Object: SHEET-SECTION (The :GWL Package) Mixins: SKELETON-UI-ELEMENT , VANILLA-MIXIN Description Basic mixin to support an object representing a section of an HTML sheet (i.e. web page). Currently this simply mixes in skeleton-ui-element, and the functionality is not extended. Sheet-section is also mixed into base-html-sheet, so it and any of its subclasses will be considered as sheet-sections if they are the child of a base-ajax-sheet. Input Slots (optional) BASHEE [ from SKELETON-UI-ELEMENT ] gdl object Object to have its settable computed-slots and/or query-plist set from the fields on the form upon submission. Defaults to self. DOM-ID [ from SKELETON-UI-ELEMENT ] string This is the auto-computed dom-id which should be used for rendering this section.
```

---

## index.html (chunk 2/8)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/16/index.html
Type: reference

```
dering this section. If you use the main-div HTML string for rendering this object as a page section, then you do not have to generate the :div tag yourself - the main-div will be a string of HTML which is wrapped in the correct :div tag already. FORCE-VALIDATION-FOR [ from SKELETON-UI-ELEMENT ] list of gdl objects of type form-control The validation-function will be forced on these objects when a form is submitted, even if the object's html form-control does not happen to be included in the values submitted with the form. Defaults to nil. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. HTML-SECTIONS [ from SKELETON-UI-ELEMENT ] List of HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk.
```

---

## index.html (chunk 3/8)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/16/index.html
Type: reference

```
in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded. HTML-SECTIONS-VALID [ from SKELETON-UI-ELEMENT ] List of valid HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded.
```

---

## index.html (chunk 4/8)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/16/index.html
Type: reference

```
INNER-HTML [ from SKELETON-UI-ELEMENT ] string This can be used with (str .) [in cl-who] or (:princ .) [in htmlGen] to output this section of the page, without the wrapping :div tag [so if you use this, your code would be responsible for wrapping the :div tag with :id (the dom-id).] JS-TO-EVAL [ from SKELETON-UI-ELEMENT ] string of valid javascript This Javascript will be send with the Ajax response, and evaluated after the innerHTML for this section has been replaced. ORDERED-FORM-CONTROLS [ from SKELETON-UI-ELEMENT ] list of gdl objects, which should be of type 'base-form-control . [Note -- this slot is not really necessary for protecting out-of-bounds sequence references anymore, the form-control processor protects against this by itself now]. These objects are validated and bashed first, in the order given. If the cardinality of one form-control depends on another as in the example below, then you should list those dependent objects first. Default is nil. examples: ...
```

---

## index.html (chunk 5/8)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/16/index.html
Type: reference

```
st, in the order given. If the cardinality of one form-control depends on another as in the example below, then you should list those dependent objects first. Default is nil. examples: ... :computed-slots ((number-of-nozzles (the number-of-nozzles-form value)) (ordered-form-controls (append (list-elements (the inner-flange-form)) (list (the number-of-nozzles-form))))) :objects ((inner-flange-form :type 'menu-form-control :choice-plist (list :hey "hey" :now "now") :default :hey :sequence (:size (the number-of-nozzles))) (number-of-nozzles-form :type 'text-form-control :prompt "Number of Shell Nozzles Required: " :domain :number :default 0) POSSIBLE-NILS [ from SKELETON-UI-ELEMENT ] list of keyword symbols Messages corresponding to form fields which could be missing from form submission (e.g. checkbox fields). Defaults to the names of any children or hidden-children of type menu-form-control or checkbox-form-control.
```

---

## index.html (chunk 6/8)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/16/index.html
Type: reference

```
. checkbox fields). Defaults to the names of any children or hidden-children of type menu-form-control or checkbox-form-control. PRESET-ALL? [ from SKELETON-UI-ELEMENT ] boolean This switch determines whether all form-controls should be preset before the final setting, in order to allow any interdependencies to be detected for validation or detecting changed values. If this is specified as a non-nil value, then any nil values of (the preset?) on individual form controls will be ignored. If this is specified as nil, then (the preset?) of individual form-controls (default of these is also nil) will be respected. Default is nil. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists.
```

---

## index.html (chunk 7/8)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/16/index.html
Type: reference

```
ces All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, defaulting) RESPONDENT [ from SKELETON-UI-ELEMENT ] gdl object Object to respond to the form submission. Defaults to self. Examples FLAG -- fill in!!! Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 8/8)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/16/index.html
Type: reference

```
o self. Examples FLAG -- fill in!!! Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/4)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/15/index.html
Type: reference

```
GendL Application - SESSION-CONTROL-MIXIN Package Documentation Object: SESSION-CONTROL-MIXIN (The :GWL Package) Mixins: VANILLA-MIXIN Author Brian Sorg, Liberating Insight LLC (revised Dave Cooper, Genworks) Description Mixin to the root object of the part which you wish to have session control over Input Slots (optional) HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. ORG-TYPE Type of original object, useful when viewing session report log RECOVERY-EXPIRES-AT expiration time of the recovery object After the recovery object has replaced the orginal instance at what time should the recovery instance expire? RECOVERY-URL Url to which a user will be redirected if requesting a session that has been cleared ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy).
```

---

## index.html (chunk 2/4)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/15/index.html
Type: reference

```
t has been cleared ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information SESSION-DURATION Length of time a session should last without activity in minutes STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. USE-RECOVERY-OBJECT? boolean Determines whether expired sessions are replaced by recovery object. Default is nil. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree.
```

---

## index.html (chunk 3/4)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/15/index.html
Type: reference

```
expired sessions are replaced by recovery object. Default is nil. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, settable) EXPIRES-AT Universal time after which the session should expire GDL Functions CLEAR-EXPIRED-SESSION This is the function called to check for and handle session control keyword arguments: debug? Boolean , Default Value: NIL Prints debug statement if needed CLEAR-NOW? boolean Test to run to see if this session has expired and needs to be cleared now. SESSION-CLEAN-UP gets called right before the instance is going to get cleared Is intended to be used to stop any instance states that may not be elequently handled by the garbage collector. ie database connections, multiprocessing locks, open streams etc.
```

---

## index.html (chunk 4/4)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/15/index.html
Type: reference

```
arbage collector. ie database connections, multiprocessing locks, open streams etc. SET-EXPIRES-AT Method which will set the expires-at slot to the current time + the session-duration Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/16)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/10/index.html
Type: reference

```
GendL Application - GWL-RULE-OBJECT Package Documentation Object: GWL-RULE-OBJECT (The :GWL Package) Mixins: BASE-HTML-GRAPHICS-SHEET , BASE-RULE-OBJECT Description Used to display a rule as a GWL web page. Mixes together base-html-sheet and base-rule-object . Input Slots (optional) AFTER-PRESENT! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the respondent of a form, to do some processing after the respondent's write-html-sheet function runs to present the object. AFTER-SET! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the requestor of a form, to do some processing after the requestor's form values are set into the specified bashee. BASHEE [ from SKELETON-UI-ELEMENT ] gdl object Object to have its settable computed-slots and/or query-plist set from the fields on the form upon submission. Defaults to self.
```

---

## index.html (chunk 2/16)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/10/index.html
Type: reference

```
ts settable computed-slots and/or query-plist set from the fields on the form upon submission. Defaults to self. BEFORE-PRESENT! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the respondent of a form, to do some processing before the respondent's write-html-sheet function runs to present the object. This can be useful especially for objects which are subclasses of higher-level mixins such as application-mixin and node-mixin , where you do not have direct access to the write-html-sheet function and typically only define the model-inputs function. It is not always reliable to do processing in the model-inputs function, since some slots which depend on your intended modifications may already have been evaluated by the time the model-inputs function runs.
```

---

## index.html (chunk 3/16)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/10/index.html
Type: reference

```
on runs. BEFORE-RESPONSE! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in a user specialization of base-html-sheet, to do some processing before the header-plist is evaluated and before the HTTP response is actually initiated. BEFORE-SET! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in the requestor of a form, to do some processing before the requestor's form values are set into the specified bashee. BOUNDING-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object. CENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located.
```

---

## index.html (chunk 4/16)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/10/index.html
Type: reference

```
the reference box of this object should be located. CHECK-SANITY? [ from BASE-HTML-SHEET ] boolean Determines whether a a sanity check is done (with the check-sanity function) before presenting the response page if this page is a respondent. Default is NIL. DOM-ID [ from SKELETON-UI-ELEMENT ] string This is the auto-computed dom-id which should be used for rendering this section. If you use the main-div HTML string for rendering this object as a page section, then you do not have to generate the :div tag yourself - the main-div will be a string of HTML which is wrapped in the correct :div tag already. FORCE-VALIDATION-FOR [ from SKELETON-UI-ELEMENT ] list of gdl objects of type form-control The validation-function will be forced on these objects when a form is submitted, even if the object's html form-control does not happen to be included in the values submitted with the form. Defaults to nil.
```

---

## index.html (chunk 5/16)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/10/index.html
Type: reference

```
's html form-control does not happen to be included in the values submitted with the form. Defaults to nil. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. HTML-SECTIONS [ from SKELETON-UI-ELEMENT ] List of HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded. HTML-SECTIONS-VALID [ from SKELETON-UI-ELEMENT ] List of valid HTML sections to be scanned and possibly replaced in response to GDL Ajax calls. Override this slot at your own risk. The default is all sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded.
```

---

## index.html (chunk 6/16)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/10/index.html
Type: reference

```
sections who are most recently laid out on the respondent sheet, and this is set programmatically every time the sheet section's main-div is demanded. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil INNER-HTML [ from SKELETON-UI-ELEMENT ] string This can be used with (str .) [in cl-who] or (:princ .) [in htmlGen] to output this section of the page, without the wrapping :div tag [so if you use this, your code would be responsible for wrapping the :div tag with :id (the dom-id).] JS-TO-EVAL [ from SKELETON-UI-ELEMENT ] string of valid javascript This Javascript will be send with the Ajax response, and evaluated after the innerHTML for this section has been replaced. LENGTH [ from GEOMETRY-VIEW-MIXIN ] number Length ("height" of screen window) of the graphics viewport. Default is 300.
```

---

## index.html (chunk 7/16)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/10/index.html
Type: reference

```
om GEOMETRY-VIEW-MIXIN ] number Length ("height" of screen window) of the graphics viewport. Default is 300. LOCAL-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. ORDERED-FORM-CONTROLS [ from SKELETON-UI-ELEMENT ] list of gdl objects, which should be of type 'base-form-control . [Note -- this slot is not really necessary for protecting out-of-bounds sequence references anymore, the form-control processor protects against this by itself now]. These objects are validated and bashed first, in the order given.
```

---

## index.html (chunk 8/16)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/10/index.html
Type: reference

```
necessary for protecting out-of-bounds sequence references anymore, the form-control processor protects against this by itself now]. These objects are validated and bashed first, in the order given. If the cardinality of one form-control depends on another as in the example below, then you should list those dependent objects first. Default is nil. examples: ... :computed-slots ((number-of-nozzles (the number-of-nozzles-form value)) (ordered-form-controls (append (list-elements (the inner-flange-form)) (list (the number-of-nozzles-form))))) :objects ((inner-flange-form :type 'menu-form-control :choice-plist (list :hey "hey" :now "now") :default :hey :sequence (:size (the number-of-nozzles))) (number-of-nozzles-form :type 'text-form-control :prompt "Number of Shell Nozzles Required: " :domain :number :default 0) ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object.
```

---

## index.html (chunk 9/16)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/10/index.html
Type: reference

```
in :number :default 0) ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). POSSIBLE-NILS [ from SKELETON-UI-ELEMENT ] list of keyword symbols Messages corresponding to form fields which could be missing from form submission (e.g. checkbox fields). Defaults to the names of any children or hidden-children of type menu-form-control or checkbox-form-control. PRESET-ALL? [ from SKELETON-UI-ELEMENT ] boolean This switch determines whether all form-controls should be preset before the final setting, in order to allow any interdependencies to be detected for validation or detecting changed values.
```

---

## index.html (chunk 10/16)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/10/index.html
Type: reference

```
hould be preset before the final setting, in order to allow any interdependencies to be detected for validation or detecting changed values. If this is specified as a non-nil value, then any nil values of (the preset?) on individual form controls will be ignored. If this is specified as nil, then (the preset?) of individual form-controls (default of these is also nil) will be respected. Default is nil. PROCESS-COOKIES! [ from BASE-HTML-SHEET ] void This is an empty function by default, but can be overridden in a user specialization of base-html-sheet, to do some processing before the header-plist is evaluated and before the HTTP response is actually initiated, but after the cookies-received have been set. RETURN-OBJECT [ from BASE-HTML-SHEET ] gdl object Default object to which control will return with the write-back-link method ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy).
```

---

## index.html (chunk 11/16)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/10/index.html
Type: reference

```
l return with the write-back-link method ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). RULE-DESCRIPTION [ from BASE-RULE-OBJECT ] string Short description of the rule (generally one line). Defaults to NIL. RULE-DESCRIPTION-HELP [ from BASE-RULE-OBJECT ] string Verbose description of the purpose of the rule. RULE-RESULT [ from BASE-RULE-OBJECT ] string The basic return-value, or result, of evaluating the rule. RULE-RESULT-HELP [ from BASE-RULE-OBJECT ] string Verbose description of how the rule result is computed. RULE-TITLE [ from BASE-RULE-OBJECT ] string Title to be used with the rule object. Defaults to NIL. SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STANDARD-VIEWS [ from BASE-HTML-GRAPHICS-SHEET ] plist of keywords and 3d vectors .
```

---

## index.html (chunk 12/16)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/10/index.html
Type: reference

```
ements of sequences as flat lists. Any children which throw errors come back as a plist with error information STANDARD-VIEWS [ from BASE-HTML-GRAPHICS-SHEET ] plist of keywords and 3d vectors . Indicates the views to show in the graphics controls. STRINGS-FOR-DISPLAY [ from BASE-RULE-OBJECT ] string Determines the rule's default name in various internal GDL contexts. Defaults to the rule-title , or "Unnamed Rule" if rule-title is NIL. SUPPRESS-DISPLAY? [ from BASE-RULE-OBJECT ] boolean Determines whether the rule is displayed by default in reports etc. TARGET [ from BASE-HTML-SHEET ] string Name of a browser frame or window to display this page. Default of NIL indicates to use the same window. TRANSITORY-SLOTS [ from BASE-HTML-SHEET ] list of keyword symbols Messages corresponding to form fields which should not be retained against Updates to the model (e.g. calls to the update! function or hitting the Update button or link in the browser in development mode).
```

---

## index.html (chunk 13/16)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/10/index.html
Type: reference

```
fields which should not be retained against Updates to the model (e.g. calls to the update! function or hitting the Update button or link in the browser in development mode). Defaults to NIL (the empty list). USE-BSPLINES? [ from BASE-HTML-GRAPHICS-SHEET ] boolean Determines whether to use native bspline data in the vrml VIEW-OBJECT [ from GEOMETRY-VIEW-MIXIN ] gdl web-drawing object This must be overridden in the specialized class. VIOLATED? [ from BASE-RULE-OBJECT ] boolean Indicates whether this rule violates a standard condition. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. WIDTH [ from GEOMETRY-VIEW-MIXIN ] number Width of the graphics viewport. Default is 300. Input Slots (optional, settable) DIGITATION-MODE [ from BASE-HTML-GRAPHICS-SHEET ] keyword symbol, one of :zoom-and-center , :report-point , or :measure-distance .
```

---

## index.html (chunk 14/16)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/10/index.html
Type: reference

```
is 300. Input Slots (optional, settable) DIGITATION-MODE [ from BASE-HTML-GRAPHICS-SHEET ] keyword symbol, one of :zoom-and-center , :report-point , or :measure-distance . If :zoom-and-center , sets the user-center and user-scale accordingly when graphics area is clicked. If :report-point , the slot digitized-point is set with the x y value. If measure-distance , the slot :digitized-distance is set with the resultant distance. Default is :zoom-and-center IMAGE-FORMAT [ from BASE-HTML-GRAPHICS-SHEET ] keyword symbol Determines the default image format. Defaults to :png VIEW [ from BASE-HTML-GRAPHICS-SHEET ] keyword symbol Determines the default view from the standard-views . Defaults to :trimetric. ZOOM-FACTOR [ from BASE-HTML-GRAPHICS-SHEET ] number The factor used for zooming in or out. ZOOM-MODE [ from BASE-HTML-GRAPHICS-SHEET ] keyword symbol, one of :in, :out, or :none, or nil If :in, then clicks in the graphics area will increase the zoom factor by (the zoom-factor).
```

---

## index.html (chunk 15/16)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/10/index.html
Type: reference

```
OOM-MODE [ from BASE-HTML-GRAPHICS-SHEET ] keyword symbol, one of :in, :out, or :none, or nil If :in, then clicks in the graphics area will increase the zoom factor by (the zoom-factor). If :out, then clicks will decrease the factor by that amount. If :none or nil, then clicks will have no effect. Input Slots (optional, defaulting) DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line.
```

---

## index.html (chunk 16/16)
Source: yadd-reference/package-dokumentations/8/object-docs/dokumentation/10/index.html
Type: reference

```
ern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. RESPONDENT [ from SKELETON-UI-ELEMENT ] gdl object Object to respond to the form submission. Defaults to self. Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

