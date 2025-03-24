# Gendl Documentation - package_5_objects

## index.html (chunk 1/5)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/39/index.html
Type: reference

```
GendL Application - TEXT-LINE Package Documentation Object: TEXT-LINE (The :GEOM-BASE Package) Mixins: BASE-OBJECT Description Outputs a single line of text for graphical display. Input Slots (optional) BOUNDING-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object. CENTER 3d-point Center of the text. Specify this or start, not both. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil LOCAL-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object.
```

---

## index.html (chunk 2/5)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/39/index.html
Type: reference

```
o nil LOCAL-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information START 3d-point Start of the text. Specify this or center, not both.
```

---

## index.html (chunk 3/5)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/39/index.html
Type: reference

```
at lists. Any children which throw errors come back as a plist with error information START 3d-point Start of the text. Specify this or center, not both. STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. WIDTH [ from BASE-OBJECT ] number X-axis dimension of the reference box. Defaults to zero. Input Slots (optional, defaulting) DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object.
```

---

## index.html (chunk 4/5)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/39/index.html
Type: reference

```
BJECT ] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e.
```

---

## index.html (chunk 5/5)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/39/index.html
Type: reference

```
m BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). Computed Slots LENGTH [ from BASE-OBJECT ] number Y-axis dimension of the reference box. Defaults to zero. Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/22/index.html
Type: reference

```
GendL Application - GLOBAL-POLYGON-PROJECTION Package Documentation Object: GLOBAL-POLYGON-PROJECTION (The :GEOM-BASE Package) Mixins: IFS-OUTPUT-MIXIN, BASE-OBJECT Description A polygon ``extruded'' for a given distance along a single vector. For planar polygons, the projection vector must not be orthogonal to the normal of the plane of the polygon. The vertices and projection-vector are given in the global coordinate system, so the local center and orientation do not affect the positioning or orientation of this part. Input Slots (required) PROJECTION-DEPTH number The resultant distance from the two end faces of the extrusion. VERTEX-LIST list of 3d points The vertex list making up the polyline, same as the input for global-polyline. Input Slots (optional) HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil.
```

---

## index.html (chunk 2/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/22/index.html
Type: reference

```
boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil LOCAL-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . OFFSET keyword symbol The direction of extrusion with respect to the vertices in vertex-list and the projection-vector: :up Indicates to start from current location of vertices and move in the direction of the projection-vector. :down Indicates to start from current location of vertices and move in the direction opposite the projection-vector.
```

---

## index.html (chunk 3/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/22/index.html
Type: reference

```
e projection-vector. :down Indicates to start from current location of vertices and move in the direction opposite the projection-vector. :center Indicates to start from current location of vertices and move in the direction of the projection-vector and opposite the projection-vector, going half the projection-depth in each direction. ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. PROJECTION-VECTOR 3d vector Indicates the straight path along which the extrusion should occur. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists.
```

---

## index.html (chunk 4/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/22/index.html
Type: reference

```
VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, defaulting) CENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located.
```

---

## index.html (chunk 5/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/22/index.html
Type: reference

```
where the center of the reference box of this object should be located. DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. LENGTH [ from BASE-OBJECT ] number Y-axis dimension of the reference box. Defaults to zero.
```

---

## index.html (chunk 6/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/22/index.html
Type: reference

```
[ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. LENGTH [ from BASE-OBJECT ] number Y-axis dimension of the reference box. Defaults to zero. ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). WIDTH [ from BASE-OBJECT ] number X-axis dimension of the reference box. Defaults to zero. Computed Slots BOUNDING-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object.
```

---

## index.html (chunk 7/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/22/index.html
Type: reference

```
dinates, of the rectangular volume bounding the tree of geometric objects rooted at this object. Examples (in-package :gdl-user) (define-object global-polygon-projection-sample (global-polygon-projection) :computed-slots ((display-controls (list :color :gold-old :transparency 0.3)) (projection-depth 5) (vertex-list (list (make-point 0 0 0) (make-point 10 10 0) (make-point 30 10 0) (make-point 40 0 0) (make-point 30 -10 0) (make-point 10 -10 0) (make-point 0 0 0))))) (generate-sample-drawing :objects (make-object 'global-polygon-projection-sample) :projection-direction (getf *standard-views* :trimetric)) Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/8)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/7/index.html
Type: reference

```
GendL Application - BEZIER-CURVE Package Documentation Object: BEZIER-CURVE (The :GEOM-BASE Package) Mixins: BASE-OBJECT Description GDL currently supports third-degree Bezier curves, which are defined using four 3D control-points . The Bezier curve always passes through the first and last control points and lies within the convex hull of the control points. At the start point (i.e. the first control point), the curve is tangent to the vector pointing from the start point to the second control point. At the end point (i.e. the last control point), the curve is tangent to the vector pointing from the end point to the third control point. Input Slots (required) CONTROL-POINTS list of 4 3d points Specifies the control points for the Bezier curve. Input Slots (optional) HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil.
```

---

## index.html (chunk 2/8)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/7/index.html
Type: reference

```
MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil LOCAL-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy).
```

---

## index.html (chunk 3/8)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/7/index.html
Type: reference

```
nality, e.g. SVG/Raphael and X3DOM. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL.
```

---

## index.html (chunk 4/8)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/7/index.html
Type: reference

```
lly this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, defaulting) CENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located. DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern.
```

---

## index.html (chunk 5/8)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/7/index.html
Type: reference

```
ength, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. LENGTH [ from BASE-OBJECT ] number Y-axis dimension of the reference box. Defaults to zero. ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). WIDTH [ from BASE-OBJECT ] number X-axis dimension of the reference box. Defaults to zero.
```

---

## index.html (chunk 6/8)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/7/index.html
Type: reference

```
eference box. Defaults to zero. Computed Slots BOUNDING-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object. GDL Functions CIRCLE-INTERSECTION-2D list of 3d points Returns points of intersection in the Z plane between this Bezier curve and the circle in the Z plane with center center and radius radius . arguments: center 3D Point The center of the circle to be intersected radius Number The radius of the circle to be intersected keyword arguments: accuracy Number , Default Value: (* 10.0 DOUBLE-FLOAT-EPSILON) Target accuracy maximum-number-of-iterations Integer , Default Value: 30 Maximum iterations of polynomial solver LINE-INTERSECTION-2D list of 3d points Returns points of intersection in the Z plane between this Bezier curve and the infinite line containing point point and direction vector .
```

---

## index.html (chunk 7/8)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/7/index.html
Type: reference

```
2D list of 3d points Returns points of intersection in the Z plane between this Bezier curve and the infinite line containing point point and direction vector . Use the between? function if you wish to establish whether the point is contained in a particular line segment. arguments: point 3D Point Any point in the line to be intersected vector 3D Vector The direction of the line to be intersected keyword arguments: accuracy Number , Default Value: (* 10.0 DOUBLE-FLOAT-EPSILON) Target accuracy maximum-number-of-iterations Integer , Default Value: 30 Maximum iterations of polynomial solver POINT 3d point Returns the point on this Bezier curve corresponding to the given parameter , which should be between 0 and 1.
```

---

## index.html (chunk 8/8)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/7/index.html
Type: reference

```
arguments: parameter Number Curve parameter, between zero and one (0 and 1) Examples (in-package :gdl-user) (define-object bezier-sample (bezier-curve) :computed-slots ((control-points (list (make-point 0 0 0) (make-point 1 1 0) (make-point 2 1 0) (make-point 3 0 0)))) :objects ((points-display :type 'points-display :points (the control-points)))) (generate-sample-drawing :objects (let ((self (make-object 'bezier-sample))) (list self (the points-display)))) Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/37/index.html
Type: reference

```
GendL Application - SPHERE Package Documentation Object: SPHERE (The :GEOM-BASE Package) Mixins: IFS-OUTPUT-MIXIN, ARCOID-MIXIN , BASE-OBJECT Description The set of points equidistant from a given center point. Input Slots (required) RADIUS number Distance from center to any point on the sphere. Input Slots (optional) BOUNDING-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object. END-ANGLE [ from ARCOID-MIXIN ] angle in radians End angle of the arc. Defaults to twice pi. END-HORIZONTAL-ARC angle in radians Ending horizontal angle for a partial sphere. Default is twice pi. END-VERTICAL-ARC angle in radians Ending vertical angle for a partial sphere. Default is pi/2. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil.
```

---

## index.html (chunk 2/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/37/index.html
Type: reference

```
al sphere. Default is pi/2. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil INNER-RADIUS number Radius of inner hollow for a hollow sphere. Default is NIL, for a non-hollow sphere. LOCAL-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object. NUMBER-OF-HORIZONTAL-SECTIONS number How many lines of latitude to show on the sphere in some renderings. Default value is 4. NUMBER-OF-VERTICAL-SECTIONS number How many lines of longitude to show on the sphere in some renderings. Default value is 4.
```

---

## index.html (chunk 3/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/37/index.html
Type: reference

```
any lines of longitude to show on the sphere in some renderings. Default value is 4. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information START-ANGLE [ from ARCOID-MIXIN ] angle in radians Start angle of the arc. Defaults to zero. START-HORIZONTAL-ARC angle in radians Starting horizontal angle for a partial sphere. Default is 0.
```

---

## index.html (chunk 4/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/37/index.html
Type: reference

```
ARCOID-MIXIN ] angle in radians Start angle of the arc. Defaults to zero. START-HORIZONTAL-ARC angle in radians Starting horizontal angle for a partial sphere. Default is 0. START-VERTICAL-ARC angle in radians Starting vertical angle for a partial sphere. Default is -pi/2. STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, defaulting) CENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located.
```

---

## index.html (chunk 5/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/37/index.html
Type: reference

```
ENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located. DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern.
```

---

## index.html (chunk 6/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/37/index.html
Type: reference

```
es how far into the line or curve to start the dash pattern. ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). Computed Slots HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. LENGTH [ from BASE-OBJECT ] number Y-axis dimension of the reference box. Defaults to zero. WIDTH [ from BASE-OBJECT ] number X-axis dimension of the reference box. Defaults to zero.
```

---

## index.html (chunk 7/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/37/index.html
Type: reference

```
Examples (in-package :gdl-user) (define-object sphere-sample (sphere) :computed-slots ((radius 150) (number-of-vertical-sections 10) (number-of-horizontal-sections 10) (display-controls (list :color :green-forest-medium)))) (generate-sample-drawing :objects (make-object 'sphere-sample) :projection-direction :trimetric) Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/13)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/25/index.html
Type: reference

```
GendL Application - HORIZONTAL-DIMENSION Package Documentation Object: HORIZONTAL-DIMENSION (The :GEOM-BASE Package) Mixins: LINEAR-DIMENSION , VANILLA-MIXIN Description Creates a dimension annotation along the horizontal axis. Input Slots (required) END-POINT [ from LINEAR-DIMENSION ] 3d point Actual point where the dimension will stop measuring START-POINT [ from LINEAR-DIMENSION ] 3d point Actual point where the dimension will start measuring Input Slots (optional) ARROWHEAD-LENGTH [ from LINEAR-DIMENSION ] length (from tip to tail) of arrowhead glyph Defaults to twice the arrowhead-width ARROWHEAD-STYLE [ from LINEAR-DIMENSION ] keyword symbol Style for arrowhead on end of leader-line . Currently supported values are :none , :wedge (the Default), and :double-wedge . ARROWHEAD-STYLE-2 [ from LINEAR-DIMENSION ] keyword symbol Style for arrowhead on end of leader-line . Currently supported values are :none (the Default), :wedge , and :double-wedge .
```

---

## index.html (chunk 2/13)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/25/index.html
Type: reference

```
from LINEAR-DIMENSION ] keyword symbol Style for arrowhead on end of leader-line . Currently supported values are :none (the Default), :wedge , and :double-wedge . ARROWHEAD-WIDTH [ from LINEAR-DIMENSION ] width of arrowhead glyph Defaults to half the character-size. BASE-PLANE-NORMAL [ from LINEAR-DIMENSION ] Must be specified in the subclass except for angular BOUNDING-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object. CHARACTER-SIZE [ from LINEAR-DIMENSION ] number Size (glyph height) of the label text, in model units. Defaults to 1. DIM-TEXT [ from LINEAR-DIMENSION ] string Determines the text which shows up as the dimension label. Defaults to the dim-value, which is computed specially in each specific dimension type.
```

---

## index.html (chunk 3/13)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/25/index.html
Type: reference

```
. Defaults to the dim-value, which is computed specially in each specific dimension type. DIM-TEXT-BIAS [ from LINEAR-DIMENSION ] keyword symbol, :start, :end, or :center Indicates where to position the text in the case when outside-leaders? is non-nil. Defaults to :center DIM-TEXT-START 3d point Determines where the text will start. Defaults to reasonable location for horizontal-dimension. DIM-TEXT-START-OFFSET [ from LINEAR-DIMENSION ] 3d vector (normally only 2d are used) . The dim-text-start is offset by this vector, in model space. Defaults to #(0.0 0.0 0.0) DIM-VALUE [ from LINEAR-DIMENSION ] number 2D distance relative to the base-plane-normal. Can be over-ridden in the subclass DXF-FONT [ from LINEAR-DIMENSION ] string This names the DXF font for this general-note. Defaults to (the font) . DXF-OFFSET [ from LINEAR-DIMENSION ] number The start of text will be offset by this amount for DXF output. Default is 2.
```

---

## index.html (chunk 4/13)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/25/index.html
Type: reference

```
font) . DXF-OFFSET [ from LINEAR-DIMENSION ] number The start of text will be offset by this amount for DXF output. Default is 2. DXF-SIZE-RATIO [ from LINEAR-DIMENSION ] number The scale factor for DXF character size vs PDF character size. Default is 0.8 DXF-TEXT-X-SCALE [ from LINEAR-DIMENSION ] number in percentage Adjusts the character width for DXF output. Defaults to the text-x-scale. FLIP-LEADERS? [ from LINEAR-DIMENSION ] boolean Indicates which direction the witness lines should take from the start and end points. The Default is NIL, which indicates :rear (i.e. ``up'') for horizontal-dimensions and :right for vertical-dimensions FONT [ from LINEAR-DIMENSION ] string naming a standard pdf font Font for the label text. Defaults to "Helvetica" FULL-LEADER-LINE-LENGTH [ from LINEAR-DIMENSION ] number Indicates the length of the full leader when outside-leaders? is nil.
```

---

## index.html (chunk 5/13)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/25/index.html
Type: reference

```
IMENSION ] number Indicates the length of the full leader when outside-leaders? is nil. This defaults to nil, which indicates that the full-leader's length should be auto-computed based on the given start-point and end-point. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil JUSTIFICATION [ from LINEAR-DIMENSION ] keyword symbol, :left, :right, or :center . For multi-line dim-text, this justification is applied. LEADER-1? [ from LINEAR-DIMENSION ] boolean Indicates whether the first (or only) leader line should be displayed. The Default is T LEADER-2? [ from LINEAR-DIMENSION ] boolean Indicates whether the second leader line should be displayed.
```

---

## index.html (chunk 6/13)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/25/index.html
Type: reference

```
e Default is T LEADER-2? [ from LINEAR-DIMENSION ] boolean Indicates whether the second leader line should be displayed. The Default is T LEADER-LINE-LENGTH [ from LINEAR-DIMENSION ] number Indicates the length of the first leader for the case when outside-leaders? is non-NIL LEADER-LINE-LENGTH-2 [ from LINEAR-DIMENSION ] number Indicates the length of the second leader for the case when outside-leaders? is non-NIL LEADER-TEXT-GAP [ from LINEAR-DIMENSION ] number Amount of gap between leader lines and dimension text, when the dimension text is within the leader. Defaults to half the character-size. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM.
```

---

## index.html (chunk 7/13)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/25/index.html
Type: reference

```
or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). OUTLINE-SHAPE-TYPE [ from LINEAR-DIMENSION ] keyword symbol Currently can be :bubble, :rectangle, or :none. Default is :none. OUTSIDE-LEADERS-LENGTH-FACTOR [ from LINEAR-DIMENSION ] number Indicates the default length of the outside-leaders as a multiple of arrowhead-length. Defaults to 3.
```

---

## index.html (chunk 8/13)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/25/index.html
Type: reference

```
as a multiple of arrowhead-length. Defaults to 3. OUTSIDE-LEADERS? [ from LINEAR-DIMENSION ] boolean Indicates whether the leader line(s) should be inside or outside the interval between the start and end points. The default is NIL, which indicates that the leader line(s) should be inside the interval ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence.
```

---

## index.html (chunk 9/13)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/25/index.html
Type: reference

```
aults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. TEXT-ABOVE-LEADER? [ from LINEAR-DIMENSION ] boolean Indicates whether the text is to the right or above the leader line, rather than in-line with it. Default is T. TEXT-ALONG-AXIS? [ from LINEAR-DIMENSION ] boolean Where applicable, determines whether text direction follows leader-line direction TEXT-X-SCALE [ from LINEAR-DIMENSION ] number in percentage Adjusts the character width for the dimension-text and currently only applies only to PDF output UNDERLINE? [ from LINEAR-DIMENSION ] GDL VIEW-REFERENCE-OBJECT [ from LINEAR-DIMENSION ] gdl object or nil View object which will use this dimension. Defaults to NIL. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL.
```

---

## index.html (chunk 10/13)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/25/index.html
Type: reference

```
VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. WITNESS-LINE-2? [ from LINEAR-DIMENSION ] boolean Indicates whether to display a witness line coming off the end-point . Default is T WITNESS-LINE-EXT [ from LINEAR-DIMENSION ] number Distance the witness line(s) extend beyond the leader line. Default is 0.3 WITNESS-LINE-GAP [ from LINEAR-DIMENSION ] number Distance from the start-point and end-point to the start of each witness-line. Default is 0.1 WITNESS-LINE-LENGTH [ from LINEAR-DIMENSION ] number Length of the witness lines (or of the shorter witness line in case they are different lengths) WITNESS-LINE? [ from LINEAR-DIMENSION ] boolean Indicates whether to display a witness line coming off the start-point .
```

---

## index.html (chunk 11/13)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/25/index.html
Type: reference

```
ming off the start-point . Default is T Input Slots (optional, defaulting) CENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located. DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern.
```

---

## index.html (chunk 12/13)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/25/index.html
Type: reference

```
dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. LENGTH [ from BASE-OBJECT ] number Y-axis dimension of the reference box. Defaults to zero. WIDTH [ from BASE-OBJECT ] number X-axis dimension of the reference box. Defaults to zero. Computed Slots LEADER-DIRECTION-1-VECTOR [ from LINEAR-DIMENSION ] Must be specified in the subclass except for angular LEADER-DIRECTION-2-VECTOR [ from LINEAR-DIMENSION ] Must be specified in the subclass except for angular WITNESS-DIRECTION-VECTOR [ from LINEAR-DIMENSION ] Must be specified in the subclass except for angular Examples (in-package :gdl-user) (define-object box-view (base-object) :objects ((box :type 'box :length 10 :width (* (the-child length) +phi+) :height (* (the-child :width) +phi+)) (width-dimension :type 'horizontal-dimension :character-size (/ (the box
```

---

## index.html (chunk 13/13)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/25/index.html
Type: reference

```
se-object) :objects ((box :type 'box :length 10 :width (* (the-child length) +phi+) :height (* (the-child :width) +phi+)) (width-dimension :type 'horizontal-dimension :character-size (/ (the box length) 20) :arrowhead-width (/ (the-child character-size) 3) :start-point (the box (vertex :top :left :rear)) :end-point (the box (vertex :top :right :rear))))) (generate-sample-drawing :object-roots (make-object 'box-view)) Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/20/index.html
Type: reference

```
GendL Application - GLOBAL-FILLETED-POLYLINE Package Documentation Object: GLOBAL-FILLETED-POLYLINE (The :GEOM-BASE Package) Mixins: GLOBAL-FILLETED-POLYLINE-MIXIN , VANILLA-MIXIN Description A sequence of points connected by straight line segments, whose corners are filleted according to specified radii. Please see global-filleted-polyline-mixin for documentation on the messages. Input Slots (required) VERTEX-LIST [ from GLOBAL-POLYLINE-MIXIN ] list of 3d points The vertices (``corners'') of the polyline. Input Slots (optional) CLOSED? [ from GLOBAL-POLYLINE-MIXIN ] boolean Controls whether the filleted-polyline should automatically be closed. DEFAULT-RADIUS [ from GLOBAL-FILLETED-POLYLINE-MIXIN ] number Specifies a radius to use for all vertices. Radius-list will take precedence over this. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil.
```

---

## index.html (chunk 2/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/20/index.html
Type: reference

```
s. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil LOCAL-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM.
```

---

## index.html (chunk 3/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/20/index.html
Type: reference

```
which support this functionality, e.g. SVG/Raphael and X3DOM. RADIUS-LIST [ from GLOBAL-FILLETED-POLYLINE-MIXIN ] list of numbers Specifies the radius for each vertex (``corner'') of the filleted-polyline. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree.
```

---

## index.html (chunk 4/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/20/index.html
Type: reference

```
an index number if the part is an element of a sequence. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, defaulting) CENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located. DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only).
```

---

## index.html (chunk 5/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/20/index.html
Type: reference

```
tive line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. LENGTH [ from BASE-OBJECT ] number Y-axis dimension of the reference box. Defaults to zero. ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). WIDTH [ from BASE-OBJECT ] number X-axis dimension of the reference box.
```

---

## index.html (chunk 6/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/20/index.html
Type: reference

```
ment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). WIDTH [ from BASE-OBJECT ] number X-axis dimension of the reference box. Defaults to zero. Examples (in-package :gdl-user) (define-object global-filleted-polyline-sample (global-filleted-polyline) :computed-slots ((default-radius 5) (vertex-list (list (make-point 0 0 0) (make-point 10 10 0) (make-point 30 10 0) (make-point 40 0 0) (make-point 30 -10 0) (make-point 10 -10 0) (make-point 0 0 0))))) (generate-sample-drawing :objects (make-object 'global-filleted-polyline-sample)) Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/4)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/34/index.html
Type: reference

```
GendL Application - RENDERER-MIXIN Package Documentation Object: RENDERER-MIXIN (The :GEOM-BASE Package) Mixins: VANILLA-MIXIN Description Object mixed into the base-view to compute required values to provide a rendered perspective view, as in VRML. Input Slots (required) OBJECT-ROOTS list of gdl objects Roots of the leaf objects to be displayed in this renderer view. OBJECTS list of gdl objects Leaves of the objects to be displayed in this renderer view. Input Slots (optional) 3D-BOX list of two 3d points The left-front-lower and right-rear-upper corners of the axis-aligned bounding box of the object-roots and objects . 3D-BOX-CENTER 3d point The effective view center for the scene contained in this view object. Defaults to the center of the bounding sphere of all the objects in the scene, consisting of the object-roots and the objects .
```

---

## index.html (chunk 2/4)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/34/index.html
Type: reference

```
, consisting of the object-roots and the objects . BOUNDING-SPHERE plist containing keys: :center and :radius This plist represents the tightest-fitting sphere around all the objects listed in the object-roots and the objects FIELD-OF-VIEW-DEFAULT number in angular degrees The maximum angle of the view frustrum for perspective views. Defaults to 0.1 (which results in a near parallel projection with virtually no perspective effect). HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists.
```

---

## index.html (chunk 3/4)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/34/index.html
Type: reference

```
ion, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VIEW-VECTORS plist Keys indicate view vector names (e.g. :trimetric ), and values contain the 3D vectors. Defaults to the parameter *standard-views* , but with the key corresponding to current (the view) ordered first in the plist. This list of view-vectors is used to construct the default viewpoints . VIEWPOINTS list of plists Each plist contains, based on each entry in the view-vectors , keys: :point (camera location, defaults to the 3d-box-center translated along the corresponding element of view-vectors ) by the local camera distance.
```

---

## index.html (chunk 4/4)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/34/index.html
Type: reference

```
each entry in the view-vectors , keys: :point (camera location, defaults to the 3d-box-center translated along the corresponding element of view-vectors ) by the local camera distance. The camera distance is computed based on the field-of-view angle and the bounding-sphere :orientation (3d matrix indicating camera orientation) field-of-view Angle in degrees of the view frustrum (i.e. lens angle of the virtual camera). VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/5)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/14/index.html
Type: reference

```
GendL Application - CONSTRAINED-FILLET Package Documentation Object: CONSTRAINED-FILLET (The :GEOM-BASE Package) Mixins: CONSTRAINED-ARC , VANILLA-MIXIN Description This object is the same as constrained-arc, but it is only meaningful for arc-constraints which contain two :tangent-to clauses, and it automatically trims the result to each point of tangency Input Slots (optional) BOUNDING-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object.
```

---

## index.html (chunk 2/5)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/14/index.html
Type: reference

```
ng Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil LOCAL-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists.
```

---

## index.html (chunk 3/5)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/14/index.html
Type: reference

```
N* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, defaulting) DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object.
```

---

## index.html (chunk 4/5)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/14/index.html
Type: reference

```
cating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. Computed Slots END-ANGLE [ from ARC ] angle in radians End angle of the arc. Defaults to twice pi. START-ANGLE [ from ARC ] angle in radians Start angle of the arc. Defaults to zero. Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 5/5)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/14/index.html
Type: reference

```
arc. Defaults to zero. Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/13)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/42/index.html
Type: reference

```
GendL Application - VERTICAL-DIMENSION Package Documentation Object: VERTICAL-DIMENSION (The :GEOM-BASE Package) Mixins: LINEAR-DIMENSION Description Creates a dimension annotation along the vertical axis. Input Slots (required) END-POINT [ from LINEAR-DIMENSION ] 3d point Actual point where the dimension will stop measuring START-POINT [ from LINEAR-DIMENSION ] 3d point Actual point where the dimension will start measuring Input Slots (optional) ARROWHEAD-LENGTH [ from LINEAR-DIMENSION ] length (from tip to tail) of arrowhead glyph Defaults to twice the arrowhead-width ARROWHEAD-STYLE [ from LINEAR-DIMENSION ] keyword symbol Style for arrowhead on end of leader-line . Currently supported values are :none , :wedge (the Default), and :double-wedge . ARROWHEAD-STYLE-2 [ from LINEAR-DIMENSION ] keyword symbol Style for arrowhead on end of leader-line . Currently supported values are :none (the Default), :wedge , and :double-wedge .
```

---

## index.html (chunk 2/13)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/42/index.html
Type: reference

```
] keyword symbol Style for arrowhead on end of leader-line . Currently supported values are :none (the Default), :wedge , and :double-wedge . ARROWHEAD-WIDTH [ from LINEAR-DIMENSION ] width of arrowhead glyph Defaults to half the character-size. BOUNDING-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object. CHARACTER-SIZE [ from LINEAR-DIMENSION ] number Size (glyph height) of the label text, in model units. Defaults to 1. DIM-TEXT [ from LINEAR-DIMENSION ] string Determines the text which shows up as the dimension label. Defaults to the dim-value, which is computed specially in each specific dimension type. DIM-TEXT-BIAS [ from LINEAR-DIMENSION ] keyword symbol, :start, :end, or :center Indicates where to position the text in the case when outside-leaders? is non-nil.
```

---

## index.html (chunk 3/13)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/42/index.html
Type: reference

```
IMENSION ] keyword symbol, :start, :end, or :center Indicates where to position the text in the case when outside-leaders? is non-nil. Defaults to :center DIM-TEXT-START 3d point Determines where the text will start. Defaults to reasonable location for horizontal-dimension. DIM-TEXT-START-OFFSET [ from LINEAR-DIMENSION ] 3d vector (normally only 2d are used) . The dim-text-start is offset by this vector, in model space. Defaults to #(0.0 0.0 0.0) DIM-VALUE [ from LINEAR-DIMENSION ] number 2D distance relative to the base-plane-normal. Can be over-ridden in the subclass DXF-FONT [ from LINEAR-DIMENSION ] string This names the DXF font for this general-note. Defaults to (the font) . DXF-OFFSET [ from LINEAR-DIMENSION ] number The start of text will be offset by this amount for DXF output. Default is 2. DXF-SIZE-RATIO [ from LINEAR-DIMENSION ] number The scale factor for DXF character size vs PDF character size.
```

---

## index.html (chunk 4/13)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/42/index.html
Type: reference

```
fault is 2. DXF-SIZE-RATIO [ from LINEAR-DIMENSION ] number The scale factor for DXF character size vs PDF character size. Default is 0.8 DXF-TEXT-X-SCALE [ from LINEAR-DIMENSION ] number in percentage Adjusts the character width for DXF output. Defaults to the text-x-scale. FLIP-LEADERS? [ from LINEAR-DIMENSION ] boolean Indicates which direction the witness lines should take from the start and end points. The Default is NIL, which indicates :rear (i.e. ``up'') for horizontal-dimensions and :right for vertical-dimensions FONT [ from LINEAR-DIMENSION ] string naming a standard pdf font Font for the label text. Defaults to "Helvetica" FULL-LEADER-LINE-LENGTH [ from LINEAR-DIMENSION ] number Indicates the length of the full leader when outside-leaders? is nil. This defaults to nil, which indicates that the full-leader's length should be auto-computed based on the given start-point and end-point.
```

---

## index.html (chunk 5/13)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/42/index.html
Type: reference

```
icates that the full-leader's length should be auto-computed based on the given start-point and end-point. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil JUSTIFICATION [ from LINEAR-DIMENSION ] keyword symbol, :left, :right, or :center . For multi-line dim-text, this justification is applied. LEADER-1? [ from LINEAR-DIMENSION ] boolean Indicates whether the first (or only) leader line should be displayed. The Default is T LEADER-2? [ from LINEAR-DIMENSION ] boolean Indicates whether the second leader line should be displayed.
```

---

## index.html (chunk 6/13)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/42/index.html
Type: reference

```
. The Default is T LEADER-LINE-LENGTH [ from LINEAR-DIMENSION ] number Indicates the length of the first leader for the case when outside-leaders? is non-NIL LEADER-LINE-LENGTH-2 [ from LINEAR-DIMENSION ] number Indicates the length of the second leader for the case when outside-leaders? is non-NIL LEADER-TEXT-GAP [ from LINEAR-DIMENSION ] number Amount of gap between leader lines and dimension text, when the dimension text is within the leader. Defaults to half the character-size. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM.
```

---

## index.html (chunk 7/13)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/42/index.html
Type: reference

```
s functionality, e.g. SVG/Raphael and X3DOM. ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). OUTLINE-SHAPE-TYPE [ from LINEAR-DIMENSION ] keyword symbol Currently can be :bubble, :rectangle, or :none. Default is :none. OUTSIDE-LEADERS-LENGTH-FACTOR [ from LINEAR-DIMENSION ] number Indicates the default length of the outside-leaders as a multiple of arrowhead-length. Defaults to 3. OUTSIDE-LEADERS? [ from LINEAR-DIMENSION ] boolean Indicates whether the leader line(s) should be inside or outside the interval between the start and end points.
```

---

## index.html (chunk 8/13)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/42/index.html
Type: reference

```
the leader line(s) should be inside or outside the interval between the start and end points. The default is NIL, which indicates that the leader line(s) should be inside the interval ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. TEXT-ABOVE-LEADER? [ from LINEAR-DIMENSION ] boolean Indicates whether the text is to the right or above the leader line, rather than in-line with it.
```

---

## index.html (chunk 9/13)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/42/index.html
Type: reference

```
e part is an element of a sequence. TEXT-ABOVE-LEADER? [ from LINEAR-DIMENSION ] boolean Indicates whether the text is to the right or above the leader line, rather than in-line with it. Default is T. TEXT-ALONG-AXIS? [ from LINEAR-DIMENSION ] boolean Where applicable, determines whether text direction follows leader-line direction TEXT-X-SCALE [ from LINEAR-DIMENSION ] number in percentage Adjusts the character width for the dimension-text and currently only applies only to PDF output UNDERLINE? [ from LINEAR-DIMENSION ] GDL VIEW-REFERENCE-OBJECT [ from LINEAR-DIMENSION ] gdl object or nil View object which will use this dimension. Defaults to NIL. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. WITNESS-LINE-2? [ from LINEAR-DIMENSION ] boolean Indicates whether to display a witness line coming off the end-point .
```

---

## index.html (chunk 10/13)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/42/index.html
Type: reference

```
hidden-children. Defaults to NIL. WITNESS-LINE-2? [ from LINEAR-DIMENSION ] boolean Indicates whether to display a witness line coming off the end-point . Default is T WITNESS-LINE-EXT [ from LINEAR-DIMENSION ] number Distance the witness line(s) extend beyond the leader line. Default is 0.3 WITNESS-LINE-GAP [ from LINEAR-DIMENSION ] number Distance from the start-point and end-point to the start of each witness-line. Default is 0.1 WITNESS-LINE-LENGTH [ from LINEAR-DIMENSION ] number Length of the witness lines (or of the shorter witness line in case they are different lengths) WITNESS-LINE? [ from LINEAR-DIMENSION ] boolean Indicates whether to display a witness line coming off the start-point . Default is T Input Slots (optional, defaulting) CENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located.
```

---

## index.html (chunk 11/13)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/42/index.html
Type: reference

```
es in global coordinates where the center of the reference box of this object should be located. DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. LENGTH [ from BASE-OBJECT ] number Y-axis dimension of the reference box.
```

---

## index.html (chunk 12/13)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/42/index.html
Type: reference

```
the dash pattern. HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. LENGTH [ from BASE-OBJECT ] number Y-axis dimension of the reference box. Defaults to zero. WIDTH [ from BASE-OBJECT ] number X-axis dimension of the reference box. Defaults to zero. Computed Slots BASE-PLANE-NORMAL [ from LINEAR-DIMENSION ] Must be specified in the subclass except for angular LEADER-DIRECTION-1-VECTOR [ from LINEAR-DIMENSION ] Must be specified in the subclass except for angular LEADER-DIRECTION-2-VECTOR [ from LINEAR-DIMENSION ] Must be specified in the subclass except for angular WITNESS-DIRECTION-VECTOR [ from LINEAR-DIMENSION ] Must be specified in the subclass except for angular Examples (in-package :gdl-user) (define-object vertical-dimension-sample (base-object) :objects ((box :type 'box :length 10 :width (* (the-child length) +phi+) :height (* (the-child :width) +phi+)) (length-dimension :type 'vertical-dimension :character-size (/ (the box length)
```

---

## index.html (chunk 13/13)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/42/index.html
Type: reference

```
ject) :objects ((box :type 'box :length 10 :width (* (the-child length) +phi+) :height (* (the-child :width) +phi+)) (length-dimension :type 'vertical-dimension :character-size (/ (the box length) 20) :flip-leaders? t :start-point (the box (vertex :top :left :front)) :end-point (the box (vertex :top :left :rear))))) (generate-sample-drawing :object-roots (make-object 'vertical-dimension-sample)) Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/27/index.html
Type: reference

```
GendL Application - LEADER-LINE Package Documentation Object: LEADER-LINE (The :GEOM-BASE Package) Mixins: BASE-OBJECT Description Creates a leader line with arrows on zero, one, or both ends Input Slots (required) PATH-POINTS list of 3d points Leader-line is rendered as a polyline going through these points. Input Slots (optional) ARROWHEAD-LENGTH number The length of the arrows. Defaults to (* (the arrowhead-width) 2) ARROWHEAD-STYLE keyword Controls the style of first arrowhead. Currently only :wedge is supported. Default is :wedge. ARROWHEAD-STYLE-2 keyword Controls the style and presence of second arrowhead. Currently only :wedge is supported. Default is :none. ARROWHEAD-WIDTH number The width of the arrows. Defaults to (* (the line-thickness) 5). BOUNDING-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object.
```

---

## index.html (chunk 2/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/27/index.html
Type: reference

```
t of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object. BREAK-POINTS list of two points or nil . The start and end of the break in the leader line to accomodate the dimension-text, in cases where there is overlap. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil LOCAL-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation .
```

---

## index.html (chunk 3/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/27/index.html
Type: reference

```
volume bounding this geometric object. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places.
```

---

## index.html (chunk 4/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/27/index.html
Type: reference

```
ings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, defaulting) CENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located. HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. LENGTH [ from BASE-OBJECT ] number Y-axis dimension of the reference box. Defaults to zero. ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e.
```

---

## index.html (chunk 5/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/27/index.html
Type: reference

```
rom BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). WIDTH [ from BASE-OBJECT ] number X-axis dimension of the reference box. Defaults to zero. Computed Slots DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only).
```

---

## index.html (chunk 6/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/27/index.html
Type: reference

```
aults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/14)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/0/index.html
Type: reference

```
GendL Application - ANGULAR-DIMENSION Package Documentation Object: ANGULAR-DIMENSION (The :GEOM-BASE Package) Mixins: LINEAR-DIMENSION , VANILLA-MIXIN Description This dimensional object produces a clear and concise arc dimensional annotation. Input Slots (required) ARC-OBJECT gdl object The arc being measured. BASE-PLANE-NORMAL [ from LINEAR-DIMENSION ] Must be specified in the subclass except for angular LEADER-DIRECTION-1-VECTOR [ from LINEAR-DIMENSION ] Must be specified in the subclass except for angular LEADER-DIRECTION-2-VECTOR [ from LINEAR-DIMENSION ] Must be specified in the subclass except for angular WITNESS-DIRECTION-VECTOR [ from LINEAR-DIMENSION ] Must be specified in the subclass except for angular Input Slots (optional) ARROWHEAD-LENGTH [ from LINEAR-DIMENSION ] length (from tip to tail) of arrowhead glyph Defaults to twice the arrowhead-width ARROWHEAD-STYLE [ from LINEAR-DIMENSION ] keyword symbol Style for arrowhead on end of leader-line .
```

---

## index.html (chunk 2/14)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/0/index.html
Type: reference

```
rom tip to tail) of arrowhead glyph Defaults to twice the arrowhead-width ARROWHEAD-STYLE [ from LINEAR-DIMENSION ] keyword symbol Style for arrowhead on end of leader-line . Currently supported values are :none , :wedge (the Default), and :double-wedge . ARROWHEAD-STYLE-2 [ from LINEAR-DIMENSION ] keyword symbol Style for arrowhead on end of leader-line . Currently supported values are :none (the Default), :wedge , and :double-wedge . ARROWHEAD-WIDTH [ from LINEAR-DIMENSION ] width of arrowhead glyph Defaults to half the character-size. BOUNDING-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object. CENTER-POINT 3d point The center of the arc being measured. CHARACTER-SIZE [ from LINEAR-DIMENSION ] number Size (glyph height) of the label text, in model units. Defaults to 1.
```

---

## index.html (chunk 3/14)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/0/index.html
Type: reference

```
arc being measured. CHARACTER-SIZE [ from LINEAR-DIMENSION ] number Size (glyph height) of the label text, in model units. Defaults to 1. DIM-TEXT [ from LINEAR-DIMENSION ] string Determines the text which shows up as the dimension label. Defaults to the dim-value, which is computed specially in each specific dimension type. DIM-TEXT-BIAS [ from LINEAR-DIMENSION ] keyword symbol, :start, :end, or :center Indicates where to position the text in the case when outside-leaders? is non-nil. Defaults to :center DIM-TEXT-START 3d point Determines where the text will start. Defaults to halfway along the arc, just beyond the radius. DIM-TEXT-START-OFFSET [ from LINEAR-DIMENSION ] 3d vector (normally only 2d are used) . The dim-text-start is offset by this vector, in model space. Defaults to #(0.0 0.0 0.0) DXF-FONT [ from LINEAR-DIMENSION ] string This names the DXF font for this general-note. Defaults to (the font) .
```

---

## index.html (chunk 4/14)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/0/index.html
Type: reference

```
0.0 0.0) DXF-FONT [ from LINEAR-DIMENSION ] string This names the DXF font for this general-note. Defaults to (the font) . DXF-OFFSET [ from LINEAR-DIMENSION ] number The start of text will be offset by this amount for DXF output. Default is 2. DXF-SIZE-RATIO [ from LINEAR-DIMENSION ] number The scale factor for DXF character size vs PDF character size. Default is 0.8 DXF-TEXT-X-SCALE [ from LINEAR-DIMENSION ] number in percentage Adjusts the character width for DXF output. Defaults to the text-x-scale. END-POINT 3d point The end point of the arc being measured. FLIP-LEADERS? [ from LINEAR-DIMENSION ] boolean Indicates which direction the witness lines should take from the start and end points. The Default is NIL, which indicates :rear (i.e. ``up'') for horizontal-dimensions and :right for vertical-dimensions FONT [ from LINEAR-DIMENSION ] string naming a standard pdf font Font for the label text.
```

---

## index.html (chunk 5/14)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/0/index.html
Type: reference

```
vertical-dimensions FONT [ from LINEAR-DIMENSION ] string naming a standard pdf font Font for the label text. Defaults to "Helvetica" FULL-LEADER-LINE-LENGTH [ from LINEAR-DIMENSION ] number Indicates the length of the full leader when outside-leaders? is nil. This defaults to nil, which indicates that the full-leader's length should be auto-computed based on the given start-point and end-point. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil JUSTIFICATION [ from LINEAR-DIMENSION ] keyword symbol, :left, :right, or :center . For multi-line dim-text, this justification is applied. LEADER-1? [ from LINEAR-DIMENSION ] boolean Indicates whether the first (or only) leader line should be displayed.
```

---

## index.html (chunk 6/14)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/0/index.html
Type: reference

```
For multi-line dim-text, this justification is applied. LEADER-1? [ from LINEAR-DIMENSION ] boolean Indicates whether the first (or only) leader line should be displayed. The Default is T LEADER-2? [ from LINEAR-DIMENSION ] boolean Indicates whether the second leader line should be displayed. The Default is T LEADER-LINE-LENGTH [ from LINEAR-DIMENSION ] number Indicates the length of the first leader for the case when outside-leaders? is non-NIL LEADER-LINE-LENGTH-2 [ from LINEAR-DIMENSION ] number Indicates the length of the second leader for the case when outside-leaders? is non-NIL LEADER-RADIUS number The radius for the leader-arc. LEADER-TEXT-GAP [ from LINEAR-DIMENSION ] number Amount of gap between leader lines and dimension text, when the dimension text is within the leader. Defaults to half the character-size. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation .
```

---

## index.html (chunk 7/14)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/0/index.html
Type: reference

```
lts to half the character-size. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). OUTLINE-SHAPE-TYPE [ from LINEAR-DIMENSION ] keyword symbol Currently can be :bubble, :rectangle, or :none. Default is :none.
```

---

## index.html (chunk 8/14)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/0/index.html
Type: reference

```
TYPE [ from LINEAR-DIMENSION ] keyword symbol Currently can be :bubble, :rectangle, or :none. Default is :none. OUTSIDE-LEADERS-LENGTH-FACTOR [ from LINEAR-DIMENSION ] number Indicates the default length of the outside-leaders as a multiple of arrowhead-length. Defaults to 3. OUTSIDE-LEADERS? [ from LINEAR-DIMENSION ] boolean Indicates whether the leader line(s) should be inside or outside the interval between the start and end points. The default is NIL, which indicates that the leader line(s) should be inside the interval ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information START-POINT 3d point The start point of the arc being measured.
```

---

## index.html (chunk 9/14)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/0/index.html
Type: reference

```
dren which throw errors come back as a plist with error information START-POINT 3d point The start point of the arc being measured. STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. TEXT-ABOVE-LEADER? [ from LINEAR-DIMENSION ] boolean Indicates whether the text is to the right or above the leader line, rather than in-line with it. Default is T. TEXT-ALONG-AXIS? [ from LINEAR-DIMENSION ] boolean Where applicable, determines whether text direction follows leader-line direction TEXT-ALONG-LEADER-PADDING-FACTOR number Amount of padding above leader for text-along-leader? t. This is multiplied by the character-size to get the actual padding amount. Defaults to 1/3.
```

---

## index.html (chunk 10/14)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/0/index.html
Type: reference

```
above leader for text-along-leader? t. This is multiplied by the character-size to get the actual padding amount. Defaults to 1/3. TEXT-X-SCALE [ from LINEAR-DIMENSION ] number in percentage Adjusts the character width for the dimension-text and currently only applies only to PDF output UNDERLINE? [ from LINEAR-DIMENSION ] GDL VIEW-REFERENCE-OBJECT [ from LINEAR-DIMENSION ] gdl object or nil View object which will use this dimension. Defaults to NIL. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. WITNESS-1-TO-CENTER? boolean Determines whether a witness line extends all the way from the start-point to the center. Defaults to nil. WITNESS-2-TO-CENTER? boolean Determines whether a witness line extends all the way from the end-point to the center. Defaults to nil.
```

---

## index.html (chunk 11/14)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/0/index.html
Type: reference

```
termines whether a witness line extends all the way from the end-point to the center. Defaults to nil. WITNESS-LINE-2? [ from LINEAR-DIMENSION ] boolean Indicates whether to display a witness line coming off the end-point . Default is T WITNESS-LINE-EXT [ from LINEAR-DIMENSION ] number Distance the witness line(s) extend beyond the leader line. Default is 0.3 WITNESS-LINE-GAP [ from LINEAR-DIMENSION ] number Distance from the start-point and end-point to the start of each witness-line. Default is 0.1 WITNESS-LINE-LENGTH [ from LINEAR-DIMENSION ] number Length of the witness lines (or of the shorter witness line in case they are different lengths) WITNESS-LINE? [ from LINEAR-DIMENSION ] boolean Indicates whether to display a witness line coming off the start-point . Default is T Input Slots (optional, defaulting) CENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located.
```

---

## index.html (chunk 12/14)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/0/index.html
Type: reference

```
(optional, defaulting) CENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located. DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero.
```

---

## index.html (chunk 13/14)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/0/index.html
Type: reference

```
onal third number indicates how far into the line or curve to start the dash pattern. HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. LENGTH [ from BASE-OBJECT ] number Y-axis dimension of the reference box. Defaults to zero. WIDTH [ from BASE-OBJECT ] number X-axis dimension of the reference box. Defaults to zero. Computed Slots DIM-VALUE [ from LINEAR-DIMENSION ] number 2D distance relative to the base-plane-normal. Can be over-ridden in the subclass Examples (in-package :gdl-user) (define-object angular-dimension-test (base-object) :objects ((arc :type 'arc :display-controls (list :color :green ) :radius 30 :end-angle (degrees-to-radians 90)) (dimension :type 'angular-dimension :display-controls (list :color :blue ) :leader-radius (+ (* 0.1 (the arc radius))(the arc radius)) :arc-object (the arc)) (explicit-dimension :type 'angular-dimension :center-point (the arc center) :start-point (the arc (point-on-arc (degrees-to-radians 10)))
```

---

## index.html (chunk 14/14)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/0/index.html
Type: reference

```
he arc radius))(the arc radius)) :arc-object (the arc)) (explicit-dimension :type 'angular-dimension :center-point (the arc center) :start-point (the arc (point-on-arc (degrees-to-radians 10))) :end-point (the arc (point-on-arc (degrees-to-radians 60)))))) (generate-sample-drawing :objects (list (the-object (make-object 'angular-dimension-test) arc) (the-object (make-object 'angular-dimension-test) dimension) (the-object (make-object 'angular-dimension-test) explicit-dimension)) :projection-direction (getf *standard-views* :top)) Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/16/index.html
Type: reference

```
GendL Application - CYLINDER Package Documentation Object: CYLINDER (The :GEOM-BASE Package) Mixins: IFS-OUTPUT-MIXIN, ARCOID-MIXIN , BASE-OBJECT Description An extrusion of circular cross section in which the centers of the circles all lie on a single line (i.e., a right circular cylinder). Partial cylinders and hollow cylinders are supported. Input Slots (required) LENGTH number Distance from center of start cap to center of end cap. RADIUS number Radius of the circular cross section of the cylinder. Input Slots (optional) BOTTOM-CAP? boolean Determines whether to include bottom cap in shaded renderings. Defaults to T. BOUNDING-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object. CLOSED? boolean Indicates that a partial cylinder (or cone) should have a closed gap.
```

---

## index.html (chunk 2/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/16/index.html
Type: reference

```
e of geometric objects rooted at this object. CLOSED? boolean Indicates that a partial cylinder (or cone) should have a closed gap. END-ANGLE [ from ARCOID-MIXIN ] angle in radians End angle of the arc. Defaults to twice pi. HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil INNER-RADIUS number Radius of the hollow inner portion for a hollow cylinder. LOCAL-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object. NUMBER-OF-SECTIONS integer Number of vertical sections to be drawn in wireframe rendering mode.
```

---

## index.html (chunk 3/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/16/index.html
Type: reference

```
r top corners, in global coordinates, of the rectangular volume bounding this geometric object. NUMBER-OF-SECTIONS integer Number of vertical sections to be drawn in wireframe rendering mode. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information START-ANGLE [ from ARCOID-MIXIN ] angle in radians Start angle of the arc. Defaults to zero.
```

---

## index.html (chunk 4/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/16/index.html
Type: reference

```
flat lists. Any children which throw errors come back as a plist with error information START-ANGLE [ from ARCOID-MIXIN ] angle in radians Start angle of the arc. Defaults to zero. STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. TOP-CAP? boolean Determines whether to include bottom cap in shaded renderings. Defaults to T. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. WIDTH [ from BASE-OBJECT ] number X-axis dimension of the reference box. Defaults to zero.
```

---

## index.html (chunk 5/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/16/index.html
Type: reference

```
X-axis dimension of the reference box. Defaults to zero. Input Slots (optional, defaulting) CENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located. DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern.
```

---

## index.html (chunk 6/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/16/index.html
Type: reference

```
in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). Computed Slots DIRECTION-VECTOR 3d vector Points from the start to the end. END 3d point The center of the end cap. HOLLOW? boolean Indicates whether there is an inner-radius and thus the cylinder is hollow. START 3d point The center of the start cap.
```

---

## index.html (chunk 7/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/16/index.html
Type: reference

```
. Examples (in-package :gdl-user) (define-object cylinder-sample (cylinder) :computed-slots ((display-controls (list :color :pink-spicy)) (length 10) (radius 3) (number-of-sections 25))) (generate-sample-drawing :objects (make-object 'cylinder-sample) :projection-direction (getf *standard-views* :trimetric)) Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/36/index.html
Type: reference

```
GendL Application - SAMPLE-DRAWING Package Documentation Object: SAMPLE-DRAWING (The :GEOM-BASE Package) Mixins: BASE-DRAWING , VANILLA-MIXIN Description Defines a simple drawing with a single view for displaying objects or object-roots. Input Slots (optional) BOUNDING-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object. HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil LENGTH [ from BASE-OBJECT ] number Y-axis dimension of the reference box. Defaults to zero.
```

---

## index.html (chunk 2/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/36/index.html
Type: reference

```
image file to be displayed instead of actual geometry for this object. Defaults to nil LENGTH [ from BASE-OBJECT ] number Y-axis dimension of the reference box. Defaults to zero. LOCAL-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. PAGE-LENGTH [ from BASE-DRAWING ] number in pdf points Front-to-back (or top-to-bottom) length of the paper being represented by this drawing. The default is (* 11 72) points, or 11 inches, corresponding to US standard letter-size paper.
```

---

## index.html (chunk 3/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/36/index.html
Type: reference

```
-bottom) length of the paper being represented by this drawing. The default is (* 11 72) points, or 11 inches, corresponding to US standard letter-size paper. PAGE-WIDTH [ from BASE-DRAWING ] number in pdf points Left-to-right width of the paper being represented by this drawing. The default is (* 8.5 72) points, or 8.5 inches, corresponding to US standard letter-size paper. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places.
```

---

## index.html (chunk 4/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/36/index.html
Type: reference

```
the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. SVG-CLASS [ from BASE-DRAWING ] string with css classes These classes will be included in any svg tag outputted from this drawing. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. WIDTH [ from BASE-OBJECT ] number X-axis dimension of the reference box. Defaults to zero. Input Slots (optional, defaulting) CENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located. DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object.
```

---

## index.html (chunk 5/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/36/index.html
Type: reference

```
ed. DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e.
```

---

## index.html (chunk 6/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/36/index.html
Type: reference

```
es the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/10/index.html
Type: reference

```
GendL Application - CENTER-LINE Package Documentation Object: CENTER-LINE (The :GEOM-BASE Package) Mixins: OUTLINE-SPECIALIZATION-MIXIN, BASE-OBJECT Description Creates a dashed single centerline or crosshair centerline on a circle. Input Slots (required) SIZE number The length of the centerline. Input Slots (optional) BOUNDING-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object. CIRCLE? boolean Determines whether this will be a circle crosshair. Defaults to nil. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object.
```

---

## index.html (chunk 2/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/10/index.html
Type: reference

```
OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places.
```

---

## index.html (chunk 3/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/10/index.html
Type: reference

```
k as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, defaulting) CENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located. DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g.
```

---

## index.html (chunk 4/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/10/index.html
Type: reference

```
racteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. GAP-LENGTH number Distance between dashed line segments. Defaults to 0.1. LONG-SEGMENT-LENGTH number Length of longer dashed line segments. Defaults to 1.0. ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e.
```

---

## index.html (chunk 5/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/10/index.html
Type: reference

```
ASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). SHORT-SEGMENT-LENGTH number Length of shorter dashed line segments. Defaults to 0.25. Computed Slots HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. LENGTH [ from BASE-OBJECT ] number Y-axis dimension of the reference box. Defaults to zero. WIDTH [ from BASE-OBJECT ] number X-axis dimension of the reference box. Defaults to zero.
```

---

## index.html (chunk 6/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/10/index.html
Type: reference

```
Examples (in-package :gdl-user) (define-object center-line-test (base-object) :objects ((circle-sample :type 'circle :display-controls (list :color :green) :center (make-point 10 10 10 ) :radius 10) (center-line-sample :type 'center-line :circle? t :center (the circle-sample center) :size (* 2.1 (the circle-sample radius))))) (generate-sample-drawing :objects (list (the-object (make-object 'center-line-test) circle-sample) (the-object (make-object 'center-line-test) center-line-sample)) :projection-direction (getf *standard-views* :top)) Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/8/index.html
Type: reference

```
GendL Application - BOX Package Documentation Object: BOX (The :GEOM-BASE Package) Mixins: BASE-OBJECT Description This represents a ``visible'' base-object -- a six-sided box with all the same messages as base-object, which knows how to output itself in various formats. Input Slots (optional) BOUNDING-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object.
```

---

## index.html (chunk 2/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/8/index.html
Type: reference

```
this object. Defaults to nil LOCAL-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists.
```

---

## index.html (chunk 3/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/8/index.html
Type: reference

```
ents of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, defaulting) CENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located. DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object.
```

---

## index.html (chunk 4/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/8/index.html
Type: reference

```
ASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. LENGTH [ from BASE-OBJECT ] number Y-axis dimension of the reference box. Defaults to zero.
```

---

## index.html (chunk 5/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/8/index.html
Type: reference

```
m BASE-OBJECT ] number Y-axis dimension of the reference box. Defaults to zero. ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). WIDTH [ from BASE-OBJECT ] number X-axis dimension of the reference box. Defaults to zero. Computed Slots VOLUME number Total volume of the box. GDL Functions CLOSEST-VERTEX 3d-point .
```

---

## index.html (chunk 6/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/8/index.html
Type: reference

```
Examples (in-package :gdl-user) (define-object box-sample (box) :computed-slots ((display-controls (list :color :blue-neon)) (length 10) (width (* (the length) +phi+)) (height (* (the width) +phi+)))) (generate-sample-drawing :objects (make-object 'box-sample) :projection-direction (getf *standard-views* :trimetric)) Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/8)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/40/index.html
Type: reference

```
GendL Application - TORUS Package Documentation Object: TORUS (The :GEOM-BASE Package) Mixins: IFS-OUTPUT-MIXIN, ARCOID-MIXIN , BASE-OBJECT Description A single-holed ``ring'' torus, also known as an ``anchor ring.'' This is basically a circular cylinder ``bent'' into a donut shape. Partial donuts (``elbows'') are supported. Partial ``bent'' cylinders are not currently supported. Input Slots (required) MAJOR-RADIUS number Distance from center of donut hole to centerline of the torus. MINOR-RADIUS number Radius of the bent cylinder making up the torus. RADIUS [ from ARCOID-MIXIN ] number Distance from center to any point on the arc. Input Slots (optional) BOUNDING-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object.
```

---

## index.html (chunk 2/8)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/40/index.html
Type: reference

```
ectangular volume bounding the tree of geometric objects rooted at this object. DRAW-CENTERLINE-ARC? boolean Indicates whether the bent cylinder's centerline arc should be rendered in some renderings. END-ANGLE [ from ARCOID-MIXIN ] angle in radians End angle of the arc. Defaults to twice pi. END-CAPS? boolean Indicates whether to include end caps for a partial torus in some renderings. Defaults to T. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil INNER-MINOR-RADIUS number Radius of the inner hollow part of the bent cylinder for a hollow torus.
```

---

## index.html (chunk 3/8)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/40/index.html
Type: reference

```
for a hollow torus. Defaults to NIL for a solid cylinder LOCAL-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object. NUMBER-OF-LONGITUDINAL-SECTIONS integer Indicates the number of arcs to be drawn on along ``surface'' of the torus in some wireframe renderings. NUMBER-OF-TRANSVERSE-SECTIONS integer Indicates the number of circular cross-sections of the bent cylinder to show in some wireframe renderings. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy).
```

---

## index.html (chunk 4/8)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/40/index.html
Type: reference

```
ject in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information START-ANGLE [ from ARCOID-MIXIN ] angle in radians Start angle of the arc. Defaults to zero. STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children.
```

---

## index.html (chunk 5/8)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/40/index.html
Type: reference

```
part is an element of a sequence. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, defaulting) ARC angle in radians Indicates the end angle for the donut. Defaults to twice pi for a full-circle donut. CENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located. DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only).
```

---

## index.html (chunk 6/8)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/40/index.html
Type: reference

```
e. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). Computed Slots HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. LENGTH [ from BASE-OBJECT ] number Y-axis dimension of the reference box.
```

---

## index.html (chunk 7/8)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/40/index.html
Type: reference

```
ne (1.0). Computed Slots HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. LENGTH [ from BASE-OBJECT ] number Y-axis dimension of the reference box. Defaults to zero. WIDTH [ from BASE-OBJECT ] number X-axis dimension of the reference box. Defaults to zero. Examples (in-package :gdl-user) (define-object torus-sample (torus) :computed-slots ((major-radius 150) (minor-radius 42) (draw-centerline-arc? t) (number-of-longitudinal-sections 10) (number-of-transverse-sections 10) (display-controls (list :color :green-forest-medium))) :hidden-objects ((view :type 'base-view :projection-vector (getf *standard-views* :trimetric) :page-width (* 5 72) :page-length (* 5 72) :objects (list self)))) (generate-sample-drawing :objects (make-object 'torus-sample) :projection-direction :trimetric) Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 8/8)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/40/index.html
Type: reference

```
ojection-direction :trimetric) Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/23/index.html
Type: reference

```
GendL Application - GLOBAL-POLYLINE Package Documentation Object: GLOBAL-POLYLINE (The :GEOM-BASE Package) Mixins: GLOBAL-POLYLINE-MIXIN , VANILLA-MIXIN Description A sequence of points connected by straight line segments. Please see global-polyline-mixin for documentation on the messages. Input Slots (required) VERTEX-LIST [ from GLOBAL-POLYLINE-MIXIN ] list of 3d points The vertices (``corners'') of the polyline. Input Slots (optional) CLOSED? [ from GLOBAL-POLYLINE-MIXIN ] boolean Controls whether the filleted-polyline should automatically be closed. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object.
```

---

## index.html (chunk 2/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/23/index.html
Type: reference

```
to be displayed instead of actual geometry for this object. Defaults to nil LOCAL-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists.
```

---

## index.html (chunk 3/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/23/index.html
Type: reference

```
from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, defaulting) CENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located. DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object.
```

---

## index.html (chunk 4/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/23/index.html
Type: reference

```
ect should be located. DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. LENGTH [ from BASE-OBJECT ] number Y-axis dimension of the reference box. Defaults to zero.
```

---

## index.html (chunk 5/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/23/index.html
Type: reference

```
he reference box. Defaults to zero. LENGTH [ from BASE-OBJECT ] number Y-axis dimension of the reference box. Defaults to zero. ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). WIDTH [ from BASE-OBJECT ] number X-axis dimension of the reference box. Defaults to zero.
```

---

## index.html (chunk 6/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/23/index.html
Type: reference

```
Examples (in-package :gdl-user) (define-object global-polyline-sample (global-polyline) :computed-slots ((vertex-list (list (make-point 0 0 0) (make-point 10 10 0) (make-point 30 10 0) (make-point 40 0 0) (make-point 30 -10 0) (make-point 10 -10 0) (make-point 0 0 0))))) (generate-sample-drawing :objects (make-object 'global-polyline-sample)) Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/5)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/33/index.html
Type: reference

```
GendL Application - POINTS-DISPLAY Package Documentation Object: POINTS-DISPLAY (The :GEOM-BASE Package) Mixins: OUTLINE-SPECIALIZATION-MIXIN Description Product a list of hidden-children which are GDL point objects, which will be displayed in normal renderings using the outline-specialization-mixin mechanism. Input Slots (required) POINTS list of 3d points (i e. vectors). The points to be displayed. Input Slots (optional) BOUNDING-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object.
```

---

## index.html (chunk 2/5)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/33/index.html
Type: reference

```
AGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists.
```

---

## index.html (chunk 3/5)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/33/index.html
Type: reference

```
Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, defaulting) CENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located. DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object.
```

---

## index.html (chunk 4/5)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/33/index.html
Type: reference

```
eywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. LENGTH [ from BASE-OBJECT ] number Y-axis dimension of the reference box. Defaults to zero.
```

---

## index.html (chunk 5/5)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/33/index.html
Type: reference

```
mension of the reference box. Defaults to zero. ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). WIDTH [ from BASE-OBJECT ] number X-axis dimension of the reference box. Defaults to zero. Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/41/index.html
Type: reference

```
GendL Application - TYPESET-BLOCK Package Documentation Object: TYPESET-BLOCK (The :GEOM-BASE Package) Mixins: BASE-OBJECT Description Block of text typeset using cl-typesetting. This object wraps the typeset block as a standard GDL object, so it can be placed in a view and positioned according to normal GDL positioning. You can specify the width, and by default this object will compute its length automatically from the typeset content, to fit all the lines of text into the box. Because of this computed behavior of the length, the center of the box will not, in general, be in a known location compared to the start of the text. Because of this it is recommended to use :corner, rather than :center, for positioning a base-view which contains a typeset block. In the normal case, if you want a single block in a view on a drawing, you should make the base-view object have the same width and length as the typeset-block.
```

---

## index.html (chunk 2/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/41/index.html
Type: reference

```
single block in a view on a drawing, you should make the base-view object have the same width and length as the typeset-block. The base-view should also probably have :left-margin 0 and :front-margin 0. Input Slots (optional) BOUNDING-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object. CENTER 3d-point Center of the text. Specify this or start, not both. note: that the center is no longer defaulting (so that it can self-compute properly when start is specified), so it is necessary to explicitly give either start or center for general-note. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil.
```

---

## index.html (chunk 3/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/41/index.html
Type: reference

```
-object even if specified in :objects. Default is nil. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil LENGTH number The length of the box to contain the compiled content. Defaults is (the length-default), which will exactly fit the compiled content into the specified width. If you override it to be less than this default, the content will be cropped. LOCAL-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g.
```

---

## index.html (chunk 4/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/41/index.html
Type: reference

```
ASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information START 3d-point Start of the text. Specify this or center, not both. START-LINE-INDEX number The line number to start STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence.
```

---

## index.html (chunk 5/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/41/index.html
Type: reference

```
in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, defaulting) DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only).
```

---

## index.html (chunk 6/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/41/index.html
Type: reference

```
for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). WIDTH [ from BASE-OBJECT ] number X-axis dimension of the reference box. Defaults to zero.
```

---

## index.html (chunk 7/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/41/index.html
Type: reference

```
WIDTH [ from BASE-OBJECT ] number X-axis dimension of the reference box. Defaults to zero. Computed Slots LENGTH-DEFAULT number The computed length which will exactly fit the content based on (the width). LINES list of typeset line objects The list of lines in the nominal block. Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/4/index.html
Type: reference

```
GendL Application - BASE-DRAWING Package Documentation Object: BASE-DRAWING (The :GEOM-BASE Package) Mixins: BASE-OBJECT Description Generic container object for displaying one or more scaled transformed views of geometric or text-based entities. The contained views are generally of type base-view . In a GWL application-mixin, you can include one object of this type in the ui-display-list-leaves. For the PDF output-format, you can also use the cad-output output-function to write the drawing as a PDF document. Since base-drawing is inherently a 2D object, only the top view (getf *standard-views* :top) makes sense for viewing it. Input Slots (optional) BOUNDING-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object. HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero.
```

---

## index.html (chunk 2/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/4/index.html
Type: reference

```
ngular volume bounding the tree of geometric objects rooted at this object. HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil LENGTH [ from BASE-OBJECT ] number Y-axis dimension of the reference box. Defaults to zero. LOCAL-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation .
```

---

## index.html (chunk 3/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/4/index.html
Type: reference

```
thonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. PAGE-LENGTH number in pdf points Front-to-back (or top-to-bottom) length of the paper being represented by this drawing. The default is (* 11 72) points, or 11 inches, corresponding to US standard letter-size paper. PAGE-WIDTH number in pdf points Left-to-right width of the paper being represented by this drawing. The default is (* 8.5 72) points, or 8.5 inches, corresponding to US standard letter-size paper. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists.
```

---

## index.html (chunk 4/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/4/index.html
Type: reference

```
s object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. SVG-CLASS string with css classes These classes will be included in any svg tag outputted from this drawing. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. WIDTH [ from BASE-OBJECT ] number X-axis dimension of the reference box. Defaults to zero.
```

---

## index.html (chunk 5/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/4/index.html
Type: reference

```
ypically this would be a subset of hidden-children. Defaults to NIL. WIDTH [ from BASE-OBJECT ] number X-axis dimension of the reference box. Defaults to zero. Input Slots (optional, defaulting) CENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located. DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line.
```

---

## index.html (chunk 6/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/4/index.html
Type: reference

```
ttern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0).
```

---

## index.html (chunk 7/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/4/index.html
Type: reference

```
Examples (in-package :gdl-user) (define-object cylinder-sample (cylinder) :computed-slots ((display-controls (list :color :pink-spicy)) (length 10) (radius 3) (number-of-sections 25))) (define-object base-drawing-sample (base-drawing) :objects ((main-view :type 'base-view :projection-vector (getf *standard-views* :trimetric) :object-roots (list (the surf))) (surf :type 'cylinder-sample :hidden? t))) (generate-sample-drawing :objects (make-object 'base-drawing-sample)) Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/8)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/32/index.html
Type: reference

```
GendL Application - POINT Package Documentation Object: POINT (The :GEOM-BASE Package) Mixins: SPHERE Description Visual representation of a point as a small view-independent crosshair. This means the crosshair will always appear in a ``top'' view regardless of the current view transform. The crosshair will not scale along with any zoom state unless the scale? optional input-slot is non-NIL. The default color for the crosshairs is a light grey (:grey-light-very in the *color-table*). Input Slots (optional) CROSSHAIR-LENGTH number Distance from center to end of crosshairs used to show the point. Default value is 3. END-ANGLE [ from ARCOID-MIXIN ] angle in radians End angle of the arc. Defaults to twice pi. END-HORIZONTAL-ARC [ from SPHERE ] angle in radians Ending horizontal angle for a partial sphere. Default is twice pi. END-VERTICAL-ARC [ from SPHERE ] angle in radians Ending vertical angle for a partial sphere. Default is pi/2.
```

---

## index.html (chunk 2/8)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/32/index.html
Type: reference

```
tial sphere. Default is twice pi. END-VERTICAL-ARC [ from SPHERE ] angle in radians Ending vertical angle for a partial sphere. Default is pi/2. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil INNER-RADIUS [ from SPHERE ] number Radius of inner hollow for a hollow sphere. Default is NIL, for a non-hollow sphere. LOCAL-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object. NUMBER-OF-HORIZONTAL-SECTIONS [ from SPHERE ] number How many lines of latitude to show on the sphere in some renderings. Default value is 4.
```

---

## index.html (chunk 3/8)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/32/index.html
Type: reference

```
rom SPHERE ] number How many lines of latitude to show on the sphere in some renderings. Default value is 4. NUMBER-OF-VERTICAL-SECTIONS [ from SPHERE ] number How many lines of longitude to show on the sphere in some renderings. Default value is 4. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. RADIUS [ from SPHERE ] number Distance from center to any point on the sphere. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists.
```

---

## index.html (chunk 4/8)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/32/index.html
Type: reference

```
E-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information SCALED? boolean Indicates whether the crosshairs drawn to represent the point are scaled along with any zoom factor applied to the display, or are fixed with respect to drawing space. The default is NIL, meaning the crosshairs will remain the same size regardless of zoom state. START-ANGLE [ from ARCOID-MIXIN ] angle in radians Start angle of the arc. Defaults to zero. START-HORIZONTAL-ARC [ from SPHERE ] angle in radians Starting horizontal angle for a partial sphere. Default is 0. START-VERTICAL-ARC [ from SPHERE ] angle in radians Starting vertical angle for a partial sphere. Default is -pi/2. STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places.
```

---

## index.html (chunk 5/8)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/32/index.html
Type: reference

```
al sphere. Default is -pi/2. STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, defaulting) CENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located. DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g.
```

---

## index.html (chunk 6/8)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/32/index.html
Type: reference

```
stics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function.
```

---

## index.html (chunk 7/8)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/32/index.html
Type: reference

```
absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). Computed Slots BOUNDING-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object. Examples (in-package :gdl-user) (define-object point-sample (base-object) :objects ((bezier :type 'bezier-curve :control-points (list (make-point 0 0 0) (make-point 1 1 0) (make-point 2 1 0) (make-point 3 0 0))) (points-to-show :type 'point :sequence (:size (length (the bezier control-points))) :center (nth (the-child :index) (the bezier control-points)) :radius 0.08 :display-controls (list :color :blue)))) (generate-sample-drawing :object-roots (make-object 'point-sample)) Package Documentation Copyright © 2025 Genworks ® International .
```

---

## index.html (chunk 8/8)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/32/index.html
Type: reference

```
:radius 0.08 :display-controls (list :color :blue)))) (generate-sample-drawing :object-roots (make-object 'point-sample)) Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/3)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/2/index.html
Type: reference

```
GendL Application - ARCOID-MIXIN Package Documentation Object: ARCOID-MIXIN (The :GEOM-BASE Package) Mixins: VANILLA-MIXIN Description This object is a low level object used to define an arc like object. It is not recommended to be used directly by GDL common users. For developers it should be used as a mixin. Input Slots (required) RADIUS number Distance from center to any point on the arc. Input Slots (optional) END-ANGLE angle in radians End angle of the arc. Defaults to twice pi. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists.
```

---

## index.html (chunk 2/3)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/2/index.html
Type: reference

```
dl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information START-ANGLE angle in radians Start angle of the arc. Defaults to zero. STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 3/3)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/2/index.html
Type: reference

```
ional . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/28/index.html
Type: reference

```
GendL Application - LINE Package Documentation Object: LINE (The :GEOM-BASE Package) Mixins: BASE-OBJECT Description Provides a simple way to create a line, by specifying a start point and an end point. Input Slots (required) END 3d point The end point of the line, in global coordinates. START 3d point The start point of the line, in global coordinates. Input Slots (optional) HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil LOCAL-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object.
```

---

## index.html (chunk 2/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/28/index.html
Type: reference

```
n global coordinates, of the rectangular volume bounding this geometric object. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places.
```

---

## index.html (chunk 3/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/28/index.html
Type: reference

```
om VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, defaulting) DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object.
```

---

## index.html (chunk 4/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/28/index.html
Type: reference

```
ure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). WIDTH [ from BASE-OBJECT ] number X-axis dimension of the reference box.
```

---

## index.html (chunk 5/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/28/index.html
Type: reference

```
ent function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). WIDTH [ from BASE-OBJECT ] number X-axis dimension of the reference box. Defaults to zero. Computed Slots BOUNDING-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object. CENTER 3d point The center of the line. DIRECTION-VECTOR 3d vector Points from start to end of the line. LENGTH number The distance from start to end of the line. Examples (in-package :gdl-user) (define-object line-sample (line) :computed-slots ((start (make-point -10 -10 0)) (end (make-point 10 10 0)))) (generate-sample-drawing :objects (make-object 'line-sample)) Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 6/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/28/index.html
Type: reference

```
Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/8)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/26/index.html
Type: reference

```
GendL Application - LABEL Package Documentation Object: LABEL (The :GEOM-BASE Package) Mixins: OUTLINE-SPECIALIZATION-MIXIN, BASE-OBJECT Description Produces a text label for graphical output Input Slots (required) LEADER-PATH list of 3d points List making up leader line, starting from where the arrowhead normally is. Input Slots (optional) ARROWHEAD-LENGTH length (from tip to tail) of arrowhead glyph Defaults to twice the arrowhead-width ARROWHEAD-STYLE keyword symbol Style for arrowhead at start of leader-path . Currently supported values are :none , :wedge (the Default), and :double-wedge . ARROWHEAD-STYLE-2 keyword symbol Style for arrowhead on end of leader-path . Currently supported values are :none (the Default), :wedge , and :double-wedge .
```

---

## index.html (chunk 2/8)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/26/index.html
Type: reference

```
ARROWHEAD-WIDTH width of arrowhead glyph Defaults to five times the line thickness (2.5) BOUNDING-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object. CHARACTER-SIZE number Size (glyph height) of the label text, in model units. Defaults to 10. DXF-FONT string This names the DXF font for this general-note. Defaults to (the font) . DXF-OFFSET number The start of text will be offset by this amount for DXF output. Default is 2. DXF-SIZE-RATIO number The scale factor for DXF character size vs PDF character size. Default is 0.8 DXF-TEXT-X-SCALE number in percentage Adjusts the character width for DXF output. Defaults to the text-x-scale. FONT string naming a standard pdf font Font for the label text.
```

---

## index.html (chunk 3/8)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/26/index.html
Type: reference

```
string naming a standard pdf font Font for the label text. Defaults to "Helvetica" HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. OUTLINE-SHAPE-TYPE keyword symbol Indicates shape of outline enclosing the text. Currently :none , :bubble , :rectangle , and nil are supported.
```

---

## index.html (chunk 4/8)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/26/index.html
Type: reference

```
es shape of outline enclosing the text. Currently :none , :bubble , :rectangle , and nil are supported. The default is nil ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS list of strings Text lines to be displayed as the label. Specify this or text, not both. STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence.
```

---

## index.html (chunk 5/8)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/26/index.html
Type: reference

```
, followed by an index number if the part is an element of a sequence. TEXT string Text to be displayed as the label TEXT-GAP number Amount of space between last point in leader-path and beginning of the label text. Defaults to the width of the letter "A" in the specified font and character-size . TEXT-SIDE keyword symbol, either :left or :right Determines whether the label text sits to the right or the left of the last point in the leader-path . The default is computed based on the direction of the last segment of the leader-path. VIEW-REFERENCE-OBJECT gdl object or nil View object which will use this dimension. Defaults to NIL. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, defaulting) CENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located.
```

---

## index.html (chunk 6/8)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/26/index.html
Type: reference

```
faults to NIL. Input Slots (optional, defaulting) CENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located. DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box.
```

---

## index.html (chunk 7/8)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/26/index.html
Type: reference

```
in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. LENGTH [ from BASE-OBJECT ] number Y-axis dimension of the reference box. Defaults to zero. WIDTH [ from BASE-OBJECT ] number X-axis dimension of the reference box. Defaults to zero. Computed Slots ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0).
```

---

## index.html (chunk 8/8)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/26/index.html
Type: reference

```
Examples (in-package :gdl-user) (define-object label-sample (base-object) :objects ((box :type 'box :length 10 :width (* (the-child length) +phi+) :height (* (the-child :width) +phi+)) (corner-label :type 'label :leader-path (let ((start (the box (vertex :top :right :rear)))) (list start (translate start :right (/ (the box width) 10) :rear (/ (the box width) 10)) (translate start :right (/ (the box width) 7) :rear (/ (the box width) 10)))) :text "The Corner" :character-size (/ (the box width) 15)))) (generate-sample-drawing :object-roots (make-object 'label-sample)) Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/5)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/15/index.html
Type: reference

```
GendL Application - CONSTRAINED-LINE Package Documentation Object: CONSTRAINED-LINE (The :GEOM-BASE Package) Mixins: LINE Description This object is intended to simplify the process of constructing lines using various constraints. Currently supported are 2 through-points or 1 through-point and at-angle. Note the line-constraints must be an evaluatable s-expression as this is not processed as a macro Input Slots (optional) HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil LOCAL-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object.
```

---

## index.html (chunk 2/5)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/15/index.html
Type: reference

```
left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places.
```

---

## index.html (chunk 3/5)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/15/index.html
Type: reference

```
with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, defaulting) DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black.
```

---

## index.html (chunk 4/5)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/15/index.html
Type: reference

```
idecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0).
```

---

## index.html (chunk 5/5)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/15/index.html
Type: reference

```
tion), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). WIDTH [ from BASE-OBJECT ] number X-axis dimension of the reference box. Defaults to zero. Computed Slots END [ from LINE ] 3d point The end point of the line, in global coordinates. START [ from LINE ] 3d point The start point of the line, in global coordinates. GDL Functions TANGENT-POINT Icad Compat function Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/12)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/5/index.html
Type: reference

```
GendL Application - BASE-OBJECT Package Documentation Object: BASE-OBJECT (The :GEOM-BASE Package) Mixins: VANILLA-MIXIN Description Base-Object is a superclass of most of GDL's geometric primitives. It provides an imaginary geometric reference box with a length, width, height, center, and orientation. Input Slots (optional) BOUNDING-BOX list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. IMAGE-FILE pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil LOCAL-BOX list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object.
```

---

## index.html (chunk 2/12)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/5/index.html
Type: reference

```
t. Defaults to nil LOCAL-BOX list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object. OBLIQUENESS 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places.
```

---

## index.html (chunk 3/12)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/5/index.html
Type: reference

```
k as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, defaulting) CENTER 3d point Indicates in global coordinates where the center of the reference box of this object should be located. DISPLAY-CONTROLS plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black.
```

---

## index.html (chunk 4/12)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/5/index.html
Type: reference

```
g keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. HEIGHT number Z-axis dimension of the reference box. Defaults to zero. LENGTH number Y-axis dimension of the reference box. Defaults to zero. ORIENTATION 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e.
```

---

## index.html (chunk 5/12)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/5/index.html
Type: reference

```
n Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). WIDTH number X-axis dimension of the reference box. Defaults to zero. Computed Slots COLOR-DECIMAL vector of three real numbers The RBG color of this object specified in :display-controls. Defaults to the foreground color specified in *colors-default* . This message should not normally be overridden in user application code. LOCAL-CENTER 3d point The center of this object, from the perspective of the parent. Starting from the parent's center and using the parent's orientation, this is the relative center of this object. LOCAL-CENTER* 3d point The center of this object, from the perspective of the parent.
```

---

## index.html (chunk 6/12)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/5/index.html
Type: reference

```
center of this object. LOCAL-CENTER* 3d point The center of this object, from the perspective of the parent. Starting from the parent's center and using the parent's orientation, this is the relative center of this object. LOCAL-ORIENTATION 3x3 matrix of double-float numbers Indicates the local Rotation Matrix used to create the coordinate system of this object. This is the ``local'' orientation with respect to the parent. Multiplying the parent's orientation with this matrix will always result in the absolute orientation for this part. note: An orientation of NIL indicates the 3x3 identity matrix. Hidden Objects BOUNDING-BBOX gdl object of type box A box representing the bounding-box. LOCAL-BBOX gdl object of type box A box representing the local-box. GDL Functions AXIS-VECTOR 3d vector Returns the vector pointing in the positive direction of the specified axis of this object's reference box.
```

---

## index.html (chunk 7/12)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/5/index.html
Type: reference

```
Returns the vector pointing in the positive direction of the specified axis of this object's reference box. arguments: axis Keyword One of the standard axis keywords: :lateral, :longitudinal, :vertical EDGE-CENTER 3d point Returns the center of the requested edge of this object's reference box. arguments: direction-1 Keyword One of the standard direction keywords: :right, :left, :rear, :front, :top, :bottom direction-2 Keyword A standard direction keyword orthogonal to direction-1 FACE-CENTER 3d point Returns the center of the requested face (the requested face with respect to the `wrt` argument if present, or self if `wrt` is nil) of this object's reference box. arguments: direction Keyword One of the standard direction keywords: :right, :left, :rear, :front, :top, :bottom FACE-NORMAL-VECTOR 3d vector Returns the vector pointing from this object's reference box center to its requested face-center.
```

---

## index.html (chunk 8/12)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/5/index.html
Type: reference

```
TOR 3d vector Returns the vector pointing from this object's reference box center to its requested face-center. arguments: direction Keyword One of the standard direction keywords: :right, :left, :rear, :front, :top, :bottom FACE-VERTICES list of four 3d points Returns the vertices of the indicated face. arguments: direction Direction keyword, e g. :top, :bottom etc. Indicates for which face to return the vertices GLOBAL-TO-LOCAL 3d-point This function returns the point given in global coordinates, into relative local coordinates, based on the orientation and center of the object to which the global-to-local message is sent. arguments: point 3D-point The point to be converted to local coordinates examples: Please see the examples area. IN-FACE? boolean Returns non-nil if the given point is in halfspace defined by the plane given a point and direction.
```

---

## index.html (chunk 9/12)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/5/index.html
Type: reference

```
in halfspace defined by the plane given a point and direction. arguments: point 3D point a point in the plane direction 3D vector The normal of the plane LINE-INTERSECTION-POINTS list of 3d points Returns the points of intersection between given line and the reference box of this object. arguments: p-line 3D point A point in the line u-line 3D vector The direction vector of the line LOCAL-TO-GLOBAL 3d-point This function returns the point given in relative local coordinates, converted into global coordinates, based on the orientation and center of the object to which the local-to-global message is sent. arguments: point 3D-point The local point to be converted to global coordinates examples: Please see the examples area. VERTEX 3d point Returns the center of the requested vertex (corner) of this object's reference box.
```

---

## index.html (chunk 10/12)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/5/index.html
Type: reference

```
of this object's reference box. arguments: direction-1 Keyword One of the standard direction keywords: :right, :left, :rear, :front, :top, :bottom direction-2 Keyword A standard direction keyword orthogonal to direction-1 direction-3 Keyword A standard direction keyword orthogonal to direction-1 and direction-2 Examples (in-package :gdl-user) (define-object tower (base-object) :input-slots ((number-of-blocks 50) (twist-per-block 1) (block-height 1) (block-width 5) (block-length 7)) :objects ((blocks :type 'box :sequence (:size (the number-of-blocks)) :center (translate (the center) :up (* (the-child index) (the-child height))) :width (the block-width) :height (the block-height) :length (the block-length) :orientation (alignment :rear (if (the-child first?) (rotate-vector-d (the (face-normal-vector :rear)) (the twist-per-block) (the (face-normal-vector :top))) (rotate-vector-d (the-child previous (face-normal-vector :rear)) (the twist-per-block) (the (face-normal-vector :top)))) :top
```

---

## index.html (chunk 11/12)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/5/index.html
Type: reference

```
l-vector :rear)) (the twist-per-block) (the (face-normal-vector :top))) (rotate-vector-d (the-child previous (face-normal-vector :rear)) (the twist-per-block) (the (face-normal-vector :top)))) :top (the (face-normal-vector :top)))))) ;; ;;Test run ;; #| gdl-user(46): (setq self (make-object 'tower)) #tower @ #x750666f2 gdl-user(47): (setq test-center (the (blocks 10) center)) #(0.0 0.0 10.0) gdl-user(48): (the (blocks 10) (global-to-local test-center)) #(0.0 0.0 0.0) gdl-user(49): (the (blocks 10) (local-to-global (the (blocks 10) (global-to-local test-center)))) #(0.0 0.0 10.0) gdl-user(50): gdl-user(50): (setq test-vertex (the (blocks 10) (vertex :top :right :rear))) #(1.7862364748012536 3.9127176305081863 10.5) gdl-user(51): (the (blocks 10) (global-to-local test-vertex)) #(2.500000000000001 3.500000000000001 0.5) gdl-user(52): (the (blocks 10) (local-to-global (the (blocks 10) (global-to-local test-vertex)))) #(1.786236474801254 3.9127176305081877 10.5) gdl-user(53): |# ;; ;; ;
```

---

## index.html (chunk 12/12)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/5/index.html
Type: reference

```
00001 3.500000000000001 0.5) gdl-user(52): (the (blocks 10) (local-to-global (the (blocks 10) (global-to-local test-vertex)))) #(1.786236474801254 3.9127176305081877 10.5) gdl-user(53): |# ;; ;; ;; Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/8)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/18/index.html
Type: reference

```
GendL Application - GENERAL-NOTE Package Documentation Object: GENERAL-NOTE (The :GEOM-BASE Package) Mixins: OUTLINE-SPECIALIZATION-MIXIN, BASE-OBJECT Description Creates a text note in the graphical view port and in a PDF DXF output file. Input Slots (optional) BOUNDING-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object. CENTER 3d-point Center of the text. Specify this or start, not both. note: that the center is no longer defaulting (so that it can self-compute properly when start is specified), so it is necessary to explicitly give either start or center for general-note. CHARACTER-SIZE number Specifies the character size in drawing units. DXF-FONT string This names the DXF font for this general-note. Defaults to (the font) . DXF-OFFSET number The start of text will be offset by this amount for DXF output. Default is 0.
```

---

## index.html (chunk 2/8)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/18/index.html
Type: reference

```
units. DXF-FONT string This names the DXF font for this general-note. Defaults to (the font) . DXF-OFFSET number The start of text will be offset by this amount for DXF output. Default is 0. DXF-SIZE-RATIO number The scale factor for DXF character size vs PDF character size. Default is 0.8 DXF-TEXT-X-SCALE number in percentage Adjusts the character width for DXF output. Defaults to the text-x-scale. FONT string The font for PDF. Possibilities for built-in PDF fonts are: courier courier-bold courier-boldoblique courier-oblique helvetica helvetica-bold helvetica-boldoblique helvetica-oblique symbol times-roman times-bold times-bolditalic times-italic zapfdingbats Defaults to "Courier". HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil.
```

---

## index.html (chunk 3/8)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/18/index.html
Type: reference

```
VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil JUSTIFICATION keyword symbol, :left, :right, or :center Justifies text with its box. Default is :left. LEADING number Space between lines of text. Default is 1.2 times the character size. LENGTH [ from BASE-OBJECT ] number Y-axis dimension of the reference box. Defaults to zero. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM.
```

---

## index.html (chunk 4/8)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/18/index.html
Type: reference

```
function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. OUTLINE-SHAPE-TYPE keyword symbol Currently can be :bubble, :rectangle, or :none. Default is :none. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information START 3d-point Start of the text. Specify this or center, not both. STRINGS list of strings The text to be displayed in the note. STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places.
```

---

## index.html (chunk 5/8)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/18/index.html
Type: reference

```
e of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. TEXT-X-SCALE number in percentage Adjusts the character width for PDF output. Defaults to 100. UNDERLINE? boolean Determines whether text is underlined. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. WIDTH number Determines the width of the containing box. Default is the maximum-text-width. Input Slots (optional, defaulting) DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white.
```

---

## index.html (chunk 6/8)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/18/index.html
Type: reference

```
ject. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0).
```

---

## index.html (chunk 7/8)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/18/index.html
Type: reference

```
.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). Computed Slots MAXIMUM-TEXT-WIDTH number Convienence computation giving the maximum input width required to keep one line per string Examples (in-package :gdl-user) (define-object general-note-test (base-object) :computed-slots ((blocks-note (list "David Brown" "Created by" "ABC 2" "Jane Smith" "Approved by" "CCD 2")) (blocks-center (list '(-15 5 0) '(-40 5 0) '(-55 5 0) '(-15 15 0) '(-40 15 0) '(-55 15 0))) (blocks-width (list 30 20 10 30 20 10))) :objects ((title-block :type 'box :sequence (:size (length (the blocks-center))) :display-controls (list :color :red) :center (apply-make-point (nth (the-child index ) (the blocks-center))) :length 10 :width (nth (the-child index ) (the blocks-width)) :height 0) (general-note-sample :type 'general-note :sequence (:size (length (the blocks-note)))
```

---

## index.html (chunk 8/8)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/18/index.html
Type: reference

```
e-child index ) (the blocks-center))) :length 10 :width (nth (the-child index ) (the blocks-width)) :height 0) (general-note-sample :type 'general-note :sequence (:size (length (the blocks-note))) :center (the (title-block (the-child index)) center) :character-size 2.5 :strings (nth (the-child index) (the blocks-note))))) (generate-sample-drawing :objects (list-elements (make-object 'general-note-test)) :projection-direction (getf *standard-views* :top)) Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/21/index.html
Type: reference

```
GendL Application - GLOBAL-FILLETED-POLYLINE-MIXIN Package Documentation Object: GLOBAL-FILLETED-POLYLINE-MIXIN (The :GEOM-BASE Package) Mixins: GLOBAL-POLYLINE-MIXIN Description Generates a polyline with the corners filleted according to default radius or the radius-list. Input Slots (required) VERTEX-LIST [ from GLOBAL-POLYLINE-MIXIN ] list of 3d points The vertices (``corners'') of the polyline. Input Slots (optional) CLOSED? [ from GLOBAL-POLYLINE-MIXIN ] boolean Controls whether the filleted-polyline should automatically be closed. DEFAULT-RADIUS number Specifies a radius to use for all vertices. Radius-list will take precedence over this. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object.
```

---

## index.html (chunk 2/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/21/index.html
Type: reference

```
s nil. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil LOCAL-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. RADIUS-LIST list of numbers Specifies the radius for each vertex (``corner'') of the filleted-polyline. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy).
```

---

## index.html (chunk 3/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/21/index.html
Type: reference

```
leted-polyline. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL.
```

---

## index.html (chunk 4/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/21/index.html
Type: reference

```
subset of hidden-children. Defaults to NIL. Input Slots (optional, defaulting) CENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located. DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern.
```

---

## index.html (chunk 5/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/21/index.html
Type: reference

```
the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. LENGTH [ from BASE-OBJECT ] number Y-axis dimension of the reference box. Defaults to zero. ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). WIDTH [ from BASE-OBJECT ] number X-axis dimension of the reference box. Defaults to zero. Computed Slots STRAIGHTS list of pairs of 3d points Each pair represents the start and end of each straight segment of the filleted-polyline.
```

---

## index.html (chunk 6/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/21/index.html
Type: reference

```
ts to zero. Computed Slots STRAIGHTS list of pairs of 3d points Each pair represents the start and end of each straight segment of the filleted-polyline. Hidden Objects (sequence) FILLETS sequence of fillets Each fillet is essentially an arc representing the curved elbow of the filleted-polyline. Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/11)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/29/index.html
Type: reference

```
GendL Application - LINEAR-DIMENSION Package Documentation Object: LINEAR-DIMENSION (The :GEOM-BASE Package) Mixins: OUTLINE-SPECIALIZATION-MIXIN, BASE-OBJECT Description Creates a dimension along either the horizontal, vertical, or an arbitray axis. Use horizontal-dimension , vertical-dimension , or parallel-dimension , respectively, to achieve these. Input Slots (required) BASE-PLANE-NORMAL Must be specified in the subclass except for angular END-POINT 3d point Actual point where the dimension will stop measuring LEADER-DIRECTION-1-VECTOR Must be specified in the subclass except for angular LEADER-DIRECTION-2-VECTOR Must be specified in the subclass except for angular START-POINT 3d point Actual point where the dimension will start measuring WITNESS-DIRECTION-VECTOR Must be specified in the subclass except for angular Input Slots (optional) ARROWHEAD-LENGTH length (from tip to tail) of arrowhead glyph Defaults to twice the arrowhead-width ARROWHEAD-STYLE keyword symbol Style for
```

---

## index.html (chunk 2/11)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/29/index.html
Type: reference

```
the subclass except for angular Input Slots (optional) ARROWHEAD-LENGTH length (from tip to tail) of arrowhead glyph Defaults to twice the arrowhead-width ARROWHEAD-STYLE keyword symbol Style for arrowhead on end of leader-line . Currently supported values are :none , :wedge (the Default), and :double-wedge . ARROWHEAD-STYLE-2 keyword symbol Style for arrowhead on end of leader-line . Currently supported values are :none (the Default), :wedge , and :double-wedge . ARROWHEAD-WIDTH width of arrowhead glyph Defaults to half the character-size. BOUNDING-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object. CHARACTER-SIZE number Size (glyph height) of the label text, in model units. Defaults to 1. DIM-TEXT string Determines the text which shows up as the dimension label.
```

---

## index.html (chunk 3/11)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/29/index.html
Type: reference

```
ight) of the label text, in model units. Defaults to 1. DIM-TEXT string Determines the text which shows up as the dimension label. Defaults to the dim-value, which is computed specially in each specific dimension type. DIM-TEXT-BIAS keyword symbol, :start, :end, or :center Indicates where to position the text in the case when outside-leaders? is non-nil. Defaults to :center DIM-TEXT-START 3d point Determines where the text will start. Defaults to halfway between start-point and end-point. DIM-TEXT-START-OFFSET 3d vector (normally only 2d are used) . The dim-text-start is offset by this vector, in model space. Defaults to #(0.0 0.0 0.0) DIM-VALUE number 2D distance relative to the base-plane-normal. Can be over-ridden in the subclass DXF-FONT string This names the DXF font for this general-note. Defaults to (the font) . DXF-OFFSET number The start of text will be offset by this amount for DXF output. Default is 2.
```

---

## index.html (chunk 4/11)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/29/index.html
Type: reference

```
note. Defaults to (the font) . DXF-OFFSET number The start of text will be offset by this amount for DXF output. Default is 2. DXF-SIZE-RATIO number The scale factor for DXF character size vs PDF character size. Default is 0.8 DXF-TEXT-X-SCALE number in percentage Adjusts the character width for DXF output. Defaults to the text-x-scale. FLIP-LEADERS? boolean Indicates which direction the witness lines should take from the start and end points. The Default is NIL, which indicates :rear (i.e. ``up'') for horizontal-dimensions and :right for vertical-dimensions FONT string naming a standard pdf font Font for the label text. Defaults to "Helvetica" FULL-LEADER-LINE-LENGTH number Indicates the length of the full leader when outside-leaders? is nil. This defaults to nil, which indicates that the full-leader's length should be auto-computed based on the given start-point and end-point.
```

---

## index.html (chunk 5/11)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/29/index.html
Type: reference

```
full-leader's length should be auto-computed based on the given start-point and end-point. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil JUSTIFICATION keyword symbol, :left, :right, or :center . For multi-line dim-text, this justification is applied. LEADER-1? boolean Indicates whether the first (or only) leader line should be displayed. The Default is T LEADER-2? boolean Indicates whether the second leader line should be displayed.
```

---

## index.html (chunk 6/11)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/29/index.html
Type: reference

```
The Default is T LEADER-LINE-LENGTH number Indicates the length of the first leader for the case when outside-leaders? is non-NIL LEADER-LINE-LENGTH-2 number Indicates the length of the second leader for the case when outside-leaders? is non-NIL LEADER-TEXT-GAP number Amount of gap between leader lines and dimension text, when the dimension text is within the leader. Defaults to half the character-size. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e.
```

---

## index.html (chunk 7/11)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/29/index.html
Type: reference

```
x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). OUTLINE-SHAPE-TYPE keyword symbol Currently can be :bubble, :rectangle, or :none. Default is :none. OUTSIDE-LEADERS-LENGTH-FACTOR number Indicates the default length of the outside-leaders as a multiple of arrowhead-length. Defaults to 3. OUTSIDE-LEADERS? boolean Indicates whether the leader line(s) should be inside or outside the interval between the start and end points. The default is NIL, which indicates that the leader line(s) should be inside the interval ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy).
```

---

## index.html (chunk 8/11)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/29/index.html
Type: reference

```
ould be inside the interval ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. TEXT-ABOVE-LEADER? boolean Indicates whether the text is to the right or above the leader line, rather than in-line with it. Default is T.
```

---

## index.html (chunk 9/11)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/29/index.html
Type: reference

```
it. Default is T. TEXT-ALONG-AXIS? boolean Where applicable, determines whether text direction follows leader-line direction TEXT-X-SCALE number in percentage Adjusts the character width for the dimension-text and currently only applies only to PDF output UNDERLINE? GDL VIEW-REFERENCE-OBJECT gdl object or nil View object which will use this dimension. Defaults to NIL. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. WITNESS-LINE-2? boolean Indicates whether to display a witness line coming off the end-point . Default is T WITNESS-LINE-EXT number Distance the witness line(s) extend beyond the leader line. Default is 0.3 WITNESS-LINE-GAP number Distance from the start-point and end-point to the start of each witness-line.
```

---

## index.html (chunk 10/11)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/29/index.html
Type: reference

```
rt-point and end-point to the start of each witness-line. Default is 0.1 WITNESS-LINE-LENGTH number Length of the witness lines (or of the shorter witness line in case they are different lengths) WITNESS-LINE? boolean Indicates whether to display a witness line coming off the start-point . Default is T Input Slots (optional, defaulting) CENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located. DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only).
```

---

## index.html (chunk 11/11)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/29/index.html
Type: reference

```
o :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. LENGTH [ from BASE-OBJECT ] number Y-axis dimension of the reference box. Defaults to zero. WIDTH [ from BASE-OBJECT ] number X-axis dimension of the reference box. Defaults to zero. Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/13)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/30/index.html
Type: reference

```
GendL Application - PARALLEL-DIMENSION Package Documentation Object: PARALLEL-DIMENSION (The :GEOM-BASE Package) Mixins: LINEAR-DIMENSION Description Creates a dimension annotation along an axis from a start point to an end point. Input Slots (required) END-POINT [ from LINEAR-DIMENSION ] 3d point Actual point where the dimension will stop measuring START-POINT [ from LINEAR-DIMENSION ] 3d point Actual point where the dimension will start measuring Input Slots (optional) ARROWHEAD-LENGTH [ from LINEAR-DIMENSION ] length (from tip to tail) of arrowhead glyph Defaults to twice the arrowhead-width ARROWHEAD-STYLE [ from LINEAR-DIMENSION ] keyword symbol Style for arrowhead on end of leader-line . Currently supported values are :none , :wedge (the Default), and :double-wedge . ARROWHEAD-STYLE-2 [ from LINEAR-DIMENSION ] keyword symbol Style for arrowhead on end of leader-line . Currently supported values are :none (the Default), :wedge , and :double-wedge .
```

---

## index.html (chunk 2/13)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/30/index.html
Type: reference

```
2 [ from LINEAR-DIMENSION ] keyword symbol Style for arrowhead on end of leader-line . Currently supported values are :none (the Default), :wedge , and :double-wedge . ARROWHEAD-WIDTH [ from LINEAR-DIMENSION ] width of arrowhead glyph Defaults to half the character-size. BOUNDING-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object. CHARACTER-SIZE [ from LINEAR-DIMENSION ] number Size (glyph height) of the label text, in model units. Defaults to 1. DIM-TEXT [ from LINEAR-DIMENSION ] string Determines the text which shows up as the dimension label. Defaults to the dim-value, which is computed specially in each specific dimension type. DIM-TEXT-BIAS [ from LINEAR-DIMENSION ] keyword symbol, :start, :end, or :center Indicates where to position the text in the case when outside-leaders? is non-nil.
```

---

## index.html (chunk 3/13)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/30/index.html
Type: reference

```
TEXT-BIAS [ from LINEAR-DIMENSION ] keyword symbol, :start, :end, or :center Indicates where to position the text in the case when outside-leaders? is non-nil. Defaults to :center DIM-TEXT-START 3d point Determines where the text will start. Defaults to reasonable location for horizontal-dimension. DIM-TEXT-START-OFFSET [ from LINEAR-DIMENSION ] 3d vector (normally only 2d are used) . The dim-text-start is offset by this vector, in model space. Defaults to #(0.0 0.0 0.0) DIM-VALUE [ from LINEAR-DIMENSION ] number 2D distance relative to the base-plane-normal. Can be over-ridden in the subclass DXF-FONT [ from LINEAR-DIMENSION ] string This names the DXF font for this general-note. Defaults to (the font) . DXF-OFFSET [ from LINEAR-DIMENSION ] number The start of text will be offset by this amount for DXF output. Default is 2. DXF-SIZE-RATIO [ from LINEAR-DIMENSION ] number The scale factor for DXF character size vs PDF character size.
```

---

## index.html (chunk 4/13)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/30/index.html
Type: reference

```
amount for DXF output. Default is 2. DXF-SIZE-RATIO [ from LINEAR-DIMENSION ] number The scale factor for DXF character size vs PDF character size. Default is 0.8 DXF-TEXT-X-SCALE [ from LINEAR-DIMENSION ] number in percentage Adjusts the character width for DXF output. Defaults to the text-x-scale. FLIP-LEADERS? [ from LINEAR-DIMENSION ] boolean Indicates which direction the witness lines should take from the start and end points. The Default is NIL, which indicates :rear (i.e. ``up'') for horizontal-dimensions and :right for vertical-dimensions FONT [ from LINEAR-DIMENSION ] string naming a standard pdf font Font for the label text. Defaults to "Helvetica" FULL-LEADER-LINE-LENGTH [ from LINEAR-DIMENSION ] number Indicates the length of the full leader when outside-leaders? is nil. This defaults to nil, which indicates that the full-leader's length should be auto-computed based on the given start-point and end-point.
```

---

## index.html (chunk 5/13)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/30/index.html
Type: reference

```
efaults to nil, which indicates that the full-leader's length should be auto-computed based on the given start-point and end-point. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil JUSTIFICATION [ from LINEAR-DIMENSION ] keyword symbol, :left, :right, or :center . For multi-line dim-text, this justification is applied. LEADER-1? [ from LINEAR-DIMENSION ] boolean Indicates whether the first (or only) leader line should be displayed. The Default is T LEADER-2? [ from LINEAR-DIMENSION ] boolean Indicates whether the second leader line should be displayed.
```

---

## index.html (chunk 6/13)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/30/index.html
Type: reference

```
line should be displayed. The Default is T LEADER-LINE-LENGTH [ from LINEAR-DIMENSION ] number Indicates the length of the first leader for the case when outside-leaders? is non-NIL LEADER-LINE-LENGTH-2 [ from LINEAR-DIMENSION ] number Indicates the length of the second leader for the case when outside-leaders? is non-NIL LEADER-TEXT-GAP [ from LINEAR-DIMENSION ] number Amount of gap between leader lines and dimension text, when the dimension text is within the leader. Defaults to half the character-size. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM.
```

---

## index.html (chunk 7/13)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/30/index.html
Type: reference

```
nt-ends which support this functionality, e.g. SVG/Raphael and X3DOM. ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). OUTLINE-SHAPE-TYPE [ from LINEAR-DIMENSION ] keyword symbol Currently can be :bubble, :rectangle, or :none. Default is :none. OUTSIDE-LEADERS-LENGTH-FACTOR [ from LINEAR-DIMENSION ] number Indicates the default length of the outside-leaders as a multiple of arrowhead-length. Defaults to 3. OUTSIDE-LEADERS? [ from LINEAR-DIMENSION ] boolean Indicates whether the leader line(s) should be inside or outside the interval between the start and end points.
```

---

## index.html (chunk 8/13)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/30/index.html
Type: reference

```
boolean Indicates whether the leader line(s) should be inside or outside the interval between the start and end points. The default is NIL, which indicates that the leader line(s) should be inside the interval ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence.
```

---

## index.html (chunk 9/13)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/30/index.html
Type: reference

```
by an index number if the part is an element of a sequence. TEXT-ABOVE-LEADER? [ from LINEAR-DIMENSION ] boolean Indicates whether the text is to the right or above the leader line, rather than in-line with it. Default is T. TEXT-ALONG-AXIS? [ from LINEAR-DIMENSION ] boolean Where applicable, determines whether text direction follows leader-line direction TEXT-X-SCALE [ from LINEAR-DIMENSION ] number in percentage Adjusts the character width for the dimension-text and currently only applies only to PDF output UNDERLINE? [ from LINEAR-DIMENSION ] GDL VIEW-REFERENCE-OBJECT [ from LINEAR-DIMENSION ] gdl object or nil View object which will use this dimension. Defaults to NIL. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. WITNESS-LINE-2? [ from LINEAR-DIMENSION ] boolean Indicates whether to display a witness line coming off the end-point .
```

---

## index.html (chunk 10/13)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/30/index.html
Type: reference

```
this would be a subset of hidden-children. Defaults to NIL. WITNESS-LINE-2? [ from LINEAR-DIMENSION ] boolean Indicates whether to display a witness line coming off the end-point . Default is T WITNESS-LINE-EXT [ from LINEAR-DIMENSION ] number Distance the witness line(s) extend beyond the leader line. Default is 0.3 WITNESS-LINE-GAP [ from LINEAR-DIMENSION ] number Distance from the start-point and end-point to the start of each witness-line. Default is 0.1 WITNESS-LINE-LENGTH [ from LINEAR-DIMENSION ] number Length of the witness lines (or of the shorter witness line in case they are different lengths) WITNESS-LINE? [ from LINEAR-DIMENSION ] boolean Indicates whether to display a witness line coming off the start-point . Default is T Input Slots (optional, defaulting) CENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located.
```

---

## index.html (chunk 11/13)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/30/index.html
Type: reference

```
OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located. DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero.
```

---

## index.html (chunk 12/13)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/30/index.html
Type: reference

```
e line or curve to start the dash pattern. HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. LENGTH [ from BASE-OBJECT ] number Y-axis dimension of the reference box. Defaults to zero. WIDTH [ from BASE-OBJECT ] number X-axis dimension of the reference box. Defaults to zero. Computed Slots BASE-PLANE-NORMAL [ from LINEAR-DIMENSION ] Must be specified in the subclass except for angular LEADER-DIRECTION-1-VECTOR [ from LINEAR-DIMENSION ] Must be specified in the subclass except for angular LEADER-DIRECTION-2-VECTOR [ from LINEAR-DIMENSION ] Must be specified in the subclass except for angular WITNESS-DIRECTION-VECTOR [ from LINEAR-DIMENSION ] Must be specified in the subclass except for angular Examples (in-package :gdl-user) (define-object parallel-dimension-sample (base-object) :objects ((box :type 'box :length 10 :width (* (the-child length) +phi+) :height (* (the-child :width) +phi+)) (length-dimension :type 'parallel-dimension
```

---

## index.html (chunk 13/13)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/30/index.html
Type: reference

```
dimension-sample (base-object) :objects ((box :type 'box :length 10 :width (* (the-child length) +phi+) :height (* (the-child :width) +phi+)) (length-dimension :type 'parallel-dimension :character-size (/ (the box length) 20) :start-point (the box (vertex :top :left :front)) :end-point (the box (vertex :top :right :rear))))) (generate-sample-drawing :object-roots (make-object 'parallel-dimension-sample)) Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/11/index.html
Type: reference

```
GendL Application - CIRCLE Package Documentation Object: CIRCLE (The :GEOM-BASE Package) Mixins: ARC Description The set of points equidistant from a given point. The distance from the center is called the radius, and the point is called the center. The start point of the circle is at the 3 o'clock position, and positive angles are measured anti-clockwise. Input Slots (required) RADIUS [ from ARC ] number Distance from center to any point on the arc. Input Slots (optional) BOUNDING-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object.
```

---

## index.html (chunk 2/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/11/index.html
Type: reference

```
t even if specified in :objects. Default is nil. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil LOCAL-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy).
```

---

## index.html (chunk 3/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/11/index.html
Type: reference

```
root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, defaulting) CENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located.
```

---

## index.html (chunk 4/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/11/index.html
Type: reference

```
nal, defaulting) CENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located. DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern.
```

---

## index.html (chunk 5/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/11/index.html
Type: reference

```
ird number indicates how far into the line or curve to start the dash pattern. ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). Computed Slots AREA number The area enclosed by the circle. CIRCUMFERENCE number The perimeter of the circle. END-ANGLE [ from ARC ] angle in radians End angle of the arc. Defaults to twice pi. START-ANGLE [ from ARC ] angle in radians Start angle of the arc. Defaults to zero. Examples (in-package :gdl-user) (define-object circle-sample (circle) :computed-slots ((radius 10))) (generate-sample-drawing :objects (make-object 'circle-sample)) Package Documentation Copyright © 2025 Genworks ® International .
```

---

## index.html (chunk 6/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/11/index.html
Type: reference

```
circle-sample (circle) :computed-slots ((radius 10))) (generate-sample-drawing :objects (make-object 'circle-sample)) Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/38/index.html
Type: reference

```
GendL Application - SPHERICAL-CAP Package Documentation Object: SPHERICAL-CAP (The :GEOM-BASE Package) Mixins: IFS-OUTPUT-MIXIN, ARCOID-MIXIN , BASE-OBJECT Description The region of a sphere which lies above (or below) a given plane. Although this could be created with a partial sphere using the sphere primitive, the spherical cap allows for more convenient construction and positioning since the actual center of the spherical cap is the center of its reference box. Input Slots (required) AXIS-LENGTH number The distance from the center of the base to the center of the dome. BASE-RADIUS number Radius of the base. RADIUS [ from ARCOID-MIXIN ] number Distance from center to any point on the arc. Input Slots (optional) BOUNDING-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object.
```

---

## index.html (chunk 2/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/38/index.html
Type: reference

```
m and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object. CAP-THICKNESS number Thickness of the shell for a hollow spherical-cap. Specify this or inner-base-radius, not both. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil INNER-BASE-RADIUS number Radius of base of inner for a hollow spherical-cap. Specify this or cap-thickness, not both. LOCAL-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object. NUMBER-OF-HORIZONTAL-SECTIONS integer How many lines of latitude to show on the spherical-cap in some renderings.
```

---

## index.html (chunk 3/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/38/index.html
Type: reference

```
lobal coordinates, of the rectangular volume bounding this geometric object. NUMBER-OF-HORIZONTAL-SECTIONS integer How many lines of latitude to show on the spherical-cap in some renderings. Default value is 2. NUMBER-OF-VERTICAL-SECTIONS integer How many lines of longitude to show on the spherical-cap in some renderings. Default value is 2. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists.
```

---

## index.html (chunk 4/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/38/index.html
Type: reference

```
hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, defaulting) CENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located.
```

---

## index.html (chunk 5/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/38/index.html
Type: reference

```
t Indicates in global coordinates where the center of the reference box of this object should be located. DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object.
```

---

## index.html (chunk 6/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/38/index.html
Type: reference

```
to start the dash pattern. ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). Computed Slots END-ANGLE [ from ARCOID-MIXIN ] angle in radians End angle of the arc. Defaults to twice pi. HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. LENGTH [ from BASE-OBJECT ] number Y-axis dimension of the reference box. Defaults to zero. SPHERE-CENTER 3d point Center of the sphere containing the spherical-cap. SPHERE-RADIUS number Radius of the sphere containing the spherical-cap. START-ANGLE [ from ARCOID-MIXIN ] angle in radians Start angle of the arc. Defaults to zero.
```

---

## index.html (chunk 7/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/38/index.html
Type: reference

```
US number Radius of the sphere containing the spherical-cap. START-ANGLE [ from ARCOID-MIXIN ] angle in radians Start angle of the arc. Defaults to zero. WIDTH [ from BASE-OBJECT ] number X-axis dimension of the reference box. Defaults to zero. Examples (in-package :gdl-user) (define-object spherical-cap-sample (spherical-cap) :computed-slots ((base-radius 150) (cap-thickness 7) (axis-length (* (the base-radius) +phi+)) (number-of-vertical-sections 10) (number-of-horizontal-sections 10) (display-controls (list :color :orchid-medium :transparency 0.5)))) (generate-sample-drawing :objects (make-object 'spherical-cap-sample) :projection-direction :trimetric) Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/35/index.html
Type: reference

```
GendL Application - ROUTE-PIPE Package Documentation Object: ROUTE-PIPE (The :GEOM-BASE Package) Mixins: GLOBAL-FILLETED-POLYLINE-MIXIN , OUTLINE-SPECIALIZATION-MIXIN Description Defines an alternating set of cylinders and torus sections for the elbows Input Slots (required) OUTER-PIPE-RADIUS number Radius to the outer surface of the piping. VERTEX-LIST list of 3d points Same as for global-filleted-polyline (which is mixed in to this part) Input Slots (optional) CLOSED? [ from GLOBAL-POLYLINE-MIXIN ] boolean Controls whether the filleted-polyline should automatically be closed. DEFAULT-RADIUS [ from GLOBAL-FILLETED-POLYLINE-MIXIN ] number Specifies a radius to use for all vertices. Radius-list will take precedence over this. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil.
```

---

## index.html (chunk 2/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/35/index.html
Type: reference

```
ect should effectively be a hidden-object even if specified in :objects. Default is nil. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil INNER-PIPE-RADIUS number Radius of the inner hollow part of the piping. NIL for a solid pipe. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. RADIUS-LIST [ from GLOBAL-FILLETED-POLYLINE-MIXIN ] list of numbers Specifies the radius for each vertex (``corner'') of the filleted-polyline. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy).
```

---

## index.html (chunk 3/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/35/index.html
Type: reference

```
h vertex (``corner'') of the filleted-polyline. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL.
```

---

## index.html (chunk 4/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/35/index.html
Type: reference

```
tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, defaulting) CENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located. DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern.
```

---

## index.html (chunk 5/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/35/index.html
Type: reference

```
dicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. Computed Slots BOUNDING-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object. HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. LENGTH [ from BASE-OBJECT ] number Y-axis dimension of the reference box. Defaults to zero. ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0).
```

---

## index.html (chunk 6/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/35/index.html
Type: reference

```
espect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). WIDTH [ from BASE-OBJECT ] number X-axis dimension of the reference box. Defaults to zero. Examples (in-package :gdl-user) (define-object route-pipe-sample (base-object) :objects ((pipe :type 'route-pipe :vertex-list (list #(410.36 436.12 664.68) #(404.21 436.12 734.97) #(402.22 397.48 757.72) #(407.24 397.48 801.12) #(407.24 448.0 837.0) #(346.76 448.0 837.0)) :default-radius 19 :outer-pipe-radius 7 :inner-pipe-radius nil :display-controls (list :color :blue-steel :transparency 0.0 :shininess 0.7 :spectral-color :white)))) (generate-sample-drawing :objects (the-object (make-object 'route-pipe-sample) pipe) :projection-direction (getf *standard-views* :trimetric)) Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 7/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/35/index.html
Type: reference

```
:projection-direction (getf *standard-views* :trimetric)) Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/5)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/13/index.html
Type: reference

```
GendL Application - CONSTRAINED-ARC Package Documentation Object: CONSTRAINED-ARC (The :GEOM-BASE Package) Mixins: ARC Description This object is intended to simplify the process of constructing lines using various constraints. Currently supported are 2 through-points or 1 through-point and at-angle. Note the line-constraints must be an evaluatable s-expression as this is not processed as a macro Input Slots (optional) BOUNDING-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object. END-ANGLE [ from ARC ] angle in radians End angle of the arc. Defaults to twice pi. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil.
```

---

## index.html (chunk 2/5)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/13/index.html
Type: reference

```
effectively be a hidden-object even if specified in :objects. Default is nil. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil LOCAL-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy).
```

---

## index.html (chunk 3/5)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/13/index.html
Type: reference

```
LA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information START-ANGLE [ from ARC ] angle in radians Start angle of the arc. Defaults to zero. STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL.
```

---

## index.html (chunk 4/5)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/13/index.html
Type: reference

```
to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, defaulting) DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern.
```

---

## index.html (chunk 5/5)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/13/index.html
Type: reference

```
into the line or curve to start the dash pattern. Computed Slots CENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located. ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). RADIUS [ from ARC ] number Distance from center to any point on the arc. Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/1/index.html
Type: reference

```
GendL Application - ARC Package Documentation Object: ARC (The :GEOM-BASE Package) Mixins: ARCOID-MIXIN , BASE-OBJECT Description A segment of a circle. The start point is at the 3 o'clock position, and positive angles are measured anti-clockwise. Input Slots (required) RADIUS number Distance from center to any point on the arc. Input Slots (optional) BOUNDING-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object. END-ANGLE angle in radians End angle of the arc. Defaults to twice pi. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object.
```

---

## index.html (chunk 2/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/1/index.html
Type: reference

```
E-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil LOCAL-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists.
```

---

## index.html (chunk 3/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/1/index.html
Type: reference

```
ierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information START-ANGLE angle in radians Start angle of the arc. Defaults to zero. STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, defaulting) CENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located.
```

---

## index.html (chunk 4/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/1/index.html
Type: reference

```
Input Slots (optional, defaulting) CENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located. DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern.
```

---

## index.html (chunk 5/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/1/index.html
Type: reference

```
e. The optional third number indicates how far into the line or curve to start the dash pattern. ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). Computed Slots END 3d point The end point of the arc. HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. LENGTH [ from BASE-OBJECT ] number Y-axis dimension of the reference box. Defaults to zero. START 3d point The start point of the arc. WIDTH [ from BASE-OBJECT ] number X-axis dimension of the reference box. Defaults to zero.
```

---

## index.html (chunk 6/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/1/index.html
Type: reference

```
number X-axis dimension of the reference box. Defaults to zero. GDL Functions EQUI-SPACED-POINTS list of points Returns a list of points equally spaced around the arc, including the start and end point of the arc. optional arguments: number-of-points Number , Default Value: 4 How many points to return POINT-ON-ARC 3d point The point on the arc at a certain angle from the start. arguments: angle Number in Radians TANGENT 3d vector Returns the tangent to the arc at the given point (which should be on the arc). arguments: point 3D point The point at which you want the tangent Examples (in-package :gdl-user) (define-object arc-sample (arc) :computed-slots ((radius 30) (end-angle (half pi/2)))) (generate-sample-drawing :objects (make-object 'arc-sample)) Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 7/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/1/index.html
Type: reference

```
Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/31/index.html
Type: reference

```
GendL Application - PIE-CHART Package Documentation Object: PIE-CHART (The :GEOM-BASE Package) Mixins: BASE-OBJECT Description Generates a standard Pie Chart with colored filled pie sections. This object was inspired by the pie-chart in Marc Battyani's (marc.battyani(at)fractalconcept.com) cl-pdf, with contributions from Carlos Ungil (Carlos.Ungil(at)cern.ch). Input Slots (optional) BOUNDING-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object. DATA list of numbers The relative size for each pie piece. These will be normalized to percentages. Defaults to NIL, must be specified as non-NIL to get a result. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil.
```

---

## index.html (chunk 2/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/31/index.html
Type: reference

```
olean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil INCLUDE-LEGEND? boolean Determines whether the Legend is included in standard output formats. Defaults to t . LABELS&COLORS list of lists, each containing a string and a keyword symbol This list should be the same length as data . These colors and labels will be assigned to each pie piece and to the legend. Defaults to NIL, must be specified as non-NIL to get a result. LINE-COLOR keyword symbol naming color from *color-table* . Color of the outline of the pie. Defaults to :black. LOCAL-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object.
```

---

## index.html (chunk 3/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/31/index.html
Type: reference

```
ist of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. RADIUS number The radius of the pie. Defaults to 0.35 times the width . ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists.
```

---

## index.html (chunk 4/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/31/index.html
Type: reference

```
luding elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. TITLE string Title for the chart. Defaults to the empty string. TITLE-COLOR keyword symbol naming color from *color-table* . Color of title text. Defaults to :black. TITLE-FONT string Currently this must be a PDF font name. Defaults to "Helvetica." TITLE-FONT-SIZE number Size in points of title font. Defaults to 12. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL.
```

---

## index.html (chunk 5/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/31/index.html
Type: reference

```
gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, defaulting) CENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located. DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line.
```

---

## index.html (chunk 6/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/31/index.html
Type: reference

```
ly). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. LENGTH [ from BASE-OBJECT ] number Y-axis dimension of the reference box. Defaults to zero. ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). WIDTH [ from BASE-OBJECT ] number X-axis dimension of the reference box. Defaults to zero.
```

---

## index.html (chunk 7/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/31/index.html
Type: reference

```
.0). WIDTH [ from BASE-OBJECT ] number X-axis dimension of the reference box. Defaults to zero. Examples (in-package :gdl-user) (define-object pie-sample (pie-chart) :computed-slots ((data (list 30 70)) (labels&colors '(("Expenses" :red) ("Revenue" :green))) (width 200) (title "Cash Flow"))) (generate-sample-drawing :objects (make-object 'pie-sample)) Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/17/index.html
Type: reference

```
GendL Application - ELLIPSE Package Documentation Object: ELLIPSE (The :GEOM-BASE Package) Mixins: ARCOID-MIXIN , BASE-OBJECT Description A curve which is the locus of all points in the plane the sum of whose distances from two fixed points (the foci) is a given positive constant. This is a simplified 3D ellipse which will snap to the nearest quarter if you make it a partial ellipse. For a full ellipse, do not specify start-angle or end-angle. Input Slots (required) MAJOR-AXIS-LENGTH number Length of (generally) the longer ellipse axis MINOR-AXIS-LENGTH number Length of (generally) the shorter ellipse axis RADIUS [ from ARCOID-MIXIN ] number Distance from center to any point on the arc. Input Slots (optional) BOUNDING-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object. END-ANGLE angle in radians End angle of the ellipse.
```

---

## index.html (chunk 2/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/17/index.html
Type: reference

```
right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object. END-ANGLE angle in radians End angle of the ellipse. Defaults to 2pi for full ellipse. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil LOCAL-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation .
```

---

## index.html (chunk 3/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/17/index.html
Type: reference

```
ers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information START-ANGLE angle in radians Start angle of the ellipse. Defaults to 0 for full ellipse. STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places.
```

---

## index.html (chunk 4/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/17/index.html
Type: reference

```
me of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, defaulting) CENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located. DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black.
```

---

## index.html (chunk 5/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/17/index.html
Type: reference

```
meter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). Computed Slots HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero.
```

---

## index.html (chunk 6/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/17/index.html
Type: reference

```
hould be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). Computed Slots HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. LENGTH [ from BASE-OBJECT ] number Y-axis dimension of the reference box. Defaults to zero. WIDTH [ from BASE-OBJECT ] number X-axis dimension of the reference box. Defaults to zero. Examples (in-package :gdl-user) (define-object ellipse-sample (ellipse) :computed-slots ((minor-axis-length 10) (major-axis-length (* (the minor-axis-length) +phi+)) (start-angle 0) (end-angle pi))) (generate-sample-drawing :objects (make-object 'ellipse-sample)) Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/19/index.html
Type: reference

```
GendL Application - GLOBAL-FILLETED-POLYGON-PROJECTION Package Documentation Object: GLOBAL-FILLETED-POLYGON-PROJECTION (The :GEOM-BASE Package) Mixins: GLOBAL-POLYGON-PROJECTION Description Similar to a global-polygon-projection, but the polygon is filleted as with global-filleted-polygon. Input Slots (required) PROJECTION-DEPTH [ from GLOBAL-POLYGON-PROJECTION ] number The resultant distance from the two end faces of the extrusion. VERTEX-LIST [ from GLOBAL-POLYGON-PROJECTION ] list of 3d points The vertex list making up the polyline, same as the input for global-polyline. Input Slots (optional) DEFAULT-RADIUS number Specifies a radius to use for all vertices. Radius-list will take precedence over this. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil.
```

---

## index.html (chunk 2/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/19/index.html
Type: reference

```
ly be a hidden-object even if specified in :objects. Default is nil. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil LOCAL-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . OFFSET [ from GLOBAL-POLYGON-PROJECTION ] keyword symbol The direction of extrusion with respect to the vertices in vertex-list and the projection-vector: :up Indicates to start from current location of vertices and move in the direction of the projection-vector. :down Indicates to start from current location of vertices and move in the direction opposite the projection-vector.
```

---

## index.html (chunk 3/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/19/index.html
Type: reference

```
. :down Indicates to start from current location of vertices and move in the direction opposite the projection-vector. :center Indicates to start from current location of vertices and move in the direction of the projection-vector and opposite the projection-vector, going half the projection-depth in each direction. ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. PROJECTION-VECTOR [ from GLOBAL-POLYGON-PROJECTION ] 3d vector Indicates the straight path along which the extrusion should occur. RADIUS-LIST list of numbers Specifies the radius for each vertex (``corner'') of the filleted-polyline. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy).
```

---

## index.html (chunk 4/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/19/index.html
Type: reference

```
VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, defaulting) CENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located.
```

---

## index.html (chunk 5/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/19/index.html
Type: reference

```
Defaults to NIL. Input Slots (optional, defaulting) CENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located. DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box.
```

---

## index.html (chunk 6/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/19/index.html
Type: reference

```
s in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. LENGTH [ from BASE-OBJECT ] number Y-axis dimension of the reference box. Defaults to zero. ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). WIDTH [ from BASE-OBJECT ] number X-axis dimension of the reference box. Defaults to zero.
```

---

## index.html (chunk 7/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/19/index.html
Type: reference

```
Examples (in-package :gdl-user) (define-object global-filleted-polygon-projection-sample (global-filleted-polygon-projection) :computed-slots ((display-controls (list :color :blue-steel :transparency 0.3 :shininess 0.7 :spectral-color :white)) (default-radius 5) (projection-depth 5) (vertex-list (list (make-point 0 0 0) (make-point 10 10 0) (make-point 30 10 0) (make-point 40 0 0) (make-point 30 -10 0) (make-point 10 -10 0) (make-point 0 0 0))))) (generate-sample-drawing :objects (make-object 'global-filleted-polygon-projection-sample) :projection-direction :trimetric) Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/3/index.html
Type: reference

```
GendL Application - BASE-COORDINATE-SYSTEM Package Documentation Object: BASE-COORDINATE-SYSTEM (The :GEOM-BASE Package) Mixins: BASE-OBJECT , VANILLA-MIXIN Description This provides a default 3D Cartesian coordinate system. It mixes in base-object and does not extend it in any way, so as with base-object, it provides an imaginary geometric reference box with a length, width, height, center, and orientation. Input Slots (optional) BOUNDING-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object.
```

---

## index.html (chunk 2/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/3/index.html
Type: reference

```
nil. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil LOCAL-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists.
```

---

## index.html (chunk 3/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/3/index.html
Type: reference

```
instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, defaulting) CENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located.
```

---

## index.html (chunk 4/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/3/index.html
Type: reference

```
] 3d point Indicates in global coordinates where the center of the reference box of this object should be located. DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero.
```

---

## index.html (chunk 5/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/3/index.html
Type: reference

```
or curve to start the dash pattern. HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. LENGTH [ from BASE-OBJECT ] number Y-axis dimension of the reference box. Defaults to zero. ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). WIDTH [ from BASE-OBJECT ] number X-axis dimension of the reference box. Defaults to zero. Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 6/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/3/index.html
Type: reference

```
ks Build: 1598p001
```

---

## index.html (chunk 1/10)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/6/index.html
Type: reference

```
GendL Application - BASE-VIEW Package Documentation Object: BASE-VIEW (The :GEOM-BASE Package) Mixins: BASE-OBJECT Description Generic container object for displaying a scaled transformed view of geometric or text-based objects. Base-view can be used by itself or as a child of a base-drawing In a GWL application-mixin, you can include an object of this type in the ui-display-list-leaves. For the PDF output-format, you can also use the cad-output output-function to write the view as a PDF document. Since base-view is inherently a 2D object, only the top view (getf *standard-views* :top) makes sense for viewing it. Input Slots (optional) ANNOTATION-OBJECTS list of gdl objects These objects will be displayed in each view by default, with no scaling or transform (i.e. they are in Drawing space. BORDER-BOX? boolean Determines whether a rectangular border box is drawn around the view, with the view's length and width. Defaults to nil.
```

---

## index.html (chunk 2/10)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/6/index.html
Type: reference

```
. BORDER-BOX? boolean Determines whether a rectangular border box is drawn around the view, with the view's length and width. Defaults to nil. BOUNDING-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object. CENTER 3d-point Center of the view box. Specify this or corner, not both. note: that the center is no longer defaulting (so that it can self-compute properly when corner is specified), so it is necessary to explicitly give either start or center for base-view. CORNER 3d-point Top left (i.e. rear left from top view) of the view box. Specify this or center, not both. FRONT-MARGIN number in drawing scale (e g. points). Amount of margin on front and rear of page when view-scale is to be computed automatically. Defaults to 25.
```

---

## index.html (chunk 3/10)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/6/index.html
Type: reference

```
front and rear of page when view-scale is to be computed automatically. Defaults to 25. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil IMMUNE-OBJECTS list of gdl objects These objects are immune from view scaling and transform computations and so can freely refer to the view-scale, view-center, and other view information for self-scaling views. Defaults to NIL. LEFT-MARGIN number in drawing scale (e g. points). Amount of margin on left and right of page when view-scale is to be computed automatically. Defaults to 25. LOCAL-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object.
```

---

## index.html (chunk 4/10)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/6/index.html
Type: reference

```
OX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object. OBJECT-ROOTS list of gdl objects The leaves from each of these objects will be displayed in each view by default. OBJECTS list of gdl objects These objects will be displayed in each view by default. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. PROJECTION-VECTOR 3d unitized vector Direction of camera pointing to model (the object-roots and/or the objects) to create this view.
```

---

## index.html (chunk 5/10)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/6/index.html
Type: reference

```
to model (the object-roots and/or the objects) to create this view. The view is automatically ``twisted''about this vector to result in ``up'' being as close as possible to the Z vector, unless this vector is parallel to the Z vector in which case ``up'' is taken to be the Y (rear) vector. This vector is normally taken from the *standard-views* built-in GDL parameter. Defaults to (getf *standard-views* :top) , which is the vector [0, 0, 1]. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information SNAP-TO 3d vector For a top view, this vector specifies the direction that the rear of the box should be facing. Defaults to *nominal-y-vector* .
```

---

## index.html (chunk 6/10)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/6/index.html
Type: reference

```
For a top view, this vector specifies the direction that the rear of the box should be facing. Defaults to *nominal-y-vector* . STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VIEW-CENTER 3d point in model space Point relative to each object's center to use as center of the view. VIEW-SCALE number Ratio of drawing scale (in points) to model scale for this view. Defaults to being auto-computed. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL.
```

---

## index.html (chunk 7/10)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/6/index.html
Type: reference

```
et of hidden-children. Defaults to NIL. Input Slots (optional, defaulting) DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. LENGTH [ from BASE-OBJECT ] number Y-axis dimension of the reference box. Defaults to zero.
```

---

## index.html (chunk 8/10)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/6/index.html
Type: reference

```
HT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. LENGTH [ from BASE-OBJECT ] number Y-axis dimension of the reference box. Defaults to zero. ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). WIDTH [ from BASE-OBJECT ] number X-axis dimension of the reference box. Defaults to zero. GDL Functions MODEL-POINT 3d point Takes point in view coordinates and returns corresponding point in model coordinates. arguments: view-point 3D Point Point in view coordinates VIEW-POINT 3d point Takes point in model coordinates and returns corresponding point in view coordinates.
```

---

## index.html (chunk 9/10)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/6/index.html
Type: reference

```
iew-point 3D Point Point in view coordinates VIEW-POINT 3d point Takes point in model coordinates and returns corresponding point in view coordinates. arguments: model-point 3D Point Point in model coordinates Examples (in-package :gdl-user) (define-object box-with-two-viewed-drawing (base-object) :objects ((drawing :type 'two-viewed-drawing :objects-to-draw (list (the box) (the length-dim))) (length-dim :type 'horizontal-dimension :hidden? t :start-point (the box (vertex :rear :top :left)) :end-point (the box (vertex :rear :top :right))) (box :type 'box :hidden? t :length 5 :width 10 :height 15))) (define-object two-viewed-drawing (base-drawing) :input-slots (objects-to-draw) :objects ((main-view :type 'base-view :projection-vector (getf *standard-views* :trimetric) :length (half (the length)) :center (translate (the center) :rear (half (the-child length))) :objects (the objects-to-draw)) (top-view :type 'base-view :projection-vector (getf *standard-views* :top) :length (* 0.30 (the
```

---

## index.html (chunk 10/10)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/6/index.html
Type: reference

```
gth)) :center (translate (the center) :rear (half (the-child length))) :objects (the objects-to-draw)) (top-view :type 'base-view :projection-vector (getf *standard-views* :top) :length (* 0.30 (the length)) :objects (the objects-to-draw)))) (generate-sample-drawing :objects (the-object (make-object 'box-with-two-viewed-drawing) drawing top-view)) Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/12/index.html
Type: reference

```
GendL Application - CONE Package Documentation Object: CONE (The :GEOM-BASE Package) Mixins: CYLINDER Description A pyramid with a circular cross section, with its vertex above the center of its base. Partial cones and hollow cones are supported. Input Slots (required) LENGTH [ from CYLINDER ] number Distance from center of start cap to center of end cap. RADIUS [ from CYLINDER ] number Radius of the circular cross section of the cylinder. Input Slots (optional) BOTTOM-CAP? [ from CYLINDER ] boolean Determines whether to include bottom cap in shaded renderings. Defaults to T. BOUNDING-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object. CLOSED? [ from CYLINDER ] boolean Indicates that a partial cylinder (or cone) should have a closed gap. END-ANGLE [ from ARCOID-MIXIN ] angle in radians End angle of the arc. Defaults to twice pi.
```

---

## index.html (chunk 2/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/12/index.html
Type: reference

```
CLOSED? [ from CYLINDER ] boolean Indicates that a partial cylinder (or cone) should have a closed gap. END-ANGLE [ from ARCOID-MIXIN ] angle in radians End angle of the arc. Defaults to twice pi. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil INNER-RADIUS [ from CYLINDER ] number Radius of the hollow inner portion for a hollow cylinder. INNER-RADIUS-1 number The radius of the inner hollow part at the top end for a hollow cone. INNER-RADIUS-2 number The radius of the inner hollow part at the bottom end for a hollow cone. LOCAL-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object.
```

---

## index.html (chunk 3/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/12/index.html
Type: reference

```
AL-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object. NUMBER-OF-SECTIONS [ from CYLINDER ] integer Number of vertical sections to be drawn in wireframe rendering mode. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. RADIUS-1 number The radius of the top end of the cone. RADIUS-2 number The radius of the bottom end of the cone. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy).
```

---

## index.html (chunk 4/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/12/index.html
Type: reference

```
nstance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information START-ANGLE [ from ARCOID-MIXIN ] angle in radians Start angle of the arc. Defaults to zero. STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. TOP-CAP? [ from CYLINDER ] boolean Determines whether to include bottom cap in shaded renderings. Defaults to T. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL.
```

---

## index.html (chunk 5/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/12/index.html
Type: reference

```
erings. Defaults to T. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, defaulting) CENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located. DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only).
```

---

## index.html (chunk 6/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/12/index.html
Type: reference

```
representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). Computed Slots HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. WIDTH [ from BASE-OBJECT ] number X-axis dimension of the reference box. Defaults to zero.
```

---

## index.html (chunk 7/7)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/12/index.html
Type: reference

```
WIDTH [ from BASE-OBJECT ] number X-axis dimension of the reference box. Defaults to zero. Examples (in-package :gdl-user) (define-object cone-sample (cone) :computed-slots ((display-controls (list :color :blue-neon :transparency 0.5 :shininess 0.8 :specular-color :white)) (length 10) (radius-1 2)(inner-radius-1 1) (radius-2 5) (number-of-sections 5) (inner-radius-2 3))) (generate-sample-drawing :objects (make-object 'cone-sample) :projection-direction :trimetric) Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/9/index.html
Type: reference

```
GendL Application - C-CYLINDER Package Documentation Object: C-CYLINDER (The :GEOM-BASE Package) Mixins: CYLINDER Description Provides a simple way to create a cylinder, by specifying a start point and an end point. Input Slots (required) END 3d point Center of the end cap. RADIUS [ from CYLINDER ] number Radius of the circular cross section of the cylinder. START 3d point Center of the start cap. Input Slots (optional) BOTTOM-CAP? [ from CYLINDER ] boolean Determines whether to include bottom cap in shaded renderings. Defaults to T. BOUNDING-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object. CLOSED? [ from CYLINDER ] boolean Indicates that a partial cylinder (or cone) should have a closed gap. END-ANGLE [ from ARCOID-MIXIN ] angle in radians End angle of the arc. Defaults to twice pi.
```

---

## index.html (chunk 2/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/9/index.html
Type: reference

```
that a partial cylinder (or cone) should have a closed gap. END-ANGLE [ from ARCOID-MIXIN ] angle in radians End angle of the arc. Defaults to twice pi. HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil INNER-RADIUS [ from CYLINDER ] number Radius of the hollow inner portion for a hollow cylinder. LOCAL-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object. NUMBER-OF-SECTIONS [ from CYLINDER ] integer Number of vertical sections to be drawn in wireframe rendering mode.
```

---

## index.html (chunk 3/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/9/index.html
Type: reference

```
lar volume bounding this geometric object. NUMBER-OF-SECTIONS [ from CYLINDER ] integer Number of vertical sections to be drawn in wireframe rendering mode. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information START-ANGLE [ from ARCOID-MIXIN ] angle in radians Start angle of the arc. Defaults to zero.
```

---

## index.html (chunk 4/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/9/index.html
Type: reference

```
w errors come back as a plist with error information START-ANGLE [ from ARCOID-MIXIN ] angle in radians Start angle of the arc. Defaults to zero. STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. TOP-CAP? [ from CYLINDER ] boolean Determines whether to include bottom cap in shaded renderings. Defaults to T. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. WIDTH [ from BASE-OBJECT ] number X-axis dimension of the reference box. Defaults to zero.
```

---

## index.html (chunk 5/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/9/index.html
Type: reference

```
of the reference box. Defaults to zero. Input Slots (optional, defaulting) DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. Computed Slots CENTER 3d point Center point of the center-line. CENTER-LINE list of two 3d points Represents line segment connecting center of end cap to center of start cap.
```

---

## index.html (chunk 6/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/9/index.html
Type: reference

```
uted Slots CENTER 3d point Center point of the center-line. CENTER-LINE list of two 3d points Represents line segment connecting center of end cap to center of start cap. LENGTH number Distance between cap centers. ORIENTATION 3x3 orthonormal rotation matrix Resultant orientation given the specified start and end points. Examples (in-package :gdl-user) (define-object c-cylinder-sample (c-cylinder) :computed-slots ((display-controls (list :color :plum :transparency 0.2)) (start (make-point 0 0 0)) (end (make-point 0 0 10)) (number-of-sections 7) (radius 3))) (generate-sample-drawing :objects (make-object 'c-cylinder-sample) :projection-direction (getf *standard-views* :trimetric)) Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/24/index.html
Type: reference

```
GendL Application - GLOBAL-POLYLINE-MIXIN Package Documentation Object: GLOBAL-POLYLINE-MIXIN (The :GEOM-BASE Package) Mixins: BASE-OBJECT Description Makes a connected polyline with vertices connected by straight line segments. Input Slots (required) VERTEX-LIST list of 3d points The vertices (``corners'') of the polyline. Input Slots (optional) CLOSED? boolean Controls whether the filleted-polyline should automatically be closed. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. IMAGE-FILE [ from BASE-OBJECT ] pathname or string Points to a pre-existing image file to be displayed instead of actual geometry for this object. Defaults to nil LOCAL-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object.
```

---

## index.html (chunk 2/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/24/index.html
Type: reference

```
oints The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding this geometric object. OBLIQUENESS [ from BASE-OBJECT ] 3x3 orthonormal matrix of double-float numbers This is synonymous with the orientation . ONCLICK-FUNCTION [ from BASE-OBJECT ] lambda function of zero arguments, or nil If non-nil, this function gets invoked when the user clicks the object in graphics front-ends which support this functionality, e.g. SVG/Raphael and X3DOM. ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places.
```

---

## index.html (chunk 3/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/24/index.html
Type: reference

```
s a plist with error information STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Input Slots (optional, defaulting) CENTER [ from BASE-OBJECT ] 3d point Indicates in global coordinates where the center of the reference box of this object should be located. DISPLAY-CONTROLS [ from BASE-OBJECT ] plist May contain keywords and values indicating display characteristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g.
```

---

## index.html (chunk 4/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/24/index.html
Type: reference

```
teristics for this object. The following keywords are recognized currently: :color color keyword from the *color-table* parameter, or an HTML-style hexidecimal RGB string value, e.g. "#FFFFFF" for pure white. Defaults to :black. :line-thickness an integer, defaulting to 1, indicating relative line thickness for wireframe representations of this object. :dash-pattern (currently PDF/PNG/JPEG only). This is a list of two or three numbers which indicate the length, in pixels, of the dashes and blank spaces in a dashed line. The optional third number indicates how far into the line or curve to start the dash pattern. HEIGHT [ from BASE-OBJECT ] number Z-axis dimension of the reference box. Defaults to zero. LENGTH [ from BASE-OBJECT ] number Y-axis dimension of the reference box. Defaults to zero. ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object.
```

---

## index.html (chunk 5/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/24/index.html
Type: reference

```
ro. ORIENTATION [ from BASE-OBJECT ] 3x3 matrix of double-float numbers Indicates the absolute Rotation Matrix used to create the coordinate system of this object. This matrix is given in absolute terms (i.e. with respect to the root's orientation), and is generally created with the alignment function. It should be an orthonormal matrix, meaning each row is a vector with a magnitude of one (1.0). WIDTH [ from BASE-OBJECT ] number X-axis dimension of the reference box. Defaults to zero. Computed Slots BOUNDING-BOX [ from BASE-OBJECT ] list of two 3d points The left front bottom and right rear top corners, in global coordinates, of the rectangular volume bounding the tree of geometric objects rooted at this object. LINES list of pairs of 3d points Each pair represents the start and end of each line segment in the polyline. Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 6/6)
Source: yadd-reference/package-dokumentations/5/object-docs/dokumentation/24/index.html
Type: reference

```
ch line segment in the polyline. Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

