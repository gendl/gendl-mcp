# Gendl Documentation - package_5_functions

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/70/index.html
Type: reference

```
Function: TRANSLATE-ALONG-VECTOR <-Back Function: Translate-Along-Vector TRANSLATE-ALONG-VECTOR 3d point Returns a new point which is point translated along vector by distance arguments: point 3D Point vector 3D Vector distance Number <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/27/index.html
Type: reference

```
Function: GET-W <-Back Function: Get-W GET-W double-float number Returns W component of point or vector arguments: quaternion 4D point, Quaternion, or Axis-Angle style rotation spec <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/24/index.html
Type: reference

```
Function: EQUI-SPACE-POINTS <-Back Function: Equi-Space-Points EQUI-SPACE-POINTS list of points Returns a list of equally spaced points between start and end. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/72/index.html
Type: reference

```
Function: UNITIZE-VECTOR <-Back Function: Unitize-Vector UNITIZE-VECTOR unit vector Returns the normalized unit-length vector corresponding to vector . arguments: vector 3D Vector keyword arguments: espsilon Number , Default Value: *ZERO-EPSILON* How close vector should be to 1.0 to be considered unit-length <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/73/index.html
Type: reference

```
Function: ZERO-VECTOR? <-Back Function: Zero-Vector? ZERO-VECTOR? boolean Returns non-NIL iff the vector has zero length according to Common Lisp zerop function. arguments: vector 3D Vector <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/1/index.html
Type: reference

```
Function: 3D-POINT-P <-Back Function: 3d-Point-P 3D-POINT-P boolean . function: 3d-point-p - predicate function to check if a make-point is 3D. That is, the point has 3 dimensions, representing a 3-dimensional point. usage: 3d-point-p point description: A predicate function to check if a point is 3-dimensional. The function may also be accessed by calling the function 3d-point?. examples: (3d-point-p (make-point 1 2 3)) --> t (3d-point-p (make-point 1 2 3 4)) --> nil <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/0/index.html
Type: reference

```
Function: 3D-DISTANCE <-Back Function: 3d-Distance 3D-DISTANCE number The three-dimensional distance from point-1 to point-2. arguments: point-1 3D point point-1 3D point <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/3/index.html
Type: reference

```
Function: 3D-VECTOR-P <-Back Function: 3d-Vector-P 3D-VECTOR-P boolean . function: 3d-vector-p - predicate function to check if a vector is 3D. That is, the vector has 3 dimensions, representing a 3-dimensional vector. usage: 3d-vector-p vector description: A predicate function to check if a vector is 3-dimensional. The function may also be accessed by calling the function 3d-vector?. examples: (3d-vector-p (make-vector 1 2 3)) --> t (3d-vector-p (make-vector 1 2 3 4)) --> nil <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/20/index.html
Type: reference

```
Function: DEGREE <-Back Function: Degree DEGREE number Converts angle in degrees, minutes, and seconds into radians. arguments: degrees Number optional arguments: minutes Number , Default Value: 0 <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/33/index.html
Type: reference

```
Function: INTER-LINE-SPHERE <-Back Function: Inter-Line-Sphere INTER-LINE-SPHERE 3d point or nil Returns one point of intersection between line described by point p-line and direction-vector u-line , and sphere described by center and radius . Iff the line and sphere do not intersect at all, NIL is returned. arguments: p-line 3D Point Any point on the line u-line 3D Vector Direction of the line center 3D Point Center of the sphere radius Number The radius of the sphere side-vector 3D Vector Controls which of two possible intersection points is returned <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/28/index.html
Type: reference

```
Function: GET-X <-Back Function: Get-X GET-X double-float number Returns X component of point or vector arguments: point 2D, 3D, or 4D point <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/7/index.html
Type: reference

```
Function: GEOM-BASE::ADD-MATRICES <-Back Function: Geom-Base::Add-Matrices ADD-MATRICES lisp array Adds two matrices element-by-element. rest: (matrics "Lisp Arrays of same dimensions") <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/60/index.html
Type: reference

```
Function: ROTATE-VECTOR-D <-Back Function: Rotate-Vector-D ROTATE-VECTOR-D number Rotates vector around normal by an amount of rotation specified by degrees . arguments: vector 3D Vector degrees Number normal 3D Vector <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/2/index.html
Type: reference

```
Function: 3D-POINT? <-Back Function: 3d-Point? 3D-POINT? boolean A predicate function to check if a point is 3-dimensional. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/69/index.html
Type: reference

```
Macro: TRANSLATE <-Back Macro: Translate TRANSLATE [macro] 3d point Within the context of a GDL object definition (i.e. a define-object ), translate origin by any number of offsets . arguments: origin 3D Point rest arguments: offsets Plist consisting of direction keywords and numbers A direction keyword can be one of: :top (or :up ) :bottom (or :down ) :left :right :front :rear (or :back ) <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/51/index.html
Type: reference

```
Function: QUATERNION-TO-MATRIX <-Back Function: Quaternion-To-Matrix QUATERNION-TO-MATRIX 3x3 orthonormal rotation matrix Transforms quaternion into a 3x3 rotation matrix. arguments: quaternion Quaternion, represented as a 4D Vector <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/38/index.html
Type: reference

```
Function: MATRIX*VECTOR <-Back Function: Matrix*Vector MATRIX*VECTOR lisp array Multiplies matrix by column vector of compatible dimension. arguments: matrix Lisp Array of Numbers vector Vector <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/67/index.html
Type: reference

```
Function: TRANSFORM-AND-TRANSLATE-POINT <-Back Function: Transform-And-Translate-Point TRANSFORM-AND-TRANSLATE-POINT 3d-point Returns the product of vector and transform , translated by (i.e. added to) trans-vector . arguments: vector 3D Vector transform 3x3 Rotation Matrix trans-vector 3D Vector examples: (let ((transform (make-transform '((0.0 0.0 1.0) (0.0 1.0 0.0) (1.0 0.0 0.0)))) (v (make-vector 1.0 2.0 3.0)) (t-v (make-vector 3.0 0.0 0.0))) (transform-and-translate-point v transform t-v)) ---> #(6.0 2.0 1.0) <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/32/index.html
Type: reference

```
Function: INTER-LINE-PLANE <-Back Function: Inter-Line-Plane INTER-LINE-PLANE 3d point or nil Returns one point of intersection between line described by point p-line and direction-vector u-line , and plane described by p-plane and u-plane . Iff the line and plane do not intersect at all (i.e. they are parallel), NIL is returned. arguments: p-line 3D Point Any point on the line u-line 3D Vector Direction of the line p-plane 3D Point Any point on the plane u-plane 3D Vector Normal of the plane <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/22/index.html
Type: reference

```
Function: DISTANCE-TO-LINE <-Back Function: Distance-To-Line DISTANCE-TO-LINE number Returns shortest distance from point to line. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/35/index.html
Type: reference

```
Macro: MAKE-POINT <-Back Macro: Make-Point MAKE-POINT 3d point (Internally this is the same as a 3D Vector) Returns a vector of double-floats from up to 4 numbers. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/21/index.html
Type: reference

```
Function: DEGREES-TO-RADIANS <-Back Function: Degrees-To-Radians DEGREES-TO-RADIANS number Converts degrees to radians. arguments: degrees Number <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/17/index.html
Type: reference

```
Function: COINCIDENT-POINT? <-Back Function: Coincident-Point? COINCIDENT-POINT? boolean Returns non-NIL iff the distance between point-1 and point-2 is less than tolerance . arguments: point-1 3D Point point-2 3D Point keyword arguments: tolerance Number , Default Value: *ZERO-EPSILON* <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/29/index.html
Type: reference

```
Function: GET-Y <-Back Function: Get-Y GET-Y double-float number Returns Y component of point or vector arguments: point 2D, 3D, or 4D point <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/43/index.html
Type: reference

```
Function: ORTHOGONAL-COMPONENT <-Back Function: Orthogonal-Component ORTHOGONAL-COMPONENT 3d unit vector Returns the unit vector orthogonal to reference-vector which is as close as possible to vector . arguments: vector 3D Vector reference-vector 3D Vector <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/5/index.html
Type: reference

```
Function: 3D-VECTOR? <-Back Function: 3d-Vector? 3D-VECTOR? boolean A predicate function to check if a vector is 3-dimensional. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/66/index.html
Type: reference

```
Function: SUBTRACT-VECTORS <-Back Function: Subtract-Vectors SUBTRACT-VECTORS vector Return a new vector, the result of affine vector subtraction. arguments: v1 2D, 3D, or 4D Vector v2 2D, 3D, or 4D Vector <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/18/index.html
Type: reference

```
Function: CREATE-OBLIQUENESS <-Back Function: Create-Obliqueness CREATE-OBLIQUENESS 3x3 orthonormal rotation matrix Gives the transform required to be applied to the parent's orientation to achieve alignment indicated by the arguments. The direction keywords are the same as those used with the GDL alignment function. arguments: vector-1 3D Vector direction-1 Direction Keyword vector-2 3D Vector direction-2 Direction Keyword self GDL object inheriting from base-object <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/12/index.html
Type: reference

```
Function: APPLY-MAKE-POINT <-Back Function: Apply-Make-Point APPLY-MAKE-POINT 2d, 3d, or 4d point This function takes a list of two, three, or four numbers rather than multiple arguments as with the make-point and make-vector macro. This is equivalent to calling the make-point or make-vector macro on the elements of this list. arguments: list List of 2, 3, or 4 numbers The coordinates for the point <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/50/index.html
Type: reference

```
Function: PYTHAGORIZE <-Back Function: Pythagorize PYTHAGORIZE number Returns the square root of the sum of the squares of numbers . rest arguments: numbers List of Numbers <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/58/index.html
Type: reference

```
Function: ROTATE-POINT-D <-Back Function: Rotate-Point-D ROTATE-POINT-D 3d point Returns the 3D Point resulting from rotating point about center in the plane defined by normal . The rotation can specified either by an arc length ( arc-length ) or an angle in degrees ( angle ). A second value is returned, which is the resulting angle of rotation in degrees (this is of possible use if arc-length is used to specify the rotation). arguments: point 3D Point center 3D Point normal 3D Vector keyword arguments: arc-length Number , Default Value: NIL angle Number , Default Value: NIL <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/14/index.html
Type: reference

```
Function: ARRAY-TO-LIST <-Back Function: Array-To-List ARRAY-TO-LIST list Converts array to a list. arguments: array Lisp Array of Numbers optional arguments: decimal-places Integer , Default Value: 2 Numbers will be rounded to this many decimal places <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/64/index.html
Type: reference

```
Function: SCALAR*VECTOR <-Back Function: Scalar*Vector SCALAR*VECTOR vector Returns result of multiplying the scalar number by the vector arguments: scalar Number vector 2D, 3D, or 4D Vector <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/42/index.html
Type: reference

```
Function: MULTIPLY-MATRICES <-Back Function: Multiply-Matrices MULTIPLY-MATRICES lisp array Multiplies compatible-size matrices according to normal matrix math. arguments: matrix-1 Lisp Array of Numbers matrix-2 Lisp Array of Numbers <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/48/index.html
Type: reference

```
Function: PROJ-POINT-ON-LINE <-Back Function: Proj-Point-On-Line PROJ-POINT-ON-LINE 3d-point Drops 3d-point onto line containing line-point and whose direction-vector is vector . arguments: 3d-point 3D Point line-point 3D Point vector 3D Unit Vector <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/56/index.html
Type: reference

```
Macro: ROLL <-Back Macro: Roll ROLL [macro] transformation matrix In the context of a GDL object definition (i.e. in a define-object ), returns a transformation matrix based on rotation about axis by some angle . Axis is a keyword symbol, one of: :lateral :longitudinal :vertical Angle is specified in radians. Any number of axis-angle pairs can be specified. arguments: axis Keyword Symbol angle Number rest arguments: other-axes-and-angles Plist made from axis keyword symbols and numbers <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/44/index.html
Type: reference

```
Function: PARALLEL-VECTORS? <-Back Function: Parallel-Vectors? PARALLEL-VECTORS? boolean Returns non-nil iff vector-1 and vector-2 are pointing in the same direction or opposite directions. arguments: vector-1 3D Vector vector-2 3D Vector keyword arguments: tolerance Number , Default Value: *ZERO-EPSILON* directed? Boolean , Default Value: NIL If :directed? is t, the function returns t if the vectors are both parallel and point in the same direction. The default is nil, meaning that the function will return t regardless of which way the vectors point, as long as they are parallel <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/31/index.html
Type: reference

```
Function: INTER-CIRCLE-SPHERE <-Back Function: Inter-Circle-Sphere INTER-CIRCLE-SPHERE 3d point or nil Returns point of intersection between the circle described by circle-center , circle-radius , and circle-plane-normal , and the sphere described by sphere-center and sphere-radius . Iff the circle and sphere do not intersect at all, NIL is returned. arguments: circle-center 3D Point circle-radius Number circle-plane-normal 3D Vector sphere-center 3D Point sphere-radius Number positive-angle? Boolean Controls which of two intersection points is returned keyword arguments: tolerance Controls how close the entities must come to touching to be considered as intersecting , Default Value: *ZERO-EPSILON* <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/71/index.html
Type: reference

```
Function: TRANSPOSE-MATRIX <-Back Function: Transpose-Matrix TRANSPOSE-MATRIX lisp array Transposes rows and columns of matrix . arguments: matrix Lisp Array <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/11/index.html
Type: reference

```
Function: ANGLE-BETWEEN-VECTORS-D <-Back Function: Angle-Between-Vectors-D ANGLE-BETWEEN-VECTORS-D number This function is identical to angle-between-vectors, but returns the angle in degrees. Refer to angle-between-vectors for more information. Technical note: the more argument has been introduced to support both angle-between-vectors call conventions and the legacy signature: (vector-1 vector-2 &optional reference-vector negative?) Optionally, a deprecation warning is printed when code invokes this legacy pattern.. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/41/index.html
Type: reference

```
Function: MIDPOINT <-Back Function: Midpoint MIDPOINT 3d point Returns the barycentric average (i.e. midpoint) of point1 and point2 . arguments: point1 3D Point point2 3D Point <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/37/index.html
Type: reference

```
Macro: MAKE-VECTOR <-Back Macro: Make-Vector MAKE-VECTOR 0d, 1d, 2d, 3d, or 4d vector (Internally this is the same as a Point) Returns a vector of double-floats from up to 4 numbers. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/19/index.html
Type: reference

```
Function: CROSS-VECTORS <-Back Function: Cross-Vectors CROSS-VECTORS 3d vector Returns the cross product of vector-1 and vector-2. According to the definition of cross product, this resultant vector should be orthogonal to both vector-1 and vector-2 . arguments: vector-1 3D Vector vector-2 3D Vector <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/59/index.html
Type: reference

```
Function: ROTATE-VECTOR <-Back Function: Rotate-Vector ROTATE-VECTOR number Rotates vector around normal by an amount of rotation specified by angle , which is an angle measured in radians. arguments: vector 3D Vector angle Number normal 3D Vector <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/68/index.html
Type: reference

```
Function: TRANSFORM-NUMERIC-POINT <-Back Function: Transform-Numeric-Point TRANSFORM-NUMERIC-POINT 3d-point Returns the product of vector and transform . arguments: vector 3D Vector transform 3x3 Rotation Matrix examples: (let ((transform (make-transform '((0.0 0.0 1.0) (1.0 0.0 0.0) (0.0 1.0 0.0)))) (v (make-vector 1.0 2.0 3.0))) (transform-numeric-point v transform)) ---> #(2.0 3.0 1.0) <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/8/index.html
Type: reference

```
Function: ADD-VECTORS <-Back Function: Add-Vectors ADD-VECTORS vector Return a new vector, the result of affine vector addition. arguments: v1 2D, 3D, or 4D Vector v2 2D, 3D, or 4D Vector <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/47/index.html
Type: reference

```
Function: GEOM-BASE::PRINT-CHARACTERS <-Back Function: Geom-Base::Print-Characters PRINT-CHARACTERS Print each character of the given string starting with finding character `start-char` and continuing for `search-length`. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/45/index.html
Type: reference

```
Function: POINT-ON-PLANE? <-Back Function: Point-On-Plane? POINT-ON-PLANE? boolean Determines whether or not the 3d-point lies on the plane specified by plane-point and plane-normal , within tolerance . arguments: 3d-point Point in question plane-point point on the known plane plane-normal normal to the known plane keyword arguments: tolerance Tolerance for points to be considered coincident , Default Value: *ZERO-EPSILON* <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/49/index.html
Type: reference

```
Function: PROJECTED-VECTOR <-Back Function: Projected-Vector PROJECTED-VECTOR 3d vector Returns result of projecting vector onto the plane whose normal is plane-normal . arguments: vector 3D Vector plane-normal 3D Vector <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/65/index.html
Type: reference

```
Function: SORT-POINTS-ALONG-VECTOR <-Back Function: Sort-Points-Along-Vector SORT-POINTS-ALONG-VECTOR list of points Returns points in order along given vector. <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/52/index.html
Type: reference

```
Function: QUATERNION-TO-ROTATION <-Back Function: Quaternion-To-Rotation QUATERNION-TO-ROTATION euler rotation represented as a 4d vector Transforms quaternion into a Euler angle rotation consisting of an arbitrary axis and an angle of rotation about that axis. arguments: quaternion Quaternion, represented as a 4D Vector <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/23/index.html
Type: reference

```
Function: DOT-VECTORS <-Back Function: Dot-Vectors DOT-VECTORS number Returns the dot product of vector-1 and vector-2. arguments: vector-1 2D, 3D, or 4D Vector vector-2 2D, 3D, or 4D Vector <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/6/index.html
Type: reference

```
Function: ACOSD <-Back Function: Acosd ACOSD number Returns the arc cosine of theta , converted into degrees. arguments: theta Number An angle in radians <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/30/index.html
Type: reference

```
Function: GET-Z <-Back Function: Get-Z GET-Z double-float number Returns Z component of point or vector arguments: point 3D or 4D point <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/4/index.html
Type: reference

```
Function: GEOM-BASE::3D-VECTOR-TO-ARRAY <-Back Function: Geom-Base::3d-Vector-To-Array 3D-VECTOR-TO-ARRAY 3-by-1 lisp array of double-floats Returns a 3-by-1 Lisp array of double-float numbers built from a 3D-Vector of double-floats. This can be useful for example for multiplying a GDL 3d-point (which is a 1-d vector) by a 3x3 matrix represented as a 2D Lisp array. arguments: vector 3D-Vector of double-floats (e g. created with make-vector macro) <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/13/index.html
Type: reference

```
Function: GEOM-BASE::ARRAY-TO-3D-VECTOR <-Back Function: Geom-Base::Array-To-3d-Vector ARRAY-TO-3D-VECTOR 3d vector Returns a 3D-Vector of double-floats built from a 3-by-1 Lisp array of numbers. arguments: array 3-by-1 Lisp array of numbers <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/53/index.html
Type: reference

```
Function: RADIANS-TO-DEGREES <-Back Function: Radians-To-Degrees RADIANS-TO-DEGREES number Converts angle in radians to degrees. arguments: radians Number <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/46/index.html
Type: reference

```
Function: POINT-ON-VECTOR? <-Back Function: Point-On-Vector? POINT-ON-VECTOR? boolean Determines whether or not the unknown-point lies on the ray specified by the vector pointing from first-point to second-point , within tolerance . arguments: first-point first point of vector second-point second point of vector unknown-point point in question keyword arguments: tolerance Tolerance for vectors to be considered same-direction , Default Value: *ZERO-EPSILON* <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/2)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/9/index.html
Type: reference

```
Function: ALIGNMENT <-Back Function: Alignment ALIGNMENT 3x3 orthonormal rotation matrix Constructs a rotation matrix from the given axes and vectors. Up to three pairs of axis and vector can be given. If only one pair is given, then the orthogonal component of its vector with respect to the other two global axes is used. If a second pair is given, then the orthogonal component of its vector with respect to the first vector is used. A third pair is only required if a left-handed coordinate system is desired (right-handed is the default). The third vector will always be converted to the cross of the first two, unless it is given as the reverse of this, which will force a left-handed coordinate system. Axes are direction keywords which can be one of: :right :left :rear :front :top :bottom The second axis keyword, if given, must be orthogonal to the first, and the third, if given, must be orthogonal to the first two.
```

---

## index.html (chunk 2/2)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/9/index.html
Type: reference

```
e second axis keyword, if given, must be orthogonal to the first, and the third, if given, must be orthogonal to the first two. arguments: axis-1 Direction Keyword vector1 3D Vector optional arguments: axis-2 Direction Keyword , Default Value: NIL vector2 3D Vector , Default Value: NIL axis-3 Direction Keyword , Default Value: NIL vector3 3D Vector , Default Value: NIL <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/55/index.html
Type: reference

```
Function: REVERSE-VECTOR <-Back Function: Reverse-Vector REVERSE-VECTOR vector Return the vector pointing in the opposite direction. arguments: vector 2D, 3D, or 4D Vector <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/34/index.html
Type: reference

```
Function: LENGTH-VECTOR <-Back Function: Length-Vector LENGTH-VECTOR number Return the vector's magnitude arguments: vector 3D Vector <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/62/index.html
Type: reference

```
Function: SAME-DIRECTION-VECTORS? <-Back Function: Same-Direction-Vectors? SAME-DIRECTION-VECTORS? boolean Returns non-NIL iff vector-1 and vector-2 are pointing in the same direction. arguments: vector-1 3D Vector vector-2 3D Vector keyword arguments: tolerance Number , Default Value: *ZERO-EPSILON* <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/16/index.html
Type: reference

```
Function: ATAND <-Back Function: Atand ATAND number Returns the arc tangent of theta , converted into degrees. arguments: theta Number An angle in radians <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/15/index.html
Type: reference

```
Function: ASIND <-Back Function: Asind ASIND number Returns the arc sine of theta , converted into degrees. arguments: theta Number An angle in radians <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/25/index.html
Type: reference

```
Function: GET-U <-Back Function: Get-U GET-U double-float number Returns U component of 2D parameter value. arguments: point 2D point <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/54/index.html
Type: reference

```
Function: RADIANS-TO-GRADS <-Back Function: Radians-To-Grads RADIANS-TO-GRADS number Converts angle in radians to grads. arguments: radians Number <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/26/index.html
Type: reference

```
Function: GET-V <-Back Function: Get-V GET-V double-float number Returns V component of 2D parameter value. n:arguments (point "2D point") <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/61/index.html
Type: reference

```
Function: ROTATION <-Back Function: Rotation ROTATION 3x3 orthonormal rotation matrix (as a lisp array of numbers) . Returns a transformation matrix based on a rotation by angle , specified in radians, about an arbitrary vector . arguments: vector 3D Vector angle Number <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/63/index.html
Type: reference

```
Function: GEOM-BASE::SCALAR*MATRIX <-Back Function: Geom-Base::Scalar*Matrix SCALAR*MATRIX lisp array Returns result of multiplying the scalar number by the matrix. arguments: scalar Number matrix Lisp Array of Numbers <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/36/index.html
Type: reference

```
Function: MAKE-TRANSFORM <-Back Function: Make-Transform MAKE-TRANSFORM lisp array Builds a matrix from list-of lists . arguments: list-of-lists List of lists of numbers <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/57/index.html
Type: reference

```
Function: ROTATE-POINT <-Back Function: Rotate-Point ROTATE-POINT 3d point Returns the 3D Point resulting from rotating point about center in the plane defined by normal . The rotation can specified either by an arc length ( arc-length ) or an angle in radians ( angle ). A second value is returned, which is the resulting angle of rotation in radians (this is of possible use if arc-length is used to specify the rotation). arguments: point 3D Point center 3D Point normal 3D Vector keyword arguments: arc-length Number , Default Value: NIL angle Number , Default Value: NIL <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/10/index.html
Type: reference

```
Function: ANGLE-BETWEEN-VECTORS <-Back Function: Angle-Between-Vectors ANGLE-BETWEEN-VECTORS number Returns the angle in radians between vector-1 and vector-2 . If no reference-vector given, the smallest possible angle is returned. If a reference-vector is given, computes according to the right-hand rule. If -ve is given, returns a negative number for angle if it really is negative according to the right-hand rule. arguments: vector-1 3D Vector vector-2 3D Vector optional arguments: reference-vector 3D Vector , Default Value: NIL keyword arguments: epsilon Number , Default Value: *ZERO-EPSILON* Determines how small of an angle is considered to be zero -ve Boolean , Default Value: NIL <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/39/index.html
Type: reference

```
Function: MATRIX-TO-QUATERNION <-Back Function: Matrix-To-Quaternion MATRIX-TO-QUATERNION quaternion represented as a 4d vector Transforms rotation matrix into the corresponding quaternion. arguments: matrix 3x3 Orthonormal Rotation Matrix (as a Lisp Array of Numbers) <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

## index.html (chunk 1/1)
Source: yadd-reference/package-dokumentations/5/function-docs/dokumentation/40/index.html
Type: reference

```
Macro: MERGE-DISPLAY-CONTROLS <-Back Macro: Merge-Display-Controls MERGE-DISPLAY-CONTROLS plist of display controls This macro "merges" the given display controls list with that coming as a trickle-down slot from the parent. It will replace any common keys and add any new keys. arguments: display-controls Plist The new display controls to be merged with the defaults from the parent <-Back Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

