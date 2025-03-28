
# Creating Output from Gendl / Genworks GDL Object Hierarchies


Output formats can be used in conjunction with the `with-format` macro
to produce customized output from Gendl objects, as shown in the
examples below.

If you create new Gendl objects, you can also create output-functions
for your new object for the various formats by using the define-lens
macro, for example:

```
 (define-lens (pdf my-widget)()
   :output-functions
   ((cad-output 
     ()
     (pdf:move-to (the start))
     (pdf:line-to (the end)))))
```

Please see the reference documentation for `define-view` for more
details.


## PDF

PDF is the native graphics and document format of Gendl and as such is
the most developed. Most graphical and textual objects in Gendl have PDF
views defined for them with at least a `cad-output` method. The term
`cad-output` is somewhat dated, as the output could refer to a paper
report with charts and graphs as much as a drawing of mechanical
parts, so these names may be refined in future Gendl releases.

 Here is an example of producing a PDF of the built-in robot assembly:

```
 (with-format (pdf "/tmp/robot.pdf" :page-length (* 6.5 72) 
		   :page-width (* 8.5 72)
		   :view-transform (getf *standard-views* :trimetric)
		   :view-scale 37)
   (write-the-object (make-object 'gwl-user::robot-assembly) (cad-output-tree)))
```


 Available slots for the PDF writer and their defaults:

* `(page-width 612)`
* `(page-length 792)` 
* `(view-transform (getf *standard-views* :top))`
* `(view-center (make-point 0 0 0))` ;; in page coordinates
* `(view-scale 1)`


## DXF

DXF is AutoCAD's drawing exchange format. Currently most of the
geometric objects and drawing and text objects have a `cad-output`
method for this output-format. The Gendl DXF writer currently outputs a
relatively old-style AutoCAD Release 11/12 DXF header. In future Gendl
versions this will be switchable to be able to target different
release levels of the DXF format.

Here is an example of producing a DXF of the built-in robot assembly:

```
 (with-format (dxf "/tmp/robot.dxf"
    	         :view-transform (getf *standard-views* :front)
                 :view-scale 37 :view-center (make-point 0 0 0))
   (write-the-object (make-object 'gwl-user::robot-assembly) (cad-output-tree)))
```

Note that the DXF writer is currently 2D in nature and therefore takes
a :view-transform to specify a view plane onto which all 3D entities
are projected.

A 3D version of the DXF writer may be available in a future Gendl
release.

Available slots for the DXF writer and their defaults:

* `(view-transform (getf *standard-views* :top))`
* `(view-center (make-point 0 0 0))` ;; in page coordinates
* `(view-scale 1)`


## X3D

Gendl currently contains an X3D writer which maps Gendl primitive
geometric objects (boxes, spheres, etc) into their X3D
equivalents. This is mainly used for viewing models in-browser with
the x3dom javascript library (built-in and supported by Gendl) Future
Gendl releases will include more object types and mechanisms to
specify viewpoints, lighting, textures, etc.


  (with-format (x3d "/tmp/robot.wrl")
    (write-the-object (make-object 'gwl-user::robot-assembly) (world)))





## Iges

### Overview

We supply an IGES output-format with the optional Genworks GDL NURBS
Surfaces Facility. The IGES format will also convert certain Gendl
wireframe primitives into appropriate curve and surface objects for
IGES output (e.g. `line` into `linear-curve`, `arc` into `arc-curve`).

 Here are some examples of simple use:

```
  (with-format (iges "/tmp/try.iges")
    (write-the cad-output-tree))

```

```

  (with-format (iges "/tmp/try.iges")
    (write-the cad-output))

```

### Iges levels

The concept of the "level" in Iges is apparently similar to the
"layer" in AutoCAD and some other CAD systems, but an Iges entity can have more than one level.

Although Gendl geometric objects contain a `levels` slot which is
meant to be a list of level numbers, it is currently supported to
output only the first of these the Iges output format. If you need to
output more than one level then please contact Genworks to discuss a
possible enhancement to do this.

Currently you can specify the level for Iges in a Gendl geometric
object either as the first of the `levels` slot, or as the `layer`
slot.  [Here](https://genworks.com/package-dokumentations/23object-docsdokumentation/40/index.html)
is the reference documentation for the relevant Gendl object:

 

### brep solids representation in Iges

For breps, you can output them as individual breps or as a bag of the
faces (trimmed surfaces) making up the brep.

This is determined with the format variable `:breps-format`, which can
have the value `:breps` (the default) or `:surfaces`, e.g.

```
  (with-format (Iges "/tmp/try.iges" :breps-format :surfaces)  ...)  ;; or
```

```
   (with-format (Iges "/tmp/try.iges" :breps-format :breps)  ...)
```   


### units in iges

The iges units can be specified with the format variable `:units`, e.g:

  (with-format (iges "/tmp/try.iges" :units :millimeters) ...)  ;; or
  (with-format (iges "/tmp/try.iges" :units :feet) ...)  ;; or


 The value of this format variable should be specified as one of the
 following allowed keyword symbols:

```
   (:inches :millimeters :feet :miles :meters :kilometers
    :mils :microns :centimeters :microinches :no-units-specified)
```    

 The default is `:inches`.

### other format variables for Iges

The following additional format variables are supported, and can be used
according to the same syntax as described above for `:breps-format`
and `:units`:

* `:analytic-curves?` Boolean, indicates whether conic sections should
  be outputted as such in Iges (`t`), or as generalized NURBS curves
  (`nil`). Default is `t`.

* `:units-scale` Number. This is a scale factor applied to the
  `:units`.



## html-format

The HTML output format is used extensively throughout GWL (Gendl's web
application subsystem) for generating dynamic web page hierarchies
corresponding to Gendl object hierarchies. Please refer to the main
GDL Documentation for information on GWL and html-format.
