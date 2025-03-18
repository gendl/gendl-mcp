# Gendl Documentation - package_0_objects

## index.html (chunk 1/4)
Source: yadd-reference/package-dokumentations/0/object-docs/dokumentation/0/index.html
Type: reference

```
GendL Application - CODEBASE-DIRECTORY-NODE Package Documentation Object: CODEBASE-DIRECTORY-NODE (The :CL-LITE Package) Mixins: DIRECTORY-NODE Description Models a filesystem directory for use by the cl-lite program. Input Slots (optional) BIN-SUBDIR-NAMES list of strings Identifies the names of directories considered to hold binaries. Default is (list "bin" "patch") CREATE-FASL? boolean Determines whether to write a concatenated fasl for the build. Defaults to nil. NOTE: this is not currently supported in cl-lite. FASL-OUTPUT-NAME string Names the built concatenated fasl when (the create-fasl?) is non-nil. Defaults to (the local-name) FASL-OUTPUT-PATH string or pathname object Designates the pathname for the filesystem directory in which the built concatenated fasls are written. Defaults to (glisp:temporary-folder) FASL-OUTPUT-TYPE string Names the fasl extension used by the compiler. Defaults to the local fasl output type.
```

---

## index.html (chunk 2/4)
Source: yadd-reference/package-dokumentations/0/object-docs/dokumentation/0/index.html
Type: reference

```
to (glisp:temporary-folder) FASL-OUTPUT-TYPE string Names the fasl extension used by the compiler. Defaults to the local fasl output type. HIDDEN? [ from VANILLA-MIXIN* ] boolean Indicates whether the object should effectively be a hidden-object even if specified in :objects. Default is nil. LOAD-ALWAYS? boolean Determines whether to load the individual compiled fasls even if the source has not changed. Defaults to nil (i.e. we assume we are loading into a clean system and need all the initial definitions.). ROOT [ from VANILLA-MIXIN* ] gdl instance The root-level node in this object's ``tree'' (instance hierarchy). SAFE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances All objects from the :objects specification, including elements of sequences as flat lists. Any children which throw errors come back as a plist with error information SOURCE-FILES-TO-IGNORE list of strings Lists directory names which should be ignored as having compilable source code for the build.
```

---

## index.html (chunk 3/4)
Source: yadd-reference/package-dokumentations/0/object-docs/dokumentation/0/index.html
Type: reference

```
w errors come back as a plist with error information SOURCE-FILES-TO-IGNORE list of strings Lists directory names which should be ignored as having compilable source code for the build. SPECIAL-SUBDIR-NAMES list of strings Identifies the names of directories which are part of a vc-system control files and therefore should be treated as special subdirectories. Default is (list "CVS") TYPE-MAPPING plist of keywords and lists of strings Maps directory names to their default type classifications. VISIBLE-CHILDREN [ from VANILLA-MIXIN* ] list of gdl instances Additional objects to display in Tatu tree. Typically this would be a subset of hidden-children. Defaults to NIL. Computed Slots STRINGS-FOR-DISPLAY [ from VANILLA-MIXIN* ] string or list of strings Determines how the name of objects of this type will be printed in most places.
```

---

## index.html (chunk 4/4)
Source: yadd-reference/package-dokumentations/0/object-docs/dokumentation/0/index.html
Type: reference

```
is type will be printed in most places. This defaults to the name-for-display (generally the part's name as specified in its parent), followed by an index number if the part is an element of a sequence. Package Documentation Copyright © 2025 Genworks ® International . All rights reserved. Genworks Build: 1598p001
```

---

