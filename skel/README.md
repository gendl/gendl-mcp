# Gendl-Skel

A Gendl-based utility for generating skeleton project structures.

## Overview

Gendl-Skel is a meta-project that demonstrates how Gendl objects can
be used to model and generate file structures. It provides a simple
API for creating new Gendl project directories with all the necessary
files to get started.

The core concept is that the object structure mirrors the file
structure that will be generated, with each file object knowing how to
generate its content.

## Smart Features

- Conditional .asd file generation to avoid permission issues
- Object-oriented approach to project scaffolding
- Each file/directory in the target project is represented by a Gendl object
- Recursive directory creation and file writing
- Minimal API with just a project name required
- Templates for common files (package.lisp, init.lisp, etc.)

## Usage

After loading the project:

```lisp
;; Create a new project named "ivory-tower"
(theo (make-object 'gendl-skel:assembly :project-name "ivory-tower") write-to-disk!)
```

This will create:
- /projects/xfer/ivory-tower/
- /projects/xfer/ivory-tower/init.lisp
- /projects/xfer/ivory-tower/source/
- /projects/xfer/ivory-tower/source/package.lisp
- /projects/xfer/ivory-tower/source/file-ordering.isc
- /projects/xfer/ivory-tower/source/assembly.lisp

## Project Structure

### Object Hierarchy

- `file-base` - Base mixin that all file and directory objects inherit from
- `assembly` - Main object that generates the entire project structure
- `project-directory`, `source-directory` - Represent directories
- `init-file`, `package-file`, etc. - Represent individual files

### File Structure

- `source/package.lisp` - Defines the gendl-skel package
- `source/file-base.lisp` - Contains base mixin with disk writing functionality
- `source/assembly.lisp` - Contains all file and directory objects and the main assembly
- `init.lisp` - For loading the project

### Conditional .asd File Generation

The generated init.lisp file includes a smart approach to .asd file generation:

```lisp
;; Generate/update the ASDF system definition if needed
(unless (probe-file (merge-pathnames "my-project.asd" *my-project-path*))
  (cl-lite *my-project-path* :create-asd-file? t))
```

This ensures the .asd file is only generated if it doesn't already
exist, which helps avoid permission issues in shared environments.

## Extensibility

This project can be extended in several ways:

1. Add more file templates for different project types
2. Add parameters for customizing the generated content
3. Add options for different components (web interface, database, etc.)
4. Create specialized templates for domain-specific applications

## License

This project is distributed under the same license as Gendl.
