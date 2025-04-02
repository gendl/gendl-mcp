# Gendl-Skel Project Generator Guide

## Overview

Gendl-Skel is a utility for generating skeleton project structures for Gendl applications. It creates a complete directory structure with all necessary files to get your project started.

## Basic Usage

```lisp
;; Load the utility
(ql:quickload :gendl-skel)

;; Create a new project
(gendl-skel:gendl-skel :project-name "my-awesome-project" :base-path "/projects/xfer/")
```

## Generated Structure

The utility creates the following structure:

```
my-awesome-project/
  ├── my-awesome-project.asd  # Auto-generated ASDF file
  ├── init.lisp               # Project initialization file
  ├── source/                 # Source code directory
      ├── package.lisp        # Package definition
      ├── file-ordering.isc   # File loading order configuration
      └── assembly.lisp       # Main assembly object definition
```

## Generated Files

### init.lisp

The initialization file contains all necessary code to load your project:
- Defines a project path parameter
- Generates an ASDF file if needed
- Adds the project to Quicklisp local projects
- Loads the project
- Creates a default instance for testing

### package.lisp

Defines your project's package and exports the assembly symbol.

### assembly.lisp

Contains a starter assembly object definition with:
- Example input slots with documentation
- Example computed-slots
- Example child objects (a 3D box)
- Default parameters and display controls

### file-ordering.isc

Specifies the correct loading order for source files (package.lisp first).

## Customization

After generating the project, you'll typically:
1. Add your own object definitions to assembly.lisp or create new source files
2. Update the package.lisp file to export additional symbols
3. Update the file-ordering.isc if you add new source files
4. Modify the default assembly object to suit your needs

## Best Practices

1. **Use as Starting Point**: The generated project is a starting point; customize it to fit your needs.
2. **Keep Object Definitions Modular**: Add new files for complex components.
3. **Update file-ordering.isc**: Remember to update this when adding new files.
4. **Leverage Default Documentation**: The generated files include documentation templates; fill them with detailed descriptions.
5. **Test Incrementally**: Load and test your project after each significant change.

## Troubleshooting

If you encounter issues:
- Check that your project name is a valid Lisp symbol
- Ensure you have write permissions for the target directory
- If a project with the same name exists, it will not be overwritten
