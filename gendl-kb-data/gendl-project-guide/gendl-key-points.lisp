;;;; Gendl Project Structure Key Points
;;;;
;;;; Include this at the beginning of your conversation with Claude
;;;; to provide context about Gendl project structure best practices.

#|

GENDL PROJECT STRUCTURE BEST PRACTICES
=====================================

0. PROJECT GENERATION:
   - Use the gendl-skel utility: (gendl-skel:gendl-skel :project-name "my-project")
   - This creates a complete project structure with all necessary files

1. DIRECTORY STRUCTURE:
   - /project-name/           Root directory
   - /project-name/source/    Source files
   - /project-name/*.asd      ASDF system definition

2. PACKAGE DEFINITION:
   - Use (gdl:define-package :your-package-name) without :use clauses
   - GDL, GDL-USER, etc. are automatically included

3. SYSTEM LOADING:
   - Use cl-lite for automatic dependency management
   - Example:
     (load-quicklisp)
     (cl-lite "/path/to/project/" :create-asd-file? t)
     (pushnew "/path/to/project/" ql:*local-project-directories* :test #'equalp)
     (ql:quickload :your-project)

4. OBJECT REFERENCES:
   - Use "theo" instead of "the-object" for cleaner syntax
   - Example: (theo my-object slot-name)
   - For nested: (theo container child-object slot-name)

5. BEST PRACTICES:
   - Use file-ordering.isc for controlling file load order
   - Break complex objects into modular components
   - Let cl-lite manage ASDF file generation

|#
