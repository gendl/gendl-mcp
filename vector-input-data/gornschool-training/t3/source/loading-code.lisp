(in-package :training-3)

(define-object loading-code (base-training-sheet)
  :computed-slots
  ((code-1 (list "(cl-lite \"/codebase/airplane\")"))
   (code-2 (list "(\"common\" \"wing\" \"fuselage\" \"engine\" \"source\")"))
   (code-3 (list "(\"package\")"))
   (code-4 (list "(\"engine\")"))
   (body-content (with-cl-who-string ()
		   ((:div :class "main-page-container" :style "grid-template-columns: 600px auto;")
		    ((:div :class "main-page-item")
		     (:p "In previous topics, we have seen that there is some requirement to manage the load order. With small numbers of source files this is manageable manually, but once the number of source files exceeds maybe 5 or 6, this becomes an onerous and somewhat tedious task")
		     (:p "GendL has a utility to support this called "
			 (:span :class "function" "cl-lite")". It can be called from either the REPL or it can be initiated from the init file (gdlinit.cl). "
			 (:span :class "function" "cl-lite")" supports the following features"
			 (:ul (:li "It will load any .lisp file found in any "
				   (:em (:b "source"))" directory below the target directory. So in the example codebase on the right, if "
				   (:span :class "function" "cl-lite")" was applied to the "
				   (:em (:b "airplane"))" directory it would load .lisp files from "
				   (:em (:b "source"))", "
				   (:em (:b "common/source"))", "
				   (:em (:b "engine/source"))", "
				   (:em (:b "fuselage/source"))" and "
				   (:em (:b "wing/source")))
			      (:li "The order in which directories are loaded may be specified at any level using a "
				   (:b "system-ordering.isc")" file")
			      (:li "The order in which files in a source folder are loaded may be specified for that "
				   (:em (:b "source"))" directory in a "
				   (:b "file-ordering.isc")" file")
			      (:li "Directories or files to be ignored can be specified in an  "
				   (:b "ignore-list.isc")" file")))
		     (:p "The structure of the "
			 (:b "system-ordering.isc")", "
			 (:b "file-ordering.isc")" and "
			 (:b "ignore-list.isc")" is identical; it is a literal list of strings identifying target folders or files. Note that "
			 (:b "ignore-list.isc")" may apply to either directories or files whilst "
			 (:b "system-ordering.isc")" only applies to directories and "
			 (:b "file-ordering.isc")" only applies to files"))
		    ((:div :class "main-page-item")
		     ((:img :src (format nil "/~a-images/codebase.png" *publish-prefix*) :style "width: auto; height: 300px; margin: 0 0 0 0 ;")))
		    ((:div :class "main-page-item")
		     (:h3 "Executing cl-lite")
		     (:p (:span :class "function" "cl-lite")" takes a pathname argument. The pathname may be a string or a logical pathname")
		     (str (code-example (the code-1)))
		     (:p "If we wanted to enforce a "
			 (:em "load order")" for the directories we would use the "
			 (:b "system-ordering.isc file")". In this example, assuming multiple packages which mirror the directory structure, we would probably want to load "
			 (:em (:b "common"))" first, then "
			 (:em (:b "engine"))", "
			 (:em (:b "wing"))" and "
			 (:em (:b "fuselage"))" (since they would "
			 (:span :class "object-keyword" ":use")" the "
			 (:span :class "package-name" ":airplane-common")" package) and lastly the "
			 (:em (:b "source"))" directory since it would "
			 (:span :class "object-keyword" ":use")" all of the previously defined packages. So "
			 (:b "system-ordering.isc")" would look like this")
		     (str (code-example (the code-2)))
		     (:p "Assuming each of the folders has a package file, we would want to load that as the first file when the directory is loaded, so each "
			 (:em  (:b"source"))" folder would have a "
			 (:b "file-ordering.isc")" file like this")
		     (str (code-example (the code-3)))
		     (:p "2 things to note here"
			 (:ul (:li "There is no need to specify the file extension")
			      (:li "There is only a requirement to specify files which have special load order, any other files in the folder are loaded by default in alphabetical order")))
		     (:p "Finally, if for some reason we wished to suppress the load of the engine module, we would use the "
			 (:b "ignore-list.isc")" file in the "
			 (:em  (:b "airplane"))" directory as shown below")
		     (str (code-example (the code-4)))))))))

			
