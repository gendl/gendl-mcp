(in-package :training-1)

(define-object installation (base-training-sheet)

  :input-slots
  (index-url)

  :computed-slots
  (
   (body-content
    (with-cl-who-string ()

      (:p "You have several options for developing and deploying applications using GendL and Genworks GDL. "
          (:em "GendL")
          " refers to an open-source project hosted at "
          (:a :href "https://gitlab.common-lisp.net/gendl/gendl.git" "common-lisp.net")
          " whose copyright is owned by "
          (:a :href "https://genworks.com" "Genworks")
          " and which is licensed under the "
          (:a :href "https://www.gnu.org/licenses/agpl-3.0.en.html" "GNU Affero General Public License")
          " The AGPL licensing means essentially that GendL and its derivatives
can be used free of charge in any way you want, as long as you comply
with AGPL by making available your application source code to your
users. If you were to host or distribute a compiled GendL application
without complying with AGPL requirements, you would be putting
yourself and any company you're associated with into legal jeopardy.")

      (:p "Please do not be concerned about legal jeopardy, however, as there are several ways to make good use of GendL and KBE while avoiding this risk:"
          (:ul
           (:li "Host your application source code at a public repository such as "
                (:a :href "https://github.com" "Github") " or a " (:a :href "https://gitlab.common-lisp.net" "Gitlab") " instance.")
           (:li "Host your running applications using our Hosting Infrastructure (coming soon), which will automatically take care of offering source code to users.")
           (:li "Contact Genworks and arrange to purchase a commercial Genworks GDL seat (these seats are built with commercial Allegro Common Lisp and
come with technical support as well as other Enterprise-oriented features.)")))


      (:p
       "Up to date prebuilt GendL executable images, based on Clozure CL
1.12, are available for Windows and Linux on 64-bit Intel
processors. For other Operating Systems and Processors, you can use
the Docker container approach (see below), or build GendL from source
on a compliant CL implementation such as Allegro CL, Clozure CL (CCL),
Clasp, LispWorks, or SBCL, on your OS platform of choice (see below). ")

      (:h3 "Prebuilt GendL for Windows and Linux")
      (:p "You can download the latest prebuilt 64-bit Gendl images
 for Windows or Linux according to the following build combinations:"

	  ;;make a table of the following combos, each with a download link. I'll do the first as an example. Combos are:
	  ;; 
	  ;; Linux/SBCL/zip
	  ;; Linux/CCL/zip
	  ;; Windows/SBCL/zip
	  ;; Windows/CCL/zip
	  ;; Windows/SBCL/exe
	  ;; Windows/CCL/exe
	  ;;
	  ;; The table will have 3 columns: OS, Lisp, and Archive Type. The download link will be in the Archive Type column.
	  ;; The table will have 6 rows, one for each of the above combinations.

	  (:div
	   :class "mt-8 flow-root"
	   (:div
	    :class "-mx-4 -my-2 overflow-x-auto sm:-mx-6 lg:-mx-8"
	    (:div
     :class "inline-block min-w-full py-2 align-middle sm:px-6 lg:px-8"
	     (:table :class "table-auto border-2 border-green-700 mx-6 divide-x divide-y"
	       (:thead
		(:tr
		 (dolist (heading (list "OS" "Lisp" "File Type" "Download" "GPG Signature"))
		   (htm 
		    (:th
		     :scope "col" :class "text-center border-2 border-gray-600 py-3.5 pl-4 pr-3 text-lg font-semibold text-gray-900" (str heading))))))
	       (:tbody
		:class "divide-x divide-y"
		(dolist (row (list
			      (list "Windows" "SBCL" "Zip" "https://downloads.genworks.com/gendl1598-beta-windows-sbcl.zip" "https://downloads.genworks.com/gendl1598-beta-windows-sbcl.zip.sig")
			      (list "Windows" "Clozure CL" "Zip" "https://downloads.genworks.com/gendl1598-beta-windows-ccl.zip" "https://downloads.genworks.com/gendl1598-beta-windows-ccl.zip.sig")
			      (list "Windows" "SBCL" "exe" "https://downloads.genworks.com/gendl1598-beta-windows-sbcl.exe" "https://downloads.genworks.com/gendl1598-beta-windows-sbcl.exe.sig")
			      (list "Windows" "Clozure CL" "exe" "https://downloads.genworks.com/gendl1598-beta-windows-ccl.exe" "https://downloads.genworks.com/gendl1598-beta-windows-ccl.exe.sig")
			      (list "Linux" "SBCL" "Zip" "https://downloads.genworks.com/gendl1598-beta-linux-ccl.zip" "https://downloads.genworks.com/gendl1598-beta-linux-ccl.zip.sig")
			      (list "Linux" "Clozure CL" "Zip"
				    "https://downloads.genworks.com/gendl1598-beta-linux-ccl.exe"
				    "https://downloads.genworks.com/gendl1598-beta-linux-ccl.exe.sig"
				    )))
		  (destructuring-bind (os lisp archive-type link sig-link) row
		    (htm
		     (:tr
		      (:td :class "px-6 border-2 border-gray-600 whitespace-nowrap py-4 pl-4 pr-3 text-sm  text-gray-900 " (str os))
		      (:td :class "px-6 border-2 border-gray-600 whitespace-nowrap py-4 pl-4 pr-3 text-sm  text-gray-900 " (str lisp))
		      (:td :class "px-6 border-2 border-gray-600 whitespace-nowrap py-4 pl-4 pr-3 text-sm  text-gray-900 " (str archive-type))
		      (:td :class "px-6 border-2 border-gray-600 whitespace-nowrap py-4 pl-4 pr-3 text-sm  text-gray-900 " (:a :href link "Download"))
		      (:td :class "px-6 border-2 border-gray-600 whitespace-nowrap py-4 pl-4 pr-3 text-sm  text-gray-900 " (:a :href sig-link "GPG Signature *"))
		      )))))))

	    (:div :class "inline-block min-w-full py-2 align-middle sm:px-6 lg:px-8"
		  "* " (:a :href "https://downloads.genworks.com/publickey.asc"
			   "Public Key for GPG Signatures")))))


      (:p "Note the large Zip files which on Windows may take several minutes to
extract using the \"Extract All\" action from Windows File
Explorer. The free 7-zip program available from"
          (:a :href "https://www.7-zip.org" "7-zip.org")
          " is much faster at extracting these archives than the built-in one in
 Windows File Explorer. The "
	  (:span :class "general-keyword" "unzip")
	  " command which ships with Git Bash Shell is also much faster.")
      (:p "Once it is finished extracting, you may run the "
          (:span :class "general-keyword" "run-gdl[.bat]")
          " script which can be found at the toplevel of then extracted directory.
This script will launch Gnu Emacs, which will in turn launch and initialize a GendL instance as a subprocess.")

      (:p
       "Note that you may have to override the default Windows security settings
to allow the script to run, when running it for the first time,
i.e. the .exe archive will show as coming from an \"unverified
publisher,\" and this is expected for now. Windows may also ask you
about a certain service or port being started by your program. You can
safely disallow it, if you plan to use only \"localhost\" to access
your local Gendl server. If you plan to access your Gendl server from
other computers on your network, you will want to grant permission to
the service or port.")
     

      (:h3 "GendL as a Docker container")

      (:p
       "You can run the latest development version of GendL on any operating
system where you have  a bash shell and "
       (:a :href "https://docker.com" "Docker")
       " installed (free version sufficient). On Windows, the easiest way to get Docker and a bash shell is
by installing the Microsoft-supported "
       (:a :href "https://docs.microsoft.com/en-us/windows/wsl/install" "Windows System for Linux 2 (WSL2)"))
      (:p 
       " To get GendL running in a container, follow these steps: "
       (:ol (:li "Clone the "
                 (:a :href "https://gitlab.common-lisp.net/gendl/gendl.git" "GendL repository")
                 ", for example with the command "
                 (:span :class "general-keyword" "git clone https://gitlab.common-lisp.net/gendl/gendl.git"))
            (:li ". This repository will contain a file " (:span :class "general-keyword" "docker/run [--sbcl]")
                 ". Run this script.  It will fetch and run a prebuilt container image matching the repository branch you have cloned.
You may choose your branch from the official GendL codebase branches:"
                 (:dl (:dt (:b "master")) (:dd "The current official stable released version.")
                      (:dt (:b "devo")) (:dd "The current version under active development. Contains newest features but may be unstable.")
                      (:dt (:b "release/1598")) (:dd "The current release candidate for GendL 1598 (or other version), used for producing beta releases")))
            (:li "Load the file from the repository: "
                 (:span :class "general-keyword" "emacs/gdl.el")
                 " into your emacs (e.g. using "
                 (:span :class "general-keyword" "M-x load-file") ")")
            (:li "In emacs, connect to the GendL server using "
                 (:span :class "general-keyword" "M-x slime-connect")
                 " and specify the host as "
                 (:span :class "general-keyword" "localhost")
                 " and the port as "
                 (:span :class "general-keyword" "4200"))))


      (:h3 "GendL compiled from source")

      (:p
       "If you are already set up with Common Lisp and Quicklisp, you can load and start
GendL with the following commands: "
       (:span :class "general-keyword" "(ql:quickload :gendl) (gendl:start-gendl!)")
       ". If you'd like to work with the latest development (\"devo\") version
of GendL, then you may clone its repository into your "
       (:span :class "general-keyword" "quicklisp/local-projects/")
       " directory, then the above command will load it from there rather than its official distribution location (the
official distribution will typically be built from the latest "
       (:span :class "general-keyword" "master")
       " branch. In order to clone the "
       (:span :class "general-keyword" "devo")
       " branch, the commands would be:" :br
       (:span :class "general-keyword" "git clone https://gitlab.common-lisp.net/gendl/gendl.git; cd gendl; git checkout devo"))


      (:h3 "Commercial Genworks GDL")
      (:p "Genworks offers commercial Genworks GDL distributions built with
the commercial Allegro Common Lisp system. These distributions require an encrypted license file
to run and are available through an email-based download gateway.")
      (:p "If you would like to inquire about an entitlement for supported commercial
Genworks GDL, then please contact Genworks at "
          (:span :class "general-keyword" "licensing@genworks.com")
          " and you will be provided with further information.")))))

                    

          

                          


