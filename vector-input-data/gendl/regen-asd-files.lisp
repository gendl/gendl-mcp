(in-package :gdl-user)

(defparameter *gendl-dir* (asdf:system-source-directory :gendl))

(defun regen-asd-files (&key (gendl-dir *gendl-dir*))
  (dolist (dir (list "gwl-graphics/"
		     "glisp/"
		     "demos/4bar/"
		     "demos/robot/"
		     "demos/ledger/"
		     "demos/wire-world/"
		     "demos/bus/"
		     "regression/"
		     "gwl/"
		     "documentation/training/g102-tud/examples/"
		     "documentation/training/"
		     "documentation/tutorial/"
		     "surf/"
		     "base/"
		     "setup-cffi/"
		     "geom-base/"
		     "apps/tree/"
		     "apps/tasty/"
		     "apps/ta2/"
		     "apps/graphs/"
		     "apps/geysr/"
		     "apps/translators/"
		     "apps/dom/"
		     "apps/yadd/"
		     "cl-lite/"
		     "./"))
    (cl-lite (merge-pathnames dir  gendl-dir) :create-asd-file? t)))
