(in-package :gwl-user)

(define-object file-upload-1 (base-html-page)
  :computed-slots
  ((uploaded-path "" :settable)
   (body
    (with-lhtml-string () (str (the development-links))
      (str
       (with-form-string (:enctype "multipart/form-data")
         (:table (:tr (:td (:input :type "file" :name :uploaded-path :value (the uploaded-path))))
           (:tr (:td (:input :type "submit" :name "upload" :value "Upload"))
                (:td (str (the uploaded-path)))))))))))

(publish-gwl-app "/file-upload-1" "gwl-user::file-upload-1")
