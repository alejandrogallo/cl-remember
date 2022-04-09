(in-package :remember)

(flet ((reader (what) (lambda (entry) (getf entry what))))
  (forms:defform pictures (:id *form-name*
                           :enctype "multipart/form-data")
    ((pictures
      :list
      :type (list :file
                  :multiple-p t
                  :accept "image/"
                  :upload-handler 'handle-picture-upload)
      :add-button t
      :remove-button t
      :label "Pictures"
      :reader (reader 'pictures)
      :writer
      (lambda (value entry)
        (when (and value (car value))
          (hunchentoot:log-message* 0 "Getting file ~a to put in object" value)
          (push (forms::file-path (car value))
                (getf entry 'pictures))))))))

(defun handle-picture-upload (file-field)
    (let* ((new-file-name (create-random-token))
           (new-path (merge-pathnames new-file-name
                                      (getf *config* :pictures-path))))
      (hunchentoot:log-message* 0 "~%Saving picture ~a to ~a~%"
                                (forms::file-path file-field) new-path)
      (uiop:copy-file (forms::file-path file-field)
                      (ensure-directories-exist new-path))
      (setf (forms::file-path file-field)
            (uiop:enough-pathname new-path (getf *config* :pictures-path)))
      (setf (forms::file-name file-field) new-file-name)))

(defmethod render-form (form (name (eql 'pictures)))
  (let ((pictures-list (car (forms::form-fields form))))
    (print 'aaaaaaaaaaaaaaaaaaa)
    (print (forms::field-value (cdr pictures-list)))
    (print 'aaaaaaaaaaaaaaaaaaa)
    (flet ((field-name (i) (format nil "pictures[~a].pictures" i)))
      (who:with-html-output-to-string (forms.who:*html*)
        (:h3 (:i :class "fa fa-camera"))
        (:form :action (forms::form-action form)
               :method (forms::form-method form)
               :enctype "multipart/form-data"
               :id (forms::form-id form)
               (:input :type "file"
                       :class "form-control"
                       :accept "image/"
                       :name (field-name 0)
                       :capture "camera")
               (:div :class "row row-cols-md-2 g-4"
                     (:div :class "col"
                           (loop for field in (forms::field-value (cdr pictures-list))
                                 for i from 0
                                 do (let ((path (typecase field
                                                  (string field)
                                                  (pathname field)
                                                  (t (forms::field-name field)))))
                                      (who:htm
                                       (:div :class "card"
                                             (:a :href (format nil "/static/~a" path)
                                                 (:img :src (format nil "/static/~a" path)
                                                       :class "card-img-top"))
                                             )))))))
        #+nil
        (forms:with-form-theme 'forms.who:bootstrap-form-theme
          (forms:with-form-renderer :who
            (who:str (forms:render-form form))))))))
