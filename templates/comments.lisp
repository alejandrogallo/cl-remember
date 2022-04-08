(in-package :remember)

(flet ((reader (what) (lambda (entry) (getf entry what)))
       (writer (what) (lambda (value entry) (setf (getf entry what) value))))

  (forms:defform comments (:id *form-name*)
    ((comments :string
               :placeholder "Comments"
               :reader (reader 'comments)
               :writer (writer 'comments)
               :label "Comments")))

  (defmethod render-form (form (name (eql 'comments)))
    (let ((comment (cdar (forms::form-fields form))))
      (who:with-html-output-to-string (forms.who:*html*)
        (:h3 (:i :class "fa fa-comment"))
        (:form :action (forms::form-action form)
               :method (forms::form-method form)
               :id (forms::form-id form)
               (:textarea ;; :type "text"
                :placeholder "Do you have some comments to note?"
                :class "form-control"
                :name (string-downcase (forms::field-name comment))
                ;; :value (string-downcase (forms::field-name comment))
                :rows "10"
                (who:str (forms::field-value comment)))
               (who:str (forms::field-value comment)))))))
