(in-package :remember)

(flet ((reader (what) (lambda (entry) (getf entry what)))
       (writer (what) (lambda (value entry) (setf (getf entry what) value))))
  (forms:defform picture-annotation (:id *form-name*
                                     :enctype "multipart/form-data")
    ((pictures :list
               :type (list :file
                           :multiple-p t
                           :accept "image/")
               :label "Pictures"
               :reader (reader 'pictures))
     (exact-image-text :string
                       :reader (reader 'exact-image-text)
                       :writer (writer 'exact-image-text)
                       :label "Exact text of the image(s)")
     (translation-german :string
                          :reader (reader 'translation-german)
                          :writer (writer 'translation-german)
                          :label "Deutsche Übersetzung")
     (translation-english :string
                          :reader (reader 'translation-english)
                          :writer (writer 'translation-english)
                          :label "Translation English")
     (translation-hebrew :string
                          :reader (reader 'translation-hebrew)
                          :writer (writer 'translation-hebrew)
                          :label "תרגום עברית"))))


(defmethod render-form (form (name (eql 'picture-annotation)))
  (let ((pictures-list (car (forms::form-fields form)))
        (fields (cdr (forms::form-fields form))))
    (print (forms::field-value (cdr pictures-list)))
    (who:with-html-output-to-string (forms.who:*html*)
      (:div :class "row"
            (:div :class "col"
                  (loop :for field :in (forms::field-value (cdr pictures-list))
                        :do (let ((path (typecase field
                                                (string field)
                                                (pathname field)
                                                (t (forms::field-name field)))))
                                    (who:htm
                                     (:img :src (format nil "/static/~a" path)
                                           :class "rounded img-thumbnail")))))
            (:div :class "col"
                  (:form :action (forms::form-action form)
                         :method (forms::form-method form)
                         :id (forms::form-id form)
                         (loop :for (-field-name . field) :in fields
                               :for field-name = (string-downcase -field-name)
                               :do (who:htm
                                    (:label :for field-name
                                            (:span (who:str (forms::field-label field))))
                                    (:textarea :type "text"
                                               :id field-name
                                               :class "form-control"
                                               :rows 4
                                               :name field-name
                                               (who:str (forms:field-value field)))))))))))
