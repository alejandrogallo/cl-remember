;;;; remember.lisp

(in-package #:remember)

#|

((:first-name ""))

|#


(defparameter *no-image-url*
  "https://longwoodgardens.org/sites/default/files/highlight_images/76758.jpg")




(defvar *acceptor* (make-instance 'hunchentoot:easy-acceptor
                                  :port 9000))

(defvar *entry-schema* nil)


(let ((default-date '(day 42 month 42 year 1942 text "")))
  (setq *entry-schema*
        `((:name first-name :init "")
          (:name given-name :init "")
          (:name date-of-birth :init ,default-date)
          (:name date-of-death :init ,default-date)
          (:name group :init 0)
          (:name row :init 0)
          (:name gravestone-number :init 0)
          (:name is-the-gravestone-there :init t)
          (:name is-the-gravestone-readable :init t)
          (:name is-the-gravestone-standing :init t)
          (:name geolocation :init (LATITUDE 48.27061 LONGITUDE 16.4167))
          (:name pictures :init ())
          (:name comments :init ""))))

(defvar *prepare-item-function* nil)
(setq *prepare-item-function*
      (lambda (entry)
        (declare (optimize (debug 3)))
        (let* ((schema-fields (mapcar (lambda (s) (getf s :name)) *entry-schema*))
               (entry-fields (loop for i in entry by #'cddr collect i))
               (missing-fields (set-difference schema-fields entry-fields)))
          (loop for field in missing-fields
                do (setf (getf entry field)
                         (copy-tree (getf (find field *entry-schema*
                                                :key (lambda (f) (getf f :name)))
                                          :init))))
          entry)))

(defvar *list-item-formater*)
(setq *list-item-formater*
      (lambda (item)
        (who:with-html-output-to-string (s)
          (:h5 (who:str (format nil "<b>~a</b>, ~a"
                                (getf item 'family-name)
                                (getf item 'first-name))))
          (:div :class "btn-group"
                (:button :class "btn btn-outline-success" :type "button"
                         "Group")
                (:button :class "btn btn-success"
                         (who:str (getf item 'group)))
                (:button :class "btn btn-outline-primary" :type "button"
                         "Row")
                (:button :class "btn btn-primary"
                         (who:str (getf item 'row)))
                #+img
                (:img :src (if (getf item 'picture-paths)
                               (car (getf item 'picture-paths))
                               *no-image-url*)
                      :width "64"
                      :class "img-fluid rounded")))))

(defun read-entries-from-file (path)
  (declare (type pathname path))
  (with-open-file (s path :direction :input)
    (read s)))

(let  ((row 0))
  (defparameter *logins*
    `((:name "Group 1"
       :password "group1"
       :entries ,(mapcar *prepare-item-function*
                          `(#1=(first-name "Gallo"
                                edited nil
                                family-name "Alejandro"
                                group ,(random (incf row))
                                row ,(random row))
                            #1# #1# #1# #1# #1# #1# #1# #1# #1# #1# #1# #1#
                            #1# #1# #1# #1# #1# #1# #1# #1# #1# #1# #1# #1#
                            #1# #1# #1# #1# #1# #1# #1# #1# #1# #1# #1# #1#
                            #1# #1# #1# #1# #1# #1# #1# #1# #1# #1# #1# #1#
                            #1# #1# #1# #1# #1# #1# #1# #1# #1# #1# #1# #1#
                            #1# #1# #1# #1# #1# #1# #1# #1# #1# #1# #1# #1#
                            #1# #1# #1# #1# #1# #1# #1# #1# #1# #1# #1# #1#
                            #1# #1# #1# #1# #1# #1# #1# #1# #1# #1# #1# #1#
                            #1# #1# #1# #1# #1# #1# #1# #1# #1# #1# #1# #1#
                            #1# #1# #1# #1# #1# #1# #1# #1# #1# #1# #1# #1#
                            #1# #1# #1# #1# #1# #1# #1# #1# #1# #1# #1# #1#
                            #1# #1# #1# #1# #1# #1# #1# #1# #1# #1# #1# #1#
                            #1# #1# #1# #1# #1# #1# #1# #1# #1# #1# #1# #1#
                            #1# #1# #1# #1# #1# #1# #1# #1# #1# #1# #1# #1#
                            #1# #1# #1# #1# #1# #1# #1# #1# #1# #1# #1# #1#
                            #1# #1# #1# #1# #1# #1# #1# #1# #1# #1# #1# #1#
                            #1# #1# #1# #1# #1# #1# #1# #1# #1# #1# #1# #1#)))
      #+nil(:name "Group 2"
       :password "group2"
       :entries ,(read-entries-from-file #P"data/login-1.lisp")))))




(hunchentoot:define-easy-handler (logout-handler :uri "/logout") ()
  (with-login
    (do-logout-user)
    (hunchentoot:redirect *login-handler-path*)))

(hunchentoot:define-easy-handler (root-handler :uri "/") ()
  (with-login
    (hunchentoot:redirect *home-handler-path*)))

(defmacro %icon (name)
  `(who:htm (:i :class (format nil "fa fa-~a" ,name))))

(defmacro %on-sm (&rest body)
  `(who:htm (:div :class "d-none d-sm-inline" ,@body)))

(hunchentoot:define-easy-handler (home-handler :uri *home-handler-path*) ()
  (with-login
    (setf (hunchentoot:content-type*) "text/html")
    (macrolet ((~badge (style &rest body)
                 `(who:htm (:span :class
                                  (format nil "badge bg-~a rounded-0" ,style)
                                  ,@body))))
     (let ((entries (getf *login* :entries)))
      (main-page
          (:title "Home")
          (:h1 (who:str (getf *login* :name)))
          (:h3 (who:fmt "~a entries" (length entries)))
          ;; (who:fmt "~a <br> ~s" (logged-in-p) *login*)
          (:div :class "container vertical-scrollable"
                :style "{overflow-y: scroll}"
                (:ul :class "list-group list-group-numbered"
                     (loop for entry in entries
                           for i from 0
                           do (who:htm
                               (:li :class "list-group-item d-flex
                                            justify-content-between
                                            align-items-start"
                                    :id (format nil "item-~a" i)
                                    (:div :class "ms-2 me-auto"
                                          (:a :class ""
                                              :href (item-field-path :item i)
                                              (who:str (funcall
                                                        *list-item-formater*
                                                        entry))))
                                    (:div
                                     (if (getf entry 'pictures)
                                         (~badge "success" (%icon "camera"))
                                         (~badge "danger" (%icon "camera")))
                                     (if (getf entry 'is-new)
                                         (~badge "info"
                                                 (%icon "plus-square")
                                                 (%on-sm " NEW")))
                                     (if (getf entry 'seen)
                                         (~badge "success"
                                                 (%icon "eye")
                                                 (%on-sm " SEEN"))
                                         (~badge "danger"
                                                 (%icon "eye-slash")
                                                 (%on-sm " UNSEEN")))
                                     (if (getf entry 'edited)
                                         (~badge "success"
                                                 (%icon "check-square-o")
                                                 (%on-sm " EDITED"))
                                         (~badge "warning"
                                                 (%icon "square-o")
                                                 (%on-sm " UNEDITED")))
                                     (:a :href (item-create-path :item i)
                                         (~badge "success"
                                                 (%icon "plus")))
                                     ))))))
          ;; javascript
          (:script (who:str
                    (ps:ps
                      (defun mark-active ()
                        (let* ((id (ps:chain location href (match (ps:regex "#(.*)")) 1))
                               (el (ps:chain document (get-element-by-id id) )))
                          (when el
                            (ps:chain el class-list (add "list-group-item-info"))
                            el)))
                      (mark-active)))))))))



(hunchentoot:define-easy-handler (login-handler :uri *login-handler-path*) ()
  (let ((post (hunchentoot:post-parameters hunchentoot:*request*)))
    (if post
        (let ((name (cdr (assoc "name" post :test #'string=)))
              (pass (cdr (assoc "password" post :test #'string=))))
          (hunchentoot:log-message* 0 "trying logging name ~s pass ~s"
                                    name pass)
          (if (authenticate-login *logins* name pass)
              ;;
              ;; correct authentication
              (progn
                (hunchentoot:log-message* 0 "~s ~s"
                                          (do-login-user name)
                                          (hunchentoot:cookie-in
                                           +login-cookie-name+))
                (hunchentoot:redirect *home-handler-path*))
              ;;
              ;; if failure, go back to login
              (hunchentoot:redirect *login-handler-path*)))
        ;; serve the login page
        ;; TODO: check if there is login
        (progn (setf (hunchentoot:content-type*) "text/html")
               (main-page
                   (:title "Login")
                   (:h1 "Remember Login")
                   (loop for login in *logins*
                         do (who:htm
                             (:form :action *login-handler-path* :method "POST"
                                    :role "form"
                                    (:legend (who:fmt "~a " (getf login :name))
                                             (:span :class "badge bg-primary"
                                                    (who:fmt "~a entries"
                                                             (length (getf login :entries)))))
                                    (:div :class "input-group mb-3"
                                          (:input :type "password"
                                                  :name "password"
                                                  :class "form-control"
                                                  :placeholder "password")
                                          (:input :type "hidden"
                                                  :name "name"
                                                  :value (getf login :name))
                                          (:div :class "input-group-append"
                                                (:button :type "submit"
                                                         :class "btn btn-outline-primary"
                                                         "Submit")))))))))))


(hunchentoot:define-easy-handler (item-create-handler
                                  :uri *item-create-handler-path*)
    (item)
  (with-login
      (let* ((item (parse-integer item))
             (current-item (nth item (getf *login* :entries)))
             (copy-item (copy-tree current-item)))
        (setf (getf copy-item 'is-new) t)
        (nconc (getf *login* :entries) (list copy-item))
        (hunchentoot:redirect (item-list-id
                               :item (- (length (getf *login* :entries)) 1))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *config*
  `(:port 9001
    :pictures-path ,(uiop:truenamize "./pictures/")
    :fields (person-name
             questionnaire
             date-of-death
             date-of-birth
             geolocation
             pictures)))

(push (hunchentoot:create-folder-dispatcher-and-handler
       "/static/"
       (getf *config* :pictures-path)
       "image/")
      hunchentoot:*dispatch-table*)

(defgeneric render-form (form name))
(defmethod render-form (form name)
  (who:with-html-output-to-string (forms.who:*html*)
    (forms:with-form-theme 'forms.who:bootstrap-form-theme
      (forms:with-form-renderer :who
        (forms:render-form form)))))

(make-date-form date-of-death)
(make-date-form date-of-birth)

(flet ((reader (what) (lambda (entry) (getf entry what)))
       (writer (what) (lambda (value entry) (setf (getf entry what) value))))

  (defun handle-picture-upload (file-field)
    (let* ((new-file-name (create-random-token))
           (new-path (merge-pathnames new-file-name
                                      (getf *config* :pictures-path))))
      (hunchentoot:log-message* 0 "~%Saving picture ~a to ~a~%"
                                (forms::file-path file-field) new-path)
      (uiop:copy-file (forms::file-path file-field)
                      (ensure-directories-exist new-path))
      (setf (forms::file-path file-field) new-path)
      (setf (forms::file-name file-field) new-file-name)))

  (forms:defform pictures (:id *form-name*
                           :enctype "multipart/form-data")
    ((pictures :list
               :type (list :file
                           :multiple-p t
                           :accept "image/"
                           :upload-handler 'handle-picture-upload)
               :add-button t
               :remove-button t
               :label "Pictures"
               :reader (reader 'pictures)
               :writer (lambda (value entry)
                         (when (and value (car value))
                           (hunchentoot:log-message* 0 "Getting file ~a to put in object" value)
                           (push (car value) (getf entry 'pictures)))
                         #+nil(setf (getf entry 'pictures)
                               (append value (getf entry 'picture))))
               )))


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
                         :id "penis"
                         :accept "image/"
                         :name (field-name 0)
                         :capture "camera")
                 (:div :class "row row-cols-md-2 g-4"
                       (:div :class "col"
                             (loop for field in (forms::field-value (cdr pictures-list))
                                   for i from 0
                                   do (let ((path (typecase field
                                                    (string field)
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


  (forms:defform person-name (:id *form-name*)
    ((first-name :string
                 :writer (writer 'first-name)
                 :reader (reader 'first-name)
                 :label "First name")
     (family-name :string
                  :writer (writer 'family-name)
                  :reader (reader 'family-name)
                  :label "Family name")
     ;; (death-date :subform :subform 'date)
     (group :integer
            :label "Group"
            :writer (writer 'group)
            :reader (reader 'group))
     (row :integer
          :label "Row"
          :writer (writer 'row)
          :reader (reader 'row))))

  (forms:defform questionnaire (:id *form-name*)
    ((is-the-gravestone-there :boolean
                              :reader (reader 'is-the-gravestone-there)
                              :writer (writer 'is-the-gravestone-there)
                              :label "Is the gravestone there?")
     (is-the-gravestone-standing :boolean
                                 :reader (reader 'is-the-gravestone-standing)
                                 :writer (writer 'is-the-gravestone-standing)
                                 :label "Is the gravestone standing?")
     (is-the-gravestone-readable :boolean
                                 :reader (reader 'is-the-gravestone-readable)
                                 :writer (writer 'is-the-gravestone-readable)
                                 :label "Is the gravestone readable?")))

  (defmethod render-form (form (name (eql 'questionnaire)))
    (let ((fields (forms::form-fields form)))
      (who:with-html-output-to-string (forms.who:*html*)
        (:form :action (forms::form-action form)
               :method (forms::form-method form)
               :id (forms::form-id form)
               (loop for (fname . field) in fields
                     do (let ((checked? (forms::field-value field)))
                          (flet ((radio-id (m) (format nil "radio-~a-~a" m fname)))
                            (who:htm
                             (:h3 (who:str (forms::field-label field)))
                             (:div :class "form-check form-switch mb-3 mt-3"
                                   (:input :type "radio"
                                           :class "form-check-input"
                                           :id (radio-id :true)
                                           :checked checked?
                                           ;; :value "true"
                                           :name (string-downcase (symbol-name fname)))
                                   (:label :class
                                           "form-check-label badge bg-success"
                                           :for (radio-id :true)
                                           (:i :class "fa fa-check")
                                           " yes"))
                             (:div :class "form-check form-switch mb-3 mt-3"
                                   (:input :type "radio"
                                           :id (radio-id :false)
                                           :class "form-check-input"
                                           :checked (not (forms::field-value field))
                                           :value "false"
                                           :name (string-downcase (symbol-name fname)))
                                   (:label :class "form-check-label badge bg-danger"
                                           :for (radio-id :false)
                                           (:i :class "fa fa-times")
                                           " No")))))))))))

;; POST HTML FORM
(hunchentoot:define-easy-handler (field-form-post-handler :uri "/post/item")
    (item field)
  (with-login
      (let* ((item (parse-integer item))
             (field (parse-integer field))
             (new nil)
             (nfields (length (getf *config* :fields)))
             (current-field (nth field (getf *config* :fields))))
        (format t "~2%:posting ========~%")
        (symbol-macrolet ((current-item (nth item (getf *login* :entries))))
          (let ((form (forms:find-form current-field)))
            (forms:handle-request form)
            (forms:fill-model-from-form form current-item)
            (setf (getf current-item 'edited) t)
            (hunchentoot:redirect (item-field-path :item item :field field)))))))


;; GET HTML FORM
(hunchentoot:define-easy-handler (field-form-handler :uri "/home/item")
    (item field)
  (with-login
      (let* ((item (parse-integer item))
             (field (parse-integer field))
             (fields (getf *config* :fields))
             (nfields (length fields))
             (current-field (nth field (getf *config* :fields))))
        (symbol-macrolet ((current-item (nth item (getf *login* :entries))))
          ;; (setf (getf (nth item (getf *login* :entries)) 'seen) t)
          (setf (getf current-item 'seen) t)
          (let ((form (forms:find-form current-field)))
            (property-page

                (:title (format nil "~@(~a~)" current-field)
                 :item item
                 :field field
                 :current-item current-item
                 :fields fields
                 :nfields nfields)

                (:h2 (who:str (forms::form-name form)))
                (:h3 (who:str (funcall *list-item-formater* current-item)))
                (:div :class ""

                      #+remember-debugg
                      (:code
                       :rows "10" :cols "120"
                       (who:str (format nil (concatenate 'string
                                                         "nfields = ~a<br>"
                                                         "field = ~a<br><br>"
                                                         "~{~a ↔ ~a~^<br>~}")
                                        current-field
                                        nfields
                                        current-item)))
                      (:br)

                      (setf (forms::form-action form)
                            (item-field-post-path :item item
                                                  :field field))
                      (print (forms:fill-form-from-model form
                                                         current-item))
                      (who:str (render-form form current-field)))))))))
