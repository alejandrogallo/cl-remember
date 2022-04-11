(in-package #:remember)

(defvar *logins* nil)
(defvar *acceptor* nil)
(defvar *entry-schema* nil)
(defvar *config* nil)
(defvar *prepare-item-function* nil)
(defvar *list-item-formater*)
(defvar *create-new-item-function*)
(defvar *sort-entries-function* nil)

;; TODO
(defvar *create-id-function* nil
  "Should be a function of entries and entry")

(defun read-entries-from-file (path)
  (declare (type pathname path))
  (with-open-file (s path :direction :input)
    (read s)))

;; (load "config.lisp")

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
                  (:ul :class "list-group"
                       (loop for entry in (progn (funcall *sort-entries-function* entries)
                                                 entries)
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
                                                          entry)))
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
                                             )))))))
            ;; javascript
            (:script (who:str
                      (ps
                        ;; get id from the url
                        (defun get-url-id ()
                          (let* ((url-line (chain location href))
                                 (m (chain url-line (match (regex "#(.*)")))))
                            (when m
                              (chain m 1))))

                        (defun mark-active ()
                          (let* ((id (get-url-id))
                                 (el (chain document (get-element-by-id id) )))
                            (when el
                              (chain el class-list (add "list-group-item-info"))
                              el)))
                        ;; run the mark active function
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
             (copy-item (funcall *create-new-item-function* current-item)))
        (setf (getf copy-item 'is-new) t)
        (nconc (getf *login* :entries) (list copy-item))
        (hunchentoot:redirect (item-list-path
                               :item (1+ item))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric render-form (form name))
(defmethod render-form (form name)
  (who:with-html-output-to-string (forms.who:*html*)
    (forms:with-form-theme 'forms.who:bootstrap-form-theme
      (forms:with-form-renderer :who
        (forms:render-form form)))))

(flet ((reader (what) (lambda (entry) (getf entry what)))
       (writer (what) (lambda (value entry) (setf (getf entry what) value))))

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
          :reader (reader 'row))
     (gravestone-number :integer
          :label "Grave"
          :writer (writer 'gravestone-number)
          :reader (reader 'gravestone-number))))

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

;; POST SAVE
(hunchentoot:define-easy-handler (saveall-handler :uri "/post/saveall") ()
  (let ((out-path (getf *config* :out-path))
        (i 0))
    (loop for login in *logins*
          do (let* ((cleaned (make-pathname
                              :name (substitute #\- #\space
                                                (getf login :name))))
                    (login-out (ensure-directories-exist
                                (merge-pathnames cleaned
                                                 out-path))))
               (incf i)
               (with-open-file (s login-out :if-exists :supersede
                                            :direction :output)
                 (format t "~&Saving ~a" login-out)
                 (format s "~s" (getf login :entries)))))
    (format nil "~&succesfully saved ~a items~%" i)))


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

                ;; probressbar
                (let ((current (float (* 100 (/ (1+ field) (length fields))))))
                  (who:htm
                   (:div :class "progress"
                         (:div :class "progress-bar"
                               :role "progressbar"
                               :style (format nil "width: ~d%" current)
                               :aria-valuenow "25"
                               :aria-valuemin "0"
                               :aria-valuemax "100"))))
                (:br)

                (:script
                 (who:str
                  (ps
                    (defvar remember-inputs-changed nil)

                    (defmacro on! (el trigger fn)
                      `(chain ,el (add-event-listener ,trigger ,fn)))

                    (defun rb-get-inputs ()
                      (chain document (query-selector-all "input, textarea")))

                    (defun rb-get-submit ()
                      (chain document (get-element-by-id "footer-submit")))

                    (defun rb-on-changed (event)
                      (chain console (log (+ "changing"
                                             event)))
                      (setq remember-inputs-changed t)
                      (let ((submit (rb-get-submit)))
                        (chain submit class-list
                               (add "animate__headShake"))))

                    (defun rb-setup-input-elements ()
                      (let ((inputs (rb-get-inputs)))
                        (loop for input in inputs
                              do (chain console (log (+ "changing "
                                                        input)))
                                 (on! input "input" #'rb-on-changed))))

                    (defun rb-init ()
                      (let ((submit (rb-get-submit)))
                        (on! submit "click"
                             (lambda () (setq remember-inputs-changed nil))))
                      (rb-setup-input-elements))

                    (setf (@ window onload)
                          #'rb-init)

                    (setf (@ window onbeforeunload)
                          (lambda () remember-inputs-changed)))))

                (:h2 (who:str (forms::form-name form)))
                (:h3 (who:str (funcall *list-item-formater* current-item)))
                (:div :class ""

                      #+remember-debug
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
