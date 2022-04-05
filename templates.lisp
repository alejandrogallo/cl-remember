(in-package #:remember)

(defparameter *fa-css*
  "https://cdn.jsdelivr.net/npm/font-awesome@4.7.0/css/font-awesome.min.css")

(defparameter *ba-css*
  '("https://cdn.jsdelivr.net/npm/bootstrap@5.1.1/dist/css/bootstrap.min.css"
    . ;; integrity
    "sha384-F3w7mX95PdgyTmZZMECAngseQB83DfGTowi0iMjiWaeVhAn4FJkqJByhZMI3AhiU"))

(defparameter *ba-js*
  '("https://cdn.jsdelivr.net/npm/bootstrap@5.1.1/dist/js/bootstrap.bundle.min.js"
    .
    "sha384-/bQdsTh/da6pkI1MST/rWKFNjaCP5gBSY4sEBT38Q/9RBh9AH40zEOg7Hlq2THRZ"))

(defparameter *form-name* "property-form")

(defun main-navigation ()
  (who:with-html-output-to-string (s)
    (:nav :class "navbar navbar-expand-lg navbar-light bg-light"
          (:div :class "container-fluid"
                (:a :href "#" :class "navbar-brand" "Remember")
                (:button :class "navbar-toggler"
                         :type "button"
                         :data-bs-toggle "collapse"
                         :data-bs-target "#navbarSupportedContent"
                         :aria-controls "navbarSupportedContent"
                         :aria-expanded "false"
                         :aria-label "Toggle navigation"
                         (:span :class "navbar-toggler-icon"))
                (:div :class "collapse navbar-collapse"
                      :id "navbarSupportedContent"
                      (:ul :class "navbar-nav me-auto mb-2 mb-lg-0"
                           (:li :class "nav-item"
                                (:a :class "nav-link"
                                    :aria-current "page"
                                    :href "/"
                                    (:i :class "fa fa-home")
                                    "Home"))
                           (:li :class "nav-item"
                                (:a :class "nav-link" :href "/logout"
                                    (:i :class "fa fa-sign-out"
                                        "Logout")))))))))

(defmacro main-page ((&key title footer) &rest body)
  `(who:with-html-output-to-string (,(gensym))
     (:html :lang "en"
            (:head (:meta :charset "UTF-8")
                   (:meta :name "apple-mobile-web-app-capable" :content "yes")
                   (:meta :name "viewport"
                          :content "width=device-width, initial-scale=1")
                   (:link :href *fa-css*
                          :rel "stylesheet")
                   (:link :href (car *ba-css*)
                          :rel "stylesheet"
                          :integrity (cdr *ba-css*)
                          :crossorigin "anonymous")
                   (:script :src (car *ba-js*)
                            :integrity (cdr *ba-js*)
                            :crossorigin "anonymous")
                   (:title (who:fmt "Remember | ~a" ,title)))
            (:body (:div :id "navigation" ,(main-navigation))
                   (:div :id "content" :class "container"
                         ,@body)
                   (:footer :id "footer" (who:str ,footer))))))

(defun property-footer (&key item field current-item nfields fields)
    "This function needs
    - field-index"
    (who:with-html-output-to-string (s)

      (:div :class "offcanvas offcanvas-start"
            :tabindex "-1"
            :id "field-list"
            :aria-labelledby "field-listLabel"
            (:div :class "offcanvas-header"
                  (:h5 :class "offcanvas-title"
                       :id "field-listLabel"
                       "All fields")
                  (:button :type "button"
                           :class "btn-close text-reset"
                           :data-bs-dismiss "offcanvas"
                           :aria-label "Close"))
            (:div :class "offcanvas-body"
                  (:div "Click on one filed to go quickly go to the editing")
                  (:ul :class "list-group"
                       (loop for field in fields
                             for i from 0
                             do (who:htm
                                 (:a :class
                                     "list-group-item list-group-item-action"
                                     :href (item-field-path :item item :field i)
                                      (:h3 :class "list-group-item-header"
                                           (who:str field))
                                      ))))))

      ;; todo improve this
      (:br) (:br) (:br) (:br)
      (:div :class "navbar navbar-light bg-light fixed-bottom"
            (:div :class "container-fluid"
                  (:div :class "btn-group"
                        (:a :href "#field-list"
                            :data-bs-toggle "offcanvas"
                            :aria-controls "field-list"
                            :role "button"
                            :tabindex "-1"
                            :class (format nil "btn btn-primary")
                            (:i  :class "fa fa-list"))
                        (:a :href (if (= field 0)
                                      "#"
                                      (item-field-path :item item
                                                       :field (1- field)))
                            :role "button"
                            :tabindex "-1"
                            :class (format nil "btn btn-warning ~a"
                                           (if (= field 0) "disabled" ""))
                            (:i :class "fa fa-angle-left")
                            " prev"))

                  (:button :class (if (getf current-item 'edited)
                                      "btn btn-success"
                                      "btn btn-danger")
                           :form *form-name*
                           :type "submit"
                           (:i :class "fa fa-bolt")
                           " Submit")

                  (:div :class "btn-group"
                        (if (< (1+ field) nfields)
                      (who:htm (:a :href (item-field-path :item item
                                                          :field (1+ field))
                                   :role "button"
                                   :tabindex "-1"
                                   :class "btn fixed-left btn-info"
                                   "next "
                                   (:i :class "fa fa-angle-right")))
                      (who:htm (:a :href (item-list-path :item item)
                                   :role "button"
                                   :tabindex "-1"
                                   :class "btn fixed-right btn-success"
                                   "Entries")))
                        (:a :href (item-list-path :item item)
                            :class "btn btn-primary" :role "button"
                            :tabindex "-1"
                            (:i :class "fa fa-home")))
                  ))
      ))

(defmacro property-page ((&key title
                            item field current-item nfields fields) &rest body)
  `(main-page (:title ,title
               :footer (property-footer :field ,field
                                        :item ,item
                                        :current-item ,current-item
                                        :fields ,fields
                                        :nfields ,nfields))
              ,@body))

(defmacro who (&rest body)
  `(who:with-html-output-to-string (s)
     ,@body))
