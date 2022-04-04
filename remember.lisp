;;;; remember.lisp

(in-package #:remember)

#|

((:first-name ""))

|#



(defvar *acceptor* (make-instance 'hunchentoot:easy-acceptor
                                  :port 9000))
(defvar *list-item-formater*)

(setq *list-item-formater*
      (lambda (item)
        (format nil ":name ~a<br> :surname ~a  « ~a ~a»"
                (getf item 'first-name)
                (getf item 'family-name)
                (getf item 'group)
                (getf item 'row))))

(defun read-configuration ()
  (with-open-file (s "config.lisp" :direction :input)
    (read s)))

(defun read-entries-from-file (path)
  (declare (type pathname path))
  (with-open-file (s path :direction :input)
    (read s)))

(let  ((row 0))
  (defparameter *logins*
    `((:name "Group 1"
       :password "group1"
       :entries (#1=(first-name "Gallo"
                                edited nil
                                family-name "Alejandro"
                                group ,(random (incf row))
                                row ,(random row)
                                geolocation (longitude 42.24 latitude 468.5))
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
                    #1# #1# #1# #1# #1# #1# #1# #1# #1# #1# #1# #1#))
      (:name "Group 2"
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

(hunchentoot:define-easy-handler (home-handler :uri *home-handler-path*) ()
  (with-login
    (setf (hunchentoot:content-type*) "text/html")
    (let ((entries (getf *login* :entries)))
      (main-page
          (:title "Home")
          (:h1 (who:str (getf *login* :name)))
          (:h3 (who:fmt "~a entries" (length entries)))
          ;; (who:fmt "~a <br> ~s" (logged-in-p) *login*)
          (:div :class "container vertical-scrollable"
                :style "{overflow-y: scroll}"
                (:ul :class "list-group"
                     (loop for entry in entries
                           for i from 0
                           do (who:htm
                               (:li :class "list-group-item d-flex justify-content-between"
                                    (:a :id (format nil "item-~a" i) :href "#"
                                        :class "sr-only")
                                    (:a :class "alert alert-info"
                                        :href (item-field-path :item i)
                                        (:span :class
                                               "position-absolute top-50
                                                start-0 translate-middle
                                                badge rounded-pill bg-dark"
                                               (who:str (1+ i)))
                                        (who:str (funcall *list-item-formater* entry)))
                                    (:div (if (getf entry :picture-paths)
                                              (who:htm (:span :class "badge bg-success"
                                                              (%icon "camera")))
                                              (who:htm (:span :class "badge bg-danger"
                                                              (%icon "camera"))))
                                          (if (getf entry :is-new)
                                              (who:htm (:span :class "badge bg-info"
                                                              (%icon "plus-square")
                                                              "NEW")))
                                          (if (getf entry 'seen)
                                              (who:htm (:span :class "badge bg-success"
                                                              (%icon "eye")
                                                              "SEEN"))
                                              (who:htm (:span :class "badge bg-danger"
                                                              (%icon "eye-slash")
                                                              "UNSEEN")))
                                          (if (getf entry 'edited)
                                              (who:htm (:span :class "badge bg-success"
                                                              (%icon "check-square-o")
                                                              "EDITED"))
                                              (who:htm (:span :class "badge bg-warning"
                                                              (%icon "square-o")
                                                              "UNEDITED")))
                                          (:a :href (who:fmt "/group/item/create/~a" i)
                                              (:span :class "badge bg-success"
                                                     (%icon "plus")))
                                          ))))))))))



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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *config*
  '(:port 9001
    :fields (person-name
             questionnaire
             geolocation)))

(defgeneric render-form (form name))
(defmethod render-form (form name)
  (who:with-html-output-to-string (forms.who:*html*)
    (forms:with-form-theme 'forms.who:bootstrap-form-theme
      (forms:with-form-renderer :who
        (forms:render-form form)))))

(flet ((reader (what) (lambda (entry) (getf entry what)))
       (writer (what) (lambda (value entry) (setf (getf entry what) value))))
  (forms:defform person-name (:action "/home/person-name"
                              :id "property-form")
    ((first-name :string
                 :writer (writer 'first-name)
                 :reader (reader 'first-name)
                 :label "First name")
     (family-name :string
                  :writer (writer 'family-name)
                  :reader (reader 'family-name)
                  :label "Family name")
     (group :integer
            :label "Group"
            :writer (writer 'group)
            :reader (reader 'group))
     (row :integer
          :label "Row"
          :writer (writer 'row)
          :reader (reader 'row))))

  (forms:defform questionnaire (:id "property-form")
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
                                 :label "Is the gravestone readable?")
     ))

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
                                           "yes"))
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
                                           "No")))))))
        #+nil
        (who:str (render-form form nil))
      )))
  )

(flet ((reader (what)
         (lambda (entry) (getf (getf entry 'geolocation) what)))
       (writer (what) (lambda (value entry)
                        (unless #1=(getf entry 'geolocation)
                                (setf #1# nil))
                        (setf (getf #1# what) value))))
  (forms:defform geolocation (:action "/home/geolocation"
                              :id "property-form"
                              :method :post)
    ((latitude :string
               :placeholder "latitude"
               :label "Latitude"
               :writer (writer 'latitude)
               :reader (reader 'latitude))
     (longitude :string
                :placeholder "longitude"
               :label "Longitude"
                :writer (writer 'longitude)
                :reader (reader 'longitude))))

  (defmethod render-form (form (name (eql 'geolocation)))
    (who:with-html-output-to-string (forms.who:*html*)
      (:link :rel "stylesheet"
             :href "https://unpkg.com/leaflet@1.7.1/dist/leaflet.css"
             :crossorigin "")
      (:script :src "https://unpkg.com/leaflet@1.7.1/dist/leaflet.js"
               :crossorigin "")
      (forms:with-form-theme 'forms.who:bootstrap-form-theme
        (forms:with-form-renderer :who
          (forms:render-form form)))

      ;; TODO
      #+(or)
      (:code
       (who:str
        (ps:ps
          (defun set-point (lat lng attrs map)
            (let ((circle (ps:chain *L
                                    (circle (list lat lng))
                                    (add-to map))))
              (ps:chain circle
                        (bind-popup (+ lat " - " lng))
                        (open-popup))))
          (let* ((main-lng "todo")
                 (main-lat "todo")
                 (map (ps:chain *L
                                (map "mapid")
                                (set-view (list main-lat main-lng) 8))))
            (ps:chain *L tile-layer
                      (ps:lisp (concatenate
                                'string
                                "https://api.mapbox.com/styles/v1/{id}/tiles/"
                                "{z}/{x}/{y}?access_token=pk.eyJ1IjoibWFwYm94"
                                "IiwiYSI6ImNpejY4NXVycTA2emYycXBndHRqcmZ3N3gif"
                                "Q.rJcFIG214AriISLbB6B5aw")))))))
      (:div :class "col-lg" :style "min-height: 500px;" :id "mapid")
        (who:str "<script>

                  function setPoint (lat, lng, attrs, map) {
                    const circle = L.circle([lat, lng], attrs).addTo(map);
                    circle.bindPopup(`${lat} - ${lng}`).openPopup();
                  }

                  const main_lng = {{entry.value.Geolocation.longitude}};
                  const main_lat = {{entry.value.Geolocation.latitude}};
                  var map = L.map('mapid')
                            .setView([ main_lat, main_lng ], 18);

                  L.tileLayer('https://api.mapbox.com/styles/v1/{id}/tiles/{z}/{x}/{y}?access_token=pk.eyJ1IjoibWFwYm94IiwiYSI6ImNpejY4NXVycTA2emYycXBndHRqcmZ3N3gifQ.rJcFIG214AriISLbB6B5aw', {
                    maxZoom: 25,
                    attribution: 'Map data &copy; <a href=\"https://www.openstreetmap.org/copyright\">OpenStreetMap</a> contributors, ' +
                    'Imagery © <a href=\"https://www.mapbox.com/\">Mapbox</a>',
                    id: 'mapbox/streets-v11',
                    tileSize: 512,
                    zoomOffset: -1
                  }).addTo(map);

                  setPoint(main_lat,
                          main_lng,
                          { color: 'blue'
                          , radius: 1
                          , fillOpacity: 0.5
                          }
                          , map);


                  map.on(\"click\", (e) => {
                    const lat = e.latlng.lat;
                    const lng = e.latlng.lng;
                    console.log(e.latlng);
                    L.circle([lat, lng], {
                      color: 'red',
                      fillColor: '#f03',
                      fillOpacity: 0.5,
                      radius: 2
                    }).addTo(map).bindPopup(`${lat} - ${lng}`).openPopup();
                    document.getElementById(\"latitude\").value = lat;
                    document.getElementById(\"longitude\").value = lng;
                  });


                  map.locate({setView: false, watch: true})
                    .on('locationfound', function(e){
                      var marker = L.marker([e.latitude, e.longitude])
                                    .bindPopup('Your are here :)');
                      var you = L.circle([e.latitude, e.longitude], e.accuracy/2, {
                        weight: 1,
                        color: 'blue',
                        fillColor: '#cacaca',
                        fillOpacity: 0.2
                      })
                      map.addLayer(marker);
                      map.addLayer(you);
                      document.getElementById(\"latitude\").value = e.latitude;
                      document.getElementById(\"longitude\").value = e.longitude;
                    })
                    .on('locationerror', e => {
                      console.log(\"No possible locate you :(\")
                    });



                  </script>")

      )))

(hunchentoot:define-easy-handler (field-form-post-handler :uri "/post/item")
    (item field)
  (with-login
      (let* ((item (parse-integer item))
             (field (parse-integer field))
             (new nil)
             (nfields (length (getf *config* :fields)))
             (current-field (nth field (getf *config* :fields)))
             (current-item (nth item (getf *login* :entries))))
        (let ((form (forms:find-form current-field)))
          (forms:handle-request form)
          (forms:fill-model-from-form form current-item)
          (setf (getf current-item 'edited) t)
          (hunchentoot:redirect (item-field-path :item item :field field))))))




(hunchentoot:define-easy-handler (field-form-handler :uri "/home/item")
    (item field)
  ;; (setf (hunchentoot:content-type*) "text/html")
  ;; (format t "~%~%:item ~a :field ~a~%~%" item field)
  (with-login
      (let* ((item (parse-integer item))
             (field (parse-integer field))
             (fields (getf *config* :fields))
             (nfields (length fields))
             (current-field (nth field (getf *config* :fields))))
        (symbol-macrolet ((current-item (nth item (getf *login* :entries))))
          ;; (setf (getf (nth item (getf *login* :entries)) 'seen) t)
          (setf (getf current-item 'seen) t)
          (property-page

              (:title (format nil "~@(~a~)" current-field)
               :item item
               :field field
               :current-item current-item
               :fields fields
               :nfields nfields)

              (:h3 (who:str (funcall *list-item-formater* current-item)))
              (:div :class ""
                    :scrolling "yes"

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
                    (let ((form (forms:find-form current-field)))
                      (setf (forms::form-action form)
                            (item-field-post-path :item item
                                                  :field field))
                      (print (list :====== :=====))
                      (print (forms:fill-form-from-model form
                                                         current-item))
                      (who:str (render-form form current-field)))))))))
