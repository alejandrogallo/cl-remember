(in-package :remember)

(defparameter *config*
  `(:port 9000
    :pictures-path ,(uiop:truenamize "florisdorf/data/")
    :fields (person-name
             questionnaire
             date-of-death
             date-of-birth
             ;; geolocation
             pictures
             comments)))

(unless *acceptor*
  (setq *acceptor* (make-instance 'hunchentoot:easy-acceptor
                                  :port (getf *config* :port))))

(make-date-form date-of-death)
(make-date-form date-of-birth)

(push (hunchentoot:create-folder-dispatcher-and-handler
       "/static/"
       (getf *config* :pictures-path)
       "image/")
      hunchentoot:*dispatch-table*)

(let ((default-date '(day 42 month 42 year 1942 text ""
                      comment ""
                      hebrew-date "")))
  (setq *entry-schema*
        `((:name first-name :init "")
          (:name family-name :init "")
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

(setq *create-new-item-function*
      (lambda (entry)
        (let ((new-item (funcall *prepare-item-function* nil)))
          (setf (getf new-item 'first-name) "???")
          (setf (getf new-item 'family-name) "???")
          (setf (getf new-item 'gravestone-number) (getf entry 'gravestone-number))
          (setf (getf new-item 'row) (getf entry 'row))
          (setf (getf new-item 'group) (getf entry 'group))
          new-item)))

(setq *sort-entries-function*
      (labels ((ensure-number (l) (typecase l
                                    (string (parse-integer l :junk-allowed t))
                                    (t l)))
               (sort-according-to (what &optional respect &key (type :integer))
                 (lambda (entry-1 entry-2)
                   (and (if respect (eq (getf entry-1 respect)
                                        (getf entry-2 respect))
                            t)
                        (case type
                          (:string (string> (getf entry-1 what)
                                            (getf entry-2 what)))
                          (:integer (< (ensure-number (getf entry-1 what))
                                       (ensure-number (getf entry-2 what)))))))))
        (lambda (entries)
          (sort entries (sort-according-to 'group))
          (sort entries (sort-according-to 'row 'group))
          (sort entries (sort-according-to 'gravestone-number 'row))
          (sort entries (sort-according-to 'first-name 'gravestone-number
                                           :type :string))
          entries)))

(defvar *cementery-name* "Fl.")
(setq *list-item-formater*
      (lambda (item)
        (who:with-html-output-to-string (s)
          (:h5 (who:str (format nil "<b>~a</b>, ~a"
                                (getf item 'family-name)
                                (getf item 'first-name))))
          (:div :class "btn-group"
                (:button :class "btn btn-outline-info" :type "button"
                         (who:str *cementery-name*))
                (:button :class "btn btn-success" :type "button"
                         "Group"
                         (:br)
                         (who:str (getf item 'group)))
                (:button :class "btn btn-primary" :type "button"
                         "Row"
                         (:br)
                         (who:str (getf item 'row)))
                (:button :class "btn btn-warning" :type "button"
                               "Grave" (:br)
                               (who:str (getf item 'gravestone-number)))
                #+img
                (:img :src (if (getf item 'picture-paths)
                               (car (getf item 'picture-paths))
                               *no-image-url*)
                      :width "64"
                      :class "img-fluid rounded"))
          )))
(flet
    ((group (n)
       `(:name ,(format nil "Group ~a" n)
         :password ,(format nil "~a" n)
         :entries ,(mapcar *create-id-function* (read-entries-from-file
                    (make-pathname
                     :name
                     (format nil "florisdorf/data/Group-~a.lisp" n)))))))
  (let  ((row 0))
    (defparameter *logins*
      `((:name "Test group"
         :password "0"
         :entries ,(mapcar *prepare-item-function*
                           `(#1=(first-name "Hans"
                                            edited nil
                                            family-name "Musterperson"
                                            group ,(random 20)
                                            row ,(random 20))
                                #1# #1# #1# #1# #1# #1# #1# #1# #1#
                                #1# #1# #1# #1# #1# #1# #1# #1# #1#
                                #1# #1# #1# #1# #1# #1# #1# #1# #1#
                                #1# #1# #1# #1# #1# #1# #1# #1# #1#
                                #1# #1# #1# #1# #1# #1# #1# #1# #1#
                                #1# #1# #1# #1# #1# #1# #1# #1# #1#
                                #1# #1# #1# #1# #1# #1# #1# #1# #1#
                                #1# #1# #1# #1# #1# #1# #1# #1# #1#
                                #1# #1# #1# #1# #1# #1# #1# #1# #1#
                                #1# #1# #1# #1# #1# #1# #1# #1# #1#
                                #1# #1# #1# #1# #1# #1# #1# #1# #1#
                                #1# #1# #1# #1# #1# #1# #1# #1# #1#
                                #1# #1# #1# #1# #1# #1# #1# #1# #1#
                                #1# #1# #1# #1# #1# #1# #1# #1# #1#
                                #1# #1# #1# #1# #1# #1# #1# #1# #1#
                                #1# #1# #1# #1# #1# #1# #1# #1# #1#
                                #1# #1# #1# #1# #1# #1# #1# #1# #1#)))
        ,(group 3)
        ,(group 4)
        ,(group 5)
        ,(group 6)
        ,(group 7)
        ,(group 8)
        ,(group 9)
        ,(group 10)
        ,(group 11)
        ,(group 12)
        ,(group 13)
        ,(group 14)
        ))))
