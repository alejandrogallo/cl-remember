(in-package :remember)

(defparameter *config*
  `(:port 9000
    :pictures-path ,(uiop:truenamize "florisdorf/data/")
    :fields (person-name
             questionnaire
             date-of-death
             date-of-birth
             geolocation
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

(let ((default-date '(day 42 month 42 year 1942 text "")))
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
(flet
    ((group (n)
       `(:name ,(format nil "Group ~a" n)
         :password ,(format nil "group~a" n)
         :entries ,(read-entries-from-file
                    (make-pathname
                     :name
                     (format nil "florisdorf/data/Group-~a.lisp" n))))))
  (let  ((row 0))
    (defparameter *logins*
      `((:name "Alejandro Group"
         :password "alejandro"
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
