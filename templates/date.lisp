(in-package :remember)

(defmacro make-date-form (name)
  (flet ((reader (-name what)
           (lambda (entry) (getf (getf entry -name) what)))
         (writer (-name what)
           (lambda (value entry) (setf (getf (getf entry -name) what) value))))
    `(forms:defform ,name (:id *form-name*)
       ((day :integer
             :placeholder "Day"
             :label "Day"
             :writer ,(writer name 'day)
             :accessor ,(reader name 'day)
             :reader ,(reader name 'day))
        (month :integer
               :placeholder "Month"
               :default-value 0
               :label "Month"
               :writer ,(writer name 'month)
               :accessor ,(reader name 'month)
               :reader ,(reader name 'month))
        (year :integer
              :placeholder "Year"
              :label "Year"
              :default-value 0
              :writer ,(writer name 'year)
              :accessor ,(reader name 'year)
              :reader ,(reader name 'year))
        #+nil
        (text :string
              :label "Text"
              :disabled-p t)
        (comment :string
                 :label "Comment"
                 :reader ,(reader name 'comment)
                 :writer ,(writer name 'comment))
        (hebrew-date :string
                     :reader ,(reader name 'hebrew-date)
                     :writer ,(writer name 'hebrew-date)
                     :label "Hebrew date")))))
