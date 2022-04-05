;;;; remember.asd

(pushnew :hunchentoot-no-ssl *features*)

(asdf:defsystem #:remember
  :description "Describe remember here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:hunchentoot
               ;; forms
               #:cl-forms
               #:cl-forms.who
               #:cl-forms.who.bootstrap
               #:anaphora
               #:cl-who
               #:parenscript)
  :components ((:file "package")
               (:file "token")
               (:file "login")
               (:file "routes")
               (:file "templates")
               (:file "templates/image")
               (:file "templates/geolocation")
               (:file "remember")))
