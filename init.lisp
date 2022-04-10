(require :asdf)
(load "remember.asd")
(ql:quickload :remember)
(load "config.lisp")
(hunchentoot:start remember::*acceptor*)

#+with-slynk
(let ((slynk-port 4003))
  (ql:quickload :slynk)
  (slynk:create-server :port slynk-port
                       :dont-close t)
  (format t "~&Started slynk server at port ~a" slynk-port))
