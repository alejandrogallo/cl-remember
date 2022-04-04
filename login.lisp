(in-package #:remember)

(defparameter *current-logins* nil)

(unless (boundp '+login-cookie-name+)
  (defconstant +login-cookie-name+ "remember-login-token"))

(defmacro with-login (&rest body)
  `(let ((*login* (get-login)))
     (if *login*
         (progn ,@body)
         (hunchentoot:redirect *login-handler-path*))))

(defun authenticate-login (logins name password)
  (awhen (find name logins
               :test (lambda (x p) (string= (getf p :name) x)))
    (string= password (getf anaphora:it :password))))

(defun logged-in-p ()
  "Return (token . pair) whenever the login is correct"
  (awhen (hunchentoot:cookie-in +login-cookie-name+ hunchentoot:*request*)
    (assoc anaphora:it *current-logins* :test #'string=)))

(defun get-login ()
  (awhen (logged-in-p)
    (awhen (find (cdr anaphora:it) *logins*
                 :test (lambda (x y) (string= x (getf y :name))))
      anaphora:it)))

(defun do-login-user (name)
  (let ((token (create-random-token)))
    (unless (assoc token *current-logins* :test #'string=)
      (hunchentoot:set-cookie +login-cookie-name+ :value token)
      (push (cons token name) *current-logins*))))

(defun do-logout-user ()
  (awhen (hunchentoot:cookie-in +login-cookie-name+ hunchentoot:*request*)
    (awhen (assoc anaphora:it *current-logins* :test #'string=)
      (hunchentoot:set-cookie +login-cookie-name+ :value nil)
      (setq *current-logins*
            (remove anaphora:it *current-logins*
                    :test (lambda (x y) (string= (car x) (car y))))))))
