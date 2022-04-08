(ql:quickload "cl-yaml")
(ql:quickload "cl-ppcre")

(defun setup-nix-profile-path ()
  "For nix users, maybe you need to link the path where
   cffi looks for native libraries to your user profile."
  (let ((nix-user-path (uiop:truename* "~/.nix-profile/lib/")))
    (format t "Setting cffi path to ~a~%" nix-user-path)
    (pushnew nix-user-path cffi:*foreign-library-directories*)))

(defun value-to-sexpr (ht)
  (declare (type hash-table ht))
  (loop for v being each hash-value of ht using (hash-key k)
        append (list (intern (string-upcase k)) v)))

(defun field-to-sexpr (field)
  (declare (type hash-table field))
  (let ((name (gethash "name" field)))
    (list (intern (format nil "~a" (string-upcase name)))
        (let ((value (gethash "value" field)))
          (etypecase value
            (list (cond
                    ;;
                    ;; This case should be erased, is just there for
                    ;; some old cases from the florisdorf cementery.
                    ;;
                    ((string= name "Dates")
                     (mapcar #'value-to-sexpr value))
                    (t (mapcar #'field-to-sexpr value))))
            ((not hash-table) value)
            (hash-table (value-to-sexpr value)))))))

(defun yaml-to-s-expr (path)
  (declare (type pathname path))
  (let ((entries (yaml:parse path)))
    (loop for entry in entries
          collect
          (loop for field in entry
                append
                (field-to-sexpr field)))))

(defun save-sexpr (sexpr path)
  (declare (type pathname path))
  (with-open-file (s path
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (format s "~s" sexp)))

(defun save-from-yaml (yaml-path path)
  (declare (type pathname path yaml-path))
  (let ((sexp (yaml-to-s-expr yaml-path)))
    (save-sexpr sexpr path)))


;;;; functions for adapting

(defun adapt-florisdorf-sexps (sexps)
  (loop for sexp in sexps
        collect
        (loop for (field val) on sexp by #'cddr
              append (case field
                       (questionnaire (apply #'concatenate (cons 'list val)))
                       (name (subst 'first-name 'first_name
                                    (subst 'family-name 'family_name
                                           val)))
                       (picture (list 'pictures
                                      (unless (string= val "") (list val))))
                       (dates (loop for date in val
                                    append
                                    (list (intern (format nil "DATE-OF-~:@(~A~)"
                                                          (getf date 'name)))
                                          date)))
                       (t (list field val))))))


(loop for yaml in (uiop:directory-files "./florisdorf/data/" "*.yaml")
      do (let* ((lisp-file (ppcre:regex-replace " "
                                                (format nil "~a.lisp"
                                                        (uiop:split-name-type
                                                         (format nil "~a" yaml)))
                                                "-"))
                (sexp (yaml-to-s-expr yaml))
                (cleaned (adapt-florisdorf-sexps sexp)))
           (format t "~&[yaml=>lisp] ~a" lisp-file)
           (with-open-file (s lisp-file :direction :output
                                        :if-exists :supersede
                                        :if-does-not-exist :create)
             (format s "~s" cleaned))))
