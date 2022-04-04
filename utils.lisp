(ql:quickload "cl-yaml")

(defun value-to-sexpr (ht)
  (declare (type hash-table ht))
  (loop for v being each hash-value of ht using (hash-key k)
        append (list (intern (string-upcase k)) v)))

(defun field-to-sexpr (field)
  (declare (type hash-table field))
  (list (intern (format nil "~a" (string-upcase (gethash "name" field))))
        (let ((value (gethash "value" field)))
          (etypecase value
            ((not hash-table) value)
            (hash-table (value-to-sexpr value))))))

(defun yaml-to-s-expr (path)
  (declare (type pathname path))
  (let ((entries (yaml:parse path)))
    (loop for entry in entries
          collect
          (loop for field in entry
                append
                (field-to-sexpr field)))))

(defun save-from-yaml (yaml-path path)
  (declare (type pathname path yaml-path))
  (let ((sexp (yaml-to-s-expr yaml-path)))
    (with-open-file (s path :direction :output :if-exists :supersede)
      (format s "~s" sexp))))
