(in-package #:remember)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp '+random-token-symbols+)
    (defconstant +random-token-symbols+
      (concatenate 'string
                   "0123456789abcdefghijklmnopqrstuvwxyz"
                   "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))))

(defun create-random-token ()
  (coerce (loop for i below 20
                collect (nth (random #.(length +random-token-symbols+))
                             '#.(coerce +random-token-symbols+ 'list)))
          'string))
