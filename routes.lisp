(in-package #:remember)

(defparameter *login-handler-path* "/login")
(defparameter *home-handler-path* "/home")

(defun item-field-path (&key item (field 0))
  (declare (type fixnum item field))
  (format nil "/home/item?item=~a&field=~a" item field))

(defun item-field-post-path (&key item (field 0))
  (declare (type fixnum item field))
  (format nil "/post/item?item=~a&field=~a" item field))

(defun item-list-id (&key item)
  (declare (type fixnum item))
  (format nil "#item-~a" item))

(defun item-list-path (&key item)
  (declare (type fixnum item))
  (format nil "~a~a" *home-handler-path* (item-list-id :item item)))
