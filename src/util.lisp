(in-package #:cl-pestilence)

(defun pick (lst)
  (nth (random (length lst)) lst))

(defun roll (n &key (d 6))
  (loop repeat n summing (+ (random d) 1)))

(defun empty? (lst)
  (null lst))

(defun hash (&rest entries)
  (let ((h (make-hash-table)))
    (loop for (k v) on entries by #'cddr
       do (setf (gethash k h) v))
    h))
