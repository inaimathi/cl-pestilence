(in-package #:cl-pestilence)

(defun pick (lst)
  (nth (random (length lst)) lst))

(defun empty? (lst)
  (null lst))
