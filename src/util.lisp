(in-package #:cl-pestilence)

(defun pick (lst)
  (nth (random (length lst)) lst))

(defun roll (n &key (d 6))
  (loop repeat n summing (+ (random d) 1)))

(defun empty? (lst)
  (null lst))
