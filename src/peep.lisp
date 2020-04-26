(in-package #:cl-pestilence)

;; A peep goes places through other places.
;; They have their daily routine, can be in :healthy, :frail, :sick, :critical or :dead condition
;; They also have an immune system that remembers what they've been infected with.
(defclass peep ()
  ((routine :initarg :routine :initform (list) :accessor routine)
   (health :initarg :health :initform 1)
   (immune-system :initarg :immune-system :initform nil :accessor immune-system)
   (contagions :initform nil :accessor contagions)))
