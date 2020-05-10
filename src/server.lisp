(in-package :cl-pestilence)

(defparameter *worlds* (make-hash-table :test 'equalp))

(house:define-http-type (:world)
    :type-expression `(gethash ,house:parameter *worlds*)
    :type-assertion `(not (null ,house:parameter)))

(house:define-json-handler (api/world/state) ((world :world))
  world)

(house:define-handler (/) ()
  (cl-who:with-html-output-to-string (s nil :prologue t :indent t)
    (:html
     (:head
      (:title "cl-pestilence")

      ;; (:link :rel "stylesheet" :href "/css/pestilence.css")

      (:script :type "text/javascript" :src "/js/base.js")
      (:script :type "text/javascript" :src "/js/pestilence.js"))

     (:body
      (:h1 "Welcome to pestilence")))))

(defun main (&optional argv &key (port 4141))
  (declare (ignorable argv))
  (house:start port))
