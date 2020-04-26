(in-package #:cl-pestilence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Places
(defclass place ()
  ((tags :initarg :tags :initform nil :accessor tags)
   (occupants :initarg :occupants :initform nil :accessor occupants)
   (points :initform 0 :accessor points)))

(defun place? (thing)
  (eq (find-class 'place) (class-of thing)))

(defun place (&key tags occupants)
  (make-instance 'place :tags tags :occupants occupants))

(defun gen-place ()
  (let ((tag (pick '(:apartment-building :house :cottage
		     :office-building :factory :store
		     :cafe :lounge :theater))))
    (place :tags (list tag))))

(defmethod details ((place place))
  (format nil "====================~%~a {~{~a~}}~%~{  ~a~^~%~}~%"
	  (first (tags place))
	  (rest (tags place))
	  (mapcar #'details (occupants place))))

(defmethod show ((place place))
  (format nil "~20@a ~5a [~{~a~}]~%"
	  (first (tags place)) (points place)
	  (mapcar #'show (occupants place))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Peeps
(defclass peep ()
  ((routine :initarg :routine :initform (list) :accessor routine)
   (todo :initarg :todo :initform nil :accessor todo)
   (health :initarg :health :initform 100 :accessor health)
   (plagues :initform nil :accessor plagues)
   (points :initform 0 :accessor points)))

(defun peep? (thing)
  (eq (find-class 'peep) (class-of thing)))

(defun peep (&key places)
  (make-instance 'peep :routine places :todo places))

(defun health->string (health)
  (cond ((>= health 90) "@")
	((>= health 80) "0")
	((>= health 70) "O")
	((>= health 50) "o")
	((>= health 30) ":")
	((>= health 1)  ".")
	(t "☠")))

(defmethod details ((peep peep))
  (format nil "[~a ~3d [~{ ~a~^ ->~}]]"
	  (health->string (health peep)) (health peep)
	  (mapcar
	   (lambda (place) (first (tags place)))
	   (routine peep))))

(defmethod show ((peep peep)) (health->string (health peep)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Plagues
(defclass plague ()
  ((host :initarg :host :initform nil :accessor host)
   (signature :initarg :host :initform "SIG" :accessor signature)
   (health :initarg :health :initform 10 :accessor health)
   (virulence :initarg :virulence :initform 10 :accessor virulence)
   (efficiency :initarg :efficiency :initform 0.2 :accessor efficiency)
   (reproduce
    :initarg :reproduce
    :initform
    #'plague
    :reader reproduce)
   (strategy
    :initarg :strategy
    :initform
    (lambda (plague peep)
      (feed! plague peep 30))
    :reader strategy)))

(defun plague ()
  (make-instance 'plague))

(defun plague? (thing)
  (eq (find-class 'place) (class-of thing)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Worlds
(defun gen-world (&key (num-places 20) (num-peeps 100))
  (let ((places (loop repeat num-places collect (gen-place))))
    (loop repeat num-peeps
       do (let* ((routine (loop repeat 5 collect (pick places)))
		 (peep (peep :places routine)))
	    (push peep (occupants (first routine)))))
    places))

(defmethod details ((world list))
  (format nil "~%~{~a~}~%" (mapcar #'details world)))

(defmethod show ((world list))
  (format nil "~%~{~a~}~%" (mapcar #'show world)))

(defmethod all-peeps ((world list))
  (loop for place in world append (all-peeps place)))

(defmethod all-peeps ((place place))
  (loop for o in (occupants place) if (peep? o) collect o))

(defmethod tick! ((world list))
  (let ((peeps (all-peeps world)))
    (loop while peeps
       do (setf peeps
		(loop for p = (pop peeps) while p
		   for res = (tick! p)
		   if res collect res))
       do (mapc #'tick! world)
       do (format t "~a" (show world)))
    (loop for p in (all-peeps world)
       do (setf (todo p) (routine p))))
  world)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; The Game
(defmethod infected-by? ((plague plague) (peep peep))
  (member (signature plague) (mapcar #'signature (plagues peep))
	  :test #'string=))

;;; Plague abilities
(defmethod tick! ((plague plague))
  (decf (health plague) 1)
  (funcall (strategy plague) plague (host plague))
  plague)

(defmethod feed! ((self plague) target amount)
  (decf (health target) amount)
  (incf (health self) (* (efficiency self) amount)))

(defmethod infect! ((self plague) (peep peep))
  (unless (infected-by? self peep)
    (let ((child (funcall (reproduce self))))
      (setf (host child) peep)
      (push child (plagues peep)))))

;;; Place abilities
(defmethod tick! ((place place))
  (incf (points place) (length (occupants place)))
  (loop for peep in (all-peeps place)
     if (dead? peep)
     do (decf (points place) 2)
     else do (loop for plague in (plagues peep)
		do (loop for victim in (remove peep (all-peeps place))
		      if (>= (virulence plague) (random 100))
		      do (infect! plague victim))))
  place)

(defmethod heal! ((self place) (peep peep) amount)
  (decf (points self) (* 2 amount))
  (incf (health peep) amount))

(defmethod vaccinat)

;;; Peep abilities
(defmethod tick! ((peep peep))
  (unless (dead? peep)
    (let ((location (pop (todo peep))))
      (incf (points peep))
      (setf (occupants location) (remove peep (occupants location)))
      (push peep (occupants (or (first (todo peep)) (first (routine peep)))))
      (setf (health peep) (min 100 (+ 5 (health peep))))
      (mapc #'tick! (plagues peep))
      (unless (empty? (todo peep))
	peep))))

(defun dead? (thing) (>= 0 (health thing)))

;;; Game abilities
(defun score (world)
  (list :peep (let ((score 0))
		(loop for p in (all-peeps world)
		   unless (dead? p)
		   do (incf score (+ (health p) (points p)))
		   do (decf score (length (plagues p))))
		score)
	:place (let ((score 0))
		 (loop for p in world
		    do (incf score (points p)))
		 score)
	:plague (let ((score 0))
		  (loop for victim in (all-peeps world)
		     do (loop for p in (plaguesvictim)
			   do (incf score (max 0 (health p)))))
		  (loop for target in world
		     if (every
			 (lambda (victim)
			   (not (empty? (plagues victim))))
			 (all-peeps target))
		     do (setf score (* 2  score)))
		  score)))

(house:define-handler (/) ()
  (cl-who:with-html-output-to-string (s nil :prologue t :indent t)
    (:html
     (:head
      (:title "cl-pestilence")

      (:link :rel "stylesheet" :href "/css/pestilence.css")

      (:script :type "text/javascript" :src "/js/base.js")
      (:script :type "text/javascript" :src "/js/pestilence.js"))

     (:body
      (:h1 "Welcome to pestilence")))))

(defun main (&optional argv &key (port 4141))
  (declare (ignorable argv))
  (house:start port))
