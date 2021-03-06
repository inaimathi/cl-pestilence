(in-package #:cl-pestilence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Places
(defclass place ()
  ((tags :initarg :tags :initform nil :accessor tags)
   (occupants :initarg :occupants :initform nil :accessor occupants)
   (points :initform 0 :accessor points)))

(defmethod json:encode-json ((p place) &optional stream)
  (json:encode-json
   (hash :tags (tags p)
	 :occupants (occupants p)
	 :points (points p))
   stream))

(defun place? (thing)
  (eq (find-class 'place) (class-of thing)))

(defun place (&key tags occupants)
  (make-instance 'place :tags tags :occupants occupants))

(defun gen-place ()
  (let* ((tag (pick '(:apartment-building :house :cottage
		     :office-building :factory :store
		      :cafe :lounge :theater)))
	 (tags (if (member tag '(:apartment-building :house :cottage))
		   (list tag :home)
		   (list tag))))
    (place :tags tags)))

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
  ((plan
    :initarg :plan :accessor plan
    :initform (lambda (world)
		(loop repeat (roll 2 :d 6)
		   collect (pick world))))
   (todo :initarg :todo :initform nil :accessor todo)
   (health :initarg :health :initform 100 :accessor health)
   (points :initform 0 :accessor points)
   (plagues :initform nil :accessor plagues)))

(defmethod json:encode-json ((p peep) &optional stream)
  (json:encode-json
   (hash :health (health p)
	 :points (points p)
	 :plagues (plagues p))
   stream))

(defun peep? (thing)
  (eq (find-class 'peep) (class-of thing)))

(defun peep () (make-instance 'peep))

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
	   (todo peep))))

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
    :accessor strategy)))

(defmethod json:encode-json ((p plague) &optional stream)
  (json:encode-json
   (hash :health (health p)
	 :virulence (virulence p)
	 :efficiency (efficiency p))
   stream))

(defun plague ()
  (make-instance 'plague))

(defun plague? (thing)
  (eq (find-class 'place) (class-of thing)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Worlds
(defun gen-world (&key (num-places 20) (num-peeps 100))
  (let ((places (loop repeat num-places collect (gen-place))))
    (loop repeat num-peeps
       do (let* ((peep (peep))
		 (todo (funcall (plan peep) places)))
	    (setf (todo peep) todo)
	    (push peep (occupants (first todo)))))
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
  (let ((peeps (all-peeps world))
	(recovery-amount (make-hash-table)))
    (loop for p in peeps
       do (setf (gethash p recovery-amount) (- 10 (length (todo p)))))
    (loop while peeps
       do (setf peeps
		(loop for p = (pop peeps) while p
		   for res = (tick! p)
		   if res collect res))
       do (mapc #'tick! world)
       do (format t "~a" (show world)))
    (loop for p being the hash-keys in recovery-amount using (hash-value v)
       do (setf (todo p) (funcall (plan p) world)
		(health p) (min 100 (+ (health p) v)))))
  world)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; The Game
(defmethod infected-by? ((plague plague) (peep peep))
  (member (signature plague) (mapcar #'signature (plagues peep))
	  :test #'string=))

(defun dead? (thing) (>= 0 (health thing)))

;;; Plague abilities
(defmethod tick! ((plague plague))
  (unless (dead? plague)
    (decf (health plague) 1)
    (funcall (strategy plague) plague (host plague))
    plague))

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

(defun pay! (a b amount)
  (when (>= (points a) amount)
    (decf (points a) amount)
    (incf (points b) amount)))

(defmethod heal! ((self place) (peep peep) amount)
  (decf (points self) amount)
  (incf (health peep) (* 2 amount)))

(defmethod vaccinate! ((self place) (plague plague) (peep peep))
  (let ((plg (funcall (reproduce plague))))
    (setf (strategy plg) (lambda (plague peep) (declare (ignorable plague peep)) nil)
	  (health plg) 0
	  (host plg) peep)
    (push plg (plagues peep))))

;;; Peep abilities
(defmethod tick! ((peep peep))
  (unless (dead? peep)
    (let ((location (pop (todo peep))))
      (mapc #'tick! (plagues peep))
      (unless (or (empty? (todo peep)) (dead? peep))
	(incf (points peep))
	(setf (occupants location) (remove peep (occupants location)))
	(push peep (occupants (first (todo peep))))
	peep))))

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
		     do (loop for p in (plagues victim)
			   do (incf score (max 0 (health p)))))
		  (loop for target in world
		     if (every
			 (lambda (victim)
			   (not (empty? (plagues victim))))
			 (all-peeps target))
		     do (setf score (* 2  score)))
		  score)))
