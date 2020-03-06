;;;; lispy-fc.lisp

(in-package #:lispy-fc)

(defgeneric turn (ctx obj))

(defclass context ()
  ((parent :initarg :parent
	   :accessor context-parent)))

(defmethod initialize-instance :after ((ctx context) &key)
  (with-slots (parent) ctx
    (unless parent
      (setf parent ctx))))

(defclass context-turn (context) ())

(defclass person ()
  ((name
    :initarg :name
    :accessor person-name)
   (health
    :initarg :health
    :accessor person-health)))

(defclass context-turn-person (context)
  ((person :initarg :person
	   :accessor context-turn-person
	   :initform (error "an initial value was not supplied for slot person"))))

(defmethod turn ((ctx context-turn) (person person))
  (turn (make-instance 'context-turn-person
		       :parent ctx
		       :person person)
	(person-health person)))

(defun make-person (name &key (health (make-instance 'health)))
  (check-type name string)
  (check-type health health)
  (make-instance 'person :name name
			 :health (make-instance 'health)))

(defgeneric inflict-damage (person damage))
(defmethod inflict-damage ((person person) damage) 
  (let ((health (person-health person)))
    (health-inflict-damage health damage)))

(defclass health ()
  ((value
    :initarg :health
    :initform 0
    :accessor health-value)))

(defclass health-damage-over-time (health)
  ((condition
    :initarg :condition
    :initform 0
    :accessor health-condition)
   (short-damage
    :initarg :short-damage
    :initform 0
    :accessor health-short-damage)
   (long-damage
    :initarg :long-damage
    :initform 0
    :accessor health-long-damage)))

(defmethod turn ((ctx context-turn-person) (health health-damage-over-time))
  (with-slots (condition short-damage long-damage) health
    (setf condition (- condition short-damage long-damage))
    (when (> short-damage 0)
      (let ((decay (max 1 (/ short-damage 2))))
	(decf short-damage decay)
	(incf long-damage short-damage)
	(setf short-damage 0)))))

(defgeneric health-inflict-damage (health damage))

(defmethod health-inflict-damage ((health health) damage)
  (decf (health-value health) damage))

(defmethod health-inflict-damage ((health health-damage-over-time) damage)
  (with-slots (condition short-damage) health
    (decf condition damage)
    (incf short-damage damage)))

(defclass health-illness (health)
  ((illness
    :initarg :illness
    :initform 0
    :accessor health-illness)))

(defclass health-tiredness (health)
  ((tiredness
    :initarg :tiredness
    :initform 0
    :accessor health-tiredness)))

(defgeneric health-effectiveness (health))

(defmethod health-effectiveness (health)
  100)

(defmethod health-effectiveness :around (health)
  (let ((effectiveness (call-next-method)))
    (/ (truncate effectiveness) 100)))

(defmethod health-effectiveness ((health health-illness))
  (let ((effectiveness (call-next-method))
	(illness (health-illness health)))
    (cond ((> illness 0)
	   (let ((penalty (+ 5 (* 5 (expt illness 2)))))
	     (* effectiveness (- 1 (/ (min 95 penalty) 100)))))
	  (t effectiveness))))

(defmethod health-effectiveness ((health health-tiredness))
  (let ((tiredness (health-tiredness health))
	(effectiveness (call-next-method)))
    (cond ((> tiredness 90) (* effectiveness 0.9))
	  ((> tiredness 60) (* effectiveness 0.95))
	  (t effectiveness))))
