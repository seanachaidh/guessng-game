(defpackage :guessing
  (:use :common-lisp
        :utils
        :experiment-framework
        :irl
        :monitors
        :physical-robot-world))

(in-package :guessing)

(defclass guessing-object ()
  ((actual-object :initform nil
                  :initarg :actual-object
                  :accessor actual-object)
                  
   (context-features :initform (list)
                     :initarg :context-features
                     :accessor context-features)))
(defgeneric get-feature-value (obj name)
  (:documentation "Gets the feature value with name for object"))

(defmethod print-object ((obj guessing-object) stream)
  (format stream "Naming object: ~a" (id (actual-object obj))))

(defmethod get-feature-value ((obj guessing-object) (name symbol))
  (let ((featval (find name (context-features obj) :test #'eq :key #'car)))
    (if (not featval)
        (error "feature not found")
        (cdr featval))))
