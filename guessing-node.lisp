(in-package :guessing)

;This is quite a crowded class

(defclass guessing-node (tree-node)
  ((range :initarg :range
          :accessor range
          :initform (list 0 1))
   (score :type integer
          :accessor score
          :initarg :score
          :initform 0)
   (feature :initarg :feature
	    :initform 'nofeature
	    :accessor feature))
  (:documentation "Class for categorizing nodes"))

(defgeneric left (node)
  (:documentation "The left child of a node"))
(defgeneric right (node)
  (:documentation "The right chid of a node"))
(defgeneric is-same-p (this other)
  (:documentation "Checks whether two meanings are the exact same"))



(defmethod print-object ((node guessing-node) stream)
      (format stream "~a [ ~f - ~f]" (feature node) (car (range node)) (cadr (range node))))

(defmethod is-same-p (this other)
  (and (not (null other))
       (not (null this))
       (= (car (range this)) (car (range other)))
       (= (cadr (range this)) (cadr (range other)))
       (eq (feature this) (feature other))))

(defmethod left ((node guessing-node))
  (car (children node)))
(defmethod right ((node guessing-node))
  (cadr (children node)))
