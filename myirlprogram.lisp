(proclaim '(optimize (debug 3)))

(asdf:make :irl)
(defpackage :testprog
  (:use :common-lisp :utils :irl :monitors))

(in-package :testprog)

(defclass myint (entity)
  ((value :type integer
          :initarg :value
          :accessor value)))

(defclass student (entity)
  ((naam :initform ""
         :initarg :naam
         :accessor naam)
   (leeftijd :initform 20
             :initarg :leeftijd
             :accessor leeftijd))
  (:documentation  "Dit is een klasse om een entiteit voor te stellen"))

(defmethod print-object ((obj student) print-stream)
  (format print-stream "~s ~a" (naam obj) (leeftijd obj)))
  
(defmethod print-object ((int myint) print-stream)
  (format print-stream "Integer value ~a" (value int)))

(defclass student-set (entity)
  ((elements :initarg :elements
             :type list
             :accessor elements)))

(defmethod print-object ((studs student-set) print-stream)
  (format print-stream "Studenten~%----------~% ~{~a~%----------~%~}~%" (elements studs)))

(defun fill-ontology (board)
  (set-data board 'studenten
    (list (make-instance 'student :naam "Pieter" :leeftijd 25)
          (make-instance 'student :naam "Sarah" :leeftijd 21)
          (make-instance 'student :naam "Bob" :leeftijd 20))))

(defparameter *studenten* (make-blackboard))
(fill-ontology *studenten*)

;~ (defprimitive search-students ((age myint) (stud student-set))
  ;~ ((age => stud)
    ;~ (let ((studs (remove-if (lambda (x)
                        ;~ (< (leeftijd x) (value age)))
              ;~ (get-data ontology 'studenten))))
      ;~ (when studs
        ;~ (bind (stud 1.0 (make-instance 'student-set :id 1 :elements studs))))))
  
  ;~ ((stud => age)
    ;~ (let ((bindval (apply #'min (mapcar (lambda(x) (leeftijd x))
                      ;~ (get-data ontology 'studenten)))))
      ;~ (bind (age 1.0 (make-instance 'myint :id 1 :value bindval))))))

(defprimitive search-students ((age myint) (stud student))
  ((age => stud)
    (let ((studs (remove-if (lambda (x)
                        (< (leeftijd x) (value age)))
              (get-data ontology 'studenten))))
      (when studs
        (loop for s in studs
          do (bind (stud 1.0 s))))))
  
  ((stud => age)
    (let ((bindval (apply #'min (mapcar (lambda(x) (leeftijd x))
                      (get-data ontology 'studenten)))))
      (bind (age 1.0 (make-instance 'myint :id 1 :value bindval))))))

;~ (activate-monitor trace-irl-in-web-browser-verbose)

(let ((bindings (evaluate-irl-program `((bind myint ?y ,(make-instance 'myint :value 21))
                        (search-students ?y ?x)) *studenten*)))
  (break)
  (loop for b in (car bindings)
    do (format t "Variable: ~a~%Score: ~a~%Value: ~a~%"
                  (var b) (score b) (value b))))
;~ (deactivate-all-monitors)
