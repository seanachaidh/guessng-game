(proclaim '(optimize (debug 3)))

(asdf:make :irl)
(defpackage :testprog
  (:use :common-lisp :utils))

(in-package :testprog)

(defclass student ()
  ((naam :initform ""
         :initarg :naam
         :accessor naam)
   (leeftijd :initform 20
             :initarg :leeftijd
             :accessor leeftijd))
  (:documentation  "Dit is een klasse om een entiteit voor te stellen"))


(defprimitive search-students ((age integer) (stud student))
  ((age => stud)
    (let ((studs (remove-if (lambda (x)
                        (< x age))
              *studenten*)))
      (when studs
        (bind stud 1.0 studs))))
  
  ((studs => age)
    (let ((bindval (apply #'min (mapcar (lambda(x) (leeftijd x)) *studenten*))))
      (bind age 1.0 bindval))))
