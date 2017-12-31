;; Dit is een testbestand voor het testen van CLOs

(defclass dier ()
  ((soort :initform ""
          :initarg :soort
          :accessor soort))
  (:documentation "Klasse voor het representeren van een dier"))

(defclass entiteit ()
  ((naam :initform ""
         :initarg :naam
         :accessor naam)
   (leeftijd :initform 20
             :initarg :leeftijd
             :accessor leeftijd))
  (:documentation  "Dit is een klasse om een entiteit voor te stellen"))
  
(defclass student (dier entiteit)
  ((studie :initform ""
           :initarg :studie
           :accessor studie))
  (:documentation "Een persoon die studeert"))

(let ((my-test (make-instance 'student :soort "mens" :naam "Pieter" :leeftijd 22 :studie "computers")))
    (format t "Soort: ~s~%" (soort my-test))
    (format t "Naam: ~s~%" (naam my-test))
    (format t "Studies: ~s~%" (studie my-test)))
