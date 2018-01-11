(in-package :guessing)

(defclass guessing-world (action-world)
    ((robot-world :type physical-robot-world
                  :initarg :robot-world
                  :accessor robot-world)
     (original-objects :type list
                       :initform nil
                       :accessor original-objects)
     (objects :type list
              :initform nil
              :accessor objects))
    (:documentation "The world we play in"))
    
    
;~ (defgeneric load-scene (world scene)
  ;~ (:documentation "Loads a different scene"))    
(defgeneric create-scaled-object (world obj)
  (:documentation "Creates an object with all the features being context-scaled"))
(defgeneric context-scale (world obj feat)
  (:documentation "Context scales the feature of an object in the current scene"))
(defgeneric locate-meaning (world meaning)
  (:documentation "Locates an object based on the meaning it bares"))

(defmethod begin-interaction ((world guessing-world) &rest parameters &key)
  (declare (ignore parameters))
  (format t "beginning interaction~%")
  (setf (current-scene (robot-world world)) (random-elt (scenes (robot-world world))))
  ;;Allways pick robot a in physical-robot-wrold
  (setf (original-objects world) (remove-if-not #'object-p
						    (entities (get-world-model (robot-world world) (name (current-scene (robot-world world))) 'a))))
  
  (setf (objects world) (loop for obj in (original-objects world)
           collect (create-scaled-object world obj)))
  (call-next-method))

(defmethod context-scale ((world guessing-world) (obj physical-robot-world-object) (feat symbol))
  (let* ((feature-list (mapcar (lambda (x)
         (abs (get-fvalue-value x feat)))
             (original-objects world)))
         (minimum-feature (reduce #'min feature-list))
         (maximum-feature (reduce #'max feature-list))
         (object-value (abs (get-fvalue-value obj feat))))
    
    (if (= (- minimum-feature maximum-feature) 0)
        0
        (/ (- object-value minimum-feature) (- maximum-feature minimum-feature)))))
    

;Very naive and slow
(defmethod create-scaled-object ((world guessing-world) (obj physical-robot-world-object))
  (let ((featurelist (loop
                       for feat in (features obj)
                       collect (cons (name feat) (context-scale world obj (name feat))))))
    (make-instance 'guessing-object :actual-object obj :context-features featurelist)))

(defmethod locate-meaning ((world guessing-world) (meaning guessing-node))
  (let ((found-objects (remove-if-not (lambda (x)
					(let ((x-feature-value (get-feature-value x (feature meaning))))
					  (and (>= x-feature-value (car (range meaning)))
					       (<= x-feature-value (cadr (range meaning))))))
				      (objects world))))
    (if (> (length found-objects) 1)
	nil
	(car found-objects))))
