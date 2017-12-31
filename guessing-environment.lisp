(in-package :guessing)

(export '(guessing-environment run-game load-scene trees wins losses trees run-game-category run-game-guessing))



(defclass guessing-environment (experiment)
  ((name  :initarg :name
          :initform (gensym "env-")
          :reader name)
   (initialized :initarg :initialized
		:initform nil
		:accessor initialized)
   (our-world :initarg :our-world
              :initform nil
              :accessor our-world))
              
  (:documentation "A class for representing the environment"))

(defgeneric load-scene (env scene)
  (:documentation "Loads a different scene"))
(defgeneric run-game-guessing (env times prunelevel)
  (:documentation "Runs the guessing game"))
(defgeneric run-game-category (env times prunelev)
  (:documentation "Runs the discrimination game"))
(defgeneric mark-agents-successfull (env success)
    (:documentation "Marks all the agents as communicated according to the given paramter"))

;;Configuration values
(define-configuration-default-value :environment-data-set (list "objects-1"))
(define-configuration-default-value :population-size 2)

(defmethod initialize-instance :after ((experiment guessing-environment) &key)
  
  (setf (our-world experiment) (make-instance 'physical-robot-world
                                              :data-sets (get-configuration experiment :environment-data-set)))
  (setf (current-scene (our-world experiment) ) (car (scenes (our-world experiment))))
  
  (setf (population experiment)
    (loop
      for i from 1 to (get-configuration experiment :population-size)
        collect (make-instance 'guessing-agent :id i
                                               :experiment experiment
                                               :name 'a)))

  (let ((tree-collection (loop for feat in
                (features (car (remove-if-not #'object-p
                                  (entities (get-world-model (our-world experiment) (name (current-scene (our-world experiment)))
                                             'a)))))
                  collect (make-instance 'guessing-tree :feat (name feat) :score 1))))
      (loop for r in (population experiment)
     do (setf (trees r) tree-collection))))

(defmethod mark-communicated-successfully ((env guessing-environment) success)
    (loop for a in (agents env)
        do (setf (communicated-successfully a) success)))

(defmethod load-scene ((env guessing-environment) (scene physical-robot-scene)) ;No idea if there is a boolean type specifier
  (setf (current-scene (our-world env)) scene)
  (loop for r in (population env)
     do (progn
	  (setf (original-objects r) (remove-if-not #'object-p
						    (entities (get-world-model (our-world env) (name (current-scene (our-world env)))
									       (name r)))))
	  (setf (objects r) (loop for obj in (original-objects r)
			       collect (create-scaled-object r obj))))))

(defmethod run-game-category ((env guessing-environment) (times integer) (prunelev integer))
  (loop repeat times
     do (let* ((current-robot (car (population env)))
	       (current-object (random-elt (objects current-robot)))
	       (tree (pick-tree current-robot current-object)))
	     (format t "Picked tree ~a ~%" tree)
	     (if (classify tree current-object (objects current-robot))
		 (format t "Classified object ~a successfully with channel ~a ~%"
			 (id (actual-object current-object)) (feat tree))
		 (format t "Failed to classify ~a~%"
			 (id (actual-object current-object))))
			 
	     (format t "Prune level: ~a ~%" *prune-level*)
	     (prune-tree tree prunelev))))

(defmethod interact ((env guessing-environment) interaction &key)
    (declare (ignore interaction))
    ;~ (format t "We are playing game ~d ~%" i)
    (load-scene env (random-elt (scenes (our-world env))))
    (let* (
       (current-speaker (speaker env))
       (current-topic (random-elt (objects current-speaker)))
       (current-hearer (hearer env))
       (speaker-topic-tree (pick-tree current-speaker current-topic))
       (speaker-classification (deep-classify speaker-topic-tree
                          current-topic
                          (objects current-speaker)))
       (speaker-word (search-best-word current-speaker speaker-classification))
       (hearer-found-meaning (search-best-meaning current-hearer speaker-word)))

      (format t "Robot ~a picked object ~a as topic ~%" (name current-speaker) (id (actual-object current-topic)))
      (format t "Robot ~a classifies object as ~a ~%" (name current-speaker) speaker-classification)
      (format t "Robot ~a says ~a ~%" (name current-speaker) speaker-word)

      (if (not hearer-found-meaning)
        (progn
          (format t "Hearer ~a does not know ~a and conceptualizes it ~%" (name current-hearer) speaker-word)
          (decrease-score current-speaker speaker-classification speaker-word)
          (conceptualize current-hearer current-topic speaker-word)
          (mark-communicated-successfully env nil)
          ;~ (setf (coherence-losses *global-logger*) (+ (coherence-losses *global-logger*) 1))
      )
      (let ((real-object (locate-meaning current-hearer hearer-found-meaning)))
        (if (or (not real-object) (not (eq (id (actual-object real-object)) (id (actual-object current-topic)))))
          (progn
            (format t "Hearer ~a has another meaning for ~a : ~a . He adapts ~%"
                (name current-hearer) speaker-word (id (actual-object real-object)))
            
            (decrease-score current-hearer hearer-found-meaning speaker-word)
            (decrease-score current-speaker speaker-classification speaker-word)
            (mark-communicated-successfully env nil))
          ;~ (setf (communicative-losses *global-logger*) (+ (communicative-losses *global-logger*) 1))
          ;~ (setf (coherence-losses *global-logger*) (+ (coherence-losses *global-logger*) 1)))
          
          (progn
            (format t "Both speaker and hearer agree on the meaning of a word ~%")
            (increase-score current-hearer hearer-found-meaning speaker-word)
            (increase-score current-speaker speaker-classification speaker-word)
            (mark-communicated-successfully env t)
            ;~ (if (is-same-p speaker-classification hearer-found-meaning)
                ;~ (setf (coherence-wins *global-logger*) (+ (coherence-wins *global-logger*) 1))
                ;~ (setf (coherence-losses *global-logger*) (+ (coherence-losses *global-logger*) 1)))
          ))))
      (loop for r in (population env)
		 do (prune-words r))))
;; Ik weet niet of ik hier een notify moet uitvoren of niet
