(in-package :guessing)

(export '(guessing-environment run-game load-scene trees wins losses trees run-game-category run-game-guessing))



(defclass guessing-environment (action-experiment)
  ((name  :initarg :name
          :initform (gensym "env-")
          :reader name)
   (initialized :initarg :initialized
		:initform nil
		:accessor initialized))
              
  (:documentation "A class for representing the environment"))

;;Configuration values
(define-configuration-default-value :environment-data-set (list "objects-1"))
(define-configuration-default-value :population-size 10)

(defmethod initialize-instance :after ((experiment guessing-environment) &key)
  
  ;~ (setf (our-world experiment) (make-instance 'physical-robot-world
                                              ;~ :data-sets (get-configuration experiment :environment-data-set)))
  ;~ (setf (current-scene (our-world experiment) ) (car (scenes (our-world experiment))))
  
  
  (setf (world experiment) (make-instance 'guessing-world
                                          :robot-world (make-instance 'physical-robot-world
                                               :data-sets (get-configuration experiment :environment-data-set))))
  (setf (population experiment)
    (loop
      for i from 1 to (get-configuration experiment :population-size)
        collect (make-instance 'guessing-agent :id i
                                               :experiment experiment
                                               :name 'a)))

  (let ((tree-collection (loop for feat in
                (features (car (remove-if-not #'object-p
                                  (entities (get-world-model (robot-world (world experiment))
                                                             (name (random-elt (scenes (robot-world (world experiment)))))
                                             'a)))))
                  collect (make-instance 'guessing-tree :feat (name feat) :score 1))))
      (loop for r in (population experiment)
     do (setf (trees r) tree-collection))))

;~ (defmethod interact ((env guessing-environment) interaction &key)
    ;~ (declare (ignore interaction))
    ;~ (load-scene env (random-elt (scenes (our-world env))))
    ;~ (let* (
       ;~ (current-speaker (speaker env))
       ;~ (current-topic (let ((obj (random-elt (objects current-speaker))))
                        ;~ (notify object-picked current-speaker obj)
                        ;~ obj))
       ;~ (current-hearer (hearer env))
       ;~ (speaker-topic-tree (pick-tree current-speaker current-topic))
       ;~ (speaker-classification (let ((cls (deep-classify speaker-topic-tree
                                       ;~ current-topic
                                       ;~ (objects current-speaker))))
                                  ;~ (notify object-classified current-topic cls)
                                  ;~ cls))
       ;~ (speaker-word (let ((w (search-best-word current-speaker speaker-classification)))
                        ;~ (notify agent-speaks current-speaker w)
                        ;~ w))
       ;~ (hearer-found-meaning (search-best-meaning current-hearer speaker-word))
       ;~ (hearer-used-word (search-used-word-for-object current-hearer current-topic)))
       
		  ;~ (if (not hearer-found-meaning)
			;~ (progn
			  ;~ (notify agent-learns current-hearer speaker-word)
			  ;~ (decrease-score current-speaker speaker-classification speaker-word)
			  ;~ (conceptualize current-hearer current-topic speaker-word)
			  ;~ (mark-communicated-successfully env nil)
		  ;~ )
		  ;~ (let ((real-object (locate-meaning current-hearer hearer-found-meaning)))
			;~ (if (or (not real-object) (not (eq (id (actual-object real-object)) (id (actual-object current-topic)))))
			  ;~ (progn
          ;~ (notify agent-adapts current-hearer speaker-word)
          
          ;~ (decrease-score current-hearer hearer-found-meaning speaker-word)
          ;~ (decrease-score current-speaker speaker-classification speaker-word)
          ;~ (mark-communicated-successfully env nil))
			  ;~ (progn
          ;~ (increase-score current-hearer hearer-found-meaning speaker-word)
          ;~ (increase-score current-speaker speaker-classification speaker-word)
          ;~ (mark-communicated-successfully env t)))))
		  ;~ (loop for r in (population env)
			 ;~ do (prune-words r))
		  ;~ (setf (used-word current-speaker) speaker-word)
		  ;~ (setf (used-word current-hearer) hearer-used-word)))
      
;;---------------------------------------------------------------------

;;Actions agents may or may not perform
(defclass pick-action (action)
	((picked-object :type guessing-object
					:initarg :picked-object
					:reader picked-object)
	 (used-word :type string
				:initarg :used-word
				:reader used-word)
	 (classification :type guessing-node
					 :initarg :classification
					 :reader classification))
  (:documentation "Action that consists of picking an object and trying to classify it"))

(defclass hear-action (action)
	((my-object :type guessing-object
			    :initarg :my-object
			    :reader my-object)
	 (your-object :type guessing-object
				  :initarg :your-object
				  :reader your-object)
	 (used-word :type string
				:initarg :used-word
				:reader used-word)
	 (my-classification :type guessing-node
						:initarg :my-classification
						:reader my-classification)
	 (your-classification :type guessing-node
						  :initarg :your-classification
						  :reader your-classification))
	(:documentation "Action for representing hearing"))
	
(defclass feedback-action (action)
	((correction :type guessing-object
				 :initarg :correction
				 :initform nil
				 :reader correction))
	(:documentation "Actions that consists of giving feedback"))

;;Methods for interaction
(defmethod act ((agent guessing-agent) (world guessing-world) (last-action (eql nil)))
	(let* ((current-topic (random-elt (objects world)))
		   (current-tree (pick-tree agent current-topic (objects world)))
		   (classification (deep-classify current-tree current-topic (objects world)))
		   (chosen-word (search-best-word agent classification)))
		(setf (used-word agent) chosen-word)
		(make-instance 'pick-action :picked-object current-topic
									:used-word chosen-word
									:classification classification)))

(defmethod act ((agent guessing-agent) (world guessing-world) (last-action pick-action))
	(let ((found-meaning (search-best-meaning agent (used-word last-action)))
		  (hearer-word (search-used-word-for-object agent (picked-object last-action) (objects world))))
		(setf (used-word agent) hearer-word)
		(if (not found-meaning)
			(progn
        (format t "We are failing")
				(conceptualize agent (picked-object last-action) (used-word last-action) (objects world))
				(setf (communicated-successfully agent) nil)
				(make-instance 'hear-action :my-object nil
											:used-word (used-word last-action)
											:your-object (picked-object last-action)
											:my-classification found-meaning
											:your-classification (classification last-action)))
											
			(let ((real-object (locate-meaning world found-meaning)))
				(if (or (not real-object) (not (eq (id (actual-object (picked-object last-action))) (id (actual-object real-object)))))
					(progn
						(setf (communicated-successfully agent) nil)
						(decrease-score agent found-meaning (used-word last-action)))
					(progn
						(setf (communicated-successfully agent) t)
						(increase-score agent found-meaning (used-word last-action))))
				(make-instance 'hear-action :my-object real-object
											:used-word (used-word last-action)
											:your-object (picked-object last-action)
											:my-classification found-meaning
											:your-classification (classification last-action))))))
											
											
(defmethod act ((agent guessing-agent) (world guessing-world) (last-action hear-action))
  (format t "finishing~%")
	(if (or (null (my-object last-action)) (not (eq (id (actual-object (my-object last-action)))
                                                  (id (actual-object (your-object last-action))))))
		(progn
			(decrease-score agent (your-classification last-action) (used-word last-action))
			(setf (communicated-successfully agent) nil))
		(progn
			(increase-score agent (your-classification last-action) (used-word last-action))
			(setf (communicated-successfully agent) t)))
	(make-instance 'no-action))

(defmethod consolidate-agent ((agent guessing-agent) (world guessing-world))
  (prune-words agent))

