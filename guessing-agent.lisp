(in-package :guessing)

(export '(get-with-best-score word-meaning))

(defparameter *salthres* 1)

(defstruct word
  form
  meaning
  score)

;; Agent here comes from the experiment framework
;; TODO: Implement actions
(defclass guessing-agent (agent)
  ((name :initarg :name
	 :initform 'noname
	 :accessor name)
   (trees :initarg :trees
	  :initform '()
	  :accessor trees
   (used-word :initarg :used-word
	          :initform nil
	          :accessor used-word)
   (original-objects :initarg :original-objects
		     :initform '()
		     :accessor original-objects)
   (words :initarg :words
	  :initform '()
	  :accessor words)
   (objects :initarg :objs
	    :initform '()
	    :accessor objects))
  (:documentation "A class for a robot"))

(defgeneric pick-tree (robot obj)
  (:documentation "Pick a tree randomly based on the score of the trees. The best tree for the given object"))
(defgeneric create-scaled-object (robot obj)
  (:documentation "Creates an object with all the features being context-scaled"))
(defgeneric context-scale (robot obj feat)
  (:documentation "Context scales the feature of an object in the current scene"))
(defgeneric search-word (robot wordstring)
  (:documentation "Searches for a word given its form"))
(defgeneric invent-word (robot meaning)
  (:documentation "Lets the robot invent words"))
(defgeneric search-best-word (robot meaning)
  (:documentation "Searches the best word for a given meaning"))
(defgeneric conceptualize (robot object word)
  (:documentation "Conceptualizes a given word for an object"))
(defgeneric decrease-score (robot meaning word)
  (:documentation "Decreases the score of a word associated with a meaning"))
(defgeneric increase-score (robot meaning word)
  (:documentation "Increases the score of a word associated with a meaning"))
(defgeneric locate-meaning (robot meaning)
  (:documentation "Locates an object based on the meaning it bares"))
(defgeneric prune-words (robot)
  (:documentation "Removes all the unused words from a robots lexicon"))
(defgeneric get-with-best-score (robot)
  (:documentation "Gets the word with the best score"))

(defmethod get-with-best-score ((robot guessing-agent))
  (reduce (lambda (x y)
	    (if (> (word-score x) (word-score y)) x y))
	  (words robot)))

(defmethod decrease-score ((robot guessing-agent) (meaning guessing-node) (word string))
  (let* ((found-word (find-if (lambda (x)
				(and (eq (word-meaning x) meaning)
				     (eq (word-form x) word)))
			      (words robot)))
	 (competing (remove-if-not (lambda (x)
				     (is-same-p (word-meaning x) (word-meaning found-word)))
				   (words robot))))
    (when (> (word-score found-word) 0)
      (setf (word-score found-word) (- (word-score found-word) 0.1)))
    (loop for c in competing
       when (> (word-score c) 0)
       do (setf (word-score c) (- (word-score c) 0.1))
       end)))


(defmethod increase-score ((robot guessing-agent) (meaning guessing-node) (word string))
  (let ((found-word (find-if (lambda (x)
			       (and (eq (word-meaning x) meaning)
				    (eq (word-form x) word)))
			     (words robot))))
    (when (< (word-score found-word) 1)
      (setf (word-score found-word) (+ (word-score found-word) 0.1)))))

(defmethod conceptualize ((robot guessing-agent) (object guessing-object) (word string))
  (let* ((tree (pick-tree robot object))
	 (new-meaning (deep-classify tree object (objects robot))))
    (push (make-word :form word :meaning new-meaning :score 0.5) (words robot))))
    
(defmethod invent-word ((robot guessing-agent) (meaning guessing-node))
  (let* ((vowels "aeiou")
	 (consonants "zrtpqsdfghjklmwxcvbn")
	 (character-list (loop for x from 1 to 6
			       when (evenp x)
				 collect (char vowels (random (length vowels)))
			       end
			       when (not (evenp x))
				 collect (char consonants (random (length consonants))))))
    (push (make-word :form (format nil "~{~A~}" character-list)
		     :meaning meaning
		     :score 0.5)
	  (words robot))
    (car (words robot))))

(defmethod prune-words ((robot guessing-agent))
  (setf (words robot) (remove-if (lambda (x)
				   (<= (word-score x) 0))
				 (words robot))))

(defmethod pick-tree ((robot guessing-agent) (obj guessing-object))
  (let* ((filtered-objects (remove-if (lambda(x)
					(eq (id (actual-object x)) (id (actual-object obj))))
					(objects robot)))
	 (selected-trees (sort (loop
				 for tree in (trees robot)
				 collect (cons tree (reduce #'min
							    (mapcar (lambda (x)
								      (abs (- (get-feature-value obj (feat tree))
									      (get-feature-value x (feat tree)))))
								    filtered-objects))))
			       (lambda (x y)
				 (> (cdr x) (cdr y)))))
	 (to-choose-from (subseq selected-trees 0 *salthres*))
	 (the-chosen-one (random-elt to-choose-from)))


    (if (= (cdr the-chosen-one) 0)
	(break "Something went wrong here"))
    
    (car (random-elt to-choose-from))))
    
    

(defmethod context-scale ((robot guessing-agent) (obj physical-robot-world-object) (feat symbol))
  (let* ((feature-list (mapcar (lambda (x)
				 (abs (get-fvalue-value x feat)))
			       (original-objects robot)))
	 (minimum-feature (reduce #'min feature-list))
	 (maximum-feature (reduce #'max feature-list))
	 (object-value (abs (get-fvalue-value obj feat))))
    
    (if (= (- minimum-feature maximum-feature) 0)
	0
	(/ (- object-value minimum-feature) (- maximum-feature minimum-feature)))))
    

;Very naive and slow
(defmethod create-scaled-object ((robot guessing-agent) (obj physical-robot-world-object))
  (let ((featurelist (loop
                       for feat in (features obj)
                       collect (cons (name feat) (context-scale robot obj (name feat))))))
    (make-instance 'guessing-object :actual-object obj :context-features featurelist)))

(defmethod search-best-meaning ((robot guessing-agent) (word string))
  (let ((found-words (remove-if-not (lambda (x) (eq (word-form x) word)) (words robot))))
    (if (> (length found-words) 0)
	(word-meaning (reduce (lambda (x y) (if (> (word-score x) (word-score y)) x y)) found-words))
	nil)))

(defmethod search-best-word ((robot guessing-agent) (meaning guessing-node))
  (let ((filtered-list (remove-if-not (lambda (x)
					(eq (word-meaning x) meaning))
				      (words robot))))
    (if (> (length filtered-list) 0)
	(word-form (reduce (lambda (x y) (if (> (word-score x) (word-score y)) x y)) filtered-list))
	(word-form (invent-word robot meaning)))))

(defmethod locate-meaning ((robot guessing-agent) (meaning guessing-node))
  (let ((found-objects (remove-if-not (lambda (x)
					(let ((x-feature-value (get-feature-value x (feature meaning))))
					  (and (>= x-feature-value (car (range meaning)))
					       (<= x-feature-value (cadr (range meaning))))))
				      (objects robot))))
    (if (> (length found-objects) 1)
	nil
	(car found-objects))))
