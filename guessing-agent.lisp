(in-package :guessing)

(export '(get-with-best-score word-meaning))

(defparameter *salthres* 1)

(defclass word ()
  ((form :type string
         :initarg :form
         :accessor form)
   (meaning :type guessing-node
            :initarg :meaning
            :accessor meaning)
   (score :type float
          :initarg :score
          :accessor score)
   (notused :type integer
            :initform 0
            :accessor notused))
  (:documentation "Class for representing word<->meaning combinations"))

;; Agent here comes from the experiment framework
;; TODO: Implement actions
(defclass guessing-agent (action-agent)
  ((name :initarg :name
	 :initform 'noname
	 :accessor name)
   (trees :initarg :trees
	  :initform '()
	  :accessor trees)
   (used-word :initarg :used-word
	          :initform nil
	          :accessor used-word)
   (original-objects :initarg :original-objects
		     :initform '()
		     :accessor original-objects)
   (words :initarg :words
	  :initform '()
	  :accessor words))
  (:documentation "A class for a robot"))

(defgeneric pick-tree (robot obj others)
  (:documentation "Pick a tree randomly based on the score of the trees. The best tree for the given object"))
(defgeneric invent-word (robot meaning)
  (:documentation "Lets the robot invent words"))
(defgeneric search-best-word (robot meaning &optional invent)
  (:documentation "Searches the best word for a given meaning"))
(defgeneric conceptualize (robot object word others)
  (:documentation "Conceptualizes a given word for an object"))
(defgeneric decrease-score (robot word &key delta)
  (:documentation "Decreases the score of a word associated with a meaning"))
(defgeneric increase-score (robot word &key delta)
  (:documentation "Increases the score of a word associated with a meaning"))
(defgeneric prune-words (robot)
  (:documentation "Removes all the unused words from a robots lexicon"))
(defgeneric get-with-best-score (robot)
  (:documentation "Gets the word with the best score"))
(defgeneric search-used-word-for-object (robot object others)
  (:documentation "Return the word that the robot would have used for the given object"))
(defgeneric get-form-competitors (robot word)
  (:documentation "Gets the words with the same meaning as the given word"))
(defgeneric get-meaning-competitors (robot word)
  (:documentation "Gets all the words with the same form as the given word"))
  
(defgeneric align-agent (agent strategy)
  (:documentation "align-agents the agent"))
  

;; --- All align-agent agent methods ---

(defmethod align-agent ((agent guessing-agent) (strategy (eql :imitate)))
  (when (eq (discourse-role agent) :hearer)
    (let ((competitors (get-form-competitors agent (used-word agent))))
      (loop for c in competitors
        do (setf (words agent) (remove c (words agent)))))))
        

(defmethod  align-agent ((agent guessing-agent) (strategy (eql :minimal)))
  (when (communicated-successfully agent)
  (let ((competitors (get-form-competitors agent (used-word agent))))
    (loop for c in competitors
      do (setf (words agent) (remove c (words agent)))))))
      
(defmethod align-agent ((agent guessing-agent) (strategy (eql :lateral-inhibition)))
  (if (communicated-successfully agent)
    (let ((competitors (get-form-competitors agent (used-word agent))))
      (increase-score agent (used-word agent))
      (loop for c in competitors
        do (decrease-score agent c)))
    (when (eql (discourse-role agent) 'speaker)
      (decrease-score agent (used-word agent)))))

(defmethod align-agent ((agent guessing-agent) (strategy (eql :th-lateral)))
  (if (communicated-successfully agent)
    (let ((competitors (if (eql (discourse-role agent) 'speaker)
                          (get-form-competitors agent (used-word agent))
                          (get-meaning-competitors agent (used-word agent)))))
      (setf (score (used-word agent)) (+ (* (score (used-word agent))
                                            (- 1 (get-configuration agent :lateral-inc-delta)))))
      (loop for c in competitors
        do (setf (score c) (- (* (score c) (- 1 (get-configuration agent :lateral-dec-delta)))))))
    (when (eql (discourse-role agent) 'speaker)
      (setf (score (used-word agent)) (- (* (score (used-word agent))
                                            (- 1 (get-configuration agent :lateral-dec-delta))))))))

(defmethod align-agent ((agent guessing-agent) (strategy (eql :special-lateral)))
  (if (communicated-successfully agent)
    (let ((competitors (get-form-competitors agent (used-word agent))))
      (setf (score (used-word agent)) (+ (* (score (used-word agent))
                                            (- 1 (get-configuration agent :lateral-inc-delta)))))
      (loop for c in competitors
        do (setf (score c) (- (* (score c) (- 1 (get-configuration agent :lateral-dec-delta)))))))
    (when (eql (discourse-role agent) 'speaker)
      (setf (score (used-word agent)) (- (* (score (used-word agent))
                                            (- 1 (get-configuration agent :lateral-dec-delta))))))))

;; -------------------------------

(defmethod search-used-word-for-object ((robot guessing-agent) (object guessing-object) (others list))
  (let* ((used-tree (pick-tree robot object others))
		 (found-meaning (classify used-tree object others nil))
		 (word (search-best-word robot found-meaning nil)))
	word))

(defmethod get-form-competitors ((robot guessing-agent) (word word))
  (remove word (find-all-if (lambda (x)
                              (is-same-p (meaning x) (meaning word)))
                    (words robot))))
                    
(defmethod get-meaning-competitors ((robot guessing-agent) (word word))
  (remove word (find-all-if (lambda (x)
                              (equal (form x) (form word)))
                    (words robot))))

(defmethod get-with-best-score ((robot guessing-agent))
  (reduce (lambda (x y)
	    (if (> (score x) (score y)) x y))
	  (words robot)))

(defmethod decrease-score ((robot guessing-agent) (word word) &key (delta 0.1))
  (setf (score word) (- (score word) delta))
  (when (< (score word) 0)
    (setf (score word) 0)))

(defmethod increase-score ((robot guessing-agent) (word word) &key (delta 0.1))
  (setf (score word) (+ (score word) delta))
  (when (> (score word) 1)
    (setf (score word) 1)))

(defmethod conceptualize ((robot guessing-agent) (object guessing-object) (word string) (others list))
  (let* ((tree (pick-tree robot object others))
	 (new-meaning (deep-classify tree object others)))
    (push (make-instance 'word :form word :meaning new-meaning :score 0.5) (words robot))))
    
(defmethod invent-word ((robot guessing-agent) (meaning guessing-node))
  (let* ((vowels "aeiou")
	 (consonants "zrtpqsdfghjklmwxcvbn")
	 (character-list (loop for x from 1 to 6
			       when (evenp x)
				 collect (char vowels (random (length vowels)))
			       end
			       when (not (evenp x))
				 collect (char consonants (random (length consonants))))))
    (push (make-instance 'word :form (format nil "~{~A~}" character-list)
		     :meaning meaning
		     :score 0.5)
	  (words robot))
    (car (words robot))))

(defmethod prune-words ((robot guessing-agent))
  (setf (words robot) (remove-if (lambda (x)
				   (or (<= (score x) 0)))
				 (words robot))))

(defmethod pick-tree ((robot guessing-agent) (obj guessing-object) (others list))
  (let* ((filtered-objects (remove-if (lambda(x)
					(eq (id (actual-object x)) (id (actual-object obj))))
					others))
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

(defmethod search-best-meaning ((robot guessing-agent) (word string))
  (let ((found-words (remove-if-not (lambda (x) (eq (form x) word)) (words robot))))
    (if (> (length found-words) 0)
	(meaning (reduce (lambda (x y) (if (> (score x) (score y)) x y)) found-words))
	nil)))

(defmethod search-best-word ((robot guessing-agent) (meaning guessing-node) &optional (invent t))
  (let ((filtered-list (remove-if-not (lambda (x)
					(eq (meaning x) meaning))
				      (words robot))))
    (if (> (length filtered-list) 0)
	(reduce (lambda (x y) (if (> (score x) (score y)) x y)) filtered-list)
	(if invent
		(invent-word robot meaning)
		nil))))


