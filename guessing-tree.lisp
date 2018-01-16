(in-package :guessing)

(export '(tree feat weighted-score classify deep-classify))

(defparameter *prune-level* -5)

(defclass guessing-tree (tree)
  ((score :initform 0
          :initarg :score
          :accessor score)
   (feat  :initform 'nofeat
          :initarg :feat
          :reader feat))
  (:documentation "A tree that can also categorize objects"))

(defgeneric classify (tree obj others &optional countscore)
  (:documentation "Classify an object"))
(defgeneric split-node (tree node)
  (:documentation "Splits the range of a node into two nodes with half the range"))
(defgeneric prune-tree (tree level)
  (:documentation "Prunes a tree. Warning! Prunes only the leaves by removing those with a score equal or lower then level"))
(defgeneric traverse-with-data (tree func &key data &allow-other-keys)
  (:documentation "Traverses a tree with a data object. Func is a predicate. The first node for which predicate is true is returned"))
(defgeneric deep-classify (tree obj others)
  (:documentation "Tries to classify a word until successfull"))

;tostring method
(defmethod print-object ((tree guessing-tree) stream)
  (format stream "Tree ~% Score: ~d ~% Feat: ~a ~%"
    (score tree)
    (feat tree)))

;constructor mehtod
(defmethod initialize-instance :after ((tree guessing-tree) &key &allow-other-keys)
  (add-node tree
            (make-instance 'guessing-node :feature (feat tree))))

;Actually It would be better if I created a different traverse function
(defmethod prune-tree ((tree guessing-tree) (level integer))
  (labels
      ((prune-func (node)
         (when (and (leaf? node) (not (top? node)) (<= (score node) level))
             (cut-node tree node))))
    (traverse tree #'prune-func)))

(defmethod traverse-with-data ((tree guessing-tree) (func function) &key data &allow-other-keys)
    (loop
      with nodes = (list (top tree))
      when (null nodes) return nil end
      when (funcall func (car nodes) data) return (car nodes) end
      do (let ((cur-node (car nodes)))
           (setq nodes (cdr nodes))
           (loop
             for child in (children cur-node)
             do (push child nodes)))))

(defmethod split-node ((tree guessing-tree) (node guessing-node))
  (let*
      ((difference-range (abs (- (car (range node)) (cadr (range node)))))
       (left-node (make-instance 'guessing-node
                    :range (list (car (range node))
                                 (+ (car (range node)) (/ difference-range 2)))
            :feature (feat tree)))
       (right-node (make-instance 'guessing-node
                     :range (list (+ (car (range node)) (/ difference-range 2))
                                  (cadr (range node)))
             :feature (feat tree))))
    (add-node tree right-node :parent node)
    (add-node tree left-node :parent node)))

(defmethod classify ((tree guessing-tree) (obj guessing-object) others &optional (countscore t))
  (labels
      ((test-match (node data)
         (let ((data-value (get-feature-value data (feat tree))))
           (and (leaf? node)
                (>= data-value (car (range node)))
                (<= data-value (cadr (range node)))))))
    (let* ((matching-node (traverse-with-data tree #'test-match :data obj))
       (objects-matching (remove-if-not (lambda (x)
                          (let ((xval (get-feature-value x (feat tree))))
                        (and (>= xval (car (range matching-node)))
                             (<= xval (cadr (range matching-node))))))
                        others)))
      (if (not (= (length objects-matching) 1))
          (progn
             (when countscore
                (split-node tree matching-node)
                (notify classification-finished 0))
             nil)
          (progn
            (when countscore
                (setf (score tree) (+ (score tree) 1))
                (setf (score matching-node) (+ (score matching-node) 1))
                (prune-tree tree *prune-level*)
                (notify classification-finished 1))
            matching-node)))));Return true if classification succeeds

;this procedure is iterative and it is better to fix this
;using the loop macro
(defmethod deep-classify ((tree guessing-tree) (obj guessing-object) others)
  (let ((classify-attempt (classify tree obj others)))
    (if classify-attempt
        classify-attempt
        (deep-classify tree obj others))))
