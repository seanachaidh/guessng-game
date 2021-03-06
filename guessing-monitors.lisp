(in-package :guessing)

(defparameter *classification-counter* 0)

;; Some of these monitors are copied from the example
(define-monitor plot-communicative-success
    :class 'gnuplot-graphic-generator
    :documentation "Plots communicative success"
    :data-sources '((average default-record-communicative-success))
    :update-interval 100
    :caption '("communicative success" )
    :x-label "games" 
    :y1-label "communicative success"
    :use-y-axis '(1) 
    :y1-max 1.0 :y1-min 0 
    :draw-y1-grid t :error-bars :min-max
    :graphic-type "pdf"
    :file-name (merge-pathnames (truename ".") "success.pdf")
    :add-time-and-experiment-to-file-name t)
    
(define-monitor export-communicative-success
  :class 'lisp-data-file-writer
  :documentation "Exports communicative success"
  :data-sources '(default-record-communicative-success)
  :file-name (make-pathname :directory '(:relative "rawdata") :name "communicative-success" :type "lisp")
  :add-time-and-experiment-to-file-name nil
  :column-separator " "
  :comment-string "#")

;; Extra monitor for the alignment success
(define-monitor record-alignment-success
    :class 'data-recorder)

;; Edited version of the monitor above to get a plot of the allignment success
(define-monitor plot-alignment-success
    :class 'gnuplot-graphic-generator
    :documentation "Plots communicative success"
    :data-sources '((average record-alignment-success))
    :update-interval 100
    :caption '("alignment success" )
    :x-label "games" 
    :y1-label "alignment success"
    :use-y-axis '(1) 
    :y1-max 1.0 :y1-min 0 
    :draw-y1-grid t :error-bars :min-max
    :graphic-type "pdf"
    :file-name (merge-pathnames (truename ".") "alignment.pdf")
    :add-time-and-experiment-to-file-name t)

(define-monitor export-alignment-success
  :class 'lisp-data-file-writer
  :documentation "Exports communicative success"
  :data-sources '(record-alignment-success)
  :file-name (make-pathname :directory '(:relative "rawdata") :name "alignment-success" :type "lisp")
  :add-time-and-experiment-to-file-name nil
  :column-separator " "
  :comment-string "#")

(define-monitor record-classification-success
  :average-windows 1
	:class 'data-recorder)

(define-monitor plot-classification-success
    :class 'gnuplot-graphic-generator
    :documentation "Plots classification success"
    :data-sources '((average record-classification-success))
    :update-interval 100
    :caption '("Classification success" )
    :x-label "games" 
    :y1-label "classification success"
    :use-y-axis '(1) 
    :y1-max 1.0 :y1-min 0 
    :draw-y1-grid t :error-bars :min-max
    :graphic-type "pdf"
    :file-name (merge-pathnames (truename ".") "classification.pdf")
    :add-time-and-experiment-to-file-name t)
    

(define-monitor export-classification-success
  :class 'lisp-data-file-writer
  :documentation "Exports classification success"
  :data-sources '(record-alignment-success)
  :file-name (make-pathname :directory '(:relative "rawdata") :name "classification-success" :type "lisp")
  :add-time-and-experiment-to-file-name nil
  :column-separator " "
  :comment-string "#")

(define-monitor record-name-competition
  :class 'alist-recorder
  :average-windows 1)

(define-monitor record-lexes-speaker
  :class 'data-recorder)


(define-monitor plot-name-competition 
    :class 'alist-gnuplot-graphic-generator
    :recorder 'record-name-competition
    :average-windows 1
    :draw-y-1-grid t
    :y-label "Population adoption"
    :x-label "Total number of interactions"
    :file-name (merge-pathnames (truename ".") "competition.pdf")
    :graphic-type "pdf")

(define-monitor record-average-lexicon-size
  :class 'data-recorder
  :average-windows 1)

(define-monitor export-lexicon-size
  :class 'lisp-data-file-writer
  :documentation "Exports Ontology size"
  :data-sources '(record-average-lexicon-size)
  :file-name (make-pathname :directory '(:relative "rawdata") :name "lexicon-size" :type "lisp")
  :add-time-and-experiment-to-file-name nil
  :column-separator " "
  :comment-string "#")

;; Events to be used for tracing the application
(define-event object-classified (object guessing-object) (node guessing-node))
(define-event agent-speaks (agent guessing-agent) (utterance string))
(define-event agent-adapts (agent guessing-agent) (adaption string))
(define-event object-picked (agent guessing-agent) (object guessing-object))
(define-event agent-learns (agent guessing-agent) (word string))
(define-event classification-finished (success integer))

;; Here we record tghe value for the classification success
(define-event-handler (record-classification-success classification-finished)
  (if (eq success 0)
    (format t "classification failed!~%")
    (format t "classification succeeded!~%"))
    
  ;~ (format t "AVGvals: ~a~%" (get-average-values monitor))
  
  (let ((recval (if (eq *classification-counter* 0)
                    success
                    (/ (+ (* (current-value monitor) *classification-counter*) success) (+ *classification-counter* 1)))))
    (setq *classification-counter* (+ *classification-counter* 1))
    (record-value monitor recval)))

(define-event-handler (record-classification-success interaction-finished)
  (setq *classification-counter* 0))
  
(define-event-handler (record-lexes-speaker interaction-finished)
  (record-value monitor
    (used-word (speaker experiment))))

;; Here we record the value for the alignment success
(define-event-handler (record-alignment-success interaction-finished)
	(record-value monitor
		(let* ((current-hearer (hearer experiment))
		       (current-speaker (speaker experiment))
		       (is-aligned (and (not (or (null (used-word current-speaker))
		                                 (null (used-word current-hearer))))
		                        (equal (form (used-word current-speaker))
		                               (form (used-word current-hearer))))))
		   (format t "Speaker: ~s, Hearer: ~s~%" (used-word current-speaker) (used-word current-hearer))
		   (if is-aligned 1 0))))
       

;; What percentage of agents use a certain form. IE how common is a form?
(define-event-handler (record-name-competition interaction-finished)
  (loop
    with name-counter = nil
    for agent in (population experiment)
    do (loop for lex in (words agent)
      for found = (find (form lex) name-counter :key #'first)
      if found do (setf (second found) (+ (second found) 1))
      else do (push (list (form lex) 1) name-counter))
    finally (loop for c in name-counter
        do (set-value-for-symbol monitor (intern (first c))
          (/ (second c) (length (population experiment)))))))
  
(define-event-handler (trace-interaction-in-repl interaction-finished)
  (if (communicated-successfully interaction)
    (format (monitor-stream monitor)
      "Interaction successfull~%")
    (format (monitor-stream monitor)
      "Interaction failed!~%")))
      
;;helping function
(defun relative-to-babel (path)
  (let ((firstelem (loop for x in (cdr (pathname-directory (babel-pathname)))
                        collect :up))
        (secondelem (if (null (pathname-name path))
                          (cdr (pathname-directory (truename path)))
                          (append (cdr (pathname-directory (truename path))) (list (pathname-name path))))))
    (append firstelem secondelem)))

(define-event-handler (record-lexes-speaker run-series-finished)
  (format t "~{~a~%~}" (get-average-values monitor))
  (labels ((most-used-meaning (wordlist)
      (loop
        with counterlist = nil
        for w in wordlist
        for elem = (find (meaning w) counterlist :key #'second :test #'is-same-p)
        if (not (null elem))
          do (setf (first elem) (+ (first elem)  1))
        else
          do (push (list 1 (meaning w)) counterlist)
        finally (return (second (reduce (lambda (x y)
                          (if (> (first x) (first y))
                            x
                            y))
                    counterlist))))))
    (let* ((most-used (most-used-meaning (caar (get-average-values monitor))))
           (revwords (reverse (caar (get-average-values monitor))))
           (wordlist (remove-duplicates (loop for w in revwords
                        when (is-same-p most-used (meaning w))
                          collect (form w)) :test #'string=))
           (truth-values (loop
                      for w in wordlist
                      collect (list w (loop for rw in revwords
                                          collect (equal w (form rw))))))
           (frequencies (loop for truth in truth-values
                collect (list (first truth) (loop for val in (second truth)
                                                  for counter from 1
                                                  with x = 0
                                                  when (eq val t)
                                                    do (setq x (+ x 1))
                                                  end
                                                  collect (/ x counter))))))
      (format t "plot for words: ~{~s~}~%" wordlist)
      (loop for freq in frequencies
            for filename = (first freq)
            for serial-data = (list (second freq))
            do (write-serialized-plot-data (list serial-data) (make-pathname :directory (list :relative "tmpgraph") :name filename :type "lisp")))
                                                  
      (raw-files->evo-plot
        :raw-file-paths (loop for w in wordlist
                            collect (relative-to-babel (make-pathname :directory '(:relative "tmpgraph") :name w :type "lisp")))
        :plot-file-name "competition"
        :average-windows 1
        :title "form-meaning-plot"))))
        
      

(define-event-handler (record-average-lexicon-size interaction-finished)
  (let ((value (/ (loop for a in (population experiment) sum (length (words a))) (length (population experiment)))))
    (record-value monitor value)))

(define-event-handler (trace-interaction-in-repl agent-learns)
  (format (monitor-stream monitor)
    "Agent ~a does not know the word ~s and conceptualizes it~%"
    (id agent) word))
(define-event-handler (trace-interaction-in-repl object-picked)
  (format (monitor-stream monitor)
    "Agent: ~a, picks object ~a~%" (id agent) object))
(define-event-handler (trace-interaction-in-repl object-classified)
  (format (monitor-stream monitor)
    "object: ~a classified as ~a~%" object node))
(define-event-handler (trace-interaction-in-repl agent-speaks)
  (format (monitor-stream monitor)
    "agent ~a speaks: ~s~%" (id agent) utterance))
(define-event-handler (trace-interaction-in-repl agent-adapts)
  (format (monitor-stream monitor)
    "Agent ~a does not know ~s He adapts~%" (id agent) adaption))
