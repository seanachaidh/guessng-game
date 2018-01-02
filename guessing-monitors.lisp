(in-package :guessing)

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


;; Events to be used for tracing the application
(define-event object-classified (object guessing-object) (node guessing-node))
(define-event agent-speaks (agent guessing-agent) (utterance string))
(define-event agent-adapts (agent guessing-agent) (adaption string))
(define-event object-picked (agent guessing-agent) (object guessing-object))
(define-event agent-learns (agent guessing-agent) (word string))


;; Nerer we record the value for the alignment success
(define-event-handler (record-alignment-success interaction-finished)
	(record-value monitor
		(let* ((current-hearer (hearer experiment))
		       (current-speaker (speaker experiment))
		       (is-aligned (and (not (or (null (used-word current-speaker))
		                                 (null (used-word current-hearer))))
		                        (equal (used-word current-speaker)
		                               (used-word current-hearer)))))
		   (format t "Speaker: ~s, Hearer: ~s~%" (used-word current-speaker) (used-word current-hearer))
		   (if is-aligned 1 0))))

(define-event-handler (trace-interaction-in-repl interaction-finished)
  (if (communicated-successfully interaction)
    (format (monitor-stream monitor)
      "Interaction successfull~%")
    (format (monitor-stream monitor)
      "Interaction failed!~%")))

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
