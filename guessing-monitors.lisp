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
    :file-name (merge-pathnames (truename ".") "test.pdf")
    :add-time-and-experiment-to-file-name t)


;; Events to be used for tracing the application
(define-event object-classified (object guessing-object) (node guessing-node))
(define-event agent-speaks (agent guessing-agent) (utterance string))
(define-event agent-adapts (agent guessing-agent) (adaption string))

(define-event-handler (trace-interaction-in-repl object-classified)
  (format (monitor-stream monitor)
    "object: ~a classified as ~a~%"))
(define-event-handler (trace-interaction-in-repl agent-speaks)
  (format (monitor-stream monitor)
    "agent ~a speaks: ~s~%" (id agent) utterance))
    
(define-event-handler (trace-interaction-in-repl agent-adapts)
  (format (monitor-stream monitor)
    "Agent ~a does not know ~s He adapts" (id agent) utterance))
