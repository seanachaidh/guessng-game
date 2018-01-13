(proclaim '(optimize (debug 3)))
(asdf:make :guessing-game)

(in-package :guessing)

(defparameter *robotdata-path* 
  (babel-pathname :directory (list :up "robotdata"))
  "The path of the scenes repository")

(activate-monitor plot-communicative-success)
(activate-monitor plot-alignment-success)
(activate-monitor plot-name-competition)

(activate-monitor plot-classification-success)
(activate-monitor trace-interaction-in-repl)
(activate-monitor trace-interaction)

(defparameter *testboard* (make-instance 'blackboard))
(add-data-field *testboard* 'hello (list 1 2 3 2 1 5))

(let ((plot-data (get-plot-data-from-blackboards (list *testboard*) 'hello)))
  (format t "~a~%" (serialize-plot-data plot-data)))

;~ (run-batch 'guessing-environment 1000 1)
