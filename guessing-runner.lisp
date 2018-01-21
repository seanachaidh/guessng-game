(proclaim '(optimize (debug 3)))
(asdf:make :guessing-game)

(in-package :guessing)

(defparameter *robotdata-path* 
  (babel-pathname :directory (list :up "robotdata"))
  "The path of the scenes repository")

(activate-monitor plot-communicative-success)
(activate-monitor plot-alignment-success)

(activate-monitor export-communicative-success)

;~ (activate-monitor plot-name-competition)

(activate-monitor plot-classification-success)
(activate-monitor trace-interaction-in-repl)
(activate-monitor trace-interaction)

(activate-monitor record-lexes-speaker)

(defparameter *testboard* (make-instance 'blackboard))
(add-data-field *testboard* 'hello (list (list 1 2 3 2 1 5)))

(run-experiments '((minimal ((:alignment-strategy . :minimal))))
  :population-size 10
  :number-of-interactions 1000)
  


(let ((plot-data (get-plot-data-from-blackboards (list *testboard*) 'hello)))
  (format t "~a~%" (serialize-plot-data plot-data))
  (write-serialized-plot-data plot-data (merge-pathnames (truename ".") "hello.lisp")))

;~ (raw-files->evo-plot
  ;~ :raw-file-paths '((:up "Documents" "Projects" "guessng-game" "hello"))
  ;~ :average-windows 1
  ;~ :title "hello")

;~ (run-batch 'guessing-environment 1000 1)
