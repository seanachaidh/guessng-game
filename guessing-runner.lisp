(proclaim '(optimize (debug 3)))
(asdf:make :guessing-game)

(in-package :guessing)
(trace relative-to-babel)

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

;~ (run-batch 'guessing-environment 3000 1)

(run-experiments '((minimal ((:alignment-strategy . :minimal)))
                   (imitate ((:alignment-strategy . :imitate)))
                   (lateralinh ((:alignment-strategy . :lateral-inhibition)))
                   (thinhibit ((:alignment-strategy . :th-lateral)))
                   (special ((:alignment-strategy . :special-lateral))))
  :population-size 10
  :number-of-interactions 3000)
  

(create-graph-comparing-strategies :experiment-names '("minimal" "imitate" "lateralinh" "thinhibit" "special")
                                   :measure-name "alignment-success")

(create-graph-comparing-strategies :experiment-names '("minimal" "imitate" "lateralinh" "thinhibit" "special")
                                   :measure-name "communicative-success")


(run-experiments '((minimalsmall ((:alignment-strategy . :minimal)))
                   (imitatesmall ((:alignment-strategy . :imitate)))
                   (lateralinhsmall ((:alignment-strategy . :lateral-inhibition)))
                   (thinhibitsmall ((:alignment-strategy . :th-lateral)))
                   (specialsmall ((:alignment-strategy . :special-lateral))))
  :population-size 2
  :number-of-interactions 3000)
  
(create-graph-comparing-strategies :experiment-names '("minimalsmall" "imitatesmall" "lateralinhsmall" "thinhibitsmall" "specialsmall")
                                   :measure-name "alignment-success"
                                   :file-name "alignment-success-small")

(create-graph-comparing-strategies :experiment-names '("minimalsmall" "imitatesmall" "lateralinhsmall" "thinhibitsmall" "specialsmall")
                                   :measure-name "communicative-success"
                                   :file-name "communicative-success-small")
                                   
(create-graph-comparing-strategies :experiment-names '("minimalsmall" "imitatesmall" "lateralinhsmall" "thinhibitsmall" "specialsmall")
                                   :measure-name "ontology-size"
                                   :file-name "ontology-size-small")
                                   
(create-graph-for-single-strategy :experiment-name "minimalsmall" :measure-names '("classification-success") :file-name "communicative-success")

;~ (let ((plot-data (get-plot-data-from-blackboards (list *testboard*) 'hello)))
  ;~ (format t "~a~%" (serialize-plot-data plot-data))
  ;~ (write-serialized-plot-data plot-data (merge-pathnames (truename ".") "hello.lisp")))

;~ (raw-files->evo-plot
  ;~ :raw-file-paths '((:up "Documents" "Projects" "guessng-game" "hello"))
  ;~ :average-windows 1
  ;~ :title "hello")


