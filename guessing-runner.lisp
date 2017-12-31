(proclaim '(optimize (debug 3)))
(asdf:make :guessing-game)

(in-package :guessing)

(defparameter *robotdata-path* 
  (babel-pathname :directory (list :up "robotdata"))
  "The path of the scenes repository")

(activate-monitor plot-communicative-success)

(run-batch 'guessing-environment 1000 1)
