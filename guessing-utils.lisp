(in-package :guessing)


(defun run-experiments (strategies 
                        &key
                          (population-size 10)
                          (saliency 1)
                          (number-of-interactions 2000) (number-of-series 4)
                          (monitors 
                           '("export-communicative-success")))
  (format t "~%Starting experimental runs")
  (run-batch-for-different-configurations
   :experiment-class 'guessing-environment
   :number-of-interactions number-of-interactions
   :number-of-series number-of-series
   :monitors monitors
   :shared-configuration `((:population-size . ,population-size)
                            (:saliency . ,saliency))
   :configurations strategies
   :output-dir (make-pathname :directory '(:relative "rawdata")))
  (format t "~%Experimental runs finished and data has been generated. You can now plot graphs."))
