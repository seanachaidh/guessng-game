(in-package :guessing)


(defun run-experiments (strategies 
                        &key
                          (population-size 10)
                          (saliency 1)
                          (number-of-interactions 2000) (number-of-series 4)
                          (monitors 
                           '("export-communicative-success" "export-lexicon-size" "export-classification-success" "record-lexes-speaker" "export-alignment-success")))
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
  
  
(defun create-graph-comparing-strategies (&key (file-name nil) experiment-names measure-name)
  (format t "~%Creating graph for experiments ~a with measure ~a" experiment-names measure-name)
  (raw-files->evo-plot 
   :raw-file-paths 
   (loop for experiment-name in experiment-names
      collect (relative-to-babel (make-pathname :directory (list :relative "rawdata" experiment-name) :name measure-name :type "lisp")))
   :average-windows 100 :y1-label measure-name
   :captions experiment-names                  
   :plot-directory (relative-to-babel (truename "."))
   :error-bars :stdev :error-bar-modes '(:lines)
   :title measure-name
   :plot-file-name (if (null file-name) measure-name file-name))
  (format t "~%Graphs have been created"))
  
(defun create-graph-for-single-strategy (&key (file-name nil) experiment-name measure-names)
  (format t "~%Creating graph for experiment ~a with measures ~a" experiment-name measure-names)
  (raw-files->evo-plot
   :raw-file-paths 
   (loop for measure-name in measure-names
      collect (relative-to-babel (make-pathname :directory (list :relative "rawdata" experiment-name) :name measure-name :type "lisp")))
   :average-windows 100
   :plot-directory (relative-to-babel (truename "."))
   :error-bars '(:percentile 10 90) :error-bar-modes '(:filled)
   :title "Single graph")
  (format t "~%Graphs have been created"))
