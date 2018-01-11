(in-package #:asdf)

(defsystem :guessing-game
  :depends-on (:experiment-framework
               :action-behavior-framework
               :utils
               :monitors
               :physical-robot-world)
  :serial t
  :components 
   ((:file "guessing-object")
    (:file "guessing-node")
    (:file "guessing-agent")
    (:file "guessing-monitors")
    (:file "guessing-tree")
    (:file "guessing-world")
    (:file "guessing-environment")))
