(in-package #:asdf)

(defsystem :guessing-game
  :depends-on (:experiment-framework 
               :utils
               :monitors
               :physical-robot-world)
  :serial t
  :components 
   ((:file "guessing-object")
    (:file "guessing-node")
    (:file "guessing-agent")
    (:file "guessing-tree")
    (:file "guessing-environment")
    (:file "guessing-monitors")))
