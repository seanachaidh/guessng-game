GUESSING GAME
=============
This is the guessing game by Luc Steels.
Most of its implementation is based the the basic naming game example provided by the babel framework, in particular the implementation of the alignment strategies and the utility functions used to run experiments and create plots. 

Almost every part of the framwork is used except IRL.

Most important classses
-----------------------

* Geussing envrionment: The main experiment class
* Guessing tree: The classification tree used for classifying objects
* Guessing node: Represents a node in the classification tree
* Guessing agent: Represents an agent in the population
* Guessing world: The world the agents play in
* Guessing monitors: The implementation of monitors

Options to play with
--------------------
Saliency: Influences: the trees which are picked
Alignment-strategy: Defines the strategy to use
population-size: influences th population size of the experiment

Look for more information in the report or in the file guessing-runner.lisp. This file generates plot and rawdata. All plots are generated in the root directory of the project.

Please do not remove the folder rawdata and tmpgraph