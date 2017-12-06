(ns dames.gui.javafx.DamesApp
  (:gen-class
   :name DamesApp
   :extends javafx.application.Application
   :prefix da-
   ;;:methods [[start [javafx.stage.Stage] void]]
   )
  (:import [javafx.application Application]
           [javafx.scene Group Scene]
           [javafx.scene.shape Circle]
           [javafx.stage Stage]))

(defn da-start [this stage]
  (let [circ (Circle. 40 40 30)
        root (Group. [circ])
        scene (Scene. root 400 300)]
    (doto stage
      (.setTitle "My JavaFX Application")
      (.setScene scene)
      (.show))))
