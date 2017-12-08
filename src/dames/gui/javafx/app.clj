(ns dames.gui.javafx.app
  (:gen-class :extends javafx.application.Application)
  (:import (javafx.application Application)
           (javafx.scene Scene))
  )

(defn -main [& args]
  (Application/launch dames.gui.javafx.app args))

(defn -start [this stage]
  (doto stage
    (.setTitle "Dames (JavaFX GUI)")
    (.show))
  )

