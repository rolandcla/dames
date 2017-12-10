(ns dames.gui.javafx.app
  (:gen-class :extends javafx.application.Application)
  (:import (javafx.application Application)
           (javafx.scene Scene)
           (javafx.scene.layout GridPane)
           (javafx.scene.shape Rectangle)
           (javafx.scene.paint Color))
  )

(defn -main [& args]
  (Application/launch dames.gui.javafx.app args))


(defn init-grid []
  (let [grid-pane (GridPane.)]
    (doseq [x (range 10) y (range 10)]
      (.add grid-pane
            (if (even? (+ x y))
              (Rectangle. 40 40 Color/CORNSILK)
              (Rectangle. 40 40 Color/DARKSLATEGRAY))
            y x))
    grid-pane))

(defn -start [this stage]
  (let [grid (init-grid)
        scene (Scene. grid 500 500)]
    (doto stage
      (.setTitle "Dames (JavaFX GUI)")
      (.setScene scene)
      (.show)))
  )

