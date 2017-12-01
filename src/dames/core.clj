(ns dames.core
  (:gen-class))

(defn init-damier []
  (vec
   (for [r (range 10)]
     (vec
      (for [c (range 10)]
        (cond (even? (+ r c)) nil
              (< r 4) \n
              (> r 5) \b
              :else \space))))))

(defn dans-damier? [[r c]]
  (assert (odd? (+ r c)))
  (and (<= 0 r 9)
       (<= 0 c 9)))

(defn pion? [x]
  (or (= x \n) (= x \b)))

(defn dame? [x]
  (or (= x \N) (= x \B)))

(defn noir? [x]
  (or (= x \N) (= x \n)))

(defn blanc? [x]
  (or (= x \b) (= x \B)))

(defn libre? [x]
  (= x \space))

(defn meme-couleur? [x y]
  (or (and (blanc? x) (blanc? y))
      (and (noir? x) (noir? y))))

(defn autre-couleur? [x y]
  (or (and (blanc? x) (noir? y))
      (and (noir? x) (blanc? y))))

(defn to-manoury [[r c]]
  (assert (odd? (+ r c)))
  (+ (* r 5) (quot c 2) 1))

(defn from-manoury [n]
  (assert (<= 1 n 50))
  (let [r (quot (- n 1) 5)
        c (+ (* 2 (rem (- n 1) 5))
             (if (even? r) 1 0))]
    [r c ]))


(def DIR_SE [1 1])
(def DIR_SO [1 -1])
(def DIR_NE [-1 1])
(def DIR_NO [-1 -1])

(defn get-diag [damier depart dir]
  (let [[ri ci] dir]
    (->> (iterate (fn [[r c]] [(+ r ri) (+ c ci)]) depart)
         (take-while (fn [pos] (dans-damier? pos)))
         ;;(map (fn [pos] (get-in damier pos)))
         )))

(defn coups [damier positions]
  (when (seq positions)
    (let [px (first positions)
          x (get-in damier px)]
      (cond (pion? x)
            (let [[py pz] (rest positions)
                  y (when py (get-in damier py))
                  z (when pz (get-in damier pz))]
              (cond (and (autre-couleur? x y) (libre? z)) [[1 pz [py]]]
                    (libre? y)                            [[0 py []]]))
            (dame? x)
            (or (->> (rest positions)
                     (drop-while (fn [pos] (libre? (get-in damier pos))))
                     ((fn [[py & pzs]]
                         (let [y (get-in damier py)]
                           (when (autre-couleur? x y)
                             (->> (take-while (fn [pos] (libre? (get-in damier pos))) pzs)
                                  (map (fn [pos] [1 pos [py]])))))))
                     seq)
                (->> (take-while (fn [pos] (libre? (get-in damier pos))) positions)
                     (map (fn [pos] [0 pos []]))))))))


(defn coups-dame [damier diago]
  (let [[px & pos_suivantes] diago
        x (get-in damier px)]
    (or (->> pos_suivantes
             (drop-while (fn [pos] (libre? (get-in damier pos))))
             ((fn [[py & pzs]]
                (let [y (get-in damier py)]
                  (when (autre-couleur? x y)
                    (->> (take-while (fn [pos] (libre? (get-in damier pos))) pzs)
                         (map (fn [pos] [1 pos [py]])))))))
             seq)
        (->> pos_suivantes
             (take-while (fn [pos] (libre? (get-in damier pos))))
             (map (fn [pos] [0 pos []]))))))

(defn prise-pion [damier diago]
  (let [[px py pz] diago
        x (get-in damier px)
        y (when py (get-in damier py))
        z (when pz (get-in damier pz))]
    (when (and (autre-couleur? x y) (libre? z)) [[1 pz [py]]])))

(defn depl-pion [damier diago]
  (let [[px py] diago
        y (when py (get-in damier py))]
    (when (libre? y) [[0 py []]])))

(def d1
  [
   [nil    \n     nil    \n     nil    \n     nil    \n     nil    \n    ]
   [\n     nil    \n     nil    \n     nil    \n     nil    \n     nil   ]
   [nil    \n     nil    \n     nil    \n     nil    \n     nil    \n    ]
   [\n     nil    \n     nil    \n     nil    \n     nil    \n     nil   ]
   [nil    \space nil    \space nil    \space nil    \space nil    \space]
   [\space nil    \space nil    \space nil    \space nil    \space nil  ]
   [nil    \b     nil    \b     nil    \b     nil    \b     nil    \b    ]
   [\b     nil    \b     nil    \b     nil    \b     nil    \b     nil   ]
   [nil    \b     nil    \b     nil    \b     nil    \b     nil    \b    ]
   [\b     nil    \b     nil    \b     nil    \b     nil    \b     nil   ]
   ])

(def d2
  [
   [nil    \space nil    \B     nil    \N     nil    \space nil    \space]
   [\space nil    \space nil    \space nil    \B     nil    \space nil  ]
   [nil    \space nil    \n     nil    \n     nil    \space nil    \space]
   [\space nil    \space nil    \space nil    \space nil    \b     nil  ]
   [nil    \space nil    \space nil    \space nil    \space nil    \space]
   [\space nil    \space nil    \space nil    \space nil    \space nil  ]
   [nil    \space nil    \space nil    \space nil    \space nil    \space]
   [\space nil    \space nil    \space nil    \space nil    \space nil  ]
   [nil    \space nil    \space nil    \space nil    \space nil    \space]
   [\space nil    \space nil    \space nil    \space nil    \space nil  ]
   ])


;;-----------------------------------------------------------------------------------

;; (defn deplacement-pion [damier [r c]]
;;   (let [x (get-in damier [r c])]
;;     (cond (noir? x) (->> [[(inc r) (dec c)] [(inc r) (inc c)]]
;;                          (filter (fn [rc]
;;                                    (and (dans-damier? rc)
;;                                         (libre? (get-in damier rc))))))
;;           (blanc? x) (->> [[(dec r) (dec c)] [(dec r) (inc c)]]
;;                           (filter (fn [rc]
;;                                     (and (dans-damier? rc)
;;                                          (libre? (get-in damier rc))))))
;;           :else nil)))

;; (defn prise-pion [damier [r c]]
;;   (let [x (get-in damier [r c])]
;;     (->> (for [ri [-1 1] ci [-1 1]
;;                :let [r2 (+ r ri ri) c2 (+ c ci ci)]
;;                :when (and (dans-damier? [r2 c2])
;;                           (libre? (get-in damier [r2 c2])))
;;                :let [r1 (+ r ri) c1 (+ c ci)
;;                      y (get-in damier [r1 c1])]
;;                :when (or (and (blanc? x) (noir? y))
;;                          (and (noir? x) (blanc? y)))]
;;            [[r1 c1] [r2 c2]]))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
