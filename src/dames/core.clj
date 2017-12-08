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

(defn echange [damier pos1 pos2]
  (let [x (get-in damier pos1)
        y (get-in damier pos2)]
    (-> damier
        (assoc-in pos1 y)
        (assoc-in pos2 x))))

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

(def DIRS [DIR_SE DIR_SO DIR_NE DIR_NO])
(def DIRS_N [DIR_NE DIR_NO])
(def DIRS_S [DIR_SE DIR_SO])

(defn get-diag [damier depart dir]
  (let [[ri ci] dir]
    (->> (iterate (fn [[r c]] [(+ r ri) (+ c ci)]) depart)
         (take-while (fn [pos] (dans-damier? pos)))
         ;;(map (fn [pos] (get-in damier pos)))
         )))

(defn coups-dame [damier diago]
  (let [[px & pos_suivantes] diago
        x (get-in damier px)]
    (or (->> pos_suivantes
             (drop-while (fn [pos] (libre? (get-in damier pos))))
             ((fn [[py & pzs]]
                (let [y (get-in damier py)]
                  (when (autre-couleur? x y)
                    (->> (take-while (fn [pos] (libre? (get-in damier pos))) pzs)
                         (map (fn [pos] [pos py])))))))
             seq)
        (->> pos_suivantes
             (take-while (fn [pos] (libre? (get-in damier pos))))
             (map (fn [pos] [pos nil]))))))

(defn prise-dame [damier diago]
  (let [[px & pos_suivantes] diago
        x (get-in damier px)]
    (->> pos_suivantes
         (drop-while (fn [pos] (libre? (get-in damier pos))))
         ((fn [[py & pzs]]
            (let [y (get-in damier py)]
              (when (autre-couleur? x y)
                (->> (take-while (fn [pos] (libre? (get-in damier pos))) pzs)
                     (map (fn [pos] [pos py])))))))
         seq)
    ))

(defn depl-dame [damier diago]
  (let [[px & pos_suivantes] diago]
    (->> pos_suivantes
         (take-while (fn [pos] (libre? (get-in damier pos))))
         (map (fn [pos] [[px pos] #{}])))))



(defn prise-pion [damier diago]
  (let [[px py pz] diago
        x (get-in damier px)
        y (when py (get-in damier py))
        z (when pz (get-in damier pz))]
    (when (and (autre-couleur? x y) (libre? z)) [[pz py]])))

(defn depl-pion [damier diago]
  (let [[px py] diago
        y (when py (get-in damier py))]
    (when (libre? y) [[[px py] #{}]])))

(defn rafle-pion [damier depart trajet prises]
  #_(println ">" depart trajet prises)
  (->> DIRS
       (map (fn [dir] (get-diag damier depart dir)))
       (map (fn [diago] (prise-pion damier diago)))
       (remove nil?)
       (apply concat)
       (remove (fn [[dest prise]] (prises prise)))
       (mapcat (fn [[dest prise]]
                 (let [n-trajet (conj trajet dest)
                       n-prises (conj prises prise)]
                   (if-let [rs (seq (rafle-pion (echange damier depart dest)
                                                dest n-trajet n-prises))]
                     rs
                     [[n-trajet n-prises]]
                     ))))))

(defn rafle-dame [damier depart trajet prises]
  #_(println ">" depart trajet prises)
  (->> DIRS
       (map (fn [dir] (get-diag damier depart dir)))
       (map (fn [diago] (prise-dame damier diago)))
       (remove nil?)
       (apply concat)
       (remove (fn [[dest prise]] (prises prise)))
       (mapcat (fn [[dest prise]]
                 (let [n-trajet (conj trajet dest)
                       n-prises (conj prises prise)]
                   (if-let [rs (seq (rafle-dame (echange damier depart dest)
                                                dest n-trajet n-prises))]
                     rs
                     [[n-trajet n-prises]]
                     ))))))

(defn joue-pion [damier depart]
  (if-let [rafles (seq (rafle-pion damier depart [depart] #{}))]
    (->> rafles
         (group-by (fn [[_ prises]] (count prises)))
         (apply max-key first)
         )
    (let [x (get-in damier depart)]
      (let [coups (->> (cond (blanc? x) DIRS_N
                             (noir?  x) DIRS_S)
                       (mapcat (fn [dir] (depl-pion damier (get-diag damier depart dir))))
                       vec
                       )]
        (when (seq coups) [0 coups])))))

(defn joue-dame [damier depart]
  (if-let [rafles (seq (rafle-dame damier depart [depart] #{}))]
    (->> rafles
         (group-by (fn [[_ prises]] (count prises)))
         (apply max-key first)
         )
    (let [x (get-in damier depart)]
      (let [coups (->> DIRS
                       (mapcat (fn [dir] (depl-dame damier (get-diag damier depart dir))))
                       vec
                       )]
        (when (seq coups) [0 coups])))))

(defn meilleurs-coups [damier sel-fn]
  (->> (for [r (range 10) c (range 10)] [r c])
       (filter (fn [pos] (sel-fn (get-in damier pos))))
       (map (fn [pos]
              (if (pion? (get-in damier pos))
                (joue-pion damier pos)
                (joue-dame damier pos))))
       (remove nil?)
       (reduce (fn [[pmax coups-max] [p coups]]
                 (cond (< p pmax) [pmax coups-max]
                       (> p pmax) [p coups]
                       :else      [p (concat coups-max coups)]))
               [0 []])))

(defn mod-damier [damier [trajet prises]]
  (let [dest (last trajet)]
    (reduce (fn [dm p] (assoc-in dm p \space))
            (-> damier
                (echange (first trajet) dest)
                ((fn [dm] (cond (and (== 0 (first dest))
                                      (blanc? (get-in dm dest)))
                                 (assoc-in dm dest \B)
                                 (and (== 9 (first dest))
                                      (noir? (get-in dm dest)))
                                 (assoc-in dm dest \N)
                                 :else dm
                                 ))))
            prises)))

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
   [nil    \n     nil    \B     nil    \N     nil    \space nil    \space]
   [\space nil    \space nil    \space nil    \B     nil    \space nil  ]
   [nil    \space nil    \n     nil    \n     nil    \n     nil    \space]
   [\space nil    \space nil    \space nil    \space nil    \b     nil  ]
   [nil    \n     nil    \n     nil    \space nil    \n     nil    \space]
   [\space nil    \space nil    \space nil    \space nil    \space nil  ]
   [nil    \space nil    \space nil    \space nil    \n     nil    \space]
   [\space nil    \space nil    \space nil    \space nil    \space nil  ]
   [nil    \space nil    \n     nil    \n     nil    \space nil    \n   ]
   [\space nil    \space nil    \b     nil    \space nil    \space nil  ]
   ])


;;-----------------------------------------------------------------------------------

(defn print-damier [damier]
  (doseq [row damier]
    (println (apply str (map {\B \u26c3
                        \N \u26c1
                        \b \u26c2
                        \n \u26c0
                        nil \u2588
                        \space \space}
                       row)))))


(defn -main [& args]
  (loop [damier d1

         sel-fn blanc?]
    (print-damier damier)
    (read-line)
    #_(println)
    (let [[_ coups] (meilleurs-coups damier sel-fn)
          coup (nth coups (rand-int (count coups)))]
      (recur (mod-damier damier coup)
             (if (= sel-fn blanc?) noir? blanc?)))))
