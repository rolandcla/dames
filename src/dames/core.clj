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

(defn to-manoury [[r c]]
  (assert (odd? (+ r c)))
  (+ (* r 5) (quot c 2) 1))

(defn from-manoury [n]
  (assert (<= 1 n 50))
  (let [r (quot (- n 1) 5)
        c (+ (* 2 (rem (- n 1) 5))
             (if (even? r) 1 0))]
    [r c ]))

(defn deplacement-pion [damier [r c]]
  (let [x (get-in damier [r c])]
    (cond (noir? x) (->> [[(inc r) (dec c)] [(inc r) (inc c)]]
                         (filter (fn [rc]
                                   (and (dans-damier? rc)
                                        (libre? (get-in damier rc))))))
          (blanc? x) (->> [[(dec r) (dec c)] [(dec r) (inc c)]]
                          (filter (fn [rc]
                                    (and (dans-damier? rc)
                                         (libre? (get-in damier rc))))))
          :else nil)))

(defn prise-pion [damier [r c]]
  (let [x (get-in damier [r c])]
    (->> (for [ri [-1 1] ci [-1 1]
               :let [r2 (+ r ri ri) c2 (+ c ci ci)]
               :when (and (dans-damier? [r2 c2])
                          (libre? (get-in damier [r2 c2])))
               :let [r1 (+ r ri) c1 (+ c ci)
                     y (get-in damier [r1 c1])]
               :when (or (and (blanc? x) (noir? y))
                         (and (noir? x) (blanc? y)))]
           [[r1 c1] [r2 c2]]))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
