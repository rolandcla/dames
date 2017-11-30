(ns dames.core-test
  (:require [clojure.test :refer :all]
            [dames.core :refer :all]))

(deftest manoury-test
  (testing "Manoury conversion"
    (is (= (range 1 51)
           (for [n (range 1 51)] (to-manoury (from-manoury n)))))))

(deftest damier-test
  (testing "Initialisation du damier"
    (is (let [damier (init-damier)]
          (->> (for [r (range 10) c (range 10)] [r c])
               (every? (fn [[r c]]
                         (if (nil? (get-in damier [r c]))
                           true
                           (dans-damier? [r c])))))))))
