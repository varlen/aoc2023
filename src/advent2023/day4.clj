(ns advent2023.day4
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.math :as math]))

(defn vec->set
  [v]
  (->> v
       (filter (partial not= ""))
       (map edn/read-string)
       (into #{})))

(defn pile-of-cards
  [input]
  (->> input
       str/split-lines
       (map (fn [l] (-> l
                        (str/split #": ")
                        last
                        (str/split #"\|")
                        (#(map (fn [p] (-> p
                                           (str/split #" ")
                                           )) %)))))
       (mapv (fn [[a b]] {:winning (vec->set a)
                          :mine (vec->set b)}))))

(defn score
  [match-count]
  (if (= match-count 0)
    0
    (math/pow 2 (- match-count 1))))

(defn card-matches
  [card]
  (->> card
       ((juxt :winning :mine))
       (apply set/intersection)
       count))

(defn first-challenge
  [input]
  (->> input
       pile-of-cards
       (map card-matches)
       (map score)
       (reduce +)))

; Part B
(defn second-challenge
  [input]
  (->> input
       pile-of-cards
       (reduce (fn [state card]
                 (let [current-card (-> state :current-card str)
                       current-card-copies (get (:card-count state) current-card 0)
                       cloned-cards (->> (range (:current-card state)
                                               (+ (card-matches card) (:current-card state)))
                                        (map (comp (fn [card-n] {card-n (+ current-card-copies 1)}) str inc))
                                        (apply merge)
                                        (into {}))]
                   {:card-count   (merge-with +
                                              (state :card-count)
                                              {current-card 1}
                                              cloned-cards)
                    :current-card (inc (state :current-card))
                    :steps        (concat (state :steps) [(-> state :current-card str) cloned-cards])}))
               {:current-card 1
                :steps []
                :card-count   {}})
       :card-count
       vals
       (apply +)))