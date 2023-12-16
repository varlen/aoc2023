(ns advent2023.day7
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(defn hand
  [hand-str]
  (frequencies (str/split hand-str #"")))

(defn hand-type
  [hand-str]
  (let [hand (hand hand-str)
        card-counts (->> hand vals)
        max-count (apply max card-counts)
        min-count (apply min card-counts)
        pairs (get (->> hand vals frequencies) 2 0)]
    (cond
      (= max-count 5) :five-of-a-kind
      (= max-count 4) :four-of-a-kind
      (and (= max-count 3)
           (= min-count 2)) :full-house
      (= max-count 3) :three-of-a-kind
      (= pairs 2) :two-pairs
      (= pairs 1) :one-pair
      (= max-count 1) :high-card)))

(def _strength
  (into {} (map-indexed
             (fn [a b] [b a])
             (reverse [:five-of-a-kind
                       :four-of-a-kind
                       :full-house
                       :three-of-a-kind
                       :two-pairs
                       :one-pair
                       :high-card]))))

(def _card-strength
  (into {}
        (map-indexed
          (fn [card index] [index card])
          (concat (->> (range 2 10)
                       (map str))
                  ["T" "J" "Q" "K" "A"]))))

(defn strength
  [hand-type hand-str]
  (->> hand-str
       hand-type
       _strength))

(defn compare-per-card
  [card-strength-mapping a b]
  (->>
    (map vector (str/split a #"") (str/split b #""))
    (filter (partial apply not=))
    first
    (mapv card-strength-mapping)
    (apply compare)))

(defn compare-hand
  [a b]
  (let [strength-only-order (->> [a b]
                                 (map (partial strength hand-type))
                                 (apply compare))]
    (if (not= 0 strength-only-order)
      strength-only-order
      (compare-per-card _card-strength a b))))

(defn parse-line
  [line]
  (-> line
      (str/split #" ")
      ((fn [[hand bid-str]]
         {:hand hand
          :bid  (edn/read-string bid-str)}))))

(defn winnings
  [input hand-compare-fn]
  (->> input
       str/split-lines
       (map parse-line)
       (sort (fn [{h1 :hand} {h2 :hand}]
               (hand-compare-fn h1 h2)))
       (map (fn [k] (println k) (merge k {})))
       (map-indexed (fn [i {bid :bid}]
                      (* (+ i 1) bid)))
       (reduce +)))

(defn first-challenge
  [input]
  (winnings input compare-hand))

; Part B

(def _card-strength-with-joker
  (into {}
        (map-indexed
          (fn [card index] [index card])
          (concat (->> (range 2 10)
                       (map str)
                       (concat ["J"]))
                  ["T" "Q" "K" "A"]))))

(defn joker-role
  [hand]
  (->> hand
       (into [])
       (sort-by last)
       (take-last 2)
       (map first)
       ((fn [s]
          (cond
            (= 1 (count s)) s
            (= "J" (last s)) (first s)
            :else (last s))))))

(defn hand-type-with-joker
  [hand-str]
  (let [pre-joker-hand (hand hand-str)
        joker-count (get pre-joker-hand "J" 0)
        joker-as (joker-role pre-joker-hand)
        hand (merge-with + (dissoc pre-joker-hand "J")
                         {joker-as
                          joker-count})
        card-counts (->> hand vals)
        max-count (apply max card-counts)
        min-count (apply min card-counts)
        pairs (get (->> card-counts frequencies) 2 0)]
    (cond
      (= max-count 5) :five-of-a-kind
      (= max-count 4) :four-of-a-kind
      (and (= max-count 3)
           (= min-count 2)) :full-house
      (= max-count 3) :three-of-a-kind
      (= pairs 2) :two-pairs
      (= pairs 1) :one-pair
      (= max-count 1) :high-card)))

(defn compare-hand-with-joker
  [a b]
  (let [strength-only-order (->> [a b]
                                 (map (partial strength hand-type-with-joker))
                                 (apply compare))]
    (if (not= 0 strength-only-order)
      strength-only-order
      (compare-per-card _card-strength-with-joker a b))))

(defn second-challenge
  [input]
  (winnings input compare-hand-with-joker))

