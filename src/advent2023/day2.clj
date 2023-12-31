(ns advent2023.day2
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(defn line->game-number
  [line]
  (-> line
      (str/split #": ")
      first
      (str/replace "Game " "")
      edn/read-string))

(defn parse-cubes
  "3 green -> {:green 3}"
  [cubes-str]
  (-> cubes-str
      (str/split #" ")
      ((fn [[v k]] {(keyword k) (edn/read-string v)}))))

(defn parse-draws
  "[2 blue 3 green] -> {:blue 2 :green 3}"
  [draw]
  (apply merge (mapv parse-cubes draw)))

(defn line->draws
  [line]
  (->>
    (-> line
        (str/split #": ")
        last
        (str/split #"; "))
    (mapv #(str/split % #", "))
    (mapv parse-draws)))

(defn line->game
  [line]
  (zipmap
    [:game :draws]
    ((juxt line->game-number line->draws) line)))

(defn impossible-draw?
  [criteria draw]
  ; possibly there's a programmatic way of implementing this but I'm lazying out
  (or
    (> (get draw :red 0) (get criteria :red 0))
    (> (get draw :green 0) (get criteria :green 0))
    (> (get draw :blue 0) (get criteria :blue 0))))

(defn possible-game?
  [criteria game]
  (->> game
      :draws
      (filter (partial impossible-draw? criteria))
      empty?))

(defn games
  [input]
  (->> input
       str/split-lines
       (map line->game)))

(defn possible-games
  [input criteria]
  (->> input
      games
      (filter (partial possible-game? criteria))
      (mapv :game)))

(defn possible-games-id-sum
  [input]
  (reduce + (possible-games input {:red 12 :green 13 :blue 14})))

; 2B

(defn minimum-possible-amount-of-color
  [game color]
  (->> game :draws (mapv #(get % color 0)) (apply max)))

(defn minimum-possible-set
  [game]
  (->> [:red :green :blue]
       ((juxt
          identity
          #(mapv (partial minimum-possible-amount-of-color game) %)))
       (apply zipmap)))

(defn remove-zeros-from-map
  [m]
  (into {} (filter (fn [[_ value]] (< 0 value)) m)))

(defn power
  [cube-set]
  (->> cube-set
       remove-zeros-from-map
       ((juxt :red :green :blue))
       (apply *)))

(defn minimum-set-power-sum
  [input]
  (->> input
       games
       (map minimum-possible-set)
       (map power)
       (reduce +)))