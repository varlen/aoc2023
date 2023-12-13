(ns advent2023.day3
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.set :as set]))

(def number-set (->> 10 range (map str) (into #{})))
(def non-symbols (set/union number-set #{"."}))

(defn numeric?
  [single-char-str]
  (-> #{single-char-str}
      (clojure.set/subset? number-set)))

(defn part-symbol?
  [single-char-str]
  (-> #{single-char-str}
      (clojure.set/intersection non-symbols)
      empty?))

(defn xy->keyword
  [x y]
  (keyword (str "x-" x "-y-" y)))

(defn line->xy-maps
  [line-index line]
  (->> (str/split line #"")
       (reduce
         (fn [state cur]
           (let [position (-> state :position)
                 current-number (if (numeric? cur)
                                  (-> state :number-buffer (str cur))
                                  (-> state :number-buffer))
                 is-end-of-number (and
                                    (not (numeric? cur))
                                    (not (empty? current-number)))
                 xy-map-additions (if is-end-of-number
                                    (into {}
                                          (for [column-increment (range (count current-number))]
                                            {(xy->keyword (- position (+ column-increment 1)) line-index)
                                             (edn/read-string current-number)}))
                                    {})
                 xy-symbol-map-add (if (part-symbol? cur)
                                     {(xy->keyword position line-index) cur}
                                     {})]
             {:position       (inc position)
              :number-buffer  (if is-end-of-number
                                ""
                                current-number)
              :xy-numbers-map (merge (-> state :xy-numbers-map) xy-map-additions)
              :xy-symbols-map (merge (-> state :xy-symbols-map) xy-symbol-map-add)}))
         ;initial state
         {:position       0
          :number-buffer  ""
          :xy-numbers-map {}
          :xy-symbols-map {}})))

(defn input->xy-maps
  [input]
  (->
    (reduce
      (fn [state line]
        (let [line-maps (line->xy-maps (:row state) (str line "."))]
          {:row            (-> state :row inc)
           :xy-symbols-map (-> state :xy-symbols-map (merge (line-maps :xy-symbols-map)))
           :xy-numbers-map (-> state :xy-numbers-map (merge (line-maps :xy-numbers-map)))}))
      {:row            0
       :xy-numbers-map {}
       :xy-symbols-map {}}
      (str/split-lines input))
    (select-keys [:xy-numbers-map :xy-symbols-map])))

(defn keyword->xy
  [k]
  (->>
    (-> k name (str/split #"-") (mapv [1 3]))
    (mapv edn/read-string)))

(defn unidimensional-neighborhood
  [xy axis]
  [(update xy axis inc) xy (update xy axis dec)])

(defn adjacent
  [xy-keyword]
  (let [xy (keyword->xy xy-keyword)]
    (->> (unidimensional-neighborhood xy 1)
         (mapv #(unidimensional-neighborhood % 0))
         (apply concat)
         (filter #(not= % xy))
         (mapv (partial apply xy->keyword)))))

(defn xy-maps->part-number-sum
  [xy-maps]
  (->> xy-maps
       :xy-symbols-map
       keys
       (map #(->> %
                  adjacent
                  (select-keys (xy-maps :xy-numbers-map))
                  vals
                  (into #{})))
       (map (partial into []))
       flatten
       (reduce +)))

(defn first-challenge
  [input]
  (->> input
       input->xy-maps
       xy-maps->part-number-sum))

; Part B

(defn xy-maps->asterisks
  [xy-maps]
  (->> xy-maps
       :xy-symbols-map
       (filter (fn [[_ v]] (= v "*")))
       (into {})
       keys))

(defn xy-maps->gear-ratio-sum
  [xy-maps]
  (->> xy-maps
       xy-maps->asterisks
       (map #(->> %
                  adjacent
                  (select-keys (:xy-numbers-map xy-maps))
                  vals
                  (into #{})))
       (filter (fn [s] (= 2 (count s))))
       (map (partial apply *))
       (reduce +)))

(defn second-challenge
  [input]
  (->> input
       input->xy-maps
       xy-maps->gear-ratio-sum))