(ns advent2023.day5
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.math :as math]))

(defn input->blocks
  [input]
  (-> input
      (str/replace "\r" "")
      (str/split #"\n\n")))


(comment
  ; This is going to be super EAGER since the input ranges are huge numbers,
  ; the ranges will take forever to render and use a lot of memory
  (defn range-definition->source-destination-map
    [[destination source delta]]
    (zipmap (range source (+ source delta))
            (range destination (+ destination delta)))))

; Instead of trying to build all ranges at once,
; let's rearrange the input to a more convenient format
(defn range-definition-vec->range-definition
  [[destination source delta]]
  {:input-start  source
   :input-end    (+ source delta -1)
   :output-start destination})


(defn txt-input-map-section->map
  [[map-name & range-definition-vectors]]
  {(-> map-name
       (str/replace " map:" "")
       keyword)
   (->>
     (map (fn [r]
            (mapv edn/read-string
                  (-> r (str/split #" "))))
          range-definition-vectors)
     (map range-definition-vec->range-definition))})

; destination | source | range

(defn blocks->input-map
  [[seeds-txt-block & maps-txt-blocks]]
  (merge {:seeds (mapv edn/read-string
                       (-> seeds-txt-block
                           (str/replace "seeds: " "")
                           (str/split #" ")))}
         (->> maps-txt-blocks
              (map (comp txt-input-map-section->map str/split-lines))
              (apply merge))))

(defn parse-input
  [input]
  (-> input
      input->blocks
      blocks->input-map))

(defn source->range-definition
  [s range-definitions]
  (->> range-definitions
       (filter (fn [range-definition]
                 (<= (:input-start range-definition)
                     s
                     (:input-end range-definition))))
       (into {})))

(defn source->destination
  [s range-definitions]
  (->>
    (source->range-definition s range-definitions)
    ((juxt #(get % :output-start 0) #(- s (get % :input-start 0))))
    (apply +)))

(defn seed->location
  [input-map sn] (-> sn
                     (source->destination (:seed-to-soil input-map))
                     (source->destination (:soil-to-fertilizer input-map))
                     (source->destination (:fertilizer-to-water input-map))
                     (source->destination (:water-to-light input-map))
                     (source->destination (:light-to-temperature input-map))
                     (source->destination (:temperature-to-humidity input-map))
                     (source->destination (:humidity-to-location input-map))))

(defn first-challenge
  [input]
  (let [input-map (->> input
                       parse-input)
        seeds (:seeds input-map)]
    (->> seeds
         (map (partial seed->location input-map))
         (apply min))))

; Part B
; If you look for a working solution, go straight ahead to the brute force
; in the end. It will take a long time but gives the right answer and is quite simple.
;
; The alternative implementation is not working properly but I haven't fixed it
(defn ranges-x [range-definitions [start end]]
  (->> range-definitions
       (filter #(or
                  (<= start (:input-end %) end)
                  (<= start (:input-start %) end)
                  (<= (:input-start %) end (:input-end %))
                  (<= (:input-start %) start (:input-end %))))))


(defn source-range->destination-ranges
  [range-definitions [start end]]
  (->> range-definitions
       (map (juxt :input-start :input-end))
       flatten
       (concat [start end])
       sort
       (drop 1)
       (drop-last 1)
       (map #(source->destination % range-definitions))
       (partition 2 2)
       (map vec)))

(defn second-challenge/cross-ranges-approach
  [input]
  (let [input-map (parse-input input)
        seeds-begin-end (->> input-map
                         :seeds
                         (partition 2 2)
                         (map (fn [[s n]] [s (+ n s)])))]
    (->>
      (reduce
        (fn [state transform-key]
          (print transform-key "->")
          (println (-> state :input-ranges count))
          {:input-ranges
           (->> state
                :input-ranges
                (map (partial
                       source-range->destination-ranges
                       (input-map transform-key)))
                (apply concat)
                (into #{})                                  ; avoid exploding duplicates
                (into []))
           :tk transform-key})
        {:input-ranges seeds-begin-end}
        [:seed-to-soil
         :soil-to-fertilizer
         :fertilizer-to-water
         :water-to-light
         :light-to-temperature
         :temperature-to-humidity
         :humidity-to-location])
      :input-ranges)))

; If you are reading this poor piece of code,
; I let this brute force solution running while trying other approaches
; In the end, I gave up on trying other approaches
; But I forgot the brute force running for more than 10h
; And it gave the right answer :) *star*
; Sometimes you just need to take a break from the computer LOL
(defn second-challenge-brute-forced
  [input]
  (let [input-map (parse-input input)
        transform-fn (partial seed->location input-map)]
    (->> input-map
         :seeds
         (partition 2 2)
         (map (fn [[start n]] (range start (+ start n))))
         flatten
         (map transform-fn)
         (apply min))))

