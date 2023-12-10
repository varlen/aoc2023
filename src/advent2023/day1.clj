(ns advent2023.day1
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(defn digits->number
  [algseq]
  (->> algseq
       ((juxt first last))
       (apply (fn [d u] (+ (* d 10) u)))))

(defn word->calibration-number
  [word]
  (->>
    (-> word (str/split #""))
    (map edn/read-string)
    (filter number?)
    digits->number))

(defn calibration-number-sum
  [input-text]
  (->> input-text
       str/split-lines
       seq
       (map word->calibration-number)
       (apply +)))

; B part

(def spelled-out-numbers
  ["one" "two" "three"
   "four" "five" "six"
   "seven" "eight" "nine"])

(def reverse-spelled-out-numbers
  (mapv str/reverse spelled-out-numbers))

(def digits
  ["1" "2" "3" "4" "5" "6" "7" "8" "9"])

(def word->digit
  (zipmap spelled-out-numbers digits))

(def reverse-word->digit
  (zipmap reverse-spelled-out-numbers digits))

(defn word-pattern
  [spelled-out-numbers]
  (->> spelled-out-numbers
       (str/join "|")
       re-pattern))

(defn spelled-out-numbers->digits
  [line]
  (clojure.string/replace-first line (word-pattern spelled-out-numbers) word->digit))

(defn reverse-spelled-out-numbers->digits
  [line]
  (clojure.string/replace line (word-pattern reverse-spelled-out-numbers) reverse-word->digit))


(defn calibration-number-sum-with-numeric-word
  [input-text]
  (->> input-text
       str/split-lines
       (map (juxt spelled-out-numbers->digits (comp str/reverse
                                                    reverse-spelled-out-numbers->digits
                                                    str/reverse)))
       (map (partial str/join ""))
       seq
       (map word->calibration-number)
       (apply +)))
