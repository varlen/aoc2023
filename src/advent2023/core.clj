(ns advent2023.core
  (:require [advent2023.day1 :as day1]
            [advent2023.day2 :as day2]
            [clojure.java.io :as jio]))

(defn get-input
  [day]
  (-> (str "day" day "-input.txt")
      jio/resource
      jio/file
      slurp))

(print "Day 1a:")
(println (-> 1 get-input day1/calibration-number-sum))

(print "Day 1b:")
(println (-> 1 get-input day1/calibration-number-sum-with-numeric-word))

(print "Day 2a:")
(println (->> 2 get-input day2/possible-games-id-sum))

(print "Day 2b:")
(println (-> 2 get-input advent2023.day2/minimum-set-power-sum))