(ns advent2023.core
  (:require [advent2023.day1 :as day1]
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