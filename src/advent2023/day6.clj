(ns advent2023.day6)

; Thanks for not having to parse this one

(defn race-wins
  [duration world-record-distance]
  (->> (range duration)
       (map (fn [hold-time-aka-speed]
              {:hold-for hold-time-aka-speed
               :speed    hold-time-aka-speed
               :distance (*
                           ; time with speed > 0
                           (- duration hold-time-aka-speed)
                           ; actual speed
                           hold-time-aka-speed)}))
       (filter #(< world-record-distance (:distance %)))))

(defn first-challenge
  []
  (->>
    [(race-wins 48 261)
     (race-wins 93 1192)
     (race-wins 84 1019)
     (race-wins 66 1063)]
    (map count)
    (apply *)))

(defn race-wins-lighter
  [duration world-record-distance]
  (->> (range duration)
       (map (fn [hold-time-aka-speed]
              (*
                ; time when speed is greater than 0
                (- duration hold-time-aka-speed)
                ; actual speed
                hold-time-aka-speed)))
       (filter #(< world-record-distance %))
       count))

(defn second-challenge
  []
  (race-wins-lighter
    48938466
    261119210191063))
