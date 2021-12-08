(ns calendar.2021.day-06
  "--- Day 6: Lanternfish ---
   https://adventofcode.com/2021/day/6"
  (:require [advent.core :as elves]))


(defn ->fish [x] {:timer (Integer. x)})
(defn reset-fish [] (->fish 6))
(defn new-fish [] (assoc (->fish 8)
                         :hatchling true))

(defn will-spawn-fish? [lanternfish]
  (and (= 6 (:timer lanternfish))
       (not (:hatchling lanternfish))))

(defn age-fish [lanternfish]
  (cond
    (zero? (lanternfish :timer)) (reset-fish)
    :else (update lanternfish :timer dec)))

(defn hatchlings [school]
  (-> (filter will-spawn-fish? school)
      count
      (repeat (new-fish))))

(defn append-hatchlings [school] (concat school (hatchlings school)))


(defn tick [school] (-> (map age-fish school) append-hatchlings))
(defn days [school] (iterate tick school))

(def input-fish (elves/each-line "2021/day_06.txt" :matcher #"," :mapfn ->fish))

;;(def part-one (count (nth (days input-fish) 80)))
;;(= part-one 377263)

;; part-two is absurd so I need a new way of doing this

(defn down-to-zero [x] (range x -1 -1))
(defn lfish [x] (lazy-cat (down-to-zero x) (cycle (down-to-zero 6))))
(defn newfish [] (lazy-cat (repeat 8 :hatchling) (cycle (down-to-zero 6))))

(defn births [day-num fish] (count (filter #(= 6 %) (take (inc day-num) fish))))


;; something missing here, I think I need to track the day number and use it to offset the cycle time when spawning
;; newfish. That way the number of new fish dwindles on every iteration and I can loop until i have no new fish 
;; for the iteration
(defn new-part-one
  [school days]

  (let [num-fish-born (apply + (map #(births days (lfish %)) school))]
    (+ (count school)
       (* num-fish-born (births days (newfish))))))






(new-part-one (elves/each-line "2021/day_06.txt" :matcher #"," :mapfn #(Integer. %)) 80)


(comment :tests
  ;; Initial state: 3,4,3,1,2
  ;; After  1 day:  2,3,2,0,1
  ;; After  2 days: 1,2,1,6,0,8
  ;; After  3 days: 0,1,0,5,6,7,8
  ;; After  4 days: 6,0,6,4,5,6,7,8,8         
         (= (map :timer
                 (nth (days (map ->fish [3 4 3 1 2]))
                      0))
            '(3 4 3 1 2))

         (= (map :timer
                 (nth (days (map ->fish [3 4 3 1 2]))
                      1))
            '(2 3 2 0 1))

         (= (map :timer
                 (nth (days (map ->fish [3 4 3 1 2]))
                      2))
            '(1 2 1 6 0 8))

         (= (map :timer
                 (nth (days (map ->fish [3 4 3 1 2]))
                      4))
            '(6 0 6 4 5 6 7 8 8))
;; test hatchlings
         (= (hatchlings [{:timer 6} {:timer 3} {:timer 6}])
            (list (->fish 8) (->fish 8)))

         (= (append-hatchlings (list (->fish 6) (->fish 3) (->fish 6)))
            (list (->fish 6)
                  (->fish 3)
                  (->fish 6)
                  (->fish 8)
                  (->fish 8))))
