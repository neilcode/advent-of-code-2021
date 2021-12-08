(ns calendar.2021.day-06
  "--- Day 6: Lanternfish ---
   https://adventofcode.com/2021/day/6
   
   Started part-one with a brute force solution that involved having 
   a physical presence for every fish. Part-two destroyed my CPU and 
   ate 29GB of RAM before imploding.

   Reapproaching this problem (there were tons of hints in the prompt), 
   I made an array where each index represented a day in the lanternfish 
   lifecycle. The value at a given index represents the total number of 
   fish currently on that day of their lifecycle. 
   
   Every day, the fish at day 0 get reset to day 6 (added to whoever 
   is already there) and spawn a fish that starts on day 8."
  (:require [advent.core :as elves]))

(defn next-day 
  "All fish at index (day) 0 are reset to index (day) 6 and added to whoever is already there.
   For every fish reset this way, add 1 new lanternfish to index (day) 8."
  [lifecycle]
  (let [[today & days] lifecycle]
    (-> (into [] days)
        (update 6 #(+ % today))
        (conj today)))) ;; 

(defn after-n-days [num-days input] 
  (let [lifecycle [0 0 0 0 0 0 0 0 0]
        current-day (reduce #(update %1 %2 inc) lifecycle input)
        time (iterate next-day current-day)]
    (nth time num-days)))

(defn count-fish [lifecycle] (apply + lifecycle))

(def input-fish (elves/each-line "2021/day_06.txt" :matcher #"," :mapfn #(Integer. %)))

(def part-one (-> (after-n-days 80 input-fish) count-fish))
(def part-two (-> (after-n-days 256 input-fish) count-fish))

(= 377263 part-one)
(= 1695929023803 part-two)

(comment 
  "brute-force solution that couldn't withstand part2. keeping around for fun"
         
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
  
  (def part-one (count (nth (days input-fish) 80)))
  (= part-one 377263)
)


