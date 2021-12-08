(ns calendar.2021.day-07
  (:require [advent.core :refer [sum abs-range] :as elves]))

(defn distance [a] (fn [b] (Math/abs (- a b))))

(defn mean [coll]
  (let [sum (apply + coll)
        count (count coll)]
    (cond
      (pos? count) (/ sum count)
      :else 0)))

(defn median [coll]
  (let [sorted (sort coll)
        cnt (count sorted)
        halfway (quot cnt 2)]
    (cond
      (odd? cnt) (nth sorted halfway)
      (even? cnt) (let [bottom (dec halfway)
                        left-middle  (nth sorted bottom)
                        right-middle (nth sorted halfway)]
                    (mean [left-middle right-middle])))))

(def input (elves/each-line "2021/day_07.txt" :matcher #"," :mapfn #(Integer. %)))

(defn part-one [input]
  (as-> (median input) $
    (map (distance $) input) ;; distance
    (elves/sum $)))

(defn part-two 
  "Determine the mean of the crab positions, calculate the fuel cost for each crab, summing all costs"
  [input]
  (let [avg       (float (mean input))
        walk-hi   (distance (Math/ceil avg)) ;; gotta try upper and lower bounds of the mean 
        walk-lo   (distance (Math/floor avg));; and pick whichever represents the lower fuel cost
        fuel-cost (partial abs-range 1)]
    (min
     (sum (map (fn [crab] (-> (walk-hi crab) fuel-cost sum)) 
               input))
     (sum (map (fn [crab] (-> (walk-lo crab) fuel-cost sum)) 
               input)))))

(= (part-one input) 339321)
(= (biginteger (part-two input)) 95476244)


