(ns calendar.2020.day-09
  "https://adventofcode.com/2020/day/9"
  (:require [advent.core :refer [each-line]]))

(def input (apply vector (map biginteger (each-line "day_09.txt" :path "resources/2020/"))))

;; ---- PART ONE ----
(defn kv-sums
  "Given a collection of x, return a hash-map in which all the keys and their values add up to the target number"
  [target xs]
  (into {} (map
            (fn [x] (vector (- target x) x))
            xs)))

(defn val-for-key-is-different? 
  "for a given key, returns a boolean value indicating whether a corresponding value in the map is present and not equal to the key"
  [m k]
  (boolean 
    (and (get m k false)
         (not= k (get m k)))))

(defn valid? [target preamble]
  (let [sums (kv-sums target preamble)
        validation-fn (partial val-for-key-is-different? sums)]
    (boolean (some validation-fn preamble))))

(def invalid? (complement valid?))

(defn solve
  [input validation-fn & {preamble-size :preamble :or {preamble-size 25}}]
  (filter (complement nil?)
    (for [idx (range preamble-size (count input))
          :let [target (get input idx)
                preamble (subvec input (- idx preamble-size) idx)]
          :when (not (nil? target))]
      (when (validation-fn target preamble)
        target))))

(def part-one-answer (-> input
                         (solve invalid?)
                         (first)))

;;;;;;; ---- PART TWO ---- ;;;;;;;
(def min-max (partial apply (juxt min max)))
(defn expand-right [[left right]] [left (inc right)])
(defn shrink-left [[left right]] [(inc left) right])
(defn get-window [[left right] nums] (subvec nums left (inc right)))

(defn find-contiguous-sum 
  ([target-sum nums] (find-contiguous-sum target-sum nums [0 0]))
  ([target-sum nums window]
   (let [nums-in-window (get-window window nums)
         window-sum (reduce + nums-in-window)]
     (cond
       (= target-sum window-sum) nums-in-window
       (< target-sum window-sum) (find-contiguous-sum target-sum nums (shrink-left window))
       (> target-sum window-sum) (find-contiguous-sum target-sum nums (expand-right window))))))

(def part-two-answer
  (->> (find-contiguous-sum part-one-answer input)
       (min-max)
       (reduce +)))