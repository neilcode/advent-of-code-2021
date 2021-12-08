(ns calendar.2021.day-05
  "--- Day 5: Hydrothermal Venture ---
   https://adventofcode.com/2021/day/5
   
   This one was fun. My solution involves feeding every point from each line as the key 
   to a hash-map. The corresponding value is how many lines contain that same point. 
   The abs-range bears responsibility for determining the directionality of each line,
   returning either a incrementing or decrementing range."
  (:require [advent.core :as elves][clojure.set :as s]))

(defn straight? [{{x' :x y' :y} :source {x :x y :y} :dest :as line}] (or (= x' x) (= y' y)))

(defn make-line [input-str]
  (let [[_ x1 y1 x2 y2] (re-matches #"(\d+),(\d+) -> (\d+),(\d+)" input-str)]
   {:source {:x (Integer/parseInt x1) :y (Integer/parseInt y1)} 
    :dest   {:x (Integer/parseInt x2) :y (Integer/parseInt y2)}}))

(defn abs-range 
  "Generates a range between two points on a line regardless of the order in which the points are given"
  [a b]
  (cond
    (< a b) (range a (inc b))    ;; incrementing range
    (> a b) (range a (dec b) -1) ;; decrementing range
    (= a b) (repeat a)))         ;; infinite range :D

(defn points
  "Returns a collection of [x y] coordinates that comprise a given line"
  [{{x' :x y' :y} :source {x :x y :y} :dest :as line}]
  (map vector (abs-range x' x) (abs-range y' y)))

(defn xy->intersections
  ([line] (xy->intersections line {}))
  ([line graph]
   (reduce (fn [graph point] (update graph point #(inc (or % 0))))
           graph
           (points line))))

(defn graph-lines
  ([lines] (graph-lines {} lines))
  ([graph lines] (reduce #(xy->intersections %2 %1) graph lines)))

(defn part-one
  [input]
  (->> (map make-line input)
       (filter straight?)
       graph-lines
       (filter (fn [[_point num-of-intersections]]
                 (> num-of-intersections 1)))
       count))

(defn part-two
  [input]
  (->> (map make-line input)
       graph-lines
       (filter (fn [[_point num-of-intersections]]
                 (> num-of-intersections 1)))
       count))

(def input (elves/each-line "2021/day_05.txt"))
(= (part-one input) 7438)
(= (part-two input) 21406)