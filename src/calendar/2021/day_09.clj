(ns calendar.2021.day-09
  (:require [advent.core :as elves]
            [clojure.string :as string]))

(defn up    [[x y]] (fn [grid] (get-in grid [x (- y 1)])))
(defn down  [[x y]] (fn [grid] (get-in grid [x (+ y 1)])))
(defn left  [[x y]] (fn [grid] (get-in grid [(- x 1) y])))
(defn right [[x y]] (fn [grid] (get-in grid [(+ x 1) y])))

(defn ->pnt [val [x y]]
  {:value (Integer/parseInt (str val))
   :loc [x y]
   :adjacent {:up    (up [x y])
              :down  (down [x y])
              :left  (left [x y])
              :right (right [x y])}})

(defn adjacent-pnts [grid {{:keys [up down left right]} :adjacent :as pnt}]
  (try
    (remove nil? ((juxt left up right down) grid))
    (catch NullPointerException e (println "fuuuck" pnt))))

(defn risk-level [pnt] (+ 1 (pnt :value)))

;; kinda hacky: I don't have a good way of doing an `all?` fn. So reducing (and (fn) (fn)...)
;; where the fn returns a boolean.
(defn low-point? [grid pnt]
  (reduce
   (fn [compound-bool adj]
     (and (< (pnt :value) adj)
          compound-bool))
   true
   (adjacent-pnts grid pnt)))

(defn points [input]
  (map-indexed
    (fn [y row]
      (map-indexed
       (fn [x num]
         (->pnt num [x y]))
       row))
   input))

(defn part-one [input]
  (->> (points input)
       (filter #(low-point? input %))
       #_(map risk-level)
       #_(apply +)
       #_(reduce #(+ %1 (risk-level %2)) 0)))

(def input (mapv
            (fn [s] (->> (string/split s #"")
                         (mapv #(Integer/parseInt %))))
            (elves/each-line "2021/day_09.txt")))


(adjacent-pnts input (->pnt 6 [18 0]))


(comment
  :tests
  (= '(0 5 0)
     (adjacent-pnts
      [[0 1 0]
       [1 5 1]
       [0 1 0]]
      (->pnt 1 [1 2])))

  (= '(2 2 2 2)
     (adjacent-pnts
       [[0 2 0]
        [2 1 2]
        [0 2 0]]
      (->pnt 1 [1 1])))

(= true
     (low-point?
       [[0 2 0]
        [2 1 2]
        [0 2 0]]
      (->pnt 1 [1 1])))




  )
