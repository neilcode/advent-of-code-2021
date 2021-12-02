(ns calendar.2021.day-02
  (:require [advent.core :as a]))

(def input (a/each-line "2021/day_02.txt"))
;; helpers
(defn decrease-by [x] (fn [y] (- y (Integer. x))))
(defn increase-by [x] (fn [y] (+ y (Integer. x))))
(defn answer [sub] (* (sub :horizontal) (sub :depth)))

(defn part-one [instructions]
  (reduce
   (fn [sub instruction]
     (condp re-matches instruction
       #"up (\d+)"      :>> (fn [[_ amt]] (update sub :depth (decrease-by amt)))
       #"down (\d+)"    :>> (fn [[_ amt]] (update sub :depth (increase-by amt)))
       #"forward (\d+)" :>> (fn [[_ amt]] (update sub :horizontal (increase-by amt)))))
   {:horizontal 0 :depth 0}
   instructions))

(defn part-two [instructions]
  (reduce
   (fn [sub instruction]
     (condp re-matches instruction
       #"up (\d+)"      :>> (fn [[_ amt]] (update sub :aim (decrease-by amt)))
       #"down (\d+)"    :>> (fn [[_ amt]] (update sub :aim (increase-by amt)))
       #"forward (\d+)" :>> (fn [[_ amt]] (-> sub
                                              (update :horizontal (increase-by amt))
                                              (update :depth (increase-by (* (sub :aim)
                                                                             (Integer. amt))))))))
   {:horizontal 0 :depth 0 :aim 0}
   instructions))

(= (answer (part-one input)) 2117664) ;; {:horizontal 2052, :depth 1032}
(= (answer (part-two input)) 2073416724) ;; {:horizontal 2052, :depth 1010437, :aim 1032}
