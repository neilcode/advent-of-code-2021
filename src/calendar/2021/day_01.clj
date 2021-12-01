(ns calendar.2021.day-01
  (:require [advent.core :as core]))

(def input (map #(Integer/parseInt %) (core/each-line "2021/day_01.txt")))

(def part-one
  (->> input
       (partition 2 1)
       (filter #(apply < %))
       count)) ;;1754


