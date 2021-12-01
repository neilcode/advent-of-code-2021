(ns calendar.2021.day-01
  "URL: https://adventofcode.com/2021/day/1"
  (:require [advent.core :as core]))

(def input (map #(Integer/parseInt %) (core/each-line "2021/day_01.txt")))

(def part-one
  (->> input
       (partition 2 1)
       (filter #(apply < %))
       count)) ;;1754

(def part-two
  (->> input
       (partition 3 1)
       (map #(apply + %))
       (partition 2 1)
       (filter #(apply < %))
       count)) ;;1789
