(ns calendar.2021.day-02
  (:require [advent.core :as a]))

(def input (a/each-line "2021/day_02.txt"))
;; part one
(defn answer [sub] (* (sub :horizontal) (sub :depth)))
(defn forward [sub amt] (update sub :horizontal #(+ % amt)))
(defn up      [sub amt] (update sub :depth  #(- % amt)))
(defn down    [sub amt] (update sub :depth  #(+ % amt)))
;; part two
(defn aim-up      [sub amt] (update sub :aim  #(- % amt)))
(defn aim-down    [sub amt] (update sub :aim  #(+ % amt)))
(defn aim-forward
  [sub amt]
  (as-> sub $
    (forward $ amt)
    (down $ (* amt ($ :aim)))))

(defn part-one [instructions]
  (reduce
   (fn [sub instruction]
     (condp re-matches instruction
       #"forward (\d+)" :>> (fn [[_ amt]] (forward sub (Integer. amt)))
       #"up (\d+)"      :>> (fn [[_ amt]] (up sub (Integer. amt)))
       #"down (\d+)"    :>> (fn [[_ amt]] (down sub (Integer. amt)))))
   {:horizontal 0 :depth 0}
   instructions))

(defn part-two [instructions]
  (reduce
   (fn [sub instruction]
     (condp re-matches instruction
       #"forward (\d+)" :>> (fn [[_ amt]] (aim-forward sub (Integer. amt)))
       #"up (\d+)"      :>> (fn [[_ amt]] (aim-up sub (Integer. amt)))
       #"down (\d+)"    :>> (fn [[_ amt]] (aim-down sub (Integer. amt)))))
   {:horizontal 0 :depth 0 :aim 0}
   instructions))

(-> (part-one input) answer) ;; {:horizontal 2052, :depth 1032} => 2117664
(-> (part-two input) answer) ;; {:horizontal 2052, :depth 1010437, :aim 1032} => 2073416724
