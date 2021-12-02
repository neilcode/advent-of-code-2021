(ns calendar.2021.day-02
  (:require [advent.core :as a]))

(def input (a/each-line "2021/day_02.txt"))
;; part one
(defn answer [sub] (* (sub :horizontal) (sub :depth)))
(defn forward [sub x] (update sub :horizontal #(+ % x)))
(defn up      [sub x] (update sub :depth  #(- % x)))
(defn down    [sub x] (update sub :depth  #(+ % x)))
;; part two
(defn aim-up      [sub x] (update sub :aim  #(- % x)))
(defn aim-down    [sub x] (update sub :aim  #(+ % x)))
(defn aim-forward
  [sub x]
  (as-> sub $
    (forward $ x)
    (down $ (* x ($ :aim)))))

(defn part-one [instructions]
  (reduce
   (fn [sub instruction]
     (condp re-matches instruction
       #"forward (\d+)" :>> (fn [[_ x]] (forward sub (Integer. x)))
       #"up (\d+)"      :>> (fn [[_ x]] (up sub (Integer. x)))
       #"down (\d+)"    :>> (fn [[_ x]] (down sub (Integer. x)))))
   {:horizontal 0 :depth 0}
   instructions))

(defn part-two [instructions]
  (reduce
   (fn [sub instruction]
     (condp re-matches instruction
       #"forward (\d+)" :>> (fn [[_ x]] (aim-forward sub (Integer. x)))
       #"up (\d+)"      :>> (fn [[_ x]] (aim-up sub (Integer. x)))
       #"down (\d+)"    :>> (fn [[_ x]] (aim-down sub (Integer. x)))))
   {:horizontal 0 :depth 0 :aim 0}
   instructions))

(-> (part-one input) answer) ;; {:horizontal 2052, :depth 1032} => 2117664
(-> (part-two input) answer) ;; {:horizontal 2052, :depth 1010437, :aim 1032} => 2073416724
