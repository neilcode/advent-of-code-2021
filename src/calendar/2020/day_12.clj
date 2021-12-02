(ns calendar.2020.day-12
  "https://adventofcode.com/2020/day/12"
  (:require [advent.core :refer [each-line]]
            [clojure.test :as t]))

(declare nav-fns)
(def input (each-line "day_12.txt" :path "resources/2020/"))
(defn manhattan-distance [{{:keys [x y]} :coords}] (+ (Math/abs x) (Math/abs y)))

(defn move-north [clicks vessel] (update-in vessel [:coords :y] #(+ % clicks)))
(defn move-south [clicks vessel] (update-in vessel [:coords :y] #(- % clicks)))
(defn move-east  [clicks vessel] (update-in vessel [:coords :x] #(+ % clicks)))
(defn move-west  [clicks vessel] (update-in vessel [:coords :x] #(- % clicks)))
(defn move-ahead [clicks vessel] ((get nav-fns (:heading vessel)) clicks vessel))

(defn turn-left [heading] (get {"E" "N" "N" "W" "W" "S" "S" "E"} heading))
(defn turn-right [heading] (get {"E" "S" "S" "W" "W" "N" "N" "E"} heading))
(defn bear-right
  [degrees vessel]
  (let [turns (/ degrees 90)]
    (assoc vessel :heading (->> (:heading vessel) (iterate turn-right) (drop 1) (take turns) last))))

(defn bear-left
  [degrees vessel]
  (let [turns (/ degrees 90)]
    (assoc vessel :heading (->> (:heading vessel) (iterate turn-left) (drop 1) (take turns) last))))

(def nav-fns {"N" move-north
              "S" move-south
              "E" move-east
              "W" move-west
              "F" move-ahead
              "R" bear-right
              "L" bear-left})

(defn parse-navigation [nav-fns s]
  (let [[_ nav amt] (re-matches #"([A-Z])(\d+)" s)]
    (partial (get nav-fns nav) (Integer. amt))))

(defn navigate [nav-fns instructions ship]
  (let [commands (map (fn [i] (parse-navigation nav-fns i)) instructions)]
    (reduce #(%2 %1) ship commands)))

(def part-one-answer
  (-> (navigate nav-fns input {:heading "E" :coords {:x 0 :y 0}})
      manhattan-distance))

(defn move-wp-north [clicks vessel] (update-in vessel [:waypoint :y] #(+ % clicks)))
(defn move-wp-south [clicks vessel] (update-in vessel [:waypoint :y] #(- % clicks)))
(defn move-wp-east  [clicks vessel] (update-in vessel [:waypoint :x] #(+ % clicks)))
(defn move-wp-west  [clicks vessel] (update-in vessel [:waypoint :x] #(- % clicks)))

(defn rotate-wp-90-right
  [vessel]
  (-> vessel
      (update :heading turn-right)
      (update :waypoint (fn [{:keys [x y]}] {:x y :y (* -1 x)}))))

(defn rotate-wp-right [degrees vessel]
  (let [turns (/ degrees 90)]
    (->> (iterate rotate-wp-90-right vessel)
         (drop turns)
         first)))

(defn rotate-wp-90-left
  [vessel]
  (-> vessel
      (update :heading turn-left)
      (update :waypoint (fn [{:keys [x y]}] {:x (* -1 y) :y x}))))

(defn rotate-wp-left [degrees vessel]
  (let [turns (/ degrees 90)]
    (->> (iterate rotate-wp-90-left vessel)
         (drop turns)
         first)))

(defn move-to-wp
  [times {:keys [waypoint] :as vessel}]
  (let [distance {:x (* times (waypoint :x))
                  :y (* times (waypoint :y))}]
    (update vessel :coords #(merge-with + % distance))))

(def part-two-ship
  {:heading  "E"
   :coords   {:x 0 :y 0}
   :waypoint {:x 10 :y 1}})

(def part-two-answer
  (-> (navigate {"N" move-wp-north
                 "S" move-wp-south
                 "E" move-wp-east
                 "W" move-wp-west
                 "R" rotate-wp-right
                 "L" rotate-wp-left
                 "F" move-to-wp}
                input
                part-two-ship)
      manhattan-distance))
 




