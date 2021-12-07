(ns calendar.2021.day-04
  (:require [advent.core :as elves]
            [clojure.string :as s]))

(defn parse-board
  "Returns a board as 2d Array of number along with an accompanying lookup table."
  [board-string]
  (mapv
   (fn [row]
     (let [matcher #"(\d+)\s{1,2}(\d+)\s{1,2}(\d+)\s{1,2}(\d+)\s{1,2}(\d+)"
           matches (re-matches matcher (s/trim row))]
       (mapv (fn [x] {:value (Integer/parseInt x) :checked false}) (rest matches))))
   (s/split-lines board-string)))

(defn board-content-map
  "Returns a hash-map of the given bingo board as `num => [coords]`"
  [board]
  (into {}
        (for [x [0 1 2 3 4]
              y [0 1 2 3 4]]

          [(get-in board [x y :value])
           [x y]])))

(def coordinate-map (memoize board-content-map))

(defn mark-spot [board number]
  (cond
    (find (coordinate-map board) number) (update-in board (get (coordinate-map board) number) #(merge % {:checked true}))
    :else board))

(defn rows
  "Returns a sequence of each row of the board."
  [board]
  (map identity board))

(defn cols
  "Returns a sequence of vertical columns on the board, from left to right."
  [board]
  ;; shamelessly bad
  (partition 5 5 (for [x [0 1 2 3 4]
                       y [0 1 2 3 4]]
                   (get-in board [y x]))))

(defn diagonals
  "Returns the diagonal rows on the board"
  [board]
  (let [up (range 0 (count board))
        down (range (dec (count board)) -1 -1)]

    (list (map (fn [i j] (get-in board [i j])) down up)
          (map (fn [i j] (get-in board [i j])) up up))))

(defn bingo? 
  [line] 
  (let [all-true? (partial = true)]
    (apply all-true? (map :checked line))))

(defn board-with-bingo
  "Returns a bingo-containing board or nil"
  [board]
  (when (or (some bingo? (rows board))
            (some bingo? (cols board))
            ;; lost an hour forgetting I'd read 'diagonals dont count' 
            ;; so I'm keeping it here for posterity
            #_(some bingo? (diagonals board)))
    board))

(defn unmarked [board] (remove :checked (flatten board)))

(defn board-score [{:keys [board winning-num]}]
  (->> board
       unmarked
       (map :value)
       (apply +)
       (* winning-num)))

(defn part-one [nums boards]
  (board-score
   (reduce
    (fn [boards num]
      (let [marked-boards (map #(mark-spot % num) boards)
            bingo-board (first (keep board-with-bingo marked-boards))]

        (cond
          (not= bingo-board nil) (reduced {:board bingo-board :winning-num num})
          :else marked-boards)))
    boards
    nums)))

(defn part-two [nums boards]
  (board-score
   (reduce
    (fn [boards num]
      (let [marked-boards (map #(mark-spot % num) boards)
            still-in-play (remove board-with-bingo marked-boards)]

        (cond
          (empty? still-in-play) (reduced {:board (last marked-boards) :winning-num num})
          :else still-in-play)))
    boards
    nums)))

(def input (elves/each-line "2021/day_04.txt" :matcher #"\n\n"))
(def boards (map parse-board (rest input)))
(def number-draws (as-> (first input) $num
                        (s/split $num #",")
                        (map #(Integer/parseInt %) $num)))

(= (part-one number-draws boards) 25023)
(= (part-two number-draws boards) 2634)