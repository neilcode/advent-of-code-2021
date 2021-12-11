(ns calendar.2021.day-10
  (:require [advent.core :as elves]
            [clojure.string :as s]))

(def points
  {"}" 1197
   ")" 3
   "]" 57
   ">" 25137})

(def pt-2-points
  {")" 1
   "]" 2
   "}" 3
   ">" 4})

(def bracket-pairs
  {"}" "{"
   ")" "("
   "]" "["
   ">" "<"})

(def inverted-bracket-pairs (zipmap (vals bracket-pairs) (keys bracket-pairs)))
(defn invert [brackets] (map #(inverted-bracket-pairs %) brackets))
(defn opening-bracket? [ch] (contains? (set (vals bracket-pairs)) ch))
(defn closes? [buffer closing-bracket] (= (bracket-pairs closing-bracket) (peek buffer)))

(defn check [line]
  (reduce
   (fn [{buffer :buffer :as m} ch]
     (cond
       (opening-bracket? ch) (update m :buffer #(conj % ch))
       (closes? buffer ch)   (update m :buffer pop)

       :else (reduced (update m :invalid #(conj % ch)))))
   {:buffer '() :invalid '()}
   (s/split line #"")))

(defn complete-line [incomplete-line] (-> (check incomplete-line) :buffer invert))
(defn first-invalid [line] (-> (check line) :invalid first))

(defn part-one [input]
  (let [errors (remove nil? (map first-invalid input))
        occurrences (frequencies errors)]
    (reduce-kv
      (fn [error-score ch frequency]
        (+ error-score (* frequency (points ch))))
     0
     occurrences)))

(defn part-two-score [completion]
  (reduce (fn [score ch] (+ (* 5 score)
                            (get pt-2-points (str ch))))
          0
          completion))

(defn part-two [input]
  (let [incompletes (remove first-invalid input)
        completions (map complete-line incompletes)
        scores (sort (map part-two-score completions))]
    (nth scores (/ (dec (count scores)) 2))))

(def input (elves/each-line "2021/day_10.txt"))

(= (part-one input) 339537)
(= (part-two input) 2412013412)


