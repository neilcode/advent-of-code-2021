(ns calendar.2021.day-14
  (:require [advent.core :as elves]
            [clojure.string :as s]))


(defn into-pairs [polymer] (mapv s/join (partition 2 1 polymer)))

(defn join-pairs [pairs]
  (let [[first-chunk & polymer-chunks] pairs]
    (vec
     (reduce
      (fn [polymer chunk] (concat polymer (rest chunk)))
      first-chunk
      polymer-chunks))))

(defn insert-into-pair [pair rules]
  (let [[a b] (s/split pair #"")]

    [a (get rules pair) b]))


(defn polymer-rules [input]
  (reduce
   (fn [chart conversion]
     (let [regex #"([A-Z]{2}) -> ([A-Z])"
           [_ pair result] (re-matches regex conversion)]
       (assoc chart pair result)))
   {}
   input)) ;; first lines of input are not like the rest

(defn grow [rules polymer]
  (->> polymer
       into-pairs
       (map #(insert-into-pair % rules))
       join-pairs
       s/join))


;; (defn grow [rules limit pair]
;;   (loop [step 0
;;          polymer pair]

;;     (cond
;;       (= step limit) polymer
;;       :else (recur
;;              (inc step)
;;              (->> polymer into-pairs
;;                                    (map #(insert-into-pair % rules))
;;                                    join-pairs
;;                                    s/join)))))

(def input (elves/each-line "2021/day_14.txt"))

(defn part-one [template rules limit]
  (let [initial-pairs (into-pairs template)]
    (map
     (fn [pair] (loop [step 0
                       polymer template]

                  (cond
                    (= step limit) polymer
                    :else (recur (inc step) (grow rules polymer)))))
     initial-pairs)))

(part-one (first input) (polymer-rules (drop 2 input)) 10)

(comment
  :tests
  (def test-table
    {"CH" "B"
     "HH" "N"
     "CB" "H"
     "NH" "C"
     "HB" "C"
     "HC" "B"
     "HN" "C"
     "NN" "C"
     "BH" "H"
     "NC" "B"
     "NB" "B"
     "BN" "B"
     "BB" "N"
     "BC" "B"
     "CC" "N"
     "CN" "C"})


  (part-one "NN" test-table 4);; NBBNBNBBCCNBCNCCN
  (part-one "NC" test-table 4) ;; NBBNBBNBBBNBBNBBC
  (part-one "CB" test-table 4) ;; CBHCBHHNHCBBCBHCB
  ;;NBBNBNBBCCNBCNCCN + NBBNBBNBBBNBBNBBC + CBHCBHHNHCBBCBHCB
  ;; overlapping first/last elements:
  ;;NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB
)