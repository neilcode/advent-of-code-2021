(ns calendar.2021.day-03
  (:require [advent.core :as elves]
            [clojure.string :as str]))

(def input (->> "2021/day_03.txt" elves/each-line (map #(str/split % #""))))
(defn bit-at [i] (fn [coll] (get coll i)))
(defn bits-at [idx coll] (mapv (bit-at idx) coll))
(defn to-decimal [binary-arr] (Integer/parseInt (str/join binary-arr) 2))

(defn commonalities
  "Takes a frequency map of occuring bits e.g. {'0' 5 '1' 2} and declares the most- and least-common bits"
  [freqs]
  (let [ones   (get freqs "1")
        zeroes (get freqs "0")]
    (cond
      (= ones zeroes) {:most-common "1" :least-common "0"} ;; added in part-two
      (> ones zeroes) {:most-common "1" :least-common "0"}
      (> zeroes ones) {:most-common "0" :least-common "1"})))

(defn consumption-rates
  "Assesses the most- and least-commonly occurring bit at the same position in a collection of binary numbers"
  [diagnostic-report]
  (reduce
   (fn [rates idx]
     (let [{:keys [most-common least-common]} (->> diagnostic-report (bits-at idx) frequencies commonalities)]
       {:least-common (conj (:least-common rates) least-common)
        :most-common (conj (:most-common rates) most-common)}))
   {:least-common [] :most-common []}
   (range 0 (count (first diagnostic-report)))))

(defn part-one [diagnostic-report]
  (let [{:keys [most-common least-common]} (consumption-rates diagnostic-report)]
    (* (to-decimal most-common)
       (to-decimal least-common))))

(defn find-by-bit-criteria [criterion diagnostic-report]
  (loop [filtered-nums diagnostic-report
         idx 0]
    (let [grouped-by-bit (group-by (bit-at idx) filtered-nums)
          selected-bit (criterion
                        (commonalities
                         (-> grouped-by-bit
                             (update "1" count)
                             (update "0" count))))]
      (cond
        (= 1 (count filtered-nums)) (first filtered-nums) ;; win
        :else (recur
               (get grouped-by-bit selected-bit)
               (inc idx))))))

(defn part-two [diagnostic-report]
  (let [oxygen-rating (to-decimal (find-by-bit-criteria :most-common diagnostic-report))
        c02-rating (to-decimal (find-by-bit-criteria :least-common diagnostic-report))]
    (* oxygen-rating c02-rating)))

(= (part-one input) 4138664)
(= (part-two input) 4273224)