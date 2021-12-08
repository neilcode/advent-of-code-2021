(ns advent.core
  (:gen-class)
  (:require [clojure.string :as string]))

(defn each-line
  "Returns a list of strings representing lines in the given file"
  [filename & {:keys [root path matcher mapfn]
               :or
               {root (System/getProperty "user.dir")
                path "resources/"
                matcher #"\n"
                mapfn identity}}]
  (as-> (str root "/" path filename) $input
    (slurp $input)
    (string/split $input matcher)
    (map mapfn $input)))

(defn abs-range 
  "Generates a range between two points on a line regardless of the order in which the points are given"
  [a b]
  (cond
    (< a b) (range a (inc b))    ;; incrementing range
    (> a b) (range a (dec b) -1) ;; decrementing range
    (= a b) (repeat a)))

(defn difference [a b] (Math/abs (- a b)))
(defn sum [coll] (apply + coll))

(defn -main [& _args]
  (println "Hello"))
