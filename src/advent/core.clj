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

(defn -main [& _args]
  (println "Hello"))
