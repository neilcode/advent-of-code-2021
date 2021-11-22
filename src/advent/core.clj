(ns advent.core
  (:gen-class)
  (:require [clojure.string :as string]))

(defn each-line
  "Returns a list of strings representing lines in the given file"
  [filename & {:keys [root path]
               :or
               {root (System/getProperty "user.dir")
                path "src/calendar/"}}]
  (-> (slurp (str root "/" path filename))
      (string/split #"\n")))



(defn -main [& _args]
  (println "Hello"))
