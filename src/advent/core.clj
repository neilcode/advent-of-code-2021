(ns advent.core
  (:gen-class)
  (:require [clojure.string :as string]))

(defn each-line
  "Returns a list of strings representing lines in the given file"
  [filename & {:keys [root path matcher]
               :or
               {root (System/getProperty "user.dir")
                path "resources/"
                matcher #"\n"}}]
  (-> (slurp (str root "/" path filename))
      (string/split matcher)))

(defn -main [& _args]
  (println "Hello"))
