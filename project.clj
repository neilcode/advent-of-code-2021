(defproject advent-of-code-2021 "0.1.0-SNAPSHOT"
  :description "Solutions for the 2021 Advent of Code calendar"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/tools.trace "0.7.11"]]
  :aot :all
  :main advent.core
  :uberjar-name "advent.jar")
