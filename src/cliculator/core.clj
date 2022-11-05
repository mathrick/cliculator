(ns cliculator.core
  (:require [clojure.set :refer [union]])
  (:require [clojure.core.match :refer [match]])
  (:gen-class))

(load "ops")
(load "eval")

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
