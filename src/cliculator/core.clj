(ns cliculator.core
  (:require [clojure.set :refer [union]])
  (:require [clojure.core.match :refer [match]])
  (:require [instaparse.core :as insta :refer [defparser]])
  (:gen-class))

(load "ops")
(load "eval")
(load "parser")

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
