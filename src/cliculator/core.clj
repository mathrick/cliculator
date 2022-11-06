(ns cliculator.core
  (:require [clojure.set :refer [union]])
  (:require [clojure.string :refer [join trim]])
  (:require [clojure.core.match :refer [match]])
  (:require [instaparse.core :as insta :refer [defparser]])
  (:require [cli-matic.core :as cli])
  (:gen-class))

(load "ops")
(load "eval")
(load "parser")

(defn handler-eval-single [{:keys [style expr]}]
  "Handler function for CLI eval command"
  (try
    (println (eval-op (parse style expr)))
    (catch IllegalArgumentException e
      (println (ex-message e)))))

(defn handler-repl [{:keys [style expr quiet]}]
  "Handler function for CLI repl command"
  (let [styles (map #(.toLowerCase (str %)) (keys parsers))
        quit-cmd ":quit"
        prompt "? "
        result "> "]
    (loop [style style
           ;; line-seq reads in a line immediately, which means we wouldn't be able to show the
           ;; prompt first, so we need to make it fully lazy manually
           input (lazy-seq (line-seq (java.io.BufferedReader. *in*)))
           greeting true]
      (when (and greeting (not quiet))
        (println (format "Using notation %s" style))
        (println (format "Type one of %s to switch notation, or %s to quit"
                         (join ", " styles) quit-cmd)))
      (when-not quiet
        (print prompt))
      (flush)
      (let [line (first input)
            ;; Wrap it in a lazy seq so it doesn't read until the result and next prompt has been shown
            next-line (lazy-seq (next input))
            trimmed (and line (trim line))]
        (cond
          (or (not line)
              (= trimmed quit-cmd)) nil ;; exit
          ((set styles) trimmed) (recur (clojure.edn/read-string trimmed) next-line true)
          (empty? trimmed) (recur style next-line false)
          true (do
                 (try
                   (println (format "%s%s" (str (when-not quiet result))
                                    (eval-op (parse style line))))
                   (catch IllegalArgumentException e
                     (println (ex-message e))))
                 (recur style next-line false)))))))

(def CLI-opt-style
  [{:option "style" :short "s" :type (set (keys parsers)) :default :ordinary
    :description "Arithmetic notation to use for parsing input expressions"}])

(def CLI
  {:app {:comman "cliculator"
         :description "Simple CLI calculator"
         :version "0.1.0"}
   :commands [{:command "eval"
               :description "Evaluate and print a single expression"
               :opts (into CLI-opt-style
                           [{:option "expr" :short 0 :type :string :as "EXPR"
                             :description "input expression to evaluate"}])
               :runs handler-eval-single}
              {:command "repl"
               :description "Enter interactive REPL"
               :opts (into CLI-opt-style
                           [{:option "quiet" :short "q" :type :with-flag
                             :description "Don't print REPL prompt and result markers"}])
               :runs handler-repl}]})

(defn -main
  "Main entrypoint for Cliculator"
  [& args]
  (cli/run-cmd args CLI))
