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
           input nil
           greeting true]
      (when (and greeting (not quiet))
        (println (format "Using notation %s" style))
        (println (format "Type one of %s to switch notation, or %s to quit"
                         (join ", " styles) quit-cmd)))
      (when-not quiet
        (print prompt))
      (flush)
      ;; Need to wait to bind input until now, rather than at the top of the loop, since
      ;; line-seq reads in a line immediately, which means we wouldn't be able to show the
      ;; prompt first
      (let [input (or input (line-seq (java.io.BufferedReader. *in*)))
            line (first input)
            trimmed (trim line)]
        (cond
          (= trimmed quit-cmd) nil ;; exit
          ((set styles) trimmed) (recur (clojure.edn/read-string trimmed) (next input) true)
          (empty? trimmed) (recur style (next input) false)
          true (do
                 (try
                   (println (format "%s%s" (str (when-not quiet result))
                                    (eval-op (parse style line))))
                   (catch IllegalArgumentException e
                     (println (ex-message e))))
                 (recur style (next input) false)))))))

(def CLI
  {:app {:comman "cliculator"
         :description "Simple CLI calculator"
         :version "0.1.0"}
   :global-opts [{:option "style" :short "s" :type (set (keys parsers)) :default :ordinary
                  :description "Arithmetic notation to use for parsing input expressions"}]
   :commands [{:command "eval"
               :description "Evaluate and print a single expression"
               :opts [{:option "expr" :short 0 :type :string :as "EXPR"
                       :description "input expression to evaluate"}]
               :runs handler-eval-single}
              {:command "repl"
               :description "Enter interactive REPL"
               :opts [{:option "quiet" :short "q" :type :with-flag
                       :description "Don't print REPL prompt and result markers"}]
               :runs handler-repl}]})

(defn -main
  "Main entrypoint for Cliculator"
  [& args]
  (cli/run-cmd args CLI))
