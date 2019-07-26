(ns mal.step1_read_print
  (:require [mal.reader :as r]
            [mal.printer :as p]
            [clojure.repl :as repl])
  (:gen-class))

(defn read-mal [s]
  (r/read-str s))

(defn eval-mal [exp]
  exp)

(defn print-mal [res]
  (p/as-str res))

(defn rep [s]
  (-> s read-mal eval-mal print-mal))

(comment
  (print-mal false)
  (rep "( + 2 (* 3 4) ) -> (+ 2 (* 3 4))")
  )

(defn -main [& args]
  (loop []
    (print "user> ")
    (flush)
    (try
      (when-let [l (read-line)]
        (when-not (re-seq #"^\s*$|^\s*;.*$" l)              ; blank/comment
          (println (rep l))))
      (catch Throwable e
        (repl/pst e)))
    (recur)))
