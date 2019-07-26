(ns mal.step2_eval
  (:require [mal.reader :as r]
            [mal.printer :as p]
            [clojure.repl :as repl])
  (:gen-class))

(defn read-mal [s]
  (r/read-str s))

(declare eval-mal)

(def env {'+ + '- - '* * '/ /})

(defn eval-ast [ast]
  (cond
    (symbol? ast) (or (get env ast) (throw (Exception. (str "undefined symbol:" ast))))
    (list? ast) (map eval-mal ast)
    (map? ast) (into {} (map eval-mal ast))
    (vector? ast) (mapv eval-mal ast)
    :else ast))



(defn eval-mal [exp]
  (cond
    (and (list? exp) (empty? exp)) exp
    (list? exp) (apply (eval-ast (first exp)) (eval-ast (rest exp)))
    :else (eval-ast exp)))

(defn print-mal [res]
  (p/as-str res))



(defn rep [s]
  (->> s read-mal eval-mal print-mal))

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
