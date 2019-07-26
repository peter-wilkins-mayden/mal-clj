(ns mal.core
  (:require [mal.printer :as p]))

(def user-ns {'= =
              'nil? nil?
              'not not
              'true? true?
              'false? false?
              'string? string?
              'symbol symbol
              'symbol? symbol?
              'keyword keyword
              'keyword? keyword?
              'number? number?
              'prn (fn [& xs] (println (clojure.string/join " " (map #(p/as-str % true) xs))))
              'println (fn [& xs] (println (clojure.string/join " " (map #(p/as-str % false) xs))))
              'pr-str (fn [& xs] (clojure.string/join " " (map #(p/as-str % true) xs)))
              'str (fn [& xs] (clojure.string/join "" (map #(p/as-str % false) xs)))
              'slurp slurp
              '< <
              '<= <=
              '> >
              '>= >=
              '+ +
              '- -
              '* *
              '/ /
              'list list
              'list? seq?
              'vector vector
              'vector? vector?
              'hash-map hash-map
              'map? map?
              'assoc assoc
              'dissoc dissoc
              'get get
              'contains? contains?
              'sequential? sequential?
              'cons cons
              'nth nth
              'first first
              'rest rest
              'empty? empty?
              'count count
              'apply apply
              'deref deref
              'reset! reset!
              'swap! swap!})