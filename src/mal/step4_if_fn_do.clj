(ns mal.step4_if_fn_do
  (:require [mal.reader :as r]
            [mal.printer :as p]
            [mal.env :as e]
            [mal.core :as core]
            [clojure.repl :as repl])
  (:gen-class))

(defn pl [& xs]
  (do
    (apply println xs)
    (flush)))

(defn read-mal [s]
  (r/read-str s))

(declare eval-mal)

(defn kv-pairs [ks vs]
  (let [[ks [_ rest-key]] (split-with #(not= '& %) ks)]
    (if rest-key
      (conj (mapv vector ks vs) [rest-key (drop (count ks) vs)])
      (mapv vector ks vs))))

(defn new-env [env ks vs]
  (reduce (fn [env [k v]]
            (e/env-set env k (eval-mal env v)))
          (e/inner-env env)
          (kv-pairs ks vs)))

(defn eval-mal [env exp]
  ;(pl "eval-mal: " exp " " (type exp))
  (let [eval-ast (fn [env ast]
                   (cond
                     (symbol? ast) (e/env-get env ast)
                     (list? ast) (doall (map #(eval-mal env %) ast))
                     :else ast))]
    (if (list? exp)
      (case (first exp)
        nil exp
        fn* (let [[_ ks body] (vec exp)]
              (fn [& vs] (eval-mal (new-env env ks vs) body)))

        if (let [[_ pred this & that] (vec exp)]
             (let [p (eval-mal env pred)]
               (if (or (nil? p) (false? p))
                 (when (seq that) (eval-mal env (first that)))
                 (eval-mal env this))))

        do (last (eval-ast env (rest exp)))

        def! (get @(e/env-set env (second exp) (eval-mal env (last exp))) (second exp))

        let* (let [[ks vs] (apply map list (partition 2 (second exp)))]
               (eval-mal (new-env env ks vs) (last exp)))

        (let [[f & args] (eval-ast env exp)]
          (apply f args)))
      (eval-ast env exp))))

(defn print-mal [res]
  (p/as-str res))

(defonce env (new-env nil (keys core/user-ns) (vals core/user-ns)))

(defn rep [s]
  (->> s
       read-mal
       (eval-mal env)
       print-mal))

(comment
  (print-mal false)
  (rep "( + 2 (* 3 4) ) ")
  (rep "(let* (a 2) (+ a 1))")
  (rep "( (fn* (a b) (+ a b)) 2 3)")
  (rep "( (fn* (& bs) (apply + bs)) 2 3)")
  (rep "(def! a (fn* (c) (+ c 1)))")
  (rep "(a 23)")
  (rep "(def! not (fn* (a) (if a false true)))")
  (rep "(not nil)")


  (apply map list (partition 2 [1 2 3 4 5 6]))
  )

(defn -main [& _]
  ;(rep "(def! not (fn* (a) (if a false true)))") not working
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
