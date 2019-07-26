(ns mal.step0_repl
  (:gen-class))

(defn read-mal [s]
  s)

(defn eval-mal [exp]
  exp)

(defn print-mal [res]
  (println res))

(defn rep [s]
  (-> s read-mal eval-mal print-mal))


(defn -main [& args]
  (loop []
    (print "user> ")
    (flush)
    (rep (read-line))
    (recur)))
