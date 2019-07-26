(ns mal.printer)

(defn escape [s]
  (-> s (clojure.string/replace "\\" "\\\\")
      (clojure.string/replace "\"" "\\\"")
      (clojure.string/replace "\n" "\\n")))

(defn as-str [obj]
  (cond
    (nil? obj) "nil"
    (string? obj) (str "\"" (escape obj) "\"")
    (list? obj) (str "(" (clojure.string/join " " (map as-str obj)) ")")
    (vector? obj) (str "[" (clojure.string/join " " (map #(pr-str %) obj)) "]")
    (map? obj) (str "{" (clojure.string/join " " (map (fn [[k v]]
                                                        (str (pr-str k) " "
                                                             (pr-str v))) obj)) "}")
    (= (type obj) clojure.lang.Atom) (str "(atom " (pr-str @obj) ")")
    :else (str obj)))