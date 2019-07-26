(ns mal.printer)

(defn escape [s]
  (-> s (clojure.string/replace "\\" "\\\\")
      (clojure.string/replace "\"" "\\\"")
      (clojure.string/replace "\n" "\\n")))

(defn as-str
  ([obj] (as-str obj true))
  ([obj _r]
   (cond
     (nil? obj) "nil"
     (string? obj) (if _r (str "\"" (escape obj) "\"") obj)

     (list? obj) (str "(" (clojure.string/join " " (map #(as-str % _r) obj)) ")")
     (vector? obj) (str "[" (clojure.string/join " " (map #(as-str % _r) obj)) "]")
     (map? obj) (str "{" (clojure.string/join " " (map (fn [[k v]]
                                                         (str (as-str k _r) " "
                                                              (as-str v _r))) obj)) "}")
     (= (type obj) clojure.lang.Atom) (str "(atom " (as-str @obj _r) ")")
     :else (str obj))))