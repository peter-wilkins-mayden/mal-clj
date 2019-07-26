(ns mal.reader)

(def tok-re #"[\s,]*(~@|[\[\]{}()'`~^@]|\"(?:[\\].|[^\\\"])*\"?|;.*|[^\s\[\]{}()'\"`@,;]+)")
(def int-re #"^-?[0-9]+$")
(def str-re #"^\"((?:[\\].|[^\\\"])*)\"$")
(def badstr-re #"^\"")

(defn unescape [s]
  (-> s
      (clojure.string/replace "\\\\" "\u029e")
      (clojure.string/replace "\\\"" "\"")
      (clojure.string/replace "\\n" "\n")
      (clojure.string/replace "\u029e" "\\")))

(defn rdr [tokens]
  {:tokens tokens :position (atom 0)})

(defn rdr-peek [{:keys [tokens position]}]
  (get tokens @position))

(defn rdr-next [{:keys [tokens position]}]
  (let [i @position]
    (swap! position inc)
    (get tokens i)))

(declare read-form)

(defn read-atom [rdr]
  (let [t (rdr-next rdr)]
    (cond
      (= t "nil") nil
      (re-find int-re t) (Integer/parseInt t)
      (re-find str-re t) (-> (re-find str-re t) second unescape)
      (re-find badstr-re t) (throw (Exception. "expected closing \", found EOF"))
      (= \: (get t 0)) (keyword (subs t 1))
      (= t "true") true
      (= t "false") false
      :else (symbol t))))

(defn read-list [rdr open close]
  (assert (= open (rdr-next rdr)))                           ; advances position
  (loop [lst []]
    (let [token (rdr-peek rdr)]
      (cond
        (= close token) (do (rdr-next rdr) lst)
        (not token) (throw (Exception. (str "expected " close ", found EOF")))
        :else (recur (conj lst (read-form rdr)))))))

(defn read-form [rdr]
  (let [token (rdr-peek rdr)]
    (cond
      (= "'" token)  (do (rdr-next rdr) (list 'quote (read-form rdr)))
      (= "`" token)  (do (rdr-next rdr) (list 'quasiquote (read-form rdr)))
      (= "~" token)  (do (rdr-next rdr) (list 'unquote (read-form rdr)))
      (= "~@" token) (do (rdr-next rdr) (list 'splice-unquote (read-form rdr)))

      (= "^" token)  (do (rdr-next rdr) (let [m (read-form rdr)]
                                        (list 'with-meta (read-form rdr) m)))
      (= "@" token)  (do (rdr-next rdr) (list 'deref (read-form rdr)))
      (= "(" token) (apply list (read-list rdr "(" ")"))
      (= "{" token) (apply list (read-list rdr "{" "}"))
      (= "[" token) (apply list (read-list rdr "[" "]"))
      (= ")" token) (throw (Exception. "unbalanced form"))
      :else (read-atom rdr))))

(defn tokenize [s]
  (mapv second (re-seq tok-re s)))

(comment
  (let [r (rdr (tokenize "(+ 1 2)"))]
    (read-form r))
  )

(defn read-str [s]
  (-> (tokenize s)
      rdr
      read-form))