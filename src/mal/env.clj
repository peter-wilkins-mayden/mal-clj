(ns mal.env)

(defn inner-env [outer]
  (atom {:outer outer}))

(defn env-set [env key value]
  ;(println env)
  (swap! env assoc key value)
  env)

(defn env-get [env key]
  (or (get @env key)
      (when (get @env :outer) (env-get (get @env :outer) key))
      (throw (Exception. (str key " not found")))))


