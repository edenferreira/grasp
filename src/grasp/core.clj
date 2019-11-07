(ns grasp.core
  (:require [clojure.repl :as repl]))

(defonce log (atom []))
(defonce current-execution-id (atom :initial-execution-context))

(defn search-for-var [p]
  (if (var? p)
    p
    (->> (all-ns)
         (mapcat ns-map)
         (filter #(and (var? (second %))
                       (identical? p (var-get (second %)))))
         first
         second)))

(defn grab-fn-call [f]
  (let [f-var (search-for-var f)
        f-meta (meta f-var)]
    (fn grabbed [& args]
      (let [result (try
                     {:return (apply f args)}
                     (catch Throwable t
                       {:exception t
                        :exception-as-map (Throwable->map t)}))]
        (merge {:args args
                :var f-var}
               f-meta
               result)))))
