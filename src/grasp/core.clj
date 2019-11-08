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

(defn grabbed-fn [f]
  (let [f-var (search-for-var f)
        f-meta (meta f-var)]
    (fn grabbed [& args]
      (let [result (try
                     {:return (apply f args)}
                     (catch Throwable t
                       {:exception t
                        :exception-as-map (Throwable->map t)}))]
        (merge f-meta
               {:args args
                :var f-var}
               result)))))

(defn grab-a-value [value]
  (let [value-var (search-for-var value)]
    (merge (meta value-var)
           {:value value
            :var value-var})))

(defn emit-grab-call [grab-id
                      [f & args :as form]
                      log
                      execution-id]
  `(let [f# ~f
         grabbed# (grabbed-fn f#)
         result# (grabbed# ~@args)
         grab-id# ~grab-id]
     (swap! ~log conj
            (merge (cond-> {:form (quote ~form)
                            :execution-id (deref ~execution-id)}
                     grab-id# (assoc :grab-id grab-id#))
                   result#))
     (if (contains? result# :return)
       (:return result#)
       (throw (:exception result#)))))

(defmacro grab-call
  ([form]
   (emit-grab-call nil form log current-execution-id))
  ([grab-id form]
   (emit-grab-call grab-id form log current-execution-id))
  ([grab-id form & {:keys [log execution-id]}]
   (emit-grab-call grab-id form log execution-id)))

(defn emit-grab-value [grab-id
                       form
                       log
                       execution-id]
  `(let [value# ~form
         grab-id# ~grab-id
         result# (grab-a-value value#)]
     (swap! ~log conj
            (merge (cond-> {:form (quote ~form)
                            :execution-id (deref ~execution-id)}
                     grab-id# (assoc :grab-id grab-id#))
                   result#))
     (:value result#)))

(defmacro grab-value
  ([form]
   (emit-grab-value nil form log current-execution-id))
  ([grab-id form]
   (emit-grab-value grab-id form log current-execution-id))
  ([grab-id form & {:keys [log execution-id]}]
   (emit-grab-value grab-id form log execution-id)))
