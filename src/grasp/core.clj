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
     (println (str (apply str (Character/toChars 128401))
                   (apply str (Character/toChars 128070))
                   (apply str (Character/toChars 128073)))
              "Call grabbed from form" (pr-str (quote ~form)))
     (if (contains? result# :return)
       (:return result#)
       (throw (:exception result#)))))

(defmacro grab-call
  "Grabs the call and add its execution to the log.
   It knows how to deal with exceptions, and adds to the log
   the metadata of the fn being call, the var, the args that were
   passed as parameter and the result. If it is a pure function (it should)
   you can replay exactly what happened and than deep dive
   in that function with more sparse (or not) grabs to pinpoint the
   problem that you are trying to find.
   Beside the form you can pass a grab-id as the first parameter,
   and if you so choose even the atom and execution id atoms, but
   totally optional and you would be better just using the global
   ones.
   Grab-id and execution-id and not that special, are just to help
   filter the log to find what you want when the log gets chaotic."
  ([form]
   (emit-grab-call nil form `log `current-execution-id))
  ([grab-id form]
   (emit-grab-call grab-id form `log `current-execution-id))
  ([grab-id form & {:keys [log execution-id]}]
   (emit-grab-call grab-id form log execution-id)))

(defn emit-grab-value [grab-id form log execution-id]
  `(let [grab-id# ~grab-id
         result# (try
                   (grab-a-value ~form)
                   (catch Throwable t#
                     {:exception t#
                      :exception-as-map (Throwable->map t#)}))]
     (swap! ~log conj
            (merge (cond-> {:form (quote ~form)
                            :execution-id (deref ~execution-id)}
                     grab-id# (assoc :grab-id grab-id#))
                   result#))
     (println (str (apply str (Character/toChars 128401))
                   (apply str (Character/toChars 128070))
                   (apply str (Character/toChars 128073)))
              "Value grabbed from form" (pr-str (quote ~form)))
     (if (contains? result# :value)
       (:value result#)
       (throw (:exception result#)))))

(defmacro grab-value
  "Grabs the value and add it to the log.
   It knows how to deal with exceptions, and adds to the log
   the metadata of the fn being call, the var, the args that were
   passed as parameter and the result.
   Beside the form you can pass a grab-id as the first parameter,
   and if you so choose even the atom and execution id atoms, but
   totally optional and you would be better just using the global
   ones.
   Grab-id and execution-id and not that special, are just to help
   filter the log to find what you want when the log gets chaotic."
  ([form]
   (emit-grab-value nil form `log `current-execution-id))
  ([grab-id form]
   (emit-grab-value grab-id form `log `current-execution-id))
  ([grab-id form & {:keys [log execution-id]}]
   (emit-grab-value grab-id form log execution-id)))

(defn emit-grab [grab-id form log execution-id]
  (if (and (list? form)
           (-> form first symbol?)
           (-> form first resolve var-get fn?))
    (emit-grab-call grab-id form log execution-id)
    (emit-grab-value grab-id form log execution-id)))

(defmacro grab
  "It chooses if should grab as a call or a value,
   to see what each does see grab-call and grab-value docs"
  ([form]
   (emit-grab nil form `log `current-execution-id))
  ([grab-id form]
   (emit-grab grab-id form `log `current-execution-id))
  ([grab-id form & {:keys [log execution-id]}]
   (emit-grab grab-id form log execution-id)))

(defmacro ->grab-call
  "see grab-call docs"
  [form grab-id]
  (emit-grab-call grab-id form `log `current-execution-id))

(defmacro ->>grab-call
  "see grab-call docs"
  [grab-id form]
  (emit-grab-call grab-id form `log `current-execution-id))

(defmacro ->grab-value
  "see grab-value docs"
  [form grab-id]
  (emit-grab-value grab-id form `log `current-execution-id))

(defmacro ->>grab-value
  "see grab-value docs"
  [grab-id form]
  (emit-grab-value grab-id form `log `current-execution-id))

(defmacro ->grab
  "see grab docs"
  [form grab-id]
  (emit-grab grab-id form `log `current-execution-id))

(defmacro ->>grab
  "see grab docs"
  [grab-id form]
  (emit-grab grab-id form `log `current-execution-id))
