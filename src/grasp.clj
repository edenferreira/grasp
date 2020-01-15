(ns grasp
  (:refer-clojure :exclude [-> ->>]))

(defonce ^:dynamic *log* (atom []))
(def ^:dynamic *execution-id* :execution-id)
(def ^:dynamic *log-max-size* 1000)

(defn search-for-var [p]
  (if (var? p)
    p
    (clojure.core/->> (all-ns)
                      (mapcat ns-map)
                      (filter #(and (var? (second %))
                                    (identical? p (var-get (second %)))))
                      first
                      second)))

(defn tap [v]
  (tap> v)
  (swap! *log*
         (fn [log]
           (let [log (conj (vec log) v)]
             (if (clojure.core/-> log count (> *log-max-size*))
               (rest log)
               log))))
  v)

(defmacro -> [& forms]
  (let [forms' (interleave forms (repeat `tap))]
    `(clojure.core/-> ~@forms')))

(defmacro ->> [& forms]
  (let [forms' (interleave forms (repeat `tap))]
    `(clojure.core/->> ~@forms')))
