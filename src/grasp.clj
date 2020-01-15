(ns grasp)

(defonce ^:dynamic *log* (atom []))
(def ^:dynamic *execution-id* :execution-id)
(def ^:dynamic *log-max-size* 1000)

(defn search-for-var [p]
  (if (var? p)
    p
    (->> (all-ns)
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
             (if (-> log count (> *log-max-size*))
               (rest log)
               log))))
  v)
