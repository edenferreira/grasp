(ns grasp
  (:refer-clojure :exclude [-> ->>])
  (:require [clojure.pprint :as pprint])
  (:import [clojure.lang IObj]))

(defn search-for-var [p]
  (if (var? p)
    p
    (clojure.core/->> (all-ns)
                      (mapcat ns-map)
                      (filter #(and (var? (second %))
                                    (identical? p (var-get (second %)))))
                      first
                      second)))

(defn grab* [v original-form exception locals]
  (tap> (if (instance? IObj v)
          (with-meta v
                     (merge (meta v)
                            {::grasped? true
                             ::original-form original-form
                             ::locals locals
                             ::stacktrace (mapv StackTraceElement->vec
                                                (.getStackTrace exception))}))
          v))
  v)

;; shameslessly copied
;; https://github.com/stuartsierra/lazytest/blob/master/modules/lazytest/src/main/clojure/lazytest/expect.clj#L4-L8
;; what should I do EPL 1.0?
(defn- local-bindings
  "Returns a map of the names of local bindings to their values."
  [env]
  (reduce (fn [m sym] (assoc m `'~sym sym))
	  {} (keys env)))

(defmacro grab [exp]
  `(try
     (grab* ~exp
            '~&form
            (ex-info "just" {:want [:the :line]})
            ~(local-bindings &env))
     (catch Exception e#
       (grab* e#
              '~&form
              (ex-info "just" {:want [:the :line]})
              ~(local-bindings &env))
       (throw e#))))

;; Replacements

(defmacro -> [& forms]
  (let [forms' (interleave forms (repeat `grab))]
    `(clojure.core/-> ~@forms')))

(defmacro ->> [& forms]
  (let [forms' (interleave forms (repeat `grab))]
    `(clojure.core/->> ~@forms')))

;; Sinks

(defn add-pretty-print-sink! []
  (add-tap pprint/pprint))

(defn remove-pretty-print-sink! []
  (remove-tap pprint/pprint))

(defn ^:private rebl-sink [v]
  ((requiring-resolve `cognitect.rebl/submit)
   '(submited by grasp)
   v))

(defn add-rebl-sink!
  "you probably don't need this because rebl already
   capture taps, but if you want to navigate faster through
   them, this can help"
  []
  (add-tap rebl-sink))

(defn remove-rebl-sink! []
  (remove-tap rebl-sink))
