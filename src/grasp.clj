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

(defn grab* [v original-form]
  (tap> (if (instance? IObj v)
          (with-meta v
                     (merge (meta v)
                            {::grasped? true
                             ::original-form original-form}))
          v))
  v)

(defmacro grab [exp]
  `(try
     (grab* ~exp '~&form)
     (catch Exception e#
       (grab* e# '~&form)
       (throw e#))))

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

(defmacro -> [& forms]
  (let [forms' (interleave forms (repeat `grab))]
    `(clojure.core/-> ~@forms')))

(defmacro ->> [& forms]
  (let [forms' (interleave forms (repeat `grab))]
    `(clojure.core/->> ~@forms')))
