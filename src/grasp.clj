(ns grasp
  (:refer-clojure :exclude [-> ->> let])
  (:require [clojure.pprint :as pprint])
  (:import [clojure.lang IObj]))

(defn search-for-var
  "If the parameter is a value it looks for the var
   that contains this value. For rich objects, like maps
   vectors and the like it is precise, but for primitives
   like numbers it will find the first var that contains that
   number, but maybe will not find the one you were actually
   looking for"
  [p]
  (if (var? p)
    p
    (clojure.core/->> (all-ns)
                      (mapcat ns-map)
                      (filter #(and (var? (second %))
                                    (identical? p (var-get (second %)))))
                      first
                      second)))

(def ^:private mapper (atom (fn [m v] m)))

(defn set-mapper!
  "Receives a fn that will be passed the grab metadata and the value
   itself and should return a new metadata for the grab.
   The only meta that will be added by this lib regardless is the attribute
   ':grasp/grasped?'.
   The mapper fn can be removed with the `unset-mapper!` fn."
  [f]
  (reset! mapper f)
  nil)

(defn unset-mapper!
  "Remove any mapper added with `set-mapper!`"
  []
  (reset! mapper (fn [m v] m))
  nil)

(defn grab* [v original-form exception locals]
  (tap> (if (instance? IObj v)
          (with-meta v
                     (merge (meta v)
                            (assoc
                             (@mapper {::original-form original-form
                                       ::locals locals
                                       ::stacktrace (mapv StackTraceElement->vec
                                                          (.getStackTrace exception))}
                              v)
                             ::grasped? true)))
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

(defmacro grab
  "It grabs the value of the expression parameter.
   It also grabs local bindings, stacktrace and the original form
   and adds to the metadata of the value if accepts metadata, so
   primitives like numbers for example won't have this feature.

   This can introduce a memory leak depending on the sink you are
   using, for example if it is a atom, so keep this in mind and regularly
   purge it if you have a long running repl and is grabbing a lot of value."
  [exp]
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
  (clojure.core/let [forms' (interleave forms (repeat `grab))]
    `(clojure.core/-> ~@forms')))

(defmacro ->> [& forms]
  (clojure.core/let [forms' (interleave forms (repeat `grab))]
    `(clojure.core/->> ~@forms')))

(defn emit-let-bindings [bindings]
  (vec
   (mapcat (fn [[b e]]
             [b (list `grab e)])
           (partition 2 bindings))))

(defmacro let [bindings & body]
  `(clojure.core/let ~(emit-let-bindings bindings)
     ~@body))

;; Sinks

(def pprint-sink! pprint/pprint)

(defn form+value-sink [f v]
  (if (::grasped? (meta v))
    (f (::original-form (meta v)) v)
    (f v v)))

(defn rebl-sink! [v]
  (form+value-sink
   (requiring-resolve `cognitect.rebl/submit)
   v))

(def log (atom []))
(defn persistent-sink! [v]
  (swap! log conj v))
