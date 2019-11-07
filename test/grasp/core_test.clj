(ns grasp.core-test
  (:require [clojure.test :as t]
            [grasp.core :as grasp]
            [matcher-combinators.test :refer [match?]]
            [matcher-combinators.matchers :as m]))

(defn grabbable [x y z]
  {:sum (+ x y z)
   :subtraction (- x y z)
   :multiplication (* x y z)})

(def exception (ex-info "oh noes" {:oh :no}))

(defn grabbable-throw! [_x _y]
  (throw exception))

(t/deftest grab-call
  (let [grabbed (grasp/grab-fn-call grabbable)]
    (t/is (match? {:return
                   {:sum 6
                    :subtraction -4
                    :multiplication 6}
                   :args [1 2 3]
                   :file (m/regex #"grasp/core_test.clj$")
                   :line 7
                   :name 'grabbable
                   :var #'grabbable}
             (grabbed 1 2 3)))
    (t/is (= (the-ns 'grasp.core-test)
             (:ns (grabbed 1 2 3)))))

  (let [grabbed (grasp/grab-fn-call grabbable-throw!)]
    (t/is (match? {:exception (partial identical? exception)
                   :exception-as-map (Throwable->map exception)}
                  (grabbed 5 6)))))
