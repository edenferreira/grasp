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

(t/deftest grab-fn-call
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

(def grabbable-value {:a :b :c :d})
(def other-grabbable-value {:a :b :c :d})

(t/deftest grab-a-value
  (t/testing "we are able to get the value with some metadata"
    (t/is (match? {:value {:a :b :c :d}
                   :var #'grabbable-value
                   :file (m/regex #"grasp/core_test.clj$")
                   :line 37
                   :name 'grabbable-value}
                  (grasp/grab-a-value grabbable-value)))
    (t/is (= (the-ns 'grasp.core-test)
             (:ns (grasp/grab-a-value grabbable-value)))))
  (t/testing "we are able to get the right var"
    (t/is (match? {:value {:a :b :c :d}
                   :var #'other-grabbable-value
                   :line 38}
                  (grasp/grab-a-value other-grabbable-value)))))
