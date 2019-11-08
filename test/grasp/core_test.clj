(ns grasp.core-test
  (:require [clojure.test :as t]
            [grasp.core :as grasp]
            [matcher-combinators.test :refer [match? thrown-match?]]
            [matcher-combinators.matchers :as m])
  (:import (clojure.lang ExceptionInfo)))

(defn grabbable [x y z]
  {:sum (+ x y z)
   :subtraction (- x y z)
   :multiplication (* x y z)})

(def exception (ex-info "oh noes" {:oh :no}))

(defn grabbable-throw! [_x _y]
  (throw exception))

(t/deftest grabbed-fn
  (let [grabbed (grasp/grabbed-fn grabbable)]
    (t/is (match? {:return
                   {:sum 6
                    :subtraction -4
                    :multiplication 6}
                   :args [1 2 3]
                   :file (m/regex #"grasp/core_test.clj$")
                   :line 8
                   :name 'grabbable
                   :var #'grabbable}
                  (grabbed 1 2 3)))
    (t/is (= (the-ns 'grasp.core-test)
             (:ns (grabbed 1 2 3)))))

  (let [grabbed (grasp/grabbed-fn grabbable-throw!)]
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
                   :line 38
                   :name 'grabbable-value}
                  (grasp/grab-a-value grabbable-value)))
    (t/is (= (the-ns 'grasp.core-test)
             (:ns (grasp/grab-a-value grabbable-value)))))
  (t/testing "we are able to get the right var"
    (t/is (match? {:value {:a :b :c :d}
                   :var #'other-grabbable-value
                   :line 39}
                  (grasp/grab-a-value other-grabbable-value)))))

(t/deftest grab-call
  (let [log (atom [])
        execution-id (atom :some-execution-id)
        two 2
        three 3
        four 4]
    (t/testing "the return is unmodified, but we grab
                the information of the call"
      (t/is (= {:multiplication 24
                :subtraction -5
                :sum 9}
               (grasp/grab-call :some-grab-id
                                (grabbable two three four)
                                :log log
                                :execution-id execution-id)))
      (t/is (match? [{:return {:multiplication 24
                               :subtraction -5
                               :sum 9}
                      :var #'grabbable
                      :form '(grabbable two three four)
                      :grab-id :some-grab-id
                      :execution-id :some-execution-id}]
                    @log))))
  (let [log (atom [])
        execution-id (atom :some-other-execution-id)
        zero 0
        one 1]
    (t/testing "the exception is unmodified and thrown,
                but we grab the information of the call"
      (t/is (thrown-match?
              ExceptionInfo
              {:oh :no}
              (grasp/grab-call :some-other-grab-id
                               (grabbable-throw! zero one)
                               :log log
                               :execution-id execution-id)))
      (t/is (match? [{:exception-as-map (Throwable->map exception)
                      :var #'grabbable-throw!
                      :form '(grabbable-throw! zero one)
                      :grab-id :some-other-grab-id
                      :execution-id :some-other-execution-id}]
                    @log))))
  (let [log (atom [])
        execution-id (atom :some-execution-id)]
    (t/testing "we know how to deal with fns that return nil"
      (t/is (= nil
               (grasp/grab-call :some-grab-id
                                (println "Hello there!")
                                :log log
                                :execution-id execution-id)))
      (t/is (match? [{:return nil
                      :var #'println
                      :form '(println "Hello there!")}]
                    @log))))
  (let [log (atom [])
        execution-id (atom :some-execution-id)]
    (grasp/grab-call nil
                     (println "Hello there!")
                     :log log
                     :execution-id execution-id)
    (t/is (match? [{:grab-id m/absent}]
                  @log))))

(t/deftest grab-value
  (let [log (atom [])
        execution-id (atom :some-execution-id)
        something {:some :thing}]
    (t/testing "we grab the information of the value
                but return it unmodified"
      (t/is (= something
               (grasp/grab-value :some-grab-id
                                 something
                                 :log log
                                 :execution-id execution-id)))
      (t/is (match? [{:value something
                      :form 'something
                      :grab-id :some-grab-id
                      :execution-id :some-execution-id}]
                    @log)))
    (let [log (atom [])
          execution-id (atom :some-execution-id)]
      (grasp/grab-value nil
                        something
                        :log log
                        :execution-id execution-id)
      (t/is (match? [{:grab-id m/absent}]
                    @log)))))

(t/deftest grab
  (let [log (atom [])
        execution-id (atom :some-execution-id)
        something {:some :thing}]
    (t/testing "we know try to guess if we should grab the value
                or the call, we don't try that hard"
      (t/is (= something
               (grasp/grab :some-grab-id
                           something
                           :log log
                           :execution-id execution-id)))
      (t/is (= {:sum 18
                :subtraction -8
                :multiplication 210}
               (grasp/grab :other-grab-id
                           (grabbable 5 6 7)
                           :log log
                           :execution-id execution-id)))
      (t/is (match? [{:value something
                      :form 'something
                      :grab-id :some-grab-id
                      :execution-id :some-execution-id}
                     {:return {:sum 18
                               :subtraction -8
                               :multiplication 210}
                      :form '(grabbable 5 6 7)
                      :grab-id :other-grab-id
                      :execution-id :some-execution-id}]
                    @log)))))

(t/deftest grab->
  (let [log (atom [])
        execution-id (atom :some-execution-id)
        something {:some :thing}]
    (t/testing "we know try to guess if we should grab the value
                or the call, we don't try that hard"
      (t/is (= something
               (grasp/grab :some-grab-id
                           something
                           :log log
                           :execution-id execution-id)))
      (t/is (= {:sum 18
                :subtraction -8
                :multiplication 210}
               (grasp/grab :other-grab-id
                           (grabbable 5 6 7)
                           :log log
                           :execution-id execution-id)))
      (t/is (match? [{:value something
                      :form 'something
                      :grab-id :some-grab-id
                      :execution-id :some-execution-id}
                     {:return {:sum 18
                               :subtraction -8
                               :multiplication 210}
                      :form '(grabbable 5 6 7)
                      :grab-id :other-grab-id
                      :execution-id :some-execution-id}]
                    @log)))))
