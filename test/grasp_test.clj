(ns grasp-test
  (:require [clojure.test :refer [deftest is testing]]
            [matcher-combinators.test :refer [match? thrown-match?]]
            [matcher-combinators.matchers :as m]
            [grasp])
  (:import (clojure.lang ExceptionInfo)))

(defmacro capturing-tap [binding-vec & forms]
  (let [binding# (first binding-vec)]
    `(let [captured-tap# (atom [])]
       (with-redefs
         [tap>
          (fn ~['a]
            (swap! captured-tap# conj ~(quote a))
            true)]
         (let [~binding# captured-tap#]
           ~@forms)))))

(deftest tap
  (let [a 1]
    (capturing-tap [tapped]
      (is (= {:a 1} (grasp/grab {:a a})))
      (is (= [{:a 1}] @tapped))
      (is (match? {:grasp/grasped? true
                   :grasp/original-form '(grasp/grab {:a a})
                   :grasp/locals {'a 1}
                   :grasp/stacktrace (m/pred (partial every? vector))}
                  (meta (last @tapped))))))

  (capturing-tap [tapped]
    (is (= 1 (grasp/grab 1)))
    (is (= [1] @tapped))
    (testing "numbers can't have meta, so we just don't have it"
      (is (= nil
             (meta (last @tapped))))))

  (capturing-tap [tapped]
    (grasp/grab (with-meta {:a :b}
                           {:meta :here}))
    (testing "we keep whatever meta we had before"
      (is (match? {:grasp/grasped? true
                   :meta :here}
                  (meta (last @tapped))))))

  (capturing-tap [tapped]
    (let [ex (ex-info "Oh Noes"
                      {:what 'happened})]
      (is (thrown-match?
            ExceptionInfo
            {:what 'happened}
            (grasp/grab (throw ex))))
      (is (= [ex] @tapped)))))

(deftest tap-with-mapper
  (let [a {:b :c}]
    (grasp/set-mapper! (fn [m v]
                         (assoc m ::value v)))
    (capturing-tap [tapped]
      (is (= {:a {:b :c}} (grasp/grab {:a a})))
      (is (match? {::value {:a a}}
                  (meta (last @tapped)))))
    (grasp/unset-mapper!)
    (capturing-tap [tapped]
      (is (= {:a {:b :c}} (grasp/grab {:a a})))
      (is (match? {::value m/absent}
                  (meta (last @tapped))))))

  (testing "we can't remove grasped? from the metadata"
    (let [a {:b :c}]
      (grasp/set-mapper! (fn [m v]
                           (dissoc m :grasp/grasped?)))
      (capturing-tap [tapped]
        (grasp/grab {:a a})
        (is (match? {:grasp/grasped? true}
                    (meta (last @tapped)))))
      ;; to not tarnish the other tests
      (grasp/unset-mapper!))))

(deftest thread->
  (capturing-tap [tapped]
    (is (= 5 (grasp/-> 1 inc inc inc inc)))
    (is (= [1 2 3 4 5]
           @tapped))))

(deftest thread->>
  (capturing-tap [tapped]
    (is (= 4 (grasp/->> 2 (- 3) (- 5))))
    (is (= [2 1 4]
           @tapped))))

(deftest let-macro
  (capturing-tap [tapped]
    (is (= 5
        (grasp/let [a 1
                    b 3]
          (+ 1 a b))))
    (is (= [1 3]
           @tapped))))

;; Sinks

(deftest form+value-sink
  (letfn [(f [result form value]
            (swap! result conj [form value]))]
    (let [result (atom nil)
          value (with-meta {:a 6}
                  {:grasp/grasped? true
                   :grasp/original-form {:a '(+ 2 3)}})]
      (grasp/form+value-sink (partial f result) value)
      (is (= [[{:a '(+ 2 3)} value]]
             @result)))

    (let [result (atom nil)
          value 5]
      (grasp/form+value-sink (partial f result) value)
      (is (= [[value value]]
             @result)))))

(deftest persistent-sink!
  (grasp/persistent-sink! ::some-value)
  (is (= [::some-value]
         @grasp/log)))

(comment
  ((requiring-resolve `kaocha.repl/run) *ns*))
