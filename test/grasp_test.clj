(ns grasp-test
  (:require [clojure.test :refer [deftest is testing]]
            [matcher-combinators.test :refer [match? thrown-match?]]
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
                   :grasp/original-form '(grasp/grab {:a a})}
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

(comment
  ((requiring-resolve `kaocha.repl/run) 'grasp-test))
