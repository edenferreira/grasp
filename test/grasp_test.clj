(ns grasp-test
  (:require [clojure.test :refer [deftest is testing]]
            [grasp]))

(deftest tap
  (binding [grasp/*log-max-size* 10
            grasp/*log* (atom [])]
    (is (= {:a :value}
           (grasp/tap {:a :value})))
    (is (= [{:a :value}]
           @grasp/*log*)))

  (binding [grasp/*log-max-size* 10
            grasp/*log* (atom (range 10))]
    (grasp/tap {:a :value})
    (is (= 10
           (count @grasp/*log*)))))

(deftest thread->
  (binding [grasp/*log* (atom [])]
    (is (= 5 (grasp/-> 1 inc inc inc inc)))
    (is (= [1 2 3 4 5]
           @grasp/*log*))))

(deftest thread->>
  (binding [grasp/*log* (atom [])]
    (is (= 4 (grasp/->> 2 (- 3) (- 5))))
    (is (= [2 1 4]
           @grasp/*log*))))
