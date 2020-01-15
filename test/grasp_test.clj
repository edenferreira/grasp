(ns grasp-test
  (:require [clojure.test :refer [deftest is]]
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
