(ns oakbeach.calculator-test
  (:require [clojure.test :refer [deftest is testing]]
            [oakbeach.calculator :refer [handle-input]]))

(deftest calculator-basics
  (testing "Basic arithmetic"
    (is (= "5.0" (handle-input "2 + 3")))
    (is (= "10.0" (handle-input "20 / 2")))
    (is (= "6.0" (handle-input "2 * 3"))))

  (testing "Order of operations (PEMDAS)"
    (is (= "7.0" (handle-input "1 + 2 * 3")))
    (is (= "14.0" (handle-input "10 / 2 + 3 ^ 2"))))

  (testing "Parentheses"
    (is (= "9.0" (handle-input "(1 + 2) * 3")))
    (is (= "20.0" (handle-input "2 * (5 + 5)"))))

  (testing "Negative numbers"
    (is (= "-3.0" (handle-input "-5 + 2")))
    (is (= "8.0" (handle-input "10 + -2")))
    (is (= "-20.0" (handle-input "-(5 + 5) * 2"))))

  (testing "Error handling"
    (is (re-find #"Divide by zero" (handle-input "10 / 0")))
    (is (re-find #"Invalid character" (handle-input "1 + abc")))))