(ns clj-stats.core-test
  (:require [clojure.test :refer :all]
            [clj-stats.core :refer :all]))

(deftest foldleft
  	 (testing "foldleft failed."
    	 (is (= (list_foldleft 0 '(1 2 3 4) (fn [x y] (+ x y)) ) 10))
    	 ))

(deftest foldright
	 (testing "foldright failed."
	 (is (= (list_foldright 0 '(1 2 3 4) (fn [x y] (- x y)) ) -2))
	 ))


(deftest length
	 (testing "length failed."
	 (is (= (list_length '(1 2 3 4 5 6 7 8 9)) 9))
	 ))


