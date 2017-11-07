(ns clj_stats.core_test
  (:require [clojure.test :refer :all]
            [clj_stats.core :refer :all]))

(def test_var '(1 2 3 4 5 6 7 8 9 10))

(deftest foldleft
  	 (testing "foldleft failed."
    	 (is (= (list_foldleft 0 test_var +) 55))
    	 ))

(deftest mean
	 (testing "list_mean failed."
	 (is (= (list_mean test_var) 11/2)
	 )))

(deftest length
	 (testing "length failed"
	 (is (= (list_length test_var) (nth (lazy_rolling_length test_var) 9)))
	 ))

(deftest test_streamize
	 (testing "streamize failed."
	 (is (and (= (first (streamize test_var)) (first test_var)) (=(nth (streamize test_var) 5) (nth test_var 5)))
	 )))

(deftest lazymean
	 (testing "lasy_leftroll failed"
	 (is (= (nth (lazy_rolling_mean test_var) (- (list_length test_var) 1)) (list_mean test_var))
	 )))

(deftest merge
      (testing "lazy_merge failed"
      (is (list_equal (take 10 (lazy_merge test_var test_var)) '(1 1 2 2 3 3 4 4 5 5))
      )))