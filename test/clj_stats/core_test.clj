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

(deftest mergeSort
      (testing "mergesort failed"
      (is (list_equal (mergesort (list_reverse test_var)) test_var))
      ))

(deftest nats
	 (testing "theNats failed"
	 (is (list_equal (take 10 (theNats 0)) test_var))
	 ))

(deftest primes
	 (testing "Sieve failed"
	 (is (list_equal (take 20 (sieve (theNats 1))) '(2 3 5 7 9 11 13 15 17 19 21 23 25 27 29 31 33 35 37 39)))
	 ))

(deftest cubeSum
	 (testing "Cube Sum failed"
	 (is (list_equal (take 10 SumCubes) '(0 1 8 2 27 9 64 28 16 125)))
	 ))

(deftest nextPermutation
	 (testing "nextPerm failed"
	 (is (list_equal (flatten (take 1 (nextPerm '(5 4 3 1 2)))) '(5 4 3 2 1)))
	 ))

(deftest powerset
	 (testing "getsubsets failed"
	 (is (= (list_length (getsubsets '(1 2 3 4 5 6 7 8 9 10))) (fun_power 2 10)))
	 ))