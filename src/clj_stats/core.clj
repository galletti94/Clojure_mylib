(ns clj_stats.core
    (:require [clojure.java.io :as io]))

(defn list_reverse [xs]
      (letfn [(helper [xs res] (if (empty? xs) res (helper (rest xs) (conj res (first xs)))))]
      	     (helper xs nil)
	     ))

(defn list_foldleft [res xs f]
      (if (empty? xs) res (list_foldleft (f res (first xs)) (rest xs) f)
      ))

(defn lazy_repeat [x f]
      (lazy-seq (cons x (lazy_repeat (f x) f)))
      )

(defn fun_power [x n]
      (list_foldleft 1 (take n (lazy_repeat x #(* % 1))) *)
      )

(defn square [x]
      (fun_power x 2)
      )

(defn list_foldright [res xs f]
      (list_foldleft 0 (list_reverse xs) f)
      )

(defn list_sum [xs]
      (assert (not (empty? xs)) "list is empty")
      (list_foldleft 0 xs +)
      )

(defn list_length [xs]
     (letfn [(helper [xs res] (if (empty? xs) res (helper (rest xs) (+ res 1))))]
     (helper xs 0)
     ))

(defn list_mean [xs]
      (assert (not (empty? xs)) "list is empty")
      (/ (list_foldleft 0 xs +) (list_length xs))
      )

(defn list_variance [xs]
      (assert (not (empty? xs)) "list is empty")
      (letfn [(sum_squares [xs mean] (list_foldleft 0 xs (fn [x y] (+ x (square (- y mean))))))]
      (/ (sum_squares xs (list_mean xs)) (list_length xs))
      ))

(defn list_equal [xs ys]
      (assert (= (list_length xs) (list_length ys)))
      (if (and (empty? xs) (empty? ys)) true
      (if (not (= (first xs) (first ys))) false (list_equal (rest xs) (rest ys)))
      ))


(defn list_foldleft2 [res xs ys f]
      (assert (= (list_length xs) (list_length ys)) "lists should be of same length")
      (if (and (empty? xs) (empty? ys)) res (list_foldleft2 (f res (first xs) (first ys)) (rest xs) (rest ys) f))
      )

(defn list_sse [xs ys]
      (list_foldleft2 0 xs ys (fn [res x y] (+ res (square (- x y)))))
      )

(defn cube [x]
      (fun_power x 3)
      )

(defn fact [n]
      (letfn [(helper [n res]
      	     (if (<= n 1) res (helper (- n 1) (* res n)))
	     )]
	     (helper n 1)
	     ))

(defn streamize [xs]
      (lazy-seq (cons (first xs) (streamize (rest xs))))
      )

(defn lazy_leftroll [res xs f]
      (lazy-seq (cons (f res (first xs)) (lazy_leftroll (f res (first xs)) (rest xs) f)))
      )

(defn lazy_leftroll2 [res xs ys f]
      (lazy-seq (cons (f res (first xs) (first ys)) (lazy_leftroll2 (f res (first xs) (first ys)) (rest xs) (rest ys) f)))
      )

(defn lazy_map2 [xs ys f]
      (lazy-seq (cons (f (first xs) (first ys)) (lazy_map2 (rest xs) (rest ys) f)))
      )

(defn lazy_rolling_sum [xs]
      (lazy_leftroll 0 xs +)
      )

(defn lazy_rolling_length [xs]
      (letfn [(helper [xs res] (lazy-seq (cons (+ res 1) (helper (rest xs) (+ res 1)))))]
      (helper xs 0))
      )

(defn lazy_rolling_mean [xs]
      (lazy_map2 (lazy_rolling_sum xs) (lazy_rolling_length xs) /)
      )

(defn lazy_merge [xs ys]
      (if (empty? xs) (if (empty? ys) nil ys) (if (empty? ys) xs
      (if (< (first xs) (first ys))
      	  (lazy-seq (cons (first xs) (lazy_merge (rest xs) ys)))
      	  (lazy-seq (cons (first ys) (lazy_merge xs (rest ys))))
      	  ))))

(defn lazy_split [xs]
      (if (empty? xs) nil (if (empty? (rest xs)) (list (list (first xs))) (lazy-seq (cons (list (first xs) (second xs)) (lazy_split (rest (rest xs)))))))
      )


