(ns clj_stats.core)

(defn list_reverse [xs]
      (letfn [(helper [xs res] (if (empty? xs) res (helper (rest xs) (conj res (first xs)))))]
      	     (helper xs nil)
	     ))

(defn list_foldleft [res xs f]
      (if (empty? xs) res (list_foldleft (f res (first xs)) (rest xs) f)
      ))


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

(defn square [x]
      (* x x))


(defn list_variance [xs]
      (assert (not (empty? xs)) "list is empty")
      (letfn [(sum_squares [xs mean] (list_foldleft 0 xs (fn [x y] (+ x (square (- y mean))))))]
      (/ (sum_squares xs (list_mean xs)) (list_length xs))
      ))