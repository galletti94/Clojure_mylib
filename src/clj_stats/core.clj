(ns clj_stats.core
    (:require [clojure.java.io :as io]))


;; functions on integers ;;

(defn fact [n]
      (letfn [(helper [n res]
      	     (if (<= n 1) res (helper (- n 1) (* res n)))
	     )]
	     (helper n 1)
	     ))

(defn fib [n]
      (letfn [(helper [n nex curr]
      	     (if (= n 0) curr (helper (- n 1) (+ curr nex) nex)))]
	     (helper n 1 0)
	     ))

(defn acker [x y]
      (if (= x 0) (+ y 1)
      	  (if (= y 0) (acker (- x 1) 1)
	      (acker (- x 1) (acker x (- y 1)))
	      )))



;; functions on lists ;;

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

(defn cube [x]
      (fun_power x 3)
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

(defn list_compare [xs ys f]
      (if (and (empty? xs) (empty? ys)) true
      	  (if (not (f (first xs) (first ys))) false (list_compare (rest xs) (rest ys) f)))
	  )

(defn list_list_equal [xss yss]
     (list_compare xss yss list_equal)
     )

(defn list_foldleft2 [res xs ys f]
      (assert (= (list_length xs) (list_length ys)) "lists should be of same length")
      (if (and (empty? xs) (empty? ys)) res (list_foldleft2 (f res (first xs) (first ys)) (rest xs) (rest ys) f))
      )

(defn list_sse [xs ys]
      (list_foldleft2 0 xs ys (fn [res x y] (+ res (square (- x y)))))
      )



;; streams and lazy evaluation ;;


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

(defn theNats [init]
      (lazy-seq (cons (+ init 1) (theNats (+ init 1))))
      )

(defn sieve [nats]
      (lazy-seq (cons (first nats) (filter (fn [x] (> (mod x (first nats)) 0)) (rest nats)))
      ))

;; enumerate all pairs of Nats ordered by their sum

(defn diagonal [n]
      (letfn [(helper [i]
      	     (if (<= i n) (lazy-seq (cons (list i (- n i)) (helper (+ i 1))))
	     	 '()))]
		 (helper 0))
		 )

(defn diagonals [n]
      (lazy-seq (concat (diagonal n) (diagonals (+ n 1))))
      )

(def theNatsPairs (lazy-seq (concat (diagonals 0))))

;; enumerate all triples of Nats ordered by sum

(defn triangular [n]
      (letfn [(helper [i j]
      	     (if (< (+ i j) n) (lazy-seq (cons (list i j (- n (+ i j))) (helper (+ i 1) j)))
	     (if (<= j n) (lazy-seq (cons (list i j (- n (+ i j))) (helper 0 (+ 1 j))))
	     '())))]
	     (helper 0 0)
	     ))

(defn triangulars [n]
      (lazy-seq (concat (triangular n) (triangulars (+ n 1))))
      )


(def theNatsTriples (lazy-seq (concat (triangulars 0))))

(def theNatsPairsRmDup (filter (fn [x] (<= (first x) (second x))) theNatsPairs))

(def SumCubes (map (fn [x] (+ (cube (first x)) (cube (second x)))) theNatsPairsRmDup))




;; Powerset function ;;

(defn combine [xs ys res]
    (if (empty? ys) res (concat (combine (cons (first ys) xs) (rest ys) '()) (combine xs (rest ys) (cons (cons (first ys) xs) res)) )))

(defn getsubsets [xs]
    (cons '() (combine '() xs '())))




;; Permutations function ;;


(defn swap [xs i j]
    (concat 
          (concat 
                (concat 
                      (concat (take (- i 1) xs) (take 1 (drop (- j 1) xs))) 
                      (take (- (- j i) 1) (drop i xs)))
                      (take 1 (drop (- i 1) xs)))
                      (drop j xs))
)

(defn permutate [xs p1 p2]
      (if (> (nth xs (- p1 1)) (nth xs p2)) (permutate xs p1 (- p2 1))
      (let [xss (swap xs p1 (+ p2 1))] (concat (take p1 xss) (list_reverse (drop p1 xss))))
      ))

(defn findPivot [xs]
      (letfn [(helper [xs i] (if (= i 0) i (if (> (nth xs i) (nth xs (- i 1))) i (helper xs (- i 1)))))]
             (helper xs (- (list_length xs) 1)))
)
  
(defn nextPerm [xs] (let [pivot (findPivot xs)]
                         (lazy-seq (cons (permutate xs pivot (- (list_length xs) 1)) (nextPerm (permutate xs pivot (- (list_length xs) 1)))))))

(def Perms (take 119 (nextPerm '(1 2 3 4 5))))





;; merge sort ;;

(defn msort [xs ys]
      (if (empty? ys) xs (if (empty? xs) ys
      (if (<= (first xs) (first ys))
      	  (cons (first xs) (msort (rest xs) ys))
      	  (cons (first ys) (msort xs (rest ys)))
      	  ))))

(defn split [xs]
      (let [n (list_length xs)] (if (> n 2) (msort (split (take (quot n 2) xs)) (split (drop (quot n 2) xs)))
      (msort (take 1 xs) (drop 1 xs)))))

(defn mergesort [xs]
      (split xs))