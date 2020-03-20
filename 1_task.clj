; 1.
(defn comb [letters, n] 
   (if (= n 1)
      letters
      (for [x letters y (comb letters (- n 1)) :when (not= x (str (first (seq y))))] (str x y))))
   
(println (comb '("a", "b", "c"), 3))


; 2.
(defn comb-tail  
   ([letters, n] (comb-tail letters n letters))
   ([letters, n, acc]
   (if (= n 1)
      acc
      (recur letters (dec n) (for [x letters y acc :when (not= x (str (first (seq y))))] (str x y))))))

   
(println (comb-tail '("a", "b", "c"), 3))


; 3.
(defn my-map  
   [f, coll] 
   (for [x coll] (f x)))

(println (my-map  (fn [x] (* x x)), (range 0 5)))
