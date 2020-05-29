; 1.
(defn append [ch, coll]
   (if (= 1 (count coll))
      (if (not= ch (str (first (first coll))))
         (list (str ch (first coll))))
      (concat 
            (append ch (list (first coll)))
            (append ch (rest coll)))
   )
)

(defn iter [col1 col2]
   (if (= 1 (count col1))
      (append (first col1) col2)
      (concat 
         (append (first col1) col2)
         (iter (rest col1) col2))))


(defn comb 
   ([letters, n] (comb letters letters n))
   ([letters, acc, n]
      (if (= 1 n)
         acc
         (comb letters (iter letters acc) (dec n)))
   )
)

(println (comb '("a", "b", "c"), 3))


; 2.
(defn comb-tail
   ([letters, n] (comb letters letters n))
   ([letters, acc, n]
      (if (= 1 n)
         acc
         (recur letters (iter letters acc) (dec n)))
   )
)

(println (comb-tail '("a", "b", "c"), 2))


; 3. my-map, my-filter
(defn my-map  
   [f, coll] 
   (if (= (count coll) 1)
         (list (f (first coll)))
         (concat
            (my-map f (list (first coll)))
            (my-map f (rest coll)))
   )
)

(println (my-map (fn [x] (* x x)), (range 0 5)))
(println (my-map (fn [x] (count x)), (comb-tail '("a", "b", "c"), 3)))


(defn my-filter 
   [predicate, coll] 
   (if (= (count coll) 1)
         (if (predicate (first coll))
            (list (first coll)))
         (concat
            (my-filter predicate, (list (first coll)))
            (my-filter predicate, (rest coll)))
   )
)

(println (my-filter even?, (range 0 10)))


; 4.
; map/reduce/filter
(defn comb4 [letters, n] 
   (if (= n 1)
      letters
      (for [x letters y (comb4 letters (- n 1)) :when (not= x (str (first (seq y))))] (str x y))))
   
(println (comb4 '("a", "b", "c"), 3))


(defn comb4-tail  
   ([letters, n] (comb4-tail letters n letters))
   ([letters, n, acc]
   (if (= n 1)
      acc
      (recur letters (dec n) (for [x letters y acc :when (not= x (str (first (seq y))))] (str x y))))))

   
(println (comb4-tail '("a", "b", "c"), 3))