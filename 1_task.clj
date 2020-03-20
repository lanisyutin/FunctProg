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

(println (comb '("a", "b", "c"), 4))


; 2.
(defn comb-tail
   ([letters, n] (comb letters letters n))
   ([letters, acc, n]
      (if (= 1 n)
         acc
         (recur letters (iter letters acc) (dec n)))
   )
)

(println (comb-tail '("a", "b", "c"), 4))


; 3.
(defn my-map  
   [f, coll] 
   (if (= (count coll) 1)
         (list (f coll))
         (concat
            (my-map f (first coll))
            (my-map f (rest coll)))
      )
)

(println (my-map  (fn [x] (* x x)), (range 0 5)))
