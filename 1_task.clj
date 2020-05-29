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
(defn filter-collection [letter, coll]
   (filter 
      (fn [x] (not= (str (last x)), letter)),
          coll))

(defn concat_elem_to_each_other [elem input_list]
   (map (fn [x] (.concat x elem)) (filter-collection elem input_list)))
 
(defn product_iter [input_list acc elem]
      (concat acc (concat_elem_to_each_other elem input_list)))
 
(defn cartesian_product [first_list, second_list]
   (reduce (partial product_iter first_list) (list) second_list))
 
(defn comb4
   [input_list, power]
   (reduce (fn [accum _] (cartesian_product accum input_list)) input_list (range (dec power))))
 

(println (comb4 `("a" "b" "c") 5))
 


(filter-collection "b"  `("ba" "ab" "c"))
(concat_elem_to_each_other "b"  `("ba" "ab" "c"))
(product_iter `("a" "b" "c") `("a" "b" "c") "b")
(cartesian_product `("a" "b" "c") `("a" "b" "c"))

   