; Parralel filter
; 5.1

(defn heavy-even?[e]
    (Thread/sleep 100)
    (even? e))

(defn my-partition [n, coll]
    (if (>= n (count coll))
        (list (take n coll))
    (concat 
        (list (take n coll))
        (my-partition n (drop n coll)))))

(defn parrallel-filter[pred, coll]
(time
        (->> 
        (my-partition 30 coll)
        (map #(future (doall (filter pred %))))
        (doall) ;важно
        (mapcat deref)
        (doall)
        )))
    
(parrallel-filter heavy-even? (range 60))
(time (doall (filter heavy-even? (range 60))))


; 5.2

(declare primes)

(defn prime?
  [n]
  (if (= n 1) 
    false
    (every? false? (for [prime primes :while (< prime n)] (= (mod n prime) 0)))))

(def primes (lazy-seq
              (cons 2 (filter prime? (iterate inc 3)))))

(defn my-lazy-partition [n, coll]
    (lazy-cat 
        (list (take n coll))
        (my-lazy-partition n (drop n coll))))

(my-partition 2 (apply concat (take 1 (my-lazy-partition 10 primes))))

(defn lazy-parrallel-filter[pred, coll]
    (->> 
        (my-lazy-partition 120 coll)
        ; (take 1)
        ; (apply concat)
        (mapcat (fn [x] (->>
                (my-partition 10 x)
                (map #(future (doall (filter pred %))))
                (doall)
                (mapcat deref)))
        
        )))
        

(defn heavy-odd? [n]
    (Thread/sleep 2)
    (odd? n))


(time (doall (take 1200 (lazy-parrallel-filter heavy-odd? primes))))
(time (doall (take 1200 (filter heavy-odd? primes))))

(time (doall (nth (lazy-parrallel-filter heavy-even? primes) 1000)) )
(time (doall (nth (filter heavy-even? primes)) 1000))
