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

(drop 4 primes)
(take 2 (my-lazy-partition 10 primes))

(defn lazy-parrallel-filter[pred, coll]
    (->> 
        (my-lazy-partition 40 coll)
        (take 1)
        (my-partition 10)
        (map #(future (doall (filter pred %))))
         ;важно
        (mapcat deref)))

(defn less-than-ten? [n]
    (Thread/sleep 100)
    (< n 1000))


(time (doall (take 20 (lazy-parrallel-filter less-than-ten? primes))))
(time (doall (take 20 (filter less-than-ten? primes))))
