
(defn prime-n [n]
    (if (= n 1)
        2
    ()

))


(defn prime-n [n]
    (nth (for [nums (iterate inc 2)
          :when (prime? nums)] nums) n))

(defn prime? 
    ([2] true)
    ([a] (every? false? 
        (for [n (iterate inc 0)
            :let [p (prime-n n)]
            :while (< p a)]
                (mod-p? a p)))))        

(defn mod-p? [a, p]
    (= 0 (mod a p)))

(println (prime-n 5))




(def primes
    (lazy-seq
        (for [nums (iterate inc 2)
                :when (prime? nums)] nums)))




(println (prime? 11))



(def fibs
    (lazy-seq ()))

(let [fibs ()]
(nth fibs 10))


primes = [2..] \ [[p², p²+p..] for p in primes]

(defn mod-p? [a, p]
    (= 0 (mod a p)))
    


(println (mod-p? 3 2))



(println (for [nums (range 2 100)
:let [p (range 2 (dec nums))]
:when (not= 0 (mod nums p))] nums))