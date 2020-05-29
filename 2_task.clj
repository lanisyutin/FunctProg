; 2.1 Eratosphen

; (defn mod-p? [a, p]
;     (= 0 (mod a p)))

; (defn filter-col [col p]
;     (filter (fn [x] (not (mod-p? x p))) col))

(defn era-prime-n [n]
     (nth (reduce
        (fn [primes number]
          (if (some zero? (map (partial mod number) primes))
            primes
            (conj primes number)))
        [2]
        (take (* n n) (iterate inc 3))), n))

(era-prime-n 0)
(era-prime-n 1)
(era-prime-n 2)


; 2.2 Infinity prime

(def inf-primes
    (map era-prime-n (iterate inc 0)))

(nth inf-primes 1)
(take 5 inf-primes)
