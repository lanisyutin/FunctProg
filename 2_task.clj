; 2.1 Eratosphen

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
(era-prime-n 100)


; 2.2 Infinity prime

; (def inf-primes
;     (map era-prime-n (iterate inc 0)))

; (nth inf-primes 1)
; (take 5 inf-primes)

(declare primes)

(defn prime?
  [n]
  (if (= n 1) 
    false
    (every? false? (for [prime primes :while (< prime n)] (= (mod n prime) 0)))))

(def primes (lazy-seq
              (cons 2 (filter prime? (iterate inc 3)))))

(take 1 primes)
(take 2 primes)
(take 100 primes)