; 3 Numerical integration

; 3.0 

; functions
; trapec
(require '[clojure.test :as test])

(defn line [x] x)
(defn sqr [x]
  (Thread/sleep 10)
  (* x x))

(def ^:const h 0.0213)

(defn finite-sum 
  ([f, n]
    (if (= 0 n)
      (f 0)
      (let [point (* n h)
            val (f point)]
            (println n point val)
      (+ val (finite-sum f (dec n))
    )))))


(defn integral-f2 [f]
  (fn integral [x]
    (let [n (int (quot x h))
          x_w (* n h)
          d_x (- x x_w)
          tail (/ (* (f x) d_x) 2)]
    (+ tail (* h (finite-sum f n))))))


(int (quot 2 h))
(finite-sum line 3)

(def ^:const eps 0.1)
(test/is (<= (- ((integral-f2 line) 2) 2) eps))
(test/is (<= (- ((integral-f2 sqr) 1) 0.33) eps))


(def memo-finite-sum  (memoize (fn   ([f, n]
  (if (= 0 n)
    (f 0)
    (let [point (* n h)
          val (f point)]
          ; (println n point val)
    (+ val (memo-finite-sum  f (dec n))
  )))))))

(memo-finite-sum  sqr 10)
(memo-finite-sum  sqr 15) ; должно быть 5 строчек если есть принты

; (time (foo sqr 50))
; (time (memo-foo sqr 50))
; (quot 1.01 h)

(int (quot 2 h))
(int (quot 2.512312 h))

(defn integral-memo [f]
  (fn integral [x]
    (let [n (int (quot x h))]
      (if (= (quot x h) (float n))
        (* h (memo-finite-sum  f n))
        (let [ x_w (* n h)
              d_x (- x x_w)
              tail (/ (* (f x) d_x) 2)] 
          (+ tail (* h (memo-finite-sum  f n)))))
(my-partition 2 (nth (my-lazy-partition 10 primes) 0))
  )))

(test/is (<= (- ((integral-memo line) 2) 2) eps))
(test/is (<= (- ((integral-memo sqr) 1) 0.33) eps))


(time (doall (let [f (integral-f2 sqr)] (map f (range 0 40 0.15)))))
(time (doall (let [f (integral-memo sqr)] (map f (range 0 40 0.15)))))

(let [f (integral-memo sqr)] 
  (time (f 1) )
  (time (f 1))
  (time (f 1.01213324242314))
  (time (f 2))
  (time (f 2.512312)))


; 3.2

(defn do-step
  [f, counter]
  (let [f_0 (nth counter 0)
        x1 (nth counter 1)
        x2 (+ x1 h)] 
    [(+ f_0  (* (* (+ (f x1) (f x2)) 0.5) h)), x2]))

(defn integral-infinity-series[f]
  (let [start (f 0)]
   (iterate (partial do-step f) [start, 0])))

(take 3 (integral-infinity-series sqr))
(first (nth (integral-infinity-series sqr) 5))
; (int (quot 0.1 h))


(defn integrate [f]
  (let [series (integral-infinity-series f)]
    (fn integral [x]
    (let [ n (int (quot x h))
        value (first (nth series n))]

      (if (= (quot x h) (float n))
          value
          (+ (* (* (f x) h) 0.5) value)))
)))

(test/is (<= (- (integrate line 2) 2) eps))
(test/is (<= (- (integrate sqr 1) 0.33) eps))


(let [f (integrate sqr)] 
  (time (f 1) )
  (time (f 1))
  (time (f 1.01213324242314))
  (time (f 2))
  (time (f 2.512312)))
