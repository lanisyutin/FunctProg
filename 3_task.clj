; 3 Numerical integration

; 3.0 

; functions
(defn line [x] x)
(defn sqr [x] (* x x))

(def ^:const h 0.001)

(defn integral-f [f]
  (fn integral [x]
    (reduce + 
      (map (fn [a] (* a h)) 
        (map f 
          (for [dx (range 0 (- x h) h)] 
            (+ dx (/ h 2))))))))



 ((integral-f line) 2) ; => exact 2
 ((integral-f sqr) 1)  ; => exact 0.33


; memoize it 

(def integral-memo 
  (defn integral-f [f]
    (memoize (fn integral [x]
      (reduce + 
        (map (fn [a] (* a h)) 
          (map f 
            (for [dx (range 0 (- x h) h)] 
              (+ dx (/ h 2))))))))))

(def f-memo (memoize line))

(time (map (integral-f line) (take 20 (repeat 1000))))
(time (map (integral-memo line) (take 20 (repeat 1000))))


(time ((integral-f line) 2))
(time ((integral-f line) 100))
(time ((integral-memo line) 2))
(time ((integral-memo line) 100))