; Disjunctive normal form
(require '[clojure.string :as str])
(require '[clojure.test :as test])


;порождение константы
(defn constant [value]
    (case value
        true  (cons ::const (list true))
        false (cons ::const (list false))
        1 (cons ::const (list true))
        0 (cons ::const (list false))))

;проверка типа для константы
(defn constant? [expr] (= (first expr) ::const))

;получение значения константы
(defn constant-value [expr] (second expr))

;отрицание значения константы
(defn invert-constant [expr] (constant (not (constant-value expr))))

; ========================================================
(test/is (constant? (constant 0)))
(test/is (false? (constant-value (constant 0))))
(test/is (=
        (constant 0)
        (invert-constant (constant 1))))
; ========================================================


;порождение переменной
(defn variable [name] 
    {:pre [(keyword? name)]}
    (list ::var name))

;проверка типа для переменной
(defn variable? [expr] (= (first expr) ::var))

;получение значения для переменной
(defn variable-name [v] (second v))

;сравнение переменных
(defn same-variables? [v1 v2] 
 (and
    (variable? v1)
    (variable? v2)
    (= (variable-name v1) (variable-name v2))))


; ========================================================
(test/is (variable? (variable :x)))
(test/is (= :x 
            (variable-name (variable :x))))

(test/is (same-variables?
            (variable :x)
            (variable :x)))

(test/is (not (same-variables?
                (variable :x)
                (variable :y))))

; ========================================================


;порождение суммы
(defn sum [expr & rest]
    (cons ::sum (cons expr rest)))


;проверка типа для суммы
(defn sum? [expr]
    (= ::sum (first expr)))

(test/is (sum? (sum (variable :x) (variable :y))))

;список аргументов выражения
(defn args [expr]
    (rest expr))


;порождение отрицания выражения 
(defn invert [expr]
    (list ::inv expr))

;проверка типа для отрицания выражения
(defn invert? [expr] (= ::inv (first expr)))

(test/is (invert? (invert (variable :x))))


;порождение произведения
(defn product [expr & rest] (cons ::prod (cons expr rest)))

;проверка типа для произведения
(defn product? [expr] (= ::prod (first expr)))

(test/is (product? (product (variable :x) (variable :y))))


;порождение обратного выражения
(defn implication [expr & rest] (cons ::impl (cons expr rest)))

;проверка типа для обратного выражения
(defn implication? [expr] (= ::impl (first expr)))

(test/is (implication? (implication (variable :x) (variable :y))))



; Алгоритм приведения формулы к ДНФ:
; 1. Выразить все логические операции, участвующие в построении формулы, через дизъюнкции, конъюнкции и отрицания, используя эквивалентности
; 2. Используя законы де Моргана, переносим все отрицания к переменным и сокращаем двойные отрицания по правилу
; 3. Используя закон дистрибутивности преобразуем формулу так, чтобы все конъюнкции выполнялись раньше дизъюнкций.

    
(defn apply-de-morgan [expr]
    (cond
        (or (constant? expr) (variable? expr)) expr
        (invert? expr)
         (let [op (first (args expr))]
            (cond 
                (constant? op) (invert-constant op)
                (variable? op) expr
                (sum? op) (product (apply-de-morgan (invert (first (args op)))) (apply-de-morgan (invert (second (args op)))))
                (product? op) (sum (apply-de-morgan (invert (first (args op)))) (apply-de-morgan (invert (second (args op)))))
                (invert? op) (apply-de-morgan (first (args op)))
                (implication? op) 
                    (let [arg1 (first (args op))
                          arg2 (second (args op))]
                        (apply-de-morgan (invert (sum (invert arg1) arg2))))
                :default (throw (AssertionError. "unsupported operation"))
                ))
        ; (product? expr) (product (apply-de-morgan (first (args expr))) (apply-de-morgan (second (args expr))))
        (product? expr) 
            (let [arg1 (first (args expr))
                  arg2 (second (args expr))]
                (cond 
                    (sum? arg1) (sum (apply-de-morgan (product (first (args arg1)) arg2)) (apply-de-morgan (product (second (args arg1)) arg2)))
                    (sum? arg2) (sum (apply-de-morgan (product (first (args arg2)) arg1)) (apply-de-morgan (product (second (args arg2)) arg1)))
                    :default (product (apply-de-morgan arg1) (apply-de-morgan arg2))))

        (sum? expr) (sum (apply-de-morgan (first (args expr))) (apply-de-morgan (second (args expr))))
        (implication? expr) 
            (let [arg1 (first (args expr))
                  arg2 (second (args expr))]
                (sum (apply-de-morgan (invert arg1)) (apply-de-morgan arg2)))
    ))

; de Morgan testing
; ===================================================
(test/is (= 
    (constant 1)
    (apply-de-morgan (constant 1))))

(test/is (= 
    (constant 0)
    (apply-de-morgan (invert (constant 1)))))   

(test/is (= 
            (variable :x) 
            (apply-de-morgan (variable :x))))

(test/is (= 
            (invert (variable :x))
            (apply-de-morgan (invert (variable :x)))))
    
(test/is (= 
            (product (invert (variable :x)) (invert (variable :y)))
            (apply-de-morgan (invert (sum (variable :x) (variable :y))))))

(test/is (= 
    (variable :x)
    (apply-de-morgan (invert (invert (variable :x))))))

(test/is (= 
    (sum (invert (variable :x)) (product (invert(variable :y)) (invert(variable :z))))
    (apply-de-morgan (invert (product (variable :x) (sum (variable :y) (variable :z)))))))

(test/is (=
        (sum (variable :y) (variable :z))
        (apply-de-morgan (sum (variable :y) (variable :z)))))

(test/is (=
        (sum (variable :y) (variable :z))
        (apply-de-morgan (sum (invert (invert (variable :y))) (variable :z)))))

(test/is (=
        (sum (product (variable :y) (variable :x)) (product (variable :z) (variable :x)))
        (apply-de-morgan (product (variable :x) (sum (variable :y) (variable :z))))))

;         (apply-de-morgan (invert (product (variable :x) (variable :y))))
;         (sum (apply-de-morgan (invert (product (variable :x) (variable :y)))) (sum (variable :x) (variable :y)))
; (apply-de-morgan (implication (product (variable :x) (variable :y)) (sum (variable :x) (variable :y))))
; ==================================================


(defn evaluate [expr, vocab] 
    (cond
         (constant? expr) (constant-value expr)
         (variable? expr) (get vocab (variable-name expr))
         (invert? expr) (not (evaluate (first (args expr)) vocab))
         (sum? expr) (or (evaluate (first (args expr)) vocab) (evaluate (second (args expr)) vocab))
         (product? expr) (and (evaluate (first (args expr)) vocab) (evaluate (second (args expr)) vocab))
    ))

(def values {:x true, :y false, :z true})

; ==================================================
(test/is (evaluate (variable :x) values))

(test/is (evaluate (product (variable :x) (sum (variable :y) (variable :z))) values))
; ==================================================


(defn to-dnf-and-evaluate [expr, vocab] 
    (evaluate (apply-de-morgan expr) vocab))

(test/is (to-dnf-and-evaluate (implication (product (variable :x) (variable :y)) (sum (variable :x) (variable :y))) values))



;
; 4.2 СДНФ и полином Жегалкина
;
(require '[clojure.math.combinatorics :as test])
(use 'clojure.math.combinatorics)
(cartesian-product [1 2 3] [4 5 6])
(combinations [:x, :y] 2)

;порождение суммы по модулю 2
(defn sum_p2 [expr & rest]
    (cons ::sum_p2 (cons expr rest)))
;проверка типа для суммы
(defn sum_p2? [expr]
    (= ::sum_p2 (first expr)))

(test/is (sum_p2? (sum_p2 (variable :x) (variable :y))))


(def all-values [{:x true, :y false}, {:x true, :y true}, {:x false, :y false},{:x false, :y true}])

(defn get-con [vocab]
    (->> 
    (for [var vocab]
        (let [v (first var)
              val (second var)] 
              (if val
                (variable v)
                (invert (variable v)))))
                (reduce product))
)

; (test/is (get-con {:x true, :y false})

(for [vocab all-values] 
    (to-dnf-and-evaluate (invert (sum (variable :x) (variable :y))) vocab))


(defn pretty_print [expr]
    (cond
        (constant? expr) (constant-value expr)
        (variable? expr) (variable-name expr)
    
        (invert? expr)
            (let [op (first (args expr))]
                (str "~" (pretty_print op)))

        (product? expr) 
            (let [arg1 (first (args expr))
                    arg2 (second (args expr))]
                (str "(" (pretty_print arg1) " * " (pretty_print arg2) ")"))

        (or 
            (sum? expr)
            (sum_p2? expr)
        )
            (let [arg1 (first (args expr))
                    arg2 (second (args expr))]
                    (str "(" (pretty_print arg1) " + " (pretty_print arg2) ")"))

        :default (throw (AssertionError. "unsupported operation" expr))
    ))

    
(pretty_print (invert (sum (variable :x) (variable :y))))

(defn to-SDNF [expr]
    (->> 
        (for [vocab all-values 
              :when (to-dnf-and-evaluate expr vocab)] 
             (get-con vocab))
             (reduce sum)))

(pretty_print (to-SDNF (implication (product (variable :x) (variable :y)) (sum (variable :x) (variable :y)))))
(pretty_print (to-SDNF (invert (sum (variable :x) (variable :y)))))

; Полином Жегалкина



(defn jegalkin_transform [expr]
    (cond
        (or (constant? expr) (variable? expr)) expr
    
        (invert? expr)
         (let [op (first (args expr))]
            (sum_p2 (jegalkin_transform op) (constant 1)))

        (product? expr) 
            (let [arg1 (first (args expr))
                  arg2 (second (args expr))]
                (product (jegalkin_transform arg1) (jegalkin_transform arg2)))

        (sum? expr)
            (let [arg1 (first (args expr))
                  arg2 (second (args expr))]
                    (sum_p2 (jegalkin_transform arg1) (jegalkin_transform arg2)))
        :default (throw (AssertionError. "unsupported operation"))
    ))

(defn get_Jegalkin_poly[expr]
    (->> 
        (to-SDNF expr)
        (jegalkin_transform)))

(pretty_print (get_Jegalkin_poly (invert (sum (variable :x) (variable :y)))))
(pretty_print (get_Jegalkin_poly (implication (product (variable :x) (variable :y)) (sum (variable :x) (variable :y)))))
(pretty_print (get_Jegalkin_poly (implication (variable :x) (variable :y))))