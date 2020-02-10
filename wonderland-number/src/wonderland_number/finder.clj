(ns wonderland-number.finder)

(defn has-same-digits? [n1 n2]
  (let [s1 (set (str n1))
        s2 (set (str n2))]
    (= s1 s2)))

(defn multiple-has-same-digits? [n factor]
  (has-same-digits? n (* n factor)))

; first digit is always 1 (otherwise the number multiplied by 6 would have more than 6 digits
; other digits range from 0-9
(defn wonderland-number []
  (loop [factor 2 candidates (range 100000 199999)]
    (if (= 7 factor)
      (first candidates)
      (recur
        (inc factor)
        (filter #(multiple-has-same-digits? % factor) candidates)))))

(defn sum-cube-digits [n]
  (loop [x n sum 0]
    (if (= 0 x)
      sum
      (recur
        (quot x 10)
        (+ sum
           (reduce * (repeat 3 (rem x 10))))))))