(ns alphabet-cipher.coder)

(defn encode-letter [pair]
  (let [x (first pair)
        y (second pair)]
    (char (+ (mod (- (+ (int x) (- (int y) (int \a))) (int \a)) 26) (int \a)))))

(defn decode-letter [pair]
  (let [x (first pair)
        y (second pair)]
    (char (+ (mod (- (- (int y) (- (int x) (int \a))) (int \a)) 26) (int \a)))))

; encoding is like taking the letter and rotating right by the index of the encoding letter
(defn encode [keyword message]
  (apply str (map encode-letter (partition 2 (interleave (cycle keyword) message)))))

; decoding is like taking the encoded letter and rotating left by the index of the encoding letter
(defn decode [keyword message]
  (apply str (map decode-letter (partition 2 (interleave (cycle keyword) message)))))

(defn extract-keyword [s]
  (let [v (vec s)]
    (loop [out [(first v)]]
      (if (= (take (count v) (cycle out)) v)
        out
        (recur (conj out (nth v (count out))))))))

; deciphering is like taking the encoded letter and rotating left by the index of the original letter
; then we need to identify where the word repeats
(defn decipher [cipher message]
  (apply str (extract-keyword (map decode-letter (partition 2 (interleave message cipher))))))

