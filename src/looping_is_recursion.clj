(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (if (zero? n)
                   acc
                   (recur (* acc base) (dec n))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
  (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
   (and (empty? seq1) (empty? seq2)) true
       (or (empty? seq1) (empty? seq2)
        (not= (first seq1) (first seq2))) false
   :else
   (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [acc 0
         n a-seq]
    (cond
     (empty? n) nil
     (pred (first n))
    acc
     :else
    (recur (inc acc) (rest n)))))

(defn avg [a-seq]
  (if (empty? a-seq) nil
  (/ (apply + a-seq) (count a-seq))))

(defn parity [a-seq]
  (loop [seq a-seq acc #{}]
    (cond
      (empty? seq) acc
      (contains? acc (first seq)) (recur (rest seq) (disj acc (first seq)))
      :else
(recur (rest seq) (conj acc (first seq))))))

(defn fast-fibo [n]
  (loop [n n fibo1 0 fibo2 1]
    (if (< n 1) fibo1 (recur (dec n) fibo2 (+ fibo1 fibo2)))))

(defn cut-at-repetition [a-seq]
  (loop [only-single-obj [] b-seq a-seq]
    (cond
     (empty? b-seq) only-single-obj
     (contains? (set only-single-obj) (first b-seq)) only-single-obj
     :else
     (recur (conj only-single-obj (first b-seq)) (rest b-seq)))))
