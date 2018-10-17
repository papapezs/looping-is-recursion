(ns looping-is-recursion)

(defn power [n k]
  (let [helper (fn [acc n k]
                 (if (zero? k)
                   acc
                   (recur (* acc n) n (dec k))))]
    (helper 1 n k)))

(defn last-element [coll]
  (let [helper (fn [coll acc]
                 (if coll
                   (recur (next coll) (first coll))
                   acc))]
    (helper coll nil)))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (recur (rest a-seq) (rest b-seq))
    :else false))

(defn find-first-index [pred a-seq]
  (loop [pred pred
         a-seq a-seq
         index 0]
    (cond
      (empty? a-seq) nil
      (pred (first a-seq)) index
      :else (recur pred (rest a-seq) (inc index)))))

(defn avg [a-seq]
  (loop [sum (first a-seq)
         a-seq (rest a-seq)
         c 1]
    (cond
      (empty? a-seq) (/ sum c)
      :else (recur (+ sum (first a-seq)) (rest a-seq) (inc c)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (loop [c-odds #{}
         a-seq a-seq]
    (cond
      (empty? a-seq) c-odds
      :else (recur (toggle c-odds (first a-seq)) (rest a-seq)))))

(defn fast-fibo [n]
  (loop [fn-1 0
         fn-0 1
         count 2]
    (cond
      (< n 2) n
      (= n count) (+ fn-0 fn-1)
      :else (recur fn-0 (+ fn-0 fn-1) (inc count)))))

(defn cut-at-repetition [a-seq]
  (loop [uniq #{}
         a-seq a-seq
         b-seq []]
    (cond
      (empty? a-seq) b-seq
      (contains? uniq (first a-seq)) b-seq
      :else (recur (conj uniq (first a-seq)) (rest a-seq) (conj b-seq (first a-seq))))))

