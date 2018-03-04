(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (if (empty? (rest coll))
      true
      false)))

(defn my-last [coll]
  (if (singleton? coll)
    (first coll)
    (if (empty? coll)
      nil
      (my-last (rest coll)))))


(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))


(defn seq-max [seq-1 seq-2]
  (let [l1 (count seq-1)
        l2 (count seq-2)]
   (if (> l1 l2)
     seq-1
     seq-2)))


(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (seq-max (first a-seq)
               (longest-sequence (rest a-seq))))))


(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= elem (first a-seq))
      true
      (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq)
   '()
   (pred? (first a-seq))
   (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else
   '()))

(my-take-while odd? [1 3 4 5 7 2 9 11])

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq)
   '()
   (pred? (first a-seq))
          (my-drop-while pred? (rest a-seq))
   :else
    a-seq))

(defn seq= [a-seq b-seq]
  (let [ea (empty? a-seq)
        eb (empty? b-seq)]
  (cond
    (and ea eb) true
    (or ea eb) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else
   false)))

(defn my-map [f seq-1 seq-2]
  (let [e1 (empty? seq-1)
        e2 (empty? seq-2)
        f1 (first seq-1)
        f2 (first seq-2)]
    (cond
     (or e1 e2) '()
     :else
     (cons (f f1 f2) (my-map f (rest seq-1) (rest seq-2))))))

(defn power [n k]
  (cond
   (= k 0) 1
   (= k 1) n
  :else
    (* n (power n (- k 1)))))

(defn fib [n]
  (cond (= n 0) 0
        (= n 1) 1
        :else (+ (fib (- n 2)) (fib (- n 1)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond (<= how-many-times 0) '()
        :else
        (cons what-to-repeat
              (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (cond (<= up-to 0) '()
        :else
        (cons (- up-to 1) (my-range (- up-to 1)))))

(my-range 12)

(defn tails [a-seq]
  (cond (empty? a-seq) (list a-seq)
        :else (cons a-seq (tails (rest a-seq)))))

(tails [5 6 7])

(defn inits [a-seq]
  (if (empty? a-seq)
    (list a-seq)
    (map reverse (tails (reverse a-seq)))))

(inits [5 6 9 7])

(defn rotations [a-seq]
  (if (empty? a-seq) (cons () '())
    (rest (map concat (reverse (tails a-seq)) (inits a-seq)))))


(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq) freqs
    (let [item (first a-seq)
          old-count (if (contains? freqs item) (freqs item) 0)
          new-freqs (assoc freqs item (inc old-count)) ]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(def mymap {:a 1 :b 2})

(first mymap)

(contains? mymap :a)

(def data[1 2 5 4 4 4 3 1 7 4 8 2 5 4 3 2 3 3 4])

(defn un-frequencies [a-map]
  (if (empty? a-map) '()
    (let [what (first (first a-map))
          cnt  (second (first a-map))]
      (concat (my-repeat cnt what) (un-frequencies (rest a-map))))))

(def histogram (my-frequencies data))

histogram

(un-frequencies histogram)

(defn my-take [n coll]
  (let [cnt (count coll)]
    (cond
     (= n 0) nil
     (empty? coll) nil
      :else (cons (first coll) (my-take (- n 1) (rest coll))))))

(my-take 45 data)

(defn my-drop [n coll]
  (let [cnt (count coll)]
    (cond
     (= n 0) coll
     (< cnt n) '()
       :else (my-drop (- n 1) (rest coll)))))

(my-drop 2 data)

(vector 1 2)

(defn halve [a-seq]
  (let [ split (int (/ (count a-seq) 2)) ]
    (seq (vector (take split a-seq)
                 (drop split a-seq)))))
(halve [])

(defn seq-merge [a-seq b-seq]
  (let [cnt-a (count a-seq)
        cnt-b (count b-seq)
        fa (first a-seq)
        fb (first b-seq)]
        (cond
         (and (= cnt-a 0) (= cnt-b 0)) nil
         (= cnt-b 0) a-seq
         (= cnt-a 0) b-seq
         :else (cond
                (<= fa fb) (cons fa (seq-merge (rest a-seq) b-seq))
                :else (cons fb (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (cond
   (empty? a-seq) a-seq
   (singleton? a-seq) a-seq
   :else (let [parts (halve a-seq)]
      (seq-merge (merge-sort (first parts))
                 (merge-sort (second parts))))))

(def ts [])
(merge-sort ts)
(frequencies data)
(merge-sort data)

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

