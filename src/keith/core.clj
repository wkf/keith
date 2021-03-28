(ns keith.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io])
  (:gen-class))

(def short-sample "helloiamacorpusthankyousomuch!")
(def moby-dick (slurp (io/resource "moby-dick.txt")))
(def metamorphasis (slurp (io/resource "metamorphasis.txt")))

(def columnar-5x3x1
  {:hands
   [1 1 1 1 1   2 2 2 2 2
    1 1 1 1 1   2 2 2 2 2
    1 1 1 1 1   2 2 2 2 2
    1           2]
   :fingers
   [4 3 2 1 1   1 1 2 3 4
    4 3 2 1 1   1 1 2 3 4
    4 3 2 1 1   1 1 2 3 4
    0           0]
   :rows
   [1 1 1 1 1   1 1 1 1 1
    2 2 2 2 2   2 2 2 2 2
    3 3 3 3 3   3 3 3 3 3
    0           0]
   :columns
   [1 2 3 4 5   5 4 3 2 1
    1 2 3 4 5   5 4 3 2 1
    1 2 3 4 5   5 4 3 2 1
    0           0]
   :costs
   [3.0 1.5 1.0 1.5 3.0    3.0 1.5 1.0 1.5 3.0
    0.5 0.5 0.0 0.0 2.0    2.0 0.0 0.0 0.5 0.5
    2.0 2.0 1.5 0.5 2.5    2.5 0.5 1.5 2.0 2.0
    0.0                    0.0]})

(def dvorakish
  [\' \, \. \/ \y    \f \g \c \r \z
   \a \o \p \u \i    \d \m \t \n \s
   \; \q \w \v \x    \b \h \k \j \l
   \space            \e])

(def lock-thumbs-and-hkjl
  [1 1 1 1 1   1 1 1 1 1
   1 1 1 1 1   1 1 1 1 1
   1 1 1 1 1   1 0 0 0 0
   0           0])

(def standard-params
  {:t0 1.5 :p0 1.0 :k 10.0 :n 15000})

(def map-layout
  (memoize
    (fn [layout {:keys [hands fingers rows columns costs]}]
      (dissoc
        (into {}
          (map-indexed
            (fn [i c]
              [c
               {:index i
                :char c
                :cost (get costs i)
                :hand (get hands i)
                :finger (get fingers i)
                :row (get rows i)
                :column (get columns i)}])
            layout))
        nil))))

(def index-swaps
  (memoize
    (fn [m]
      (->> m
        (map-indexed
          (fn [i x] (and (pos? x) i)))
        (filter identity)
        vec))))

(defn swap-layout [layout swaps]
  (let [s (index-swaps swaps)
        n (count s)
        i (get s (rand-int n))
        j (get s (rand-int n))]
    (if (= i j)
      (swap-layout layout swaps)
      (-> layout
        (assoc i (nth layout j))
        (assoc j (nth layout i))))))

(defn shuffle-layout [layout swaps]
  (->> swaps index-swaps shuffle
    (map-indexed
      (fn [i j] [i j]))
    (reduce
      (fn [l [i j]]
        (assoc l i (nth layout j)))
      layout)))

(defn print-layout [layout]
  (apply printf
    "
%s %s %s %s %s   %s %s %s %s %s
%s %s %s %s %s   %s %s %s %s %s
%s %s %s %s %s   %s %s %s %s %s
        %s   %s
"
    layout)
  (flush))

(defn positional-penalty [[a]] (:cost a))

(defn abs [x] (Math/abs x))

(defn index? [a] (= (:finger a) 1))
(defn middle? [a] (= (:finger a) 2))
(defn ring? [a] (= (:finger a) 3))
(defn pinky? [a] (= (:finger a) 4))
(defn thumb? [a] (zero? (:finger a)))
(defn left? [a] (= (:hand a) 1))
(defn right? [a] (= (:hand a) 2))
(defn top? [a] (= (:row a) 1))
(defn home? [a] (= (:row a) 2))
(defn bottom? [a] (= (:row a) 3))
(defn center? [a] (= (:column a) 5))

(defn same-key? [& fs] (apply = (map :index fs)))
(defn same-finger? [& fs] (apply = (map :finger fs)))
(defn same-hand? [& fs] (apply = (map :hand fs)))

(defn consecutive-fingers? [a b]
  (let [fa (:finger a)
        fb (:finger b)]
    (and (same-hand? a b) (pos? fa) (pos? fb) (= (abs (- fb fa)) 1))))

(defn long-jump? [a b]
  (let [ra (:row a)
        rb (:row b)]
    (and (pos? ra) (pos? rb) (> (abs (- rb ra)) 1))))

(defn roll-out? [a b]
  (and a b (same-hand? a b) (not (thumb? b)) (< (:finger b) (:finger a))))

(defn roll-in? [a b]
  (and a b (same-hand? a b) (< (:finger a) (:finger b))))

(defn same-finger-penalty [[a b & _]]
  (when (and a b (same-hand? a b) (same-finger? a b) (not (same-key? a b))) 8.0))

(defn long-jump-penalty [[a b & _]]
  (when (and a b (long-jump? a b))
    (+ 1.0
      (if (same-hand? a b)
        (cond
          (thumb? a) 0.0
          (thumb? b) 0.0
          (same-finger? a b) 10.0
          (and
            (index? a)
            (bottom? a)
            (not (center? a))
            (or (middle? b) (ring? b))) 0.0
          (and (index? a) (ring? b)) 5.0
          (and (ring? a) (index? b)) 5.0
          (consecutive-fingers? a b) 5.0
          :else 0.0)
        0.0))))

(defn pinky-ring-twist-penalty [[a b & _]]
  (when (and a b
          (same-hand? a b)
          (or
            (and
              (ring? a)
              (pinky? b)
              (or (home? a) (bottom? a))
              (top? b))
            (and
              (pinky? a)
              (ring? b)
              (top? a)
              (or (home? b) (bottom? b)))))
    10.0))

(defn roll-out-penalty [[a b & _]]
  (when (roll-out? a b) 0.125))

(defn roll-in-penalty [[a b & _]]
  (when (roll-in? a b) -0.25))

(defn roll-reversal-penalty [[a b c _]]
  (when (and a b (same-hand? a b c)
          (or
            (and (middle? a) (pinky? b) (ring? c))
            (and (ring? a) (pinky? b) (middle? c))))
    20.0))

(defn long-twist-penalty [[a b c _]]
  (when (and a b c
          (or
            (and (top? a) (home? b) (bottom? c))
            (and (bottom? a) (home? b) (top? c)))
          (or
            (and (roll-out? a b) (roll-out? b c))
            (and (roll-in? a b) (roll-in? b c))))
    10.0))

(defn long-jump-sandwich-penalty [[a _ c _]]
  (when (and a c (same-hand? a c) (same-finger? a c) (long-jump? a c))
    3.0))

(defn same-hand-penalty [[a b c d]]
  (when (same-hand? a b c d) 0.5))

(defn alternate-hand-penalty [[a b c d]]
  (when (and
          (not (same-hand? a b))
          (not (same-hand? b c))
          (not (same-hand? c d)))
    0.5))

(comment
  (same-finger?
    {:finger 0 :hand 1 :row 3}
    {:finger 1 :hand 1 :row 3}
    {:finger 1 :hand 1 :row 0})
  (roll-out?
    {:finger 1 :hand 1 :row 3}
    {:finger 0 :hand 1 :row 0})
  (long-jump?
    {:finger 1 :hand 1 :row 3}
    {:finger 0 :hand 1 :row 0})
  (long-jump-penalty
    [{:finger 1 :hand 1 :row 3}
     {:finger 0 :hand 1 :row 0}])
  (consecutive-fingers?
    {:finger 1 :hand 1}
    {:finger 0 :hand 1}))

(def penalties
  {1 [positional-penalty]
   2 [same-finger-penalty
      long-jump-penalty
      pinky-ring-twist-penalty
      roll-out-penalty
      roll-in-penalty]
   3 [roll-reversal-penalty
      long-twist-penalty
      long-jump-sandwich-penalty]
   4 [same-hand-penalty
      alternate-hand-penalty]})

(defn penalize-ngram [ngram freq mapping]
  (let [key (map #(get mapping %) ngram)]
    (->>
      (get penalties (count ngram))
      (map #(* (or (% key) 0.0) freq))
      (reduce +))))

(defn ensure-ngram-size [ngrams n]
  (->> ngrams (map #(str/join (take n %))) (filter (comp #{n} count))))

(def extract-ngrams
  (memoize
    (fn [corpus]
      (let [c (str/lower-case corpus)
            n 4
            quadgrams (map reverse
                        (concat
                          (for [i (range 1 (min (inc (count c)) n))]
                            (take i c))
                          (partition n 1 c)))]
        (frequencies
          (concat
            (ensure-ngram-size quadgrams 1)
            (ensure-ngram-size quadgrams 2)
            (ensure-ngram-size quadgrams 3)
            (ensure-ngram-size quadgrams 4)))))))

(def evaluate-layout
  (memoize
    (fn [layout keyboard corpus]
      (let [mapping (map-layout layout keyboard)
            total (->> corpus extract-ngrams
                    (pmap
                      (fn [[ngram freq]]
                        (penalize-ngram ngram freq mapping)))
                    (reduce +))]
        (/ total (count corpus))))))

(defn simulate
  ([energy-fn change-fn report-fn initial-state {:keys [n] :as params}]
   (loop [i 0
          current-state initial-state
          optimal-state initial-state
          optimal-energy (energy-fn initial-state)]
     (let [[current-state current-energy]
           (simulate energy-fn change-fn report-fn current-state i params)
           [optimal-state optimal-energy]
           (if (< current-energy optimal-energy)
             [current-state current-energy]
             [optimal-state optimal-energy])]
       (report-fn current-state current-energy optimal-state optimal-energy n i)
       (if (< i n)
         (recur (inc i) current-state optimal-state optimal-energy)
         optimal-state))))
  ([energy-fn change-fn report-fn current-state i {:keys [t0 p0 k n]}]
   (let [current-energy (energy-fn current-state)
         changed-state (change-fn current-state)
         changed-energy (energy-fn changed-state)
         de (- changed-energy current-energy)]
     (if (neg? de)
       [changed-state changed-energy]
       (let [t (* (Math/exp (- (/ (* i k) n))) t0)
             p (* (Math/exp (- (/ de t))) p0)]
         (if (< (rand) p)
           [changed-state changed-energy]
           [current-state current-energy]))))))

(defn optimize-layout [layout keyboard swaps corpus params]
  (simulate
    #(evaluate-layout % keyboard corpus)
    (comp
      #(swap-layout % swaps)
      #(swap-layout % swaps))
    (fn [_ _ layout cost n i]
      (when (or (zero? (mod i (quot n 10))) (= i n))
        (printf "simulation iteration: %d/%d\n" i n)
        (printf "cost per character: %f\n" cost)
        (print-layout layout)))
    (shuffle-layout layout swaps)
    params))

(defn extract-chars-by [f mapping]
  (into {}
    (->> mapping vals
      (group-by f)
      (map
        (fn [[k v]]
          [k (map :char v)])))))

(defn sum [xs] (reduce + xs))

(defn average [total x] (* (float (/ x total)) 100))

(def hands
  {1 :left
   2 :right})

(def fingers
  {0 :thumb
   1 :index
   2 :middle
   3 :ring
   4 :pinky})

(defn analyze-chars [chars corpus]
  (let [ngrams (extract-ngrams corpus)]
    (->> chars (keep #(get ngrams (str %))) sum (average (count corpus)))))

(defn analyze-hands [mapping corpus]
  (into {}
    (for [[hand chars] (extract-chars-by :hand mapping)]
      [(get hands hand) (analyze-chars chars corpus)])))

(defn analyze-fingers [mapping corpus]
  (into {}
    (for [[[hand finger] chars] (extract-chars-by (juxt :hand :finger) mapping)]
      [[(get hands hand) (get fingers finger)] (analyze-chars chars corpus)])))

(defn extract-bigrams [ngrams]
  (->> ngrams
    (filter (comp #{2} count key))
    (filter (comp (partial apply not=) key))))

(defn analyze-bigrams [mapping corpus]
  (let [ngrams (extract-ngrams corpus)
        bigrams (extract-bigrams ngrams)
        total (->> bigrams (map val) sum)]
    (->>
      bigrams
      (map (fn [[bigram freq]]
             [bigram
              (average total freq)
              (->>
                bigram
                (keep #(get mapping %))
                (map (juxt :hand :finger)))]))
      (filter (fn [[_ _ [a b]]]
                (and a b (= a b))))
      (map (fn [[bigram freq [[hand finger] _]]]
             [bigram freq (get hands hand) (get fingers finger)]))
      (sort-by second >))))

(defn analyze-layout [layout keyboard corpus]
  (let [mapping (map-layout layout keyboard)]
    {:hands (analyze-hands mapping corpus)
     :fingers (analyze-fingers mapping corpus)
     :bigrams (analyze-bigrams mapping corpus)}))

(def analysis-template
  "
finger frequency

          left         right

pinky     %.6f%%    %.6f%%
ring      %.6f%%    %.6f%%
middle    %.6f%%    %.6f%%
index     %.6f%%    %.6f%%
thumb     %.6f%%    %.6f%%

hand      %.6f%%    %.6f%%

")

(defn print-analysis [{:keys [hands fingers bigrams]}]
  (apply printf analysis-template
    (concat
      (map #(get fingers %)
        [[:left :pinky]
         [:right :pinky]
         [:left :ring]
         [:right :ring]
         [:left :middle]
         [:right :middle]
         [:left :index]
         [:right :index]
         [:left :thumb]
         [:right :thumb]])
      (map #(get hands %) [:left :right])))
  (printf "top same finger bigrams\n")
  (doseq [[bigram freq hand finger] (take 5 bigrams)]
    (printf "%s %s %s %.6f%%\n" (name hand) (name finger) bigram freq)))

(comment

  (analyze-layout dvorakish columnar-5x3x1 short-sample)
  (analyze-layout dvorakish columnar-5x3x1 metamorphasis)


  ;; / m u f z   x p g . q
  ;; o d i s c   v n t a r
  ;; e w , y ‘   b h k j l
  ;;             ;
  ;; / m u f z   x p g . q
  ;; o d i s c   v n t a r
  ;;                                       ; w , y ‘   b h k j l
  ;; e


  (print-analysis
    (analyze-layout
      dvorakish
      columnar-5x3x1
      metamorphasis
      ))
  (print-analysis
    (analyze-layout
      dvorakish
      columnar-5x3x1
      short-sample
      )))


(comment

  (/ 15000 10)

  (optimize-layout
    dvorakish
    columnar-5x3x1
    lock-thumbs-and-hkjl
    short-sample
    standard-params)

  (use 'criterium.core)

  (with-progress-reporting
    (quick-bench
      (extract-ngrams metamorphasis)))

  (def moby-dick-chars
    (count moby-dick))

  (def moby-dick-ngrams
    (extract-ngrams moby-dick))

  (energy corpus-ngrams corpus-chars layout)

  (with-progress-reporting
    (quick-bench
      (energy
        metamorphasis-ngrams
        metamorphasis-chars
        layout)))
  (with-progress-reporting
    (quick-bench
      (energy
        moby-dick-ngrams
        moby-dick-chars
        layout)))

  (do
    (extract-ngrams metamorphasis)
    nil)

  (print-layout (optimize-layout layout corpus))
  (print-layout (optimize-layout layout metamorphasis)))

(comment
  (-> layout shuffle-layout print-layout)
  (print-layout layout)

  (get (index-layout
         (enrich-layout layout))
    \h)

  (extract-ngrams corpus)
  (extract-ngrams "bump bump")

  (rand-nth [|])

  (index-layout dvorak)

  (apply println "hello")

  (energy layout corpus)


  (partition 4 1 il corpus)

  (partition 4 1 nil "hi"))

(defn -main [& _]
  (optimize-layout
    dvorakish
    columnar-5x3x1
    lock-thumbs-and-hkjl
    metamorphasis
    standard-params)
  (shutdown-agents))
