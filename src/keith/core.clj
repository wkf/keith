(ns keith.core
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [clojure.tools.cli :as cli])
  (:import (java.awt.datatransfer DataFlavor StringSelection Transferable))
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

(def lock-thumbs
  [1 1 1 1 1   1 1 1 1 1
   1 1 1 1 1   1 1 1 1 1
   1 1 1 1 1   1 1 1 1 1
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

(defn explore-layouts [layouts swaps depth]
  (when-not (zero? depth)
    (let [s (index-swaps swaps)
          ls (for [l layouts
                   i s
                   j s :when (not= i j)]
               (-> l
                 (assoc i (nth l j))
                 (assoc j (nth l i))))]
      (lazy-cat ls (explore-layouts ls swaps (dec depth))))))

(defn explore-layout [layout swaps depth]
  (explore-layouts [layout] swaps depth))

(comment

  (print-layout dvorakish)

  (print-layout
    (first (drop 30 (explore-layout dvorakish lock-thumbs-and-hkjl 2))))

  (print-layout
    (count
      (explore-layout dvorakish lock-thumbs-and-hkjl 2)))

  (index-swaps lock-thumbs-and-hkjl)

  )

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
  (when (and a b (same-hand? a b) (same-finger? a b) (not (same-key? a b))) 10.0))

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
            (or (middle? b) (ring? b))) 5.0
          (and (index? a) (ring? b)) 8.0
          (and (ring? a) (index? b) (top? a)) 5.0
          (and (middle? a) (pinky? b)) 8.0
          (and (pinky? a) (middle? b)) 8.0
          (consecutive-fingers? a b) 8.0
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
  (when (roll-in? a b) -0.4))

(defn roll-reversal-penalty [[a b c _]]
  (when (and a b (same-hand? a b c)
          (or
            (and (middle? a) (pinky? b) (ring? c))
            (and (ring? a) (pinky? b) (middle? c))))
    15.0))

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

(defn evaluate-ngrams [ngrams mapping total]
  (/ (->> ngrams
       (pmap
         (fn [[ngram freq]]
           (penalize-ngram ngram freq mapping)))
       (reduce +))
    total))

(def extract-chars-by
  (memoize
    (fn [f mapping]
      (into {}
        (->> mapping vals
          (group-by f)
          (map
            (fn [[k v]]
              [k (map :char v)])))))))

(defn div [total x] (/ x total))

(defn imbalanced-chars-penalty [chars-by ngrams total]
  (->>
    (for [[_ chars] chars-by]
      (/ (->> chars (keep #(get ngrams (str %))) (reduce +)) total))
    (reduce -)
    abs
    (div 10)
    (+ 1.0)))

(defn imbalanced-hand-penalty [ngrams mapping total]
  (imbalanced-chars-penalty (extract-chars-by :hand mapping) ngrams total))

(defn imbalanced-finger-penalty [ngrams mapping total]
  (reduce *
    (for [[_ chars-by-finger] (group-by ffirst
                                (extract-chars-by (juxt :finger :hand) mapping))]
      (imbalanced-chars-penalty chars-by-finger ngrams total))))

(defn evaluate-balance [ngrams mapping total]
  (*
    (imbalanced-hand-penalty ngrams mapping total)
    (imbalanced-finger-penalty ngrams mapping total)))

(def evaluate-layout
  (memoize
    (fn [layout keyboard corpus]
      (let [mapping (map-layout layout keyboard)
            total (float (count corpus))
            ngrams (extract-ngrams corpus)
            ngram-score (evaluate-ngrams ngrams mapping total)
            balance-factor (evaluate-balance ngrams mapping total)]
        (* ngram-score balance-factor)))))

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

(defn print-progress [_ _ layout score n i]
  (when (or (not n) (zero? (mod i (quot n 10))) (= i n))
    (if n
      (printf "iteration: %d/%d\n" i n)
      (printf "iteration: %d\n" i))
    (printf "score: %f\n" score)
    (print-layout layout)))

(defn discover-layout [initial-layout keyboard swaps corpus params]
  (println "discovering layout")
  (simulate
    #(evaluate-layout % keyboard corpus)
    (comp
      #(swap-layout % swaps)
      #(swap-layout % swaps))
    print-progress
    (shuffle-layout initial-layout swaps)
    params))

(defn refine-layout [initial-layout keyboard swaps corpus depth]
  (println "refining layout")
  (loop [i 1
         optimal-layout initial-layout
         optimal-score (evaluate-layout initial-layout keyboard corpus)]
    (print-progress optimal-layout optimal-score optimal-layout optimal-score nil i)
    (if-let [[new-optimal-layout new-optimal-score]
             (->>
               (for [current-layout (explore-layout optimal-layout swaps 1)
                     :let [current-score (evaluate-layout current-layout keyboard corpus)]
                     :when (< current-score optimal-score)]
                 [current-layout current-score])
               (sort-by second)
               first
               not-empty)]
      (if (< i depth)
        (recur (inc i) new-optimal-layout new-optimal-score)
        new-optimal-layout)
      optimal-layout)))

(defn exhaust-layout [initial-layout keyboard swaps corpus depth]
  (println "exhausting layout")
  (let [layouts (explore-layout initial-layout swaps depth)
        n (dec (count layouts))]
    (->> layouts
      (map-indexed
        (fn [i layout]
          [i layout (evaluate-layout layout keyboard corpus)]))
      (reduce
        (fn [[optimal-layout optimal-score] [i current-layout current-score]]
          (print-progress current-layout current-score optimal-layout optimal-score n i)
          (if (< current-score optimal-score)
            [current-layout current-score]
            [optimal-layout optimal-score]))
        [initial-layout (evaluate-layout initial-layout keyboard corpus)])
      first)))

(def hands
  {1 :left
   2 :right})

(def fingers
  {0 :thumb
   1 :index
   2 :middle
   3 :ring
   4 :pinky})

(defn sum [xs] (reduce + xs))

(defn average [total x] (* (float (/ x total)) 100))

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
    {:layout layout
     :score (evaluate-layout layout keyboard corpus)
     :hands (analyze-hands mapping corpus)
     :fingers (analyze-fingers mapping corpus)
     :bigrams (analyze-bigrams mapping corpus)}))

(def analysis-template
  "
          left         right

pinky     %.6f%%    %.6f%%
ring      %.6f%%    %.6f%%
middle    %.6f%%    %.6f%%
index     %.6f%%    %.6f%%
thumb     %.6f%%    %.6f%%

hand      %.6f%%    %.6f%%

")

(defn print-report [{:keys [layout score hands fingers bigrams] :as report}]
  (println "reporting on layout")
  (printf "score: %f\n" score)
  (print-layout layout)
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
    (printf "%s %s %s %.6f%%\n" (name hand) (name finger) bigram freq))
  (flush)
  report)

(defn spit-report! [report folder]
  (let [time (System/currentTimeMillis)
        file (str folder "/" time "-" (hash report) ".edn")]
    (io/make-parents file)
    (with-open [writer (io/writer file)]
      (binding [*out* writer]
        (pr (assoc report :time time))))))

(defn slurp-reports [folder]
  (->> folder io/file file-seq (filter #(.isFile %)) (map (comp edn/read-string slurp))))

(defn ->clipboard [text]
  (let [selection (StringSelection. text)]
    (.setContents
      (.getSystemClipboard (java.awt.Toolkit/getDefaultToolkit))
      selection
      selection)))

(defn ->json [layout keyboard]
  (let [mapping (map-layout layout keyboard)
        shifted {\' \"
                 \, \!
                 \. \?
                 \/ \@
                 \- \_
                 \y \Y
                 \f \F
                 \g \G
                 \c \C
                 \r \R
                 \z \Z
                 \a \A
                 \o \O
                 \p \P
                 \u \U
                 \i \I
                 \d \D
                 \m \M
                 \t \T
                 \n \N
                 \s \S
                 \; \:
                 \q \Q
                 \w \W
                 \v \V
                 \x \X
                 \b \B
                 \h \H
                 \k \K
                 \j \J
                 \l \L
                 \e \E}
        numbers [\[ \7 \8 \9 \]  nil nil nil nil nil
                 \\ \4 \5 \6 \/  nil nil nil nil nil
                 \= \1 \2 \3 \`  nil nil nil nil nil
                 \0              nil]
        symbols [\{ \& \* \( \}  nil nil nil nil nil
                 \< \$ \% \^ \>  nil nil nil nil nil
                 \+ \| \@ \# \~  nil nil nil nil nil
                 \)              nil]
        ergodox [nil nil nil nil nil nil nil  nil nil nil nil nil nil nil
                 nil 0   1   2   3   4   nil  nil 5   6   7   8   9   nil
                 nil 10  11  12  13  14           15  16  17  18  19  nil
                 nil 20  21  22  23  24  nil  nil 25  26  27  28  29  nil
                 nil nil nil nil nil                  nil nil nil nil nil
                 nil nil 30 nil nil nil           nil nil nil nil nil 31]
        fingers [1  1  2  3  4  4  4   7  7  7  8  9  10 10
                 1  1  2  3  4  4  4   7  7  7  8  9  10 10
                 1  1  2  3  4  4         7  7  8  9  10 10
                 1  1  2  3  4  4  4   7  7  7  8  9  10 10
                 1  1  2  3  4               7  8  9  10 10
                 5  5  5  5  5  5         6  6  6  6  6  6]]
    (json/write-str
      {:label "wkf"
       :author "wkf"
       :authorUrl ""
       :fingerStart {"1" 29
                     "2" 30
                     "3" 31
                     "4" 32
                     "5" 66
                     "6" 75
                     "7" 35
                     "8" 36
                     "9" 37
                     "10" 38
                     "11" -1
                     "false" -1}
       :keyboardType "ergodox"
       :labels {}
       :keys (->
               (map-indexed
                 (fn [i j]
                   (let [c (get layout j)]
                     {:id i
                      :finger
                      (if-let [{:keys [hand finger]} (get mapping c)]
                        (condp = [hand finger]
                          [1 0] 5
                          [1 1] 4
                          [1 2] 3
                          [1 3] 2
                          [1 4] 1
                          [2 0] 6
                          [2 1] 7
                          [2 2] 8
                          [2 3] 9
                          [2 4] 10)
                        (get fingers i -1))
                      :shift (if-let [s (get shifted c)] (int s) -1)
                      :altGr (if-let [a (get numbers j)] (int a) -1)
                      :shiftAltGr (if-let [a (get symbols j)] (int a) -1)
                      :primary (if c (int c) -1)}))
                 ergodox)
               vec
               (assoc-in [64 :primary] 17)
               (assoc-in [65 :primary] 18)
               (assoc-in [67 :primary] 8)
               (assoc-in [70 :primary] -18)
               (assoc-in [71 :primary] 17)
               (assoc-in [20 :primary] 16)
               (assoc-in [21 :primary] -16)
               (assoc-in [74 :primary] 13))})))

(def opts
  [["-o" "--output OUTPUT" "output folder"
    :default "reports"
    :validate [#(.isDirectory (io/file %)) "must be a folder"]]
   ["-r" "--rounds ROUNDS" "number of rounds"
    :default 1
    :parse-fn #(Integer/parseInt %)
    :validate [pos? "must be a positive integer"]]
   ["-c" "--corpus CORPUS" "name of corpus"
    :default "metamorphasis.txt"
    :validate [io/resource "must be a resource"]]
   ["-h" "--help"]])

(defn -main [& args]
  (let [{:keys [options summary errors]} (cli/parse-opts args opts)]
    (cond
      (not-empty errors) (println errors)
      (:help options) (println summary)
      :else (let [rounds (:rounds options)
                  output (:output options)
                  corpus (slurp (io/resource (:corpus options)))
                  layout dvorakish
                  keyboard columnar-5x3x1
                  swaps lock-thumbs
                  depth 100]
              (dotimes [_ rounds]
                (-> layout
                  (discover-layout keyboard swaps corpus standard-params)
                  (refine-layout keyboard swaps corpus depth)
                  (analyze-layout keyboard corpus)
                  print-report
                  (spit-report! output)))
              (shutdown-agents)))))

(comment

  (.isDirectory (io/file (io/file "reports")))


  (->
    dvorakish
    (analyze-layout columnar-5x3x1 short-sample)
    (spit-report! "reports"))

  (def reports (slurp-reports "reports"))

  (last (sort-by :time reports))

  (->clipboard
    (->json
      [\z \f \w \g \' \j \v \. \b \/ \u \n \s \t \m \l \r \o \i \a \y \q \c \d \k \x \h \, \p \; \space \e]
      columnar-5x3x1))

  (->clipboard
    (->json
      um
      columnar-5x3x1))


  (def um
    [\- \. \u \m \q \k \f \c \g \j
     \o \a \i \n \w \p \h \s \t \r
     \' \, \y \l \x \v \d \b \z \;
     \e \space])


  4
  7
  9

  (->
    (sort-by :score reports)
    vec
    (nth 9)
    :layout
    (->json columnar-5x3x1)
    ->clipboard)
  (->
    (sort-by :time (slurp-reports "reports"))
    last
    :layout
    (->json columnar-5x3x1)
    ->clipboard)

  (->clipboard (->json (:layout (second (sort-by :score reports))) columnar-5x3x1))

  (analyze-layout dvorakish columnar-5x3x1 short-sample)
  (analyze-layout dvorakish columnar-5x3x1 metamorphasis)

  (print-report
    (analyze-layout dvorakish columnar-5x3x1 short-sample))

  (print-report
    (analyze-layout dvorakish columnar-5x3x1 metamorphasis))

  (refine-layout dvorakish columnar-5x3x1 lock-thumbs-and-hkjl short-sample 2)

  (-> dvorakish
    (discover-layout columnar-5x3x1 lock-thumbs-and-hkjl short-sample standard-params)
    (refine-layout columnar-5x3x1 lock-thumbs-and-hkjl short-sample 5)
    (analyze-layout columnar-5x3x1 short-sample)
    print-report)

  (use 'criterium.core)

  (with-progress-reporting
    (quick-bench
      (extract-ngrams metamorphasis)))

  )
