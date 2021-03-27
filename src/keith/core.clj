(ns keith.core
  (:require [clojure.java.io :as io])
  (:gen-class))

(def corpus "helloiamacorpusthankyousomuch!")

(def metamorphasis (slurp (io/resource "metamorphasis.txt")))
(def moby-dick (slurp (io/resource "moby-dick.txt")))

(def layout
  ["'" "," "." "/" "y"    "f" "g" "c" "r" "z"
   "a" "o" "p" "u" "i"    "d" "m" "t" "n" "s"
   ";" "q" "w" "v" "x"    "b" "h" "k" "j" "l"
   " "                    "e"])

(def hands
  [1 1 1 1 1   2 2 2 2 2
   1 1 1 1 1   2 2 2 2 2
   1 1 1 1 1   2 2 2 2 2
   0           0])

(def fingers
  [4 3 2 1 1   1 1 2 3 4
   4 3 2 1 1   1 1 2 3 4
   4 3 2 1 1   1 1 2 3 4
   0           0])

(def rows
  [1 1 1 1 1   1 1 1 1 1
   2 2 2 2 2   2 2 2 2 2
   3 3 3 3 3   3 3 3 3 3
   0           0])

(def columns
  [1 2 3 4 5   5 4 3 2 1
   1 2 3 4 5   5 4 3 2 1
   1 2 3 4 5   5 4 3 2 1
   0           0])

(defn mask [m]
  (->> m (map-indexed (fn [i x] (and (pos? x) i))) (filter identity) vec))

(def swaps
  (mask
    [1 1 1 1 1   1 1 1 1 1
     1 1 1 1 1   1 1 1 1 1
     1 1 1 1 1   1 0 0 0 0
     0           0]))

(defn shuffle-layout [layout]
  (let [n (count swaps)
        i (get swaps (rand-int n))
        j (get swaps (rand-int n))]
    (if (= i j)
      (shuffle-layout layout)
      (-> layout
        (assoc i (nth layout j))
        (assoc j (nth layout i))))))

(defn print-layout [layout]
  (apply printf
    "
%s %s %s %s %s   %s %s %s %s %s
%s %s %s %s %s   %s %s %s %s %s
%s %s %s %s %s   %s %s %s %s %s
        %s   %s
"
    layout))

(defn enrich-layout [layout]
  (map-indexed
    (fn [i key]
      {:index i
       :key (first key)
       :hand (get hands i)
       :finger (get fingers i)
       :row (get rows i)
       :column (get columns i)})
    layout))

(defn index-layout [layout]
  (dissoc
    (into {}
      (for [x layout] [(:key x) x]))
    nil))

(def position-penalties
  [3.0 1.0 1.0 1.5 3.0    3.0 1.5 1.0 1.0 3.0
   0.5 0.5 0.0 0.0 1.5    1.5 0.0 0.0 0.5 0.5
   2.0 2.0 1.5 1.0 2.5    2.5 1.0 1.5 2.0 2.0
   0.0                    0.0])

(defn positional-penalty [[a & _]]
  (get position-penalties (:index a)))

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
  (when (and a b (same-finger? a b) (not (same-key? a b))) 5.0))

(defn long-jump-penalty [[a b & _]]
  (when (and a b (long-jump? a b))
    (+ 1.0
      (if (same-hand? a b)
        (cond
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
  (when (roll-in? a b) -0.125))

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
  [positional-penalty
   same-finger-penalty
   long-jump-penalty
   pinky-ring-twist-penalty
   roll-out-penalty
   roll-in-penalty
   roll-reversal-penalty
   long-twist-penalty
   long-jump-sandwich-penalty
   same-hand-penalty
   alternate-hand-penalty])

(defn penalize [layout+ [ngram n]]
  (let [ngram+ (map #(get layout+ %) ngram)]
    (->> penalties (map #(* (or (% ngram+) 0.0) n)) (reduce +))))

(defn extract-ngrams [n corpus]
  (frequencies
    (map reverse
      (concat
        (for [i (range 1 (min (inc (count corpus)) n))]
          (concat
            (take (- n i) (repeat nil))
            (take i corpus)))
        (partition n 1 corpus)))))

(defn energy [ngrams layout]
  (let [layout+ (->> layout
                  enrich-layout
                  index-layout)
        total (->> ngrams
                (map (partial penalize layout+))
                (reduce +))]
    (/ total (count corpus))))

(defn simulate
  ([f g s0 t0 p0 k n]
   (loop [i 0
          s s0
          r s0
          e (f s0)]
     (let [[s e+] (simulate f g s t0 p0 k n i)
           [r e] (if (< e+ e) [s e+] [r e])]
       (when (zero? (mod i 1000))
         (printf "simulation iteration: %d/%d" i n)
         (print-layout r))
       (if (< i n)
         (recur (inc i) s r e)
         r))))
  ([f g s0 t0 p0 k n i]
   (let [e0 (f s0)
         s1 (g s0)
         e1 (f s1)
         de (- e1 e0)]
     (if (neg? de)
       [s1 e1]
       (let [t (Math/exp (- (/ (* i k) n)))
             p (Math/exp (- (/ de t)))]
         (if (< (rand) p) [s1 e1] [s0 e0]))))))

(defn optimize-layout [layout corpus]
  (let [ngrams (extract-ngrams 4 corpus)]
    (simulate
      (partial energy ngrams)
      shuffle-layout
      layout
      1.5
      1.0
      10.0
      15000)))

(comment
  (do
    (extract-ngrams 4 metamorphasis)
    nil)

  (print-layout (optimize-layout layout corpus))
  (print-layout (optimize-layout layout metamorphasis)))

(comment
  (-> layout shuffle-layout print-layout)
  (print-layout layout)

  (get (index-layout
         (enrich-layout layout))
    \h)

  (extract-ngrams 4 corpus)
  (extract-ngrams 3 "bump bump")

  (rand-nth [|])

  (index-layout dvorak)

  (apply println "hello")

  (energy layout corpus)


  (partition 4 1 il corpus)

  (partition 4 1 nil "hi"))

(defn -main [& _]
  (optimize-layout layout metamorphasis))
