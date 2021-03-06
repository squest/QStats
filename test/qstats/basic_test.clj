(ns qstats.basic-test
  (:require
    [clojure.test :refer :all]
    [qstats.basic :refer :all]
    [clojure.core.matrix.dataset :as ds]
    [clojure.core.matrix :as mat]))

(defn round
  "Round a double to the given precision (number of significant digits)"
  [d]
  (let [factor (Math/pow 10 5)]
    (/ (Math/round (* d factor)) factor)))

(defn- prime?
  "Helper function for test"
  [^long p]
  (cond (< p 2) false
        (== p 2) true
        (even? p) false
        :else (let [lim (inc (int (Math/sqrt p)))]
                (loop [i (int 3)]
                  (if (> i lim)
                    true
                    (if (== 0 (rem p i))
                      false
                      (recur (+ 2 i))))))))

(deftest mean-test
  (let [maxi 99
        single-inc (range maxi)
        single-uniform (repeat maxi 50)
        single-steps (range 0 (* 2 maxi) 2)
        single-random (repeatedly maxi (fn [] (rand-int 10)))
        mean-sr (round (/ (reduce + single-random) (count single-random) (double 1.0)))]

    (testing "mean for one-dimensional data"
      (is (== 49 (mean single-inc)))
      (is (== 50 (mean single-uniform)))
      (is (= 98.0 (mean single-steps)))
      (is (== mean-sr (round (mean single-random)))))

    (testing "mean for dataset version"
      (is (= (zipmap [:a :b :d] [49.0 50.0 mean-sr])
             (->> [single-inc single-uniform single-steps single-random]
                  (apply interleave)
                  (partition 4)
                  (ds/dataset [:a :b :c :d])
                  (mean [:a :b :d])
                  (#(zipmap (keys %) (map round (vals %))))))))

    (testing "mean for maps version"
      (is (= (zipmap [:a :b :d] (map double [49.0 50.0 mean-sr]))
             (->> [single-inc single-uniform single-steps single-random]
                  (apply map (fn [a b c d] {:a a :b b :c c :d d}))
                  (mean [:a :b :d])
                  (#(zipmap (keys %) (map round (vals %))))))))

    (testing "mean for matrix version"
      (is (= [49.0 50.0 mean-sr]
             (->> [single-inc single-uniform single-steps single-random]
                  (apply interleave)
                  (partition 4)
                  (mat/matrix)
                  (mean [0 1 3])
                  (mapv round)))))))


(deftest freq-test
  (let [maxi 100
        single-data (->> (fn [] (rand-int 10))
                         (repeatedly maxi)
                         (filter prime?))
        ndim-data (for [i single-data
                        j single-data]
                    [(* 1.0 i j) (+ 1.0 i j) (- i j 0.0) (+ 100.0 i (- 100 j))])
        matrix-version (mat/matrix ndim-data)
        ds-version (ds/dataset [:a :b :c :d] ndim-data)
        maps-version (mapv #(zipmap [:a :b :c :d] %) ndim-data)]

    ;; single-dimensional data test, nothing fancy just frequencies
    (testing "freq fn to one-dimensional data"
      (is (= (freq single-data)
             (frequencies single-data))))

    ;; n-dimensional data for both dataset and list of maps
    (testing "freq fn to n-dimensional data"
      (is (= [:a :c] (keys (freq [:a :c] ds-version))))
      (is (= [:b :d] (keys (freq [:b :d] maps-version))))
      (is (= [(->> (map first ndim-data)
                   (frequencies))
              (->> (map second ndim-data)
                   (frequencies))]
             (-> (freq [:a :b] ds-version)
                 ((juxt :a :b)))))
      (is (= [(->> (map first ndim-data)
                   (frequencies))
              (->> (map second ndim-data)
                   (frequencies))]
             (freq [0 1] matrix-version)))
      (is (= [(->> (map first ndim-data)
                   (frequencies))
              (->> (map second ndim-data)
                   (frequencies))]
             (-> (freq [:a :b] maps-version)
                 ((juxt :a :b)))))))

  (let [maxi 100
        single-data (->> (fn [] (rand-int 10))
                         (repeatedly maxi))
        primes (filter prime? single-data)
        count-primes (count primes)]

    ;; single-dimensional data test
    (testing "freq-by to one-dimensional data"
      (is (= {true count-primes false (- maxi count-primes)}
             (freq-by prime? single-data)))

      (is (= {true 50 false 50}
             (freq-by odd? (range 100))))

      (is (= {0 20 1 20 2 20 3 20 4 20}
             (freq-by #(rem % 5) (range 1 101)))))

    ;; n-dimensional data tests
    (testing "freq-by to n-dimensional data using one fn"
      (is (= {:a {true count-primes false (- maxi count-primes)}
              :c (let [ctr (count (filter prime? (range maxi)))]
                   {true ctr false (- maxi ctr)})}
             (let [dats (-> #(hash-map :a % :b %2 :c %2)
                            (map single-data (range maxi)))]
               (freq-by prime? [:a :c] dats))))

      (is (= {:a {true count-primes false (- maxi count-primes)}
              :c (let [ctr (count (filter prime? (range maxi)))]
                   {true ctr false (- maxi ctr)})}
             (let [dats (->> (interleave single-data (range maxi) (range maxi))
                             (partition 3)
                             (ds/dataset [:a :b :c]))]
               (freq-by prime? [:a :c] dats))))

      (is (= [{true count-primes false (- maxi count-primes)}
              (let [ctr (count (filter prime? (range maxi)))]
                {true ctr false (- maxi ctr)})]
             (let [dats (->> (interleave single-data (range maxi) (range maxi))
                             (partition 3)
                             (mat/matrix))]
               (freq-by prime? [0 2] dats)))))


    (testing "freq-by to n-dimensional data using one map of fs"
      (is (= {:a (let [ctr (->> primes
                                (filter prime?)
                                count)]
                   {true ctr false (- maxi ctr)})
              :b {true 50 false 50}}
             (let [dats (-> #(hash-map :a % :b %2 :c (+ % %2))
                            (map single-data (range maxi)))]
               (-> {:a prime? :b odd?}
                   (freq-by [:a :b] dats)))))

      (is (= {:a {true count-primes false (- maxi count-primes)}
              :b {true 50 false 50}}
             (let [dats (->> (interleave single-data
                                         (range maxi)
                                         (range maxi))
                             (partition 3)
                             (ds/dataset [:a :b :c]))]
               (-> {:a prime? :b #(== 0 (rem % 2))}
                   (freq-by [:a :b] dats)))))

      (is (= [{true count-primes false (- maxi count-primes)}
              {true 50 false 50}]
             (let [dats (->> (interleave single-data
                                         (range maxi)
                                         (range maxi))
                             (partition 3)
                             (mat/matrix))]
               (-> {0 prime? 2 #(== 0 (rem % 2))}
                   (freq-by [0 2] dats))))))))


