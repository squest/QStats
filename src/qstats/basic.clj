(ns qstats.basic
  (:require
    [clojure.core.matrix.dataset :as ds]
    [clojure.core.matrix :as mat]
    [qstats.utils :refer [column-index column-vals]]))

;; Functions related to mean

;; Implementations for mean

(defn- mean-ds
  [ks coll]
  (->> (-> (comp (map #(column-vals coll %))
                 (map #(/ (mat/esum %) (mat/row-count %) 1.0)))
           (sequence ks))
       (zipmap ks)))

(defn- mean-mat
  [ks coll]
  (-> (comp (map #(mat/get-column coll %))
            (map #(/ (mat/esum %) (mat/row-count %) 1.0)))
      (sequence ks)
      vec))

(defn- mean-maps
  [ks coll]
  (->> (map #(select-keys % ks) coll)
       (reduce #(merge-with + % %2))
       (merge-with * (zipmap ks (repeat (count ks) (/ 1.0 (count coll)))))))

(defn mean
  "Returns the mean of a collection, given two arguments returns the means
  of each dimension. Coll can be a list/vector of numbers or list of maps,
  dataset or matrix."
  ([coll] (/ (reduce + coll) (count coll) 1.0))
  ([ks coll]
   (cond (ds/dataset? coll) (mean-ds ks coll)
         (mat/matrix? coll) (mean-mat ks coll)
         :else (mean-maps ks coll))))

;; Functions related to frequencies

;; Implementations details for freq
(defn- freq-impl-maps
  [ks maps]
  (->> (for [k ks]
         (->> (map #(get % k) maps)
              (frequencies)))
       (zipmap ks)))

(defn- freq-impl-ds
  [cols ds]
  (->> (-> (comp (map #(column-vals ds %))
                 (map frequencies))
           (sequence cols))
       (zipmap cols)))

(defn- freq-impl-mat
  [cols mats]
  (-> (comp (map #(mat/get-column mats %))
            (map frequencies))
      (sequence cols)
      vec))

(defn freq
  "When given one argument, it assumes a single dimensional data, and
  returns the result of clojure's frequencies function.
  Given two arguments, the first one can be a collection of maps or
  datasets. Either way it returns the frequencies of each column."
  ([coll] (frequencies coll))
  ([ks coll]
   (cond
     (ds/dataset? coll) (freq-impl-ds ks coll)
     (mat/matrix? coll) (freq-impl-mat ks coll)
     :else (freq-impl-maps ks coll))))

;; Implementations for freq-by

(defn- freq-by-impl-ds-f
  [f cols ds]
  (->> (-> (comp (map #(map f (column-vals ds %)))
                 (map frequencies))
           (sequence cols))
       (zipmap cols)))

(defn- freq-by-impl-ds-fs
  [fs cols ds]
  (->> (-> (comp (map #(map (get fs %) (column-vals ds %)))
                 (map frequencies))
           (sequence cols))
       (zipmap cols)))

(defn- freq-by-impl-maps-fs
  [fs ks maps]
  (->> (for [k ks]
         (->> (map #((get fs k) (get % k)) maps)
              frequencies))
       (zipmap ks)))

(defn- freq-by-impl-maps-f
  [f ks maps]
  (->> (for [k ks]
         (->> (map #(f (get % k)) maps)
              frequencies))
       (zipmap ks)))

(defn- freq-by-impl-mat-f
  [f cols mats]
  (-> (comp (map #(map f (mat/get-column mats %)))
            (map frequencies))
      (sequence cols)
      vec))

(defn- freq-by-impl-mat-fs
  [fs cols mats]
  (-> (comp (map #(map (get fs %) (mat/get-column mats %)))
            (map frequencies))
      (sequence cols)
      vec))

(defn freq-by
  "When given one argument it returns the result of invoking
  (frequencies (map f col)).
  When given two arguments, it will do exactly like one arg but
  applying it to all keys in each map or for every column for dataset.
  The collection can be either list of maps or core.matrix's dataset.
  The fs can be a single function that applied to all supplied keys
  or it can be a map where the key is each key in keys and the val is the function
  to be applied to each of the element for that key."
  ([f coll] (frequencies (map f coll)))
  ([fs ks coll]
   (cond
     (ds/dataset? coll)
     (if (map? fs)
       (freq-by-impl-ds-fs fs ks coll)
       (freq-by-impl-ds-f fs ks coll))
     (mat/matrix? coll)
     (if (map? fs)
       (freq-by-impl-mat-fs fs ks coll)
       (freq-by-impl-mat-f fs ks coll))
     :else
     (if (map? fs)
       (freq-by-impl-maps-fs fs ks coll)
       (freq-by-impl-maps-f fs ks coll)))))

























