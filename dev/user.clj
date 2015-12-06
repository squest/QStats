(ns user)

(defn dev []
  (in-ns 'qstats.basic))

(defn run-all-test []
  (clojure.test/run-all-tests))
