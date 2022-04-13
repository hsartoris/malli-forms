(ns malli-forms.test.benchmark
  (:require
    [clojure.test :refer [deftest is]]
    [criterium.core :as cc]
    [malli-forms :as mf]
    [malli-forms.test-schemas :refer [asn-pool-schema]]))

(defn bench-add-field-specs
  "Benchmark calling mf/add-field-specs on a schema"
  []
  (cc/quick-bench (mf/add-field-specs asn-pool-schema)))

;(deftest add-field-specs
;  (let [target-mean   1.1E-4 ;; 110us
;        target-stddev 2.5E-6 ;; ~ 2.5us
;        res (cc/quick-benchmark (mf/add-field-specs asn-pool-schema) {})]
;    (is (< (first (:mean res)) target-mean)
;        "Execution time mean should be lower than target")
;    (is (< (Math/sqrt (first (:variance res))) target-stddev)
;        "Execution time standard deviation should be lower than target")))

(defmacro defbench
  "Emit a deftest block to benchmark `expr`, ensuring it takes less than
  `mean` time to run on average, and that the standard deviation of its
  execution time is less than `std-dev` on average, when provided."
  ([#_:clj-kondo/ignore name expr mean] `(defbench ~name ~expr ~mean nil))
  ([#_:clj-kondo/ignore name expr mean std-dev]
   (let [result (gensym 'result)]
     `(deftest ~name
        (let [~result (cc/quick-benchmark ~expr {})]
          (is (< (first (:mean ~result)) ~mean)
              "Execution time mean should be lower than target")
          ~(when std-dev
             `(is (< (Math/sqrt (first (:variance ~result))) ~std-dev)
                  "Execution time standard deviation should be lower than target")))))))

(defbench add-field-specs
  (mf/add-field-specs asn-pool-schema)
  1.1e-4 2.5e-6)
