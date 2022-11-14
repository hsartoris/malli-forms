(ns eftest-runner
  (:require [eftest.runner :as ef]))

(defn run-tests
  "Execute eftest. Single function so can be called from deps.edn"
  [_args]
  (ef/run-tests (ef/find-tests "test")))
