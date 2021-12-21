(ns malli-forms.util-test
  (:require
    [clojure.test :as test :refer [deftest is]]
    [malli-forms.util :as util]))

(deftest munge-name-part-test
  (is (= "some%2Ffield"
         (#'util/munge-name-part "some/field")
         (#'util/munge-name-part :some/field)))
  (is (= "some%3Cfield"
         (#'util/munge-name-part "some<field")))
  (is (= "some%3Efield"
         (#'util/munge-name-part "some>field")))
  (is (= "some_DOT_namespaced%2Fkeyword"
         (#'util/munge-name-part :some.namespaced/keyword))))

(deftest path->name-test
  (is (= "my-map[some%2Ffield]"
         (util/path->name [:my-map :some/field])))
  (is (= "something" (util/path->name [:something]))))
